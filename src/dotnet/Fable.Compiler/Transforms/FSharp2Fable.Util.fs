namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Core
open Fable.AST
open Fable.Transforms

type EnclosingMemberContext =
    | ImplicitConstructor
    | SecondaryConstructor of entityConstructorFullName: string
    | InterfaceImplementation of interfaceFullName: string option
    | UnknownMember

type Context =
    { Scope: (FSharpMemberOrFunctionOrValue * Fable.Expr) list
      ScopeInlineValues: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      /// Some expressions that create scope in F# don't do it in JS (like let bindings)
      /// so we need a mutable registry to prevent duplicated var names.
      VarNames: HashSet<string>
      GenericArgs: Map<string, Fable.Type>
      EnclosingMember: EnclosingMemberContext
      CaughtException: Fable.Ident option
      BoundThis: Fable.Ident option
    }
    static member Create() =
        { Scope = []
          ScopeInlineValues = []
          VarNames = HashSet()
          GenericArgs = Map.empty
          EnclosingMember = UnknownMember
          CaughtException = None
          BoundThis = None
        }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context * FSharpExpr -> Fable.Expr
    abstract TryReplace: Context * SourceLocation option * Fable.Type * info: Fable.CallInfo
        * thisArg: Fable.Expr option * args: Fable.Expr list -> Fable.Expr option
    abstract TryReplaceInterfaceCast: Fable.Type * interfaceName: string * Fable.Expr -> Fable.Expr option
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract AddUsedVarName: string -> unit
    abstract IsUsedVarName: string -> bool

module Helpers =
    let tryBoth (f1: 'a->'b option) (f2: 'a->'b option) (x: 'a) =
        match f1 x with
        | Some _ as res -> res
        | None ->
            match f2 x with
            | Some _ as res -> res
            | None -> None

    let rec nonAbbreviatedType (t: FSharpType) =
        if t.IsAbbreviation then nonAbbreviatedType t.AbbreviatedType else t

    let getEntityName (ent: FSharpEntity) =
        ent.CompiledName.Replace('`', '_')

    // TODO: Report bug in FCS repo, when ent.IsNamespace, FullName doesn't work.
    let getEntityFullName (ent: FSharpEntity) =
        if ent.IsNamespace
        then match ent.Namespace with Some ns -> ns + "." + ent.CompiledName | None -> ent.CompiledName
        else defaultArg ent.TryFullName ent.CompiledName

    let tryGetEntityLocation (ent: FSharpEntity) =
        // Make sure the type doesn't come from a referenced assembly
        match ent.Assembly.FileName with
        | None -> ent.ImplementationLocation
        | Some _ -> None

    let getMemberLocation (memb: FSharpMemberOrFunctionOrValue) =
        match memb.ImplementationLocation with
        | Some loc -> loc
        | None -> memb.DeclarationLocation

    let private getEntityMangledName (com: ICompiler) trimRootModule (ent: FSharpEntity) =
        match ent.TryFullName, tryGetEntityLocation ent with
        | Some fullName, _ when not trimRootModule -> fullName
        | Some fullName, Some loc ->
            let rootMod = com.GetRootModule(loc.FileName)
            if fullName.StartsWith(rootMod)
            then fullName.Substring(rootMod.Length).TrimStart('.')
            else fullName
        | _ -> ent.CompiledName

    let getEntityDeclarationName (com: ICompiler) (ent: FSharpEntity) =
        (getEntityMangledName com true ent, Naming.NoMemberPart)
        ||> Naming.sanitizeIdent (fun _ -> false)

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some Types.unit
        else false

    let findOverloadIndex (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
        let argsEqual (args1: FSharpParameter[]) (args2: FSharpParameter[]) =
            if args1.Length = args2.Length
            // Checking equality of FSharpParameter seems to be fast
            // See https://github.com/fsharp/FSharp.Compiler.Service/blob/0d87c8878c14ab2b283fbd696096e4e4714fab6b/src/fsharp/symbols/Symbols.fs#L2145
            then (args1, args2) ||> Array.forall2 (=)
            else false
        let flattenParams (m: FSharpMemberOrFunctionOrValue) =
            match m.CurriedParameterGroups |> Seq.concat |> Seq.toArray with
            // (?) Sometimes .CurriedParameterGroups contains the unit arg and sometimes doesn't
            | [|arg|] when isUnit arg.Type -> [||]
            | args -> args
        if m.IsImplicitConstructor || m.IsOverrideOrExplicitInterfaceImplementation
        then 0
        else
            // m.Overloads(false) doesn't work
            let name = m.CompiledName
            let isInstance = m.IsInstanceMember
            let parameters = flattenParams m
            let index, _found =
                ((0, false), entity.MembersFunctionsAndValues)
                ||> Seq.fold (fun (i, found) m2 ->
                    if not found && m2.IsInstanceMember = isInstance && m2.CompiledName = name then
                        // .Equals() doesn't work.
                        // .IsEffectivelySameAs() doesn't work for constructors
                        if argsEqual parameters (flattenParams m2)
                        then i, true
                        else i + 1, false
                    else i, found)
            // TODO: Log error if not found?
            index

    let getCastDeclarationName com (implementingEntity: FSharpEntity) (interfaceEntityFullName: string) =
        let entityName = getEntityMangledName com true implementingEntity
        let memberPart = Naming.StaticMemberPart(interfaceEntityFullName, None)
        Naming.sanitizeIdent (fun _ -> false) entityName memberPart

    let private getMemberMangledName (com: ICompiler) trimRootModule (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        | Some ent when ent.IsFSharpModule ->
            match getEntityMangledName com trimRootModule ent with
            | "" -> memb.CompiledName, Naming.NoMemberPart
            | moduleName -> moduleName, Naming.StaticMemberPart(memb.CompiledName, None)
        | Some ent ->
            let overloadIndex =
                match findOverloadIndex ent memb with
                | 0 -> None
                | i -> Some i
            let entName = getEntityMangledName com trimRootModule ent
            if memb.IsInstanceMember
            then entName, Naming.InstanceMemberPart(memb.CompiledName, overloadIndex)
            else entName, Naming.StaticMemberPart(memb.CompiledName, overloadIndex)
        | None -> memb.CompiledName, Naming.NoMemberPart

    let getMemberDeclarationName (com: ICompiler) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberMangledName com true memb
        ||> Naming.sanitizeIdent (fun _ -> false)

    /// Used to identify members uniquely in the inline expressions dictionary
    let getMemberUniqueName (com: ICompiler) (memb: FSharpMemberOrFunctionOrValue): string =
        getMemberMangledName com false memb
        ||> Naming.buildNameWithoutSanitation

    /// TODO: Latest FCS seems to add get_/set_ to DisplayName. Bug or feature?
    let getMemberDisplayName (memb: FSharpMemberOrFunctionOrValue) =
        Naming.removeGetSetPrefix memb.DisplayName

    let tryFindAtt fullName (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName' ->
                if fullName = fullName' then Some att else None
            | None -> None)

    let tryDefinition (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then Some typ.TypeDefinition else None

    let getFsTypeFullName (typ: FSharpType) =
        match tryDefinition typ with
        | Some tdef -> defaultArg tdef.TryFullName Naming.unknown
        | None -> Naming.unknown

    let isInline (memb: FSharpMemberOrFunctionOrValue) =
        match memb.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        // TODO: Add compiler option to inline also `OptionalInline`
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let isPublicEntity (ent: FSharpEntity) =
        not ent.Accessibility.IsPrivate

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let makeRange (r: Range.range) =
        { start = { line = r.StartLine; column = r.StartColumn }
          ``end``= { line = r.EndLine; column = r.EndColumn } }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    // let hasCaseWithFields (ent: FSharpEntity) =
    //     ent.UnionCases |> Seq.exists (fun uci -> uci.UnionCaseFields.Count > 0)

    let unionCaseTag (ent: FSharpEntity) (unionCase: FSharpUnionCase) =
        let name = unionCase.Name
        ent.UnionCases
        |> Seq.tryFindIndex (fun uci -> name = uci.Name)
        |> function
            | Some i -> i
            | None -> failwithf "Cannot find case %s in %s" name (getEntityFullName ent)

    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    let unionCaseCompiledName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt Atts.compiledName
        |> Option.map (fun att -> att.ConstructorArguments.[0] |> snd |> string)

    /// Apply case rules to case name if there's no explicit compiled name
    let applyCaseRule (rule: CaseRules) (unionCase: FSharpUnionCase) =
        match unionCaseCompiledName unionCase with
        | Some name -> name
        | None ->
            match rule with
            | CaseRules.LowerFirst -> Naming.lowerFirst unionCase.Name
            | CaseRules.None | _ -> unionCase.Name
        |> makeStrConst

    let isModuleMember (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        | Some ent -> ent.IsFSharpModule
        | None -> true // Compiler-generated members

    /// Using memb.IsValue doesn't work for function values
    /// (e.g. `let ADD = adder()` when adder returns a function)
    let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
        memb.CurriedParameterGroups.Count = 0 // && memb.GenericParameters.Count = 0

    let isModuleValueForCalls (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValueForDeclarations memb
        // Mutable public values must be called as functions (see #986)
        && (not memb.IsMutable || not (isPublicMember memb))

    let isSiblingConstructorCall (ctx: Context) (memb: FSharpMemberOrFunctionOrValue) =
        match memb.IsConstructor, ctx.EnclosingMember with
        | true, SecondaryConstructor fullName ->
            let returnType = memb.ReturnParameter.Type
            if returnType.HasTypeDefinition
            then returnType.TypeDefinition.TryFullName = Some fullName
            else false
        | _ -> false

    let hasSeqSpread (memb: FSharpMemberOrFunctionOrValue) =
        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then false else
            let args = memb.CurriedParameterGroups.[0]
            args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.bind (fun lastParam -> tryFindAtt Atts.paramList lastParam.Attributes)
            |> Option.isSome

        hasParamArray memb || hasParamSeq memb

module Patterns =
    open BasicPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) ctx e = com.Transform(ctx, e)
    let inline (|FieldName|) (fi: FSharpField) = fi.Name

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    /// DOES NOT check if the type is abbreviated, mainly intended to identify Fable.Core.Applicable
    let (|FSharpExprTypeFullName|_|) (e: FSharpExpr) =
        let t = e.Type
        if t.HasTypeDefinition then t.TypeDefinition.TryFullName else None

    let (|MemberFullName|) (memb: FSharpMemberOrFunctionOrValue) =
        memb.FullName

    let (|AttFullName|_|) (att: FSharpAttribute) =
        match att.AttributeType.TryFullName with
        | Some fullName -> Some(fullName, att)
        | None -> None

    let (|AttArguments|) (att: FSharpAttribute) =
        att.ConstructorArguments |> Seq.map snd |> Seq.toList

    let (|RefType|_|) = function
        | TypeDefinition tdef as t when tdef.TryFullName = Some Types.reference -> Some t
        | _ -> None

    let (|ForOf|_|) = function
        | Let((_, value), // Coertion to seq
              Let((_, Call(None, meth, _, [], [])),
                TryFinally(
                  WhileLoop(_,
                    Let((ident, _), body)), _)))
        | Let((_, Call(Some value, meth, _, [], [])),
                TryFinally(
                    WhileLoop(_,
                        Let((ident, _), body)), _))
            // Using only the compiled name is riskier but with the fullname we miss some cases
            // TODO: Check the return type of meth is or implements IEnumerator
            when meth.CompiledName = "GetEnumerator" ->
            // when meth.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" ->
            Some(ident, value, body)
        | _ -> None

    let (|PrintFormat|_|) fsExpr =
        match fsExpr with
        | Let((v,(Call(None,_,_,_,args) as e)),_) when v.IsCompilerGenerated ->
            match List.tryLast args with
            | Some arg ->
                if arg.Type.HasTypeDefinition
                    && arg.Type.TypeDefinition.AccessPath = Types.printfModule
                then Some e
                else None
            | None -> None
        | _ -> None

    /// This matches the boilerplate F# compiler generates for methods
    /// like Dictionary.TryGetValue (see #154)
    let (|TryGetValue|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                NewTuple(_, [Call(callee, memb, typArgs, methTypArgs,
                                    [arg; AddressOf(Value outArg2)]); Value outArg3]))
            when outArg1 = outArg2 && outArg1 = outArg3 ->
            Some (callee, memb, typArgs, methTypArgs, [arg; def])
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) = function
        | Call(Some(Call(None, createEvent,_,_,
                        [Lambda(_eventDelegate, Call(Some callee, addEvent,[],[],[Value _eventDelegate']));
                         Lambda(_eventDelegate2, Call(Some _callee2, _removeEvent,[],[],[Value _eventDelegate2']));
                         Lambda(_callback, NewDelegate(_, Lambda(_delegateArg0, Lambda(_delegateArg1, Application(Value _callback',[],[Value _delegateArg0'; Value _delegateArg1'])))))])),
                memb, typArgs, methTypArgs, args)
                when createEvent.FullName = Types.createEvent ->
            let eventName = addEvent.CompiledName.Replace("add_","")
            Some (callee, eventName, memb, typArgs, methTypArgs, args)
        | _ -> None

    let private numberTypes =
        dict [Types.int8, Int8
              Types.uint8, UInt8
              Types.int16, Int16
              Types.uint16, UInt16
              Types.int32, Int32
              Types.uint32 , UInt32
              Types.float32, Float32
              Types.float64, Float64
              Types.decimal, Decimal
               // Units of measure
              "Microsoft.FSharp.Core.sbyte`1", Int8
              "Microsoft.FSharp.Core.int16`1", Int16
              "Microsoft.FSharp.Core.int`1", Int32
              "Microsoft.FSharp.Core.float32`1", Float32
              "Microsoft.FSharp.Core.float`1", Float64
              "Microsoft.FSharp.Core.decimal`1", Decimal]

    let (|NumberKind|_|) fullName =
        match numberTypes.TryGetValue(fullName) with
        | true, kind -> Some kind
        | false, _ -> None

    let (|OptionUnion|ListUnion|ErasedUnion|StringEnum|DiscriminatedUnion|) (NonAbbreviatedType typ: FSharpType) =
        match tryDefinition typ with
        | None -> failwith "Union without definition"
        | Some tdef ->
            match defaultArg tdef.TryFullName tdef.CompiledName with
            | Types.option -> OptionUnion typ.GenericArguments.[0]
            | Types.list -> ListUnion typ.GenericArguments.[0]
            | _ ->
                tdef.Attributes |> Seq.tryPick (fun att ->
                    match att.AttributeType.TryFullName with
                    | Some Atts.erase -> Some (ErasedUnion(tdef, typ.GenericArguments))
                    | Some Atts.stringEnum ->
                        match Seq.tryHead att.ConstructorArguments with
                        | Some(_, (:? int as rule)) -> Some (StringEnum(tdef, enum<CaseRules>(rule)))
                        | _ -> Some (StringEnum(tdef, CaseRules.LowerFirst))
                    | _ -> None)
                |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))

    let (|ContainsAtt|_|) (fullName: string) (ent: FSharpEntity) =
        tryFindAtt fullName ent.Attributes

module TypeHelpers =
    open Helpers
    open Patterns

    let resolveGenParam ctxTypeArgs (genParam: FSharpGenericParameter) =
        match Map.tryFind genParam.Name ctxTypeArgs with
        | None -> Fable.GenericParam genParam.Name
        | Some typ -> typ

    let rec makeGenArgs (com: ICompiler) ctxTypeArgs (genArgs: IList<FSharpType>) =
        genArgs |> Seq.map (fun genArg ->
            if genArg.IsGenericParameter
            then resolveGenParam ctxTypeArgs genArg.GenericParameter
            else makeType com ctxTypeArgs genArg)
        |> Seq.toList

    and makeTypeFromDelegate com ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) (fullName: string) =
        if fullName.StartsWith("System.Action") then
            let argTypes =
                match Seq.tryHead genArgs with
                | Some genArg -> [makeType com ctxTypeArgs genArg]
                | None -> [Fable.Unit]
            Fable.FunctionType(Fable.DelegateType argTypes, Fable.Unit)
        elif fullName.StartsWith("System.Func") then
            let argTypes, returnType =
                match genArgs.Count with
                | 0 -> [Fable.Unit], Fable.Unit
                | 1 -> [Fable.Unit], makeType com ctxTypeArgs genArgs.[0]
                | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com ctxTypeArgs) |> Seq.toList,
                        makeType com ctxTypeArgs genArgs.[c-1]
            Fable.FunctionType(Fable.DelegateType argTypes, returnType)
        else
            try
                let argTypes =
                    tdef.FSharpDelegateSignature.DelegateArguments
                    |> Seq.map (snd >> makeType com ctxTypeArgs) |> Seq.toList
                let returnType =
                    makeType com ctxTypeArgs tdef.FSharpDelegateSignature.DelegateReturnType
                Fable.FunctionType(Fable.DelegateType argTypes, returnType)
            with _ -> // TODO: Log error here?
                Fable.FunctionType(Fable.DelegateType [Fable.Any], Fable.Any)

    and makeTypeFromDef (com: ICompiler) ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        match getEntityFullName tdef, tdef with
        | _ when tdef.IsArrayType -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Array
        | fullName, _ when tdef.IsEnum -> Fable.EnumType(Fable.NumberEnumType, fullName)
        | fullName, _ when tdef.IsDelegate -> makeTypeFromDelegate com ctxTypeArgs genArgs tdef fullName
        // Fable "primitives"
        | Types.object, _ -> Fable.Any
        | Types.unit, _ -> Fable.Unit
        | Types.bool, _ -> Fable.Boolean
        | Types.char, _ -> Fable.Char
        | Types.string, _ -> Fable.String
        | Types.regex, _ -> Fable.Regex
        | Types.option, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Option
        | Types.resizeArray, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Array
        | Types.list, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.List
        | NumberKind kind, _ -> Fable.Number kind
        // Special attributes
        | fullName, ContainsAtt Atts.stringEnum _ -> Fable.EnumType(Fable.StringEnumType, fullName)
        | _, ContainsAtt Atts.erase _ -> makeGenArgs com ctxTypeArgs genArgs |> Fable.ErasedUnion
        // Rest of declared types
        | _ -> Fable.DeclaredType(tdef, makeGenArgs com ctxTypeArgs genArgs)

    and makeType (com: ICompiler) (ctxTypeArgs: Map<string, Fable.Type>) (NonAbbreviatedType t) =
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam ctxTypeArgs t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then makeGenArgs com ctxTypeArgs t.GenericArguments |> Fable.Tuple
        // Funtion
        elif t.IsFunctionType
        then
            let argType = makeType com ctxTypeArgs t.GenericArguments.[0]
            let returnType = makeType com ctxTypeArgs t.GenericArguments.[1]
            Fable.FunctionType(Fable.LambdaType argType, returnType)
        elif t.HasTypeDefinition
        then makeTypeFromDef com ctxTypeArgs t.GenericArguments t.TypeDefinition
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    // TODO: This is intended to wrap JS expressions with `| 0`, check enum as well?
    let isSignedIntType (NonAbbreviatedType t) =
        if t.HasTypeDefinition then
            match t.TypeDefinition.TryFullName with
            | Some(Types.int8 | Types.int16 | Types.int32) -> true
            | _ -> false
        else false

    let getBaseClass (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.TryFullName <> Some Types.object ->
            Some tdef
        | _ -> None

    let rec getOwnAndInheritedFsharpMembers (tdef: FSharpEntity) = seq {
        yield! tdef.TryGetMembersFunctionsAndValues
        match tdef.BaseType with
        | Some(TypeDefinition baseDef) when tdef.TryFullName <> Some Types.object ->
            yield! getOwnAndInheritedFsharpMembers baseDef
        | _ -> ()
    }

    let getArgTypes com (memb: FSharpMemberOrFunctionOrValue) =
        // FSharpParameters don't contain the `this` arg
        Seq.concat memb.CurriedParameterGroups
        // The F# compiler "untuples" the args in methods
        |> Seq.map (fun x -> makeType com Map.empty x.Type)
        |> Seq.toList

    let isAbstract (ent: FSharpEntity) =
       tryFindAtt Atts.abstractClass ent.Attributes |> Option.isSome

    let tryGetInterfaceTypeFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0
        then nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType |> Some
        else None

    let tryGetInterfaceDefinitionFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0 then
            let t = nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType
            if t.HasTypeDefinition then Some t.TypeDefinition else None
        else None

    let tryFindMember com (entity: FSharpEntity) genArgs membCompiledName isInstance (argTypes: Fable.Type list) =
        let argsEqual (args1: Fable.Type list) args1Length (args2: IList<IList<FSharpParameter>>) =
                let args2Length = args2 |> Seq.sumBy (fun g -> g.Count)
                if args1Length = args2Length then
                    let args2 = args2 |> Seq.collect (fun g ->
                        g |> Seq.map (fun p -> makeType com genArgs p.Type) |> Seq.toList)
                    listEquals typeEquals args1 (Seq.toList args2)
                else false
        let argTypesLength = List.length argTypes
        getOwnAndInheritedFsharpMembers entity |> Seq.tryFind (fun m2 ->
            if m2.IsInstanceMember = isInstance && m2.CompiledName = membCompiledName
            then argsEqual argTypes argTypesLength m2.CurriedParameterGroups
            else false)

    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.GenericArgs t

module Identifiers =
    open Helpers
    open TypeHelpers

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with Scope = (fsRef, expr)::ctx.Scope}

    let makeIdentFrom (com: IFableCompiler) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue): Fable.Ident =
        let sanitizedName = (fsRef.CompiledName, Naming.NoMemberPart)
                            ||> Naming.sanitizeIdent (fun x -> ctx.VarNames.Contains(x) || com.IsUsedVarName(x))
        ctx.VarNames.Add sanitizedName |> ignore
        // Track all used var names in the file so they're not used for imports
        com.AddUsedVarName sanitizedName
        { Name = sanitizedName
          Type = makeType com ctx.GenericArgs fsRef.FullType
          IsMutable = fsRef.IsMutable
          IsThisArg = fsRef.IsMemberThisValue
          IsCompilerGenerated = fsRef.IsCompilerGenerated
          Range = makeRange fsRef.DeclarationLocation |> Some }

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        bindExpr ctx fsRef (Fable.IdentExpr ident), ident

    let (|BindIdent|) com ctx fsRef = bindIdentFrom com ctx fsRef

    /// Get corresponding identifier to F# value in current scope
    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        match List.tryFind (fun (fsRef',_)  -> obj.Equals(fsRef, fsRef')) ctx.Scope with
        | Some(_, Fable.IdentExpr ident) -> { ident with Range = r } |> Fable.IdentExpr |> Some
        | Some(_, boundExpr) -> Some boundExpr
        | None -> None

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = bindIdentFrom com ctx var
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (args: FSharpMemberOrFunctionOrValue list list) =
        // To prevent name clashes in JS create a scope for members
        // where variables must always have a unique name
        let ctx = { ctx with VarNames = HashSet(ctx.VarNames) }
        let ctx, transformedArgs, args =
            match args with
            // Within private members (first arg is ConstructorThisValue) F# AST uses
            // ThisValue instead of Value (with .IsMemberConstructorThisValue = true)
            | (firstArg::restArgs1)::restArgs2 when firstArg.IsConstructorThisValue ->
                let ctx, thisArg = bindIdentFrom com ctx firstArg
                { ctx with BoundThis = Some thisArg }, [thisArg], restArgs1::restArgs2
            | _ -> ctx, [], args
        let ctx, args =
            (args, (ctx, [])) ||> List.foldBack (fun tupledArg (ctx, accArgs) ->
                // The F# compiler "untuples" the args in methods
                let ctx, untupledArg = makeFunctionArgs com ctx tupledArg
                ctx, untupledArg@accArgs)
        ctx, transformedArgs @ args

    let makeTryCatch com ctx (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                // Add caughtException to context so it can be retrieved by `reraise`
                let catchContext = { catchContext with CaughtException = Some catchVar }
                Some (catchVar, com.Transform(catchContext, catchBody))
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer)

    let matchGenericParams (genArgs: Fable.Type seq) (genParams: FSharpGenericParameter seq) =
        // TODO: Log error if they've different lengths
        Seq.zip (genParams |> Seq.map (fun x -> x.Name)) genArgs

    let matchGenericParamsFrom (memb: FSharpMemberOrFunctionOrValue) (genArgs: Fable.Type seq) =
        match memb.DeclaringEntity with
        | Some ent -> Seq.append ent.GenericParameters memb.GenericParameters
        | None -> upcast memb.GenericParameters
        |> matchGenericParams genArgs

    /// Takes only the first CurriedParameterGroup into account.
    /// If there's only a single unit parameter, returns 0.
    let countNonCurriedParams (meth: FSharpMemberOrFunctionOrValue) =
        let args = meth.CurriedParameterGroups
        if args.Count = 0 then 0
        elif args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args.[0].Count

    /// Same as `countNonCurriedParams` but applied to overrides
    let countNonCurriedParamsForOverride (over: FSharpObjectExprOverride) =
        let args = over.CurriedParameterGroups
        match args with
        | [] | [_] -> 0
        // Unlike FSharpMemberOrFunctionOrValue.CurriedParameterGroups
        // overrides DO include the name self instance as parameter
        | _thisArg::firsGroup::_ ->
            match firsGroup with
            | [arg] ->
                match arg.FullTypeSafe with
                | Some t when isUnit t -> 0
                | _ -> 1
            | args -> List.length args

    // When importing a relative path from a different path where the member,
    // entity... is declared, we need to resolve the path
    let fixImportedRelativePath (com: ICompiler) (path: string) (loc: Lazy<Range.range option>) =
        if path.StartsWith(".") then
            match loc.Value with
            | Some loc ->
                let file = Path.normalizePath loc.FileName
                if file = com.CurrentFile
                then path
                else
                    Path.Combine(Path.GetDirectoryName(file), path)
                    |> Path.getRelativePath com.CurrentFile
            | None -> path // TODO: Log error
        else path

    let tryImportAttribute (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (function
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                Some(selector.Trim(), path.Trim())
            | _ -> None)

    /// Function used to check if calls must be replaced by global idents or direct imports
    let tryGlobalOrImportedMember com typ (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes |> Seq.tryPick (function
            | AttFullName(Atts.global_, att) ->
                match att with
                | AttArguments [:? string as customName] ->
                    makeTypedIdent typ customName |> Fable.IdentExpr |> Some
                | _ -> makeTypedIdent typ memb.CompiledName |> Fable.IdentExpr |> Some
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                let path =
                    lazy (getMemberLocation memb |> Some)
                    |> fixImportedRelativePath com path
                makeCustomImport typ selector path |> Some
            | _ -> None)

    let tryImportedEntity (com: ICompiler) (ent: FSharpEntity) =
        // TODO: Check also Global attribute here?
        tryImportAttribute ent.Attributes |> Option.map (fun (selector, path) ->
            let path =
                lazy tryGetEntityLocation ent
                |> fixImportedRelativePath com path
            makeCustomImport Fable.Any selector path)

    let tryEntityRef (com: ICompiler) (ent: FSharpEntity) =
        tryGetEntityLocation ent
        |> Option.map (fun entLoc ->
            let file = Path.normalizePath entLoc.FileName
            let entityName = getEntityDeclarationName com ent
            if file = com.CurrentFile
            then makeIdent entityName |> Fable.IdentExpr
            else makeInternalImport Fable.Any entityName file)

    let entityRef (com: ICompiler) r (ent: FSharpEntity) =
        match tryEntityRef com ent with
        | Some entRef -> entRef
        | None ->
            "Cannot find implementation location for entity: " + (getEntityFullName ent)
            |> addErrorAndReturnNull com r

    /// First checks if the entity is imported
    let entityRefMaybeImported (com: ICompiler) r (ent: FSharpEntity) =
        match tryImportedEntity com ent with
        | Some importedEntity -> importedEntity
        | None -> entityRef com r ent

    let tryEntityRefMaybeImported (com: ICompiler) (ent: FSharpEntity) =
        tryImportedEntity com ent
        |> Option.orElseWith (fun () -> tryEntityRef com ent)

    let private memberRefPrivate (com: IFableCompiler) r typ (entity: FSharpEntity option) memberName =
        let file, entityFullName =
            match entity with
            | Some ent ->
                tryGetEntityLocation ent
                |> Option.map (fun loc -> Path.normalizePath loc.FileName),
                getEntityFullName ent
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> Some com.CurrentFile, Naming.unknown
        match file with
        | Some file when file = com.CurrentFile ->
            makeTypedIdent typ memberName |> Fable.IdentExpr
        | Some file -> makeInternalImport typ memberName file
        | None -> sprintf "Cannot find implementation location for member: %s (%s)" memberName entityFullName
                  |> addErrorAndReturnNull com r

    let memberRefTyped (com: IFableCompiler) r typ (memb: FSharpMemberOrFunctionOrValue) =
        getMemberDeclarationName com memb
        |> memberRefPrivate com r typ memb.DeclaringEntity

    let memberRef (com: IFableCompiler) r (memb: FSharpMemberOrFunctionOrValue) =
        memberRefTyped com r Fable.Any memb

    let castToInterface (com: IFableCompiler) t (sourceEntity: FSharpEntity) interfaceFullName expr =
        if sourceEntity.IsInterface
        then expr
        else
          match com.TryReplaceInterfaceCast(t, interfaceFullName, expr) with
          | Some expr -> expr
          | None ->
            sourceEntity.DeclaredInterfaces
            |> Seq.tryFind (fun (NonAbbreviatedType t) ->
                t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some interfaceFullName)
            |> function
                // TODO!!!: Interface must be implemented by a parent type
                | None -> expr
                // If the interface has no members, cast is not necessary
                | Some t when t.TypeDefinition.MembersFunctionsAndValues.Count = 0 -> expr
                | Some _ ->
                    match tryGetEntityLocation sourceEntity with
                    | None -> expr
                    | Some entLoc ->
                        let file = Path.normalizePath entLoc.FileName
                        let funcName = getCastDeclarationName com sourceEntity interfaceFullName
                        if file = com.CurrentFile
                        then makeIdent funcName |> Fable.IdentExpr
                        else makeInternalImport Fable.Any funcName file
                        |> staticCall None t (argInfo None [expr] None)

    let callInstanceMember com r typ (genArgs: Lazy<_>) (argInfo: Fable.ArgInfo) (entity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        let callee =
            match argInfo.ThisArg with
            | Some callee ->
                // Sometimes an interface method can be called without casting. Example:
                // `let foo (x: 'T when 'T :> IDisposable) = x.Dispose()`
                match callee.Type with
                | Fable.DeclaredType(original, _) when entity.IsInterface && not original.IsInterface ->
                    castToInterface com typ original entity.FullName callee
                | _ -> callee
            | None ->
                sprintf "Unexpected static interface/override call: %s" memb.FullName
                |> attachRange r |> failwith
        let name = getMemberDisplayName memb
        match argInfo.Args with
        | [arg] when memb.IsPropertySetterMethod ->
            let t = memb.CurriedParameterGroups.[0].[0].Type
            Fable.Set(callee, Fable.FieldSet(name, makeType com (Map genArgs.Value) t), arg, r)
        | _ when memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0 ->
            get r typ callee name
        | _ ->
            let argInfo = { argInfo with ThisArg = Some callee }
            makeStrConst name |> Some |> instanceCall r typ argInfo

    let (|Replaced|_|) (com: IFableCompiler) ctx r typ argTypes (genArgs: Lazy<_>) (argInfo: Fable.ArgInfo)
            (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let isCandidate (entityFullName: string) =
            entityFullName.StartsWith("Fable.Core.")
        let tryReplace (entityFullName: string) =
            let info: Fable.CallInfo =
              { SignatureArgTypes = argTypes
                DeclaringEntityFullName = entityFullName
                Spread = argInfo.Spread
                CompiledName = memb.CompiledName
                GenericArgs = genArgs.Value
              }
            match com.TryReplace(ctx, r, typ, info, argInfo.ThisArg, argInfo.Args) with
            | Some e -> Some e
            | None ->
                match entity with
                | Some entity when entity.IsInterface ->
                    callInstanceMember com r typ genArgs argInfo entity memb |> Some
                | _ -> sprintf "Cannot resolve %s.%s" info.DeclaringEntityFullName info.CompiledName
                       |> addErrorAndReturnNull com r |> Some
        match entity with
        | Some ent ->
            match ent.TryFullName, ent.Assembly.FileName with
            | Some entityFullName, Some asmPath when not(System.String.IsNullOrEmpty(asmPath)) ->
                tryReplace entityFullName
            | Some entityFullName, _ when isCandidate entityFullName ->
                tryReplace entityFullName
            | _ -> None
        | _ -> None

    let (|Emitted|_|) com r typ argInfo (memb: FSharpMemberOrFunctionOrValue) =
        match memb.Attributes with
        | Try (tryFindAtt Atts.emit) att ->
            match Seq.tryHead att.ConstructorArguments with
            | Some(_, (:? string as macro)) ->
                let argInfo =
                    // Allow combination of Import and Emit attributes
                    match argInfo, tryGlobalOrImportedMember com Fable.Any memb with
                    | Some argInfo, Some importExpr ->
                        Some { argInfo with Fable.ThisArg = Some importExpr }
                    | _ -> argInfo
                Fable.Operation(Fable.Emit(macro, argInfo), typ, r) |> Some
            | _ -> "EmitAttribute must receive a string argument" |> attachRange r |> failwith
        | _ -> None

    let (|Imported|_|) com r typ genArgsAndArgInfo (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let importValueType = if Option.isSome genArgsAndArgInfo then Fable.Any else typ
        match tryGlobalOrImportedMember com importValueType memb, genArgsAndArgInfo, entity with
        | Some importExpr, Some(_, argInfo), _ ->
            if isModuleValueForCalls memb
            then Some importExpr
            else staticCall r typ argInfo importExpr |> Some
        | Some importExpr, None, _ ->
            Some importExpr
        | None, Some(genArgs, argInfo), Some e ->
            match tryImportedEntity com e with
            | Some classExpr ->
                match argInfo.ThisArg with
                | Some _ -> callInstanceMember com r typ genArgs argInfo e memb
                | None ->
                    if memb.IsConstructor then
                        Fable.Operation(Fable.Call(Fable.ConstructorCall classExpr, argInfo), typ, r)
                    else
                        let argInfo = { argInfo with ThisArg = Some classExpr }
                        callInstanceMember com r typ genArgs argInfo e memb
                |> Some
            | None -> None
        | _ -> None

    let inlineExpr (com: IFableCompiler) ctx (genArgs: Lazy<_>) callee args (memb: FSharpMemberOrFunctionOrValue) =
        // TODO: Log error if the inline function is called recursively
        // TODO!!! Replace the source location in the inlined expressions
        let argIdents, fsExpr = com.GetInlineExpr(memb)
        let args: Fable.Expr list = match callee with Some c -> c::args | None -> args
        let ctx, bindings =
            ((ctx, []), argIdents, args) |||> List.fold2 (fun (ctx, bindings) argId arg ->
                // Change type and mark ident as compiler-generated so it can be optimized
                let ident = { makeIdentFrom com ctx argId with
                                Type = arg.Type
                                IsCompilerGenerated = true }
                let ctx = bindExpr ctx argId (Fable.IdentExpr ident)
                ctx, (ident, arg)::bindings)
        let ctx = { ctx with GenericArgs = genArgs.Value |> Map }
        (com.Transform(ctx, fsExpr), bindings)
        ||> List.fold (fun body binding -> Fable.Let([binding], body))

    let (|Inlined|_|) (com: IFableCompiler) ctx genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        if not(isInline memb)
        then None
        else inlineExpr com ctx genArgs callee args memb |> Some

    // TODO: Move this to replacements to make more types injectable
    let injectArg com (genArgs: Lazy<(string * Fable.Type) list>) (par: FSharpParameter): Fable.Expr =
        let resolveParamGeneric index (paramGenArgs: IList<FSharpType>) =
            let parGenArg = paramGenArgs.[index]
            if parGenArg.IsGenericParameter then
                let genParamName = parGenArg.GenericParameter.Name
                genArgs.Value
                |> List.tryPick (fun (k,v) -> if k = genParamName then Some v else None)
                // We could add an error here if not found, but it will be added anyways
                // when trying to compile TypeInfo for a generic in Fable2Babel
                |> Option.defaultValue (Fable.GenericParam genParamName)
            else makeType com Map.empty parGenArg
        let parType = nonAbbreviatedType par.Type
        let range = makeRange par.DeclarationLocation |> Some
        let typDefAndGenArgs =
            // The type of the parameter must be an option
            if parType.HasTypeDefinition && parType.TypeDefinition.TryFullName = Some Types.option then
                let typ = parType.GenericArguments.[0]
                if typ.HasTypeDefinition then Some(typ.TypeDefinition, typ.GenericArguments) else None
            else None
        match typDefAndGenArgs with
        | Some(typDef, genArgs) when typDef.TryFullName = Some Types.typeResolver ->
            let f = Fable.TypeInfo(resolveParamGeneric 0 genArgs, range) |> Fable.Value |> makeDelegate []
            Fable.ObjectExpr([makeStrConst "ResolveType", f, Fable.ObjectValue], Fable.Any, None)
        | _ ->
            typDefAndGenArgs
            |> Option.bind (fun (x,_) -> x.TryFullName)
            |> Option.defaultValue Naming.unknown
            |> sprintf "Cannot inject argument of type %s"
            |> addErrorAndReturnNull com range

    let removeOmittedOptionalArguments com (memb: FSharpMemberOrFunctionOrValue) genArgs (args: Fable.Expr list) =
        if memb.CurriedParameterGroups.Count <> 1
            || memb.CurriedParameterGroups.[0].Count <> (List.length args)
        then args
        else
            (memb.CurriedParameterGroups.[0], args, (false, []))
            |||> Seq.foldBack2 (fun par arg (finish, acc) ->
                if finish
                then true, arg::acc
                else
                    match par.IsOptionalArg, arg with
                    | true, Fable.Value(Fable.NewOption(None,_)) ->
                        // Has Inject attribute?
                        if tryFindAtt Atts.inject par.Attributes |> Option.isSome
                        then true, (injectArg com genArgs par)::acc
                        else false, acc
                    | _ -> true, arg::acc)
            |> snd

    let hasAttribute attFullName (ent: FSharpEntity) =
        ent.Attributes |> Seq.exists (fun att ->
            att.AttributeType.TryFullName = Some attFullName)

    let hasInterface interfaceFullname (ent: FSharpEntity) =
        ent.AllInterfaces |> Seq.exists (fun t ->
            t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some interfaceFullname)

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ (genArgs: Fable.Type seq) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let call kind args =
            Fable.Operation(Fable.Call(kind, args), typ, r)
        let genArgs = lazy(matchGenericParamsFrom memb genArgs |> Seq.toList)
        let args = removeOmittedOptionalArguments com memb genArgs args
        let argTypes = getArgTypes com memb
        let argInfo: Fable.ArgInfo =
          { ThisArg = callee
            Args = args
            SignatureArgTypes = Some argTypes
            Spread = if hasSeqSpread memb then Fable.SeqSpread else Fable.NoSpread
            IsSiblingConstructorCall = false }
        match memb, memb.DeclaringEntity with
        | Emitted com r typ (Some argInfo) emitted, _ -> emitted
        | Imported com r typ (Some(genArgs, argInfo)) imported -> imported
        | Replaced com ctx r typ argTypes genArgs argInfo replaced -> replaced
        | Inlined com ctx genArgs callee args expr, _ -> expr
        | Try (tryGetBoundExpr ctx r) funcExpr, _ ->
            if isModuleValueForCalls memb
            then funcExpr
            else staticCall r typ argInfo funcExpr
        // Check if this is an interface or abstract/overriden method
        | _, Some entity when entity.IsInterface
                || memb.IsOverrideOrExplicitInterfaceImplementation
                || memb.IsDispatchSlot ->
            callInstanceMember com r typ genArgs argInfo entity memb
        | _ ->
            if isModuleValueForCalls memb
            then memberRefTyped com r typ memb
            else
                let argInfo =
                    if isSiblingConstructorCall ctx memb
                    then { argInfo with IsSiblingConstructorCall = true }
                    else argInfo
                memberRef com r memb |> staticCall r typ argInfo

    let makeValueFrom com (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType com ctx.GenericArgs v.FullType
        match v, v.DeclaringEntity with
        | _ when typ = Fable.Unit -> Fable.Value Fable.UnitConstant
        | Emitted com r typ None emitted, _ -> emitted
        | Imported com r typ None imported -> imported
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr, _ -> expr
        | _ -> memberRefTyped com r typ v
