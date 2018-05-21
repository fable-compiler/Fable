namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
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
      GenericArgs: Map<string, FSharpType>
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
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract AddUsedVarName: string -> unit

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
        | None -> memb.FullName, Naming.NoMemberPart

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
        | Some tdef -> defaultArg tdef.TryFullName "unknown"
        | None -> "unknown"

    let isInline (memb: FSharpMemberOrFunctionOrValue) =
        match memb.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        // TODO: Add compiler option to inline also `OptionalInline`
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let makeRange (r: Range.range) =
        { start = { line = r.StartLine; column = r.StartColumn }
          ``end``= { line = r.EndLine; column = r.EndColumn } }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    let unionCaseCompiledName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt Atts.compiledName
        |> Option.map (fun att -> att.ConstructorArguments.[0] |> snd |> string)

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        match unionCaseCompiledName unionCase with
        | Some name -> name
        | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeStrConst

    let isModuleMember (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        | Some ent -> ent.IsFSharpModule
        // Actually it's true in this case, but we don't consider compiler-generated members
        | None -> false

    /// Using memb.IsValue doesn't work for function values
    /// (e.g. `let ADD = adder()` when adder returns a function)
    let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
        memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0

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
            |> Option.bind (fun lastParam -> tryFindAtt Atts.paramSeq lastParam.Attributes)
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
                    && arg.Type.TypeDefinition.AccessPath = Types.printf
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

    // TODO: Convert this to dictionary
    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Int32" -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Single" -> Some Float32
        | "System.Double" -> Some Float64
        | "System.Decimal" -> Some Decimal
        // Units of measure
        | "Microsoft.FSharp.Core.sbyte`1" -> Some Int8
        | "Microsoft.FSharp.Core.int16`1" -> Some Int16
        | "Microsoft.FSharp.Core.int`1" -> Some Int32
        | "Microsoft.FSharp.Core.float32`1" -> Some Float32
        | "Microsoft.FSharp.Core.float`1" -> Some Float64
        | "Microsoft.FSharp.Core.decimal`1" -> Some Decimal
        | _ -> None

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
                    | Some Atts.stringEnum -> Some (StringEnum tdef)
                    | _ -> None)
                |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))

    let (|ContainsAtt|_|) (fullName: string) (ent: FSharpEntity) =
        tryFindAtt fullName ent.Attributes

module TypeHelpers =
    open Helpers
    open Patterns

    let rec makeGenArgs (com: IFableCompiler) ctxTypeArgs genArgs =
        Seq.map (makeType com ctxTypeArgs) genArgs |> Seq.toList

    and makeTypeFromDelegate com ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) (fullName: string) =
        if fullName.StartsWith("System.Action") then
            let argTypes =
                match Seq.tryHead genArgs with
                | Some genArg -> [makeType com ctxTypeArgs genArg]
                | None -> []
            Fable.FunctionType(Fable.DelegateType argTypes, Fable.Unit)
        elif fullName.StartsWith("System.Func") then
            let argTypes, returnType =
                match genArgs.Count with
                | 0 -> [], Fable.Unit
                | 1 -> [], makeType com ctxTypeArgs genArgs.[0]
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

    and makeTypeFromDef (com: IFableCompiler) ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        let getSingleGenericArg genArgs =
            Seq.tryHead genArgs
            |> Option.map (makeType com ctxTypeArgs)
            |> Option.defaultValue Fable.Any // Raise error if not found?
        match getEntityFullName tdef, tdef with
        | _ when tdef.IsArrayType -> getSingleGenericArg genArgs |> Fable.Array
        | fullName, _ when tdef.IsEnum -> Fable.EnumType(Fable.NumberEnumType, fullName)
        | fullName, _ when tdef.IsDelegate -> makeTypeFromDelegate com ctxTypeArgs genArgs tdef fullName
        // Fable "primitives"
        | Types.object, _ -> Fable.Any
        | Types.unit, _ -> Fable.Unit
        | Types.bool, _ -> Fable.Boolean
        | Types.char, _ -> Fable.Char
        | Types.string, _ -> Fable.String
        | Types.regex, _ -> Fable.Regex
        | Types.option, _ -> getSingleGenericArg genArgs |> Fable.Option
        | Types.resizeArray, _ -> getSingleGenericArg genArgs |> Fable.Array
        | Types.list, _ -> getSingleGenericArg genArgs |> Fable.List
        | NumberKind kind, _ -> Fable.Number kind
        // Special attributes
        | fullName, ContainsAtt Atts.stringEnum _ -> Fable.EnumType(Fable.StringEnumType, fullName)
        | _, ContainsAtt Atts.erase _ -> makeGenArgs com ctxTypeArgs genArgs |> Fable.ErasedUnion
        // Rest of declared types
        | _ -> Fable.DeclaredType(tdef, makeGenArgs com ctxTypeArgs genArgs)

    and makeType (com: IFableCompiler) (ctxTypeArgs: Map<string, FSharpType>) (NonAbbreviatedType t) =
        let resolveGenParam ctxTypeArgs (genParam: FSharpGenericParameter) =
            match Map.tryFind genParam.Name ctxTypeArgs with
            | None -> Fable.GenericParam genParam.Name
            | Some typ -> makeType com Map.empty typ
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
                            ||> Naming.sanitizeIdent ctx.VarNames.Contains
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

    let resolveTypes (ctx: Context) (types: FSharpType seq) =
        types |> Seq.map (fun t ->
            if t.IsGenericParameter
            then Map.tryFind t.GenericParameter.Name ctx.GenericArgs
                 // TODO: Log error if we cannot resolve the generic parameter?
                 |> Option.defaultValue t
            else t)

    let matchGenericParams (ctx: Context) (memb: FSharpMemberOrFunctionOrValue) (genArgs: FSharpType seq) =
        let genParams =
            match memb.DeclaringEntity with
            | Some ent -> Seq.append ent.GenericParameters memb.GenericParameters
            | None -> upcast memb.GenericParameters
            |> Seq.map (fun x -> x.Name)
        resolveTypes ctx genArgs |> Seq.zip genParams

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

    let entityRef (com: ICompiler) r (ent: FSharpEntity) =
        match tryGetEntityLocation ent with
        | Some entLoc ->
            let file = Path.normalizePath entLoc.FileName
            let entityName = getEntityDeclarationName com ent
            if file = com.CurrentFile
            then makeIdent entityName |> Fable.IdentExpr
            else Fable.Import(entityName, file, Fable.Internal, Fable.Any)
        | None ->
            "Cannot find implementation location for entity: " + (getEntityFullName ent)
            |> addErrorAndReturnNull com r

    let memberRefTyped (com: IFableCompiler) r typ (memb: FSharpMemberOrFunctionOrValue) =
        let memberName = getMemberDeclarationName com memb
        let file =
            match memb.DeclaringEntity with
            | Some ent -> tryGetEntityLocation ent |> Option.map (fun loc -> Path.normalizePath loc.FileName)
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> Some com.CurrentFile
        match file with
        | Some file when file = com.CurrentFile ->
            makeTypedIdent typ memberName |> Fable.IdentExpr
        | Some file -> Fable.Import(memberName, file, Fable.Internal, typ)
        | None -> "Cannot find implementation location for member: " + memb.FullName
                  |> addErrorAndReturnNull com r

    let memberRef (com: IFableCompiler) r (memb: FSharpMemberOrFunctionOrValue) =
        memberRefTyped com r Fable.Any memb

    let callInterfaceCast com t (sourceEntity: FSharpEntity) interfaceFullName expr =
        if sourceEntity.IsInterface
        then expr
        else
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
                        else Fable.Import(funcName, file, Fable.Internal, Fable.Any)
                        |> staticCall None t (argInfo None [expr] None)

    let callInstanceMember com r typ (argInfo: Fable.ArgInfo) (entity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        let callee =
            match argInfo.ThisArg with
            | Some callee ->
                // Sometimes an interface method can be called without casting. Example:
                // `let foo (x: 'T when 'T :> IDisposable) = x.Dispose()`
                match callee.Type with
                | Fable.DeclaredType(original, _) when entity.IsInterface && not original.IsInterface ->
                    callInterfaceCast com typ original entity.FullName callee
                | _ -> callee
            | None ->
                sprintf "Unexpected static interface/override call: %s" memb.FullName
                |> attachRange r |> failwith
        let name = getMemberDisplayName memb
        match argInfo.Args with
        | [arg] when memb.IsPropertySetterMethod ->
            Fable.Set(callee, makeStrConst name |> Fable.ExprSet, arg, r)
        | _ when memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0 ->
            get r typ callee name
        | _ ->
            let argInfo = { argInfo with ThisArg = Some callee }
            makeStrConst name |> Some |> instanceCall r typ argInfo

    let (|Replaced|_|) (com: IFableCompiler) ctx r typ argTypes genArgs (argInfo: Fable.ArgInfo)
            (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let isCandidate (entityFullName: string) =
            entityFullName.StartsWith("System.")
                || entityFullName.StartsWith("Microsoft.FSharp.")
                || entityFullName.StartsWith("Fable.Core.")
        match entity |> Option.bind (fun e -> e.TryFullName) with
        | Some entityFullName when isCandidate entityFullName ->
            let info: Fable.CallInfo =
              { SignatureArgTypes = argTypes
                DeclaringEntityFullName = entityFullName
                Spread = argInfo.Spread
                CompiledName = memb.CompiledName
                GenericArgs = matchGenericParams ctx memb genArgs |> Seq.toList
              }
            match com.TryReplace(ctx, r, typ, info, argInfo.ThisArg, argInfo.Args) with
            | Some e -> Some e
            | None ->
                match entity with
                | Some entity when entity.IsInterface ->
                    callInstanceMember com r typ argInfo entity memb |> Some
                | _ -> sprintf "Cannot resolve %s.%s" info.DeclaringEntityFullName info.CompiledName
                       |> addErrorAndReturnNull com r |> Some
        | _ -> None

    let (|Emitted|_|) r typ argInfo (memb: FSharpMemberOrFunctionOrValue) =
        match memb.Attributes with
        | Try (tryFindAtt Atts.emit) att ->
            match Seq.tryHead att.ConstructorArguments with
            | Some(_, (:? string as macro)) ->
                Fable.Operation(Fable.Emit(macro, argInfo), typ, r) |> Some
            | _ -> "EmitAttribute must receive a string argument" |> attachRange r |> failwith
        | _ -> None

    let tryImportAttribute (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (function
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                Some(selector.Trim(), path.Trim())
            | _ -> None)

    /// Function used to check if calls must be replaced by global idents or direct imports
    /// Relative imports are ignored because they're compiled as module members in the respective file
    /// (path needs to be calculated in regard to the importing file)
    let tryGlobalOrAbsoluteImportExpr typ (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (function
            | AttFullName(Atts.global_, att) ->
                match att with
                | AttArguments [:? string as customName] ->
                    makeTypedIdent typ customName |> Fable.IdentExpr |> Some
                | _ -> makeTypedIdent typ name |> Fable.IdentExpr |> Some
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)])
                    when not(path.StartsWith ".") -> // Ignore relative imports
                Fable.Import(selector.Trim(), path.Trim(), Fable.CustomImport, typ) |> Some
            | _ -> None)

    let (|Imported|_|) com r typ argInfo (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let importValueType = if Option.isSome argInfo then Fable.Any else typ
        match tryGlobalOrAbsoluteImportExpr importValueType memb.CompiledName memb.Attributes, argInfo, entity with
        | Some importExpr, Some argInfo, _ ->
            let emittedArgInfo = { argInfo with Fable.ThisArg = Some importExpr }
            match memb with
            // Allow combination of Import and Emit attributes
            | Emitted r typ (Some emittedArgInfo) emitted -> Some emitted
            | _ ->
                if isModuleValueForCalls memb
                then Some importExpr
                else staticCall r typ argInfo importExpr |> Some
        | Some importExpr, None, _ ->
            Some importExpr
        | None, Some argInfo, Some e ->
            // TODO: Check also Global attribute here?
            match tryImportAttribute e.Attributes with
            | Some(selector, path) ->
                match argInfo.ThisArg with
                | Some _ -> callInstanceMember com r typ argInfo e memb
                | None ->
                    let classExpr =
                        if path.StartsWith(".") |> not
                        then Fable.Import(selector, path, Fable.CustomImport, typ)
                        else entityRef com r e
                    if memb.IsConstructor then
                        Fable.Operation(Fable.Call(Fable.ConstructorCall classExpr, argInfo), typ, r)
                    else
                        let argInfo = { argInfo with ThisArg = Some classExpr }
                        callInstanceMember com r typ argInfo e memb
                |> Some
            | None -> None
        | _ -> None

    let inlineExpr (com: IFableCompiler) ctx genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        // TODO: Log error if the inline function is called recursively
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
        let ctx = { ctx with GenericArgs = matchGenericParams ctx memb genArgs |> Map }
        (com.Transform(ctx, fsExpr), bindings)
        ||> List.fold (fun body binding -> Fable.Let([binding], body))

    let (|Inlined|_|) (com: IFableCompiler) ctx genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        if not(isInline memb)
        then None
        else inlineExpr com ctx genArgs callee args memb |> Some

    let removeOmittedOptionalArguments (memb: FSharpMemberOrFunctionOrValue) (args: Fable.Expr list) =
        let argsLen = List.length args
        if memb.CurriedParameterGroups.Count <> 1
            || memb.CurriedParameterGroups.[0].Count <> argsLen
        then args
        else
            let validArgsLen, _ =
                (args, (argsLen, false)) ||> List.foldBack (fun arg (len, finish) ->
                    if finish then len, true else
                        match memb.CurriedParameterGroups.[0].[len - 1].IsOptionalArg, arg with
                        | true, Fable.Value(Fable.NewOption(None,_)) -> len - 1, false
                        | _ -> len, true)
            if validArgsLen < argsLen
            then List.take validArgsLen args
            else args

    let hasInterface interfaceFullname (ent: FSharpEntity) =
        ent.AllInterfaces |> Seq.exists (fun t ->
            t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some interfaceFullname)

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ (genArgs: FSharpType seq) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let call kind args =
            Fable.Operation(Fable.Call(kind, args), typ, r)
        let args = removeOmittedOptionalArguments memb args
        let argTypes = getArgTypes com memb
        let argInfo: Fable.ArgInfo =
          { ThisArg = callee
            Args = args
            SignatureArgTypes = Some argTypes
            Spread = if hasSeqSpread memb then Fable.SeqSpread else Fable.NoSpread
            IsSiblingConstructorCall = false }
        match memb, memb.DeclaringEntity with
        | Imported com r typ (Some argInfo) imported -> imported
        | Emitted r typ (Some argInfo) emitted, _ -> emitted
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
            callInstanceMember com r typ argInfo entity memb
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
        | Imported com r typ None imported -> imported
        | Emitted r typ None emitted, _ -> emitted
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr, _ -> expr
        | _ -> memberRefTyped com r typ v
