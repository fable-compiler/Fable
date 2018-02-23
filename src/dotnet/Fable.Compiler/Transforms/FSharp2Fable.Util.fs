namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable.Util
open Fable.Transforms

type Context =
    { scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
      scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      /// Some expressions that create scope in F# don't do it in JS (like let bindings)
      /// so we need a mutable registry to prevent duplicated var names.
      varNames: HashSet<string>
      typeArgs: Map<string, FSharpType>
      decisionTargets: Map<int, FSharpMemberOrFunctionOrValue list * FSharpExpr> option
    }
    static member Create() =
        { scope = []
          scopedInlines = []
          varNames = HashSet()
          typeArgs = Map.empty
          decisionTargets = None }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract TryGetInternalFile: FSharpEntity -> string option
    abstract GetRootModule: string -> string
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract AddInlineExpr: FSharpMemberOrFunctionOrValue * InlineExpr -> unit
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

    let getEntityLocation (ent: FSharpEntity) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation

    let getMemberLocation (memb: FSharpMemberOrFunctionOrValue) =
        match memb.ImplementationLocation with
        | Some loc -> loc
        | None -> memb.DeclarationLocation

    let findOverloadIndex (enclosingEntity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
        let argsEqual (args1: IList<IList<FSharpParameter>>) (args2: IList<IList<FSharpParameter>>) =
            if args1.Count = args2.Count then
                (args1, args2) ||> Seq.forall2 (fun g1 g2 ->
                    g1.Count = g2.Count && Seq.forall2 (=) g1 g2)
            else false
        if m.IsImplicitConstructor || m.IsOverrideOrExplicitInterfaceImplementation
        then 0
        else
            // m.Overloads(false) doesn't work
            let name = m.CompiledName
            let isInstance = m.IsInstanceMember
            ((0, false), enclosingEntity.MembersFunctionsAndValues)
            ||> Seq.fold (fun (i, found) m2 ->
                if not found && m2.IsInstanceMember = isInstance && m2.CompiledName = name then
                    // .Equals() doesn't work. TODO: Compare arg types for trait calls
                    // .IsEffectivelySameAs() doesn't work for constructors
                    if argsEqual m.CurriedParameterGroups m2.CurriedParameterGroups
                    then i, true
                    else i + 1, false
                else i, found)
            |> fst

    let private getMemberDeclarationNamePrivate removeRootModule (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =
        let removeRootModule' (ent: FSharpEntity) (fullName: string) =
            if not removeRootModule then
                fullName
            else
                let rootMod =
                    (getEntityLocation ent).FileName
                    |> Path.normalizePath
                    |> com.GetRootModule
                fullName.Replace(rootMod, ".").TrimStart('.')
        match memb.EnclosingEntity with
        | Some ent when ent.IsFSharpModule ->
            removeRootModule' ent memb.FullName
        | Some ent ->
            let isStatic = not memb.IsInstanceMember
            let separator = if isStatic then "$$" else "$"
            let overloadIndex =
                match findOverloadIndex ent memb with
                | 0 -> ""
                | i -> "_" + string i
            (removeRootModule' ent ent.FullName) + separator + memb.CompiledName + overloadIndex
        | None -> memb.FullName
        |> Naming.sanitizeIdentForbiddenChars

    let getMemberDeclarationName (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberDeclarationNamePrivate true com argTypes memb

    let getMemberDeclarationFullname (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =

        getMemberDeclarationNamePrivate false com argTypes memb

    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName ->
                if f fullName then Some att else None
            | None -> None)

    let hasAtt name atts =
        atts |> tryFindAtt ((=) name) |> Option.isSome

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
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Core.Unit"
        else false

    let makeRange (r: Range.range) = {
        start = { line = r.StartLine; column = r.StartColumn }
        ``end``= { line = r.EndLine; column = r.EndColumn }
    }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) Atts.compiledName)
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeStrConst

    let getArgCount (memb: FSharpMemberOrFunctionOrValue) =
        let args = memb.CurriedParameterGroups
        if args.Count = 0 then 0
        elif args.Count = 1 && args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args |> Seq.sumBy (fun li -> li.Count)

    let private isModuleValuePrivate checkPublicMutable (memb: FSharpMemberOrFunctionOrValue) =
        match memb.EnclosingEntity with
        | Some owner when owner.IsFSharpModule ->
            let preCondition =
                if checkPublicMutable
                then not memb.IsMutable || not (isPublicMember memb)
                else true
            preCondition
            && memb.CurriedParameterGroups.Count = 0
            && memb.GenericParameters.Count = 0
        | _ -> false

    // Mutable public values must be compiled as functions (see #986)
    let isModuleValueForCalls (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValuePrivate true memb

    let isModuleValueForDeclaration (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValuePrivate false memb

    let getObjectMemberKind (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsImplicitConstructor || memb.IsConstructor then Fable.Constructor
        elif memb.IsPropertyGetterMethod && (getArgCount memb) = 0 then Fable.Getter
        elif memb.IsPropertySetterMethod && (getArgCount memb) = 1 then Fable.Setter
        else Fable.Method

    let hasSpread (memb: FSharpMemberOrFunctionOrValue) =
        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then false else
            let args = memb.CurriedParameterGroups.[0]
            args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.map (fun lastParam -> hasAtt Atts.paramSeq lastParam.Attributes)
            |> Option.defaultValue false

        hasParamArray memb || hasParamSeq memb

module Patterns =
    open BasicPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) ctx = com.Transform ctx
    let inline (|FieldName|) (fi: FSharpField) = fi.Name

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    let (|RefType|_|) = function
        | TypeDefinition tdef as t when tdef.TryFullName = Some Types.reference -> Some t
        | _ -> None

    let (|ThisVar|_|) = function
        | BasicPatterns.ThisValue _ -> Some ThisVar
        | BasicPatterns.Value var when
            var.IsMemberThisValue || var.IsConstructorThisValue ->
            Some ThisVar
        | _ -> None

    let (|ForOfLoop|_|) = function
        | Let((_, value),
              Let((_, Call(None, memb, _, [], [])),
                TryFinally(WhileLoop(_,Let((ident, _), body)), _)))
        | Let((_, Call(Some value, memb, _, [], [])),
                TryFinally(WhileLoop(_,Let((ident, _), body)), _))
            when memb.CompiledName = "GetEnumerator" ->
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

    /// This matches the boilerplate generated to check an array's length
    /// when pattern matching
    let (|CheckArrayLength|_|) = function
        | IfThenElse
            (ILAsm ("[AI_ldnull; AI_cgt_un]",[],[matchValue]),
             Call(None,_op_Equality,[],[_typeInt],
                [ILAsm ("[I_ldlen; AI_conv DT_I4]",[],[_matchValue2])
                 Const (length,_typeInt2)]),
             Const (_falseConst,_typeBool)) -> Some (matchValue, length, _typeInt2)
        | _ -> None

    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Int32" -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Single" -> Some Float32
        | "System.Double" -> Some Float64
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.sbyte" _ -> Some Int8
        | Naming.StartsWith "Microsoft.FSharp.Core.int16" _ -> Some Int16
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
        | _ -> None

    let (|ExtendedNumberKind|_|) = function
        | "System.Int64" -> Some Int64
        | "System.UInt64" -> Some UInt64
        | "System.Decimal" -> Some Decimal
        | "System.Numerics.BigInteger" -> Some BigInt
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.int64" _ -> Some Int64
        | Naming.StartsWith "Microsoft.FSharp.Core.decimal" _ -> Some Decimal
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

    let (|Switch|_|) fsExpr =
        let isStringOrNumber (NonAbbreviatedType typ) =
            if not typ.HasTypeDefinition then false else
            match typ.TypeDefinition.TryFullName with
            | Some Types.string -> true
            | Some(NumberKind _) -> true
            | _ when typ.TypeDefinition.IsEnum -> true
            | _ -> false
        let rec makeSwitch size map matchValue e =
            match e with
            | IfThenElse(Call(None,op_Equality,[],_,[Value var; Const(case,_)]), DecisionTreeSuccess(idx, bindings), elseExpr)
                    when op_Equality.CompiledName.Equals("op_Equality") ->
                let case =
                    match case with
                    | :? int as i -> makeIntConst i |> Some
                    | :? string as s -> makeStrConst s |> Some
                    | _ -> None
                match case, matchValue with
                | Some case, Some matchValue when matchValue.Equals(var) ->
                    Some(matchValue,None,idx,bindings,case,elseExpr)
                | Some case, None when isStringOrNumber var.FullType && not var.IsMemberThisValue && not(isInline var) ->
                    Some(var,None,idx,bindings,case,elseExpr)
                | _ -> None
            | IfThenElse(UnionCaseTest(Value var,typ,case), DecisionTreeSuccess(idx, bindings), elseExpr) ->
                let unionType = typ.TypeDefinition
                let case = Fable.UnionCaseTag(case, unionType) |> Fable.Value
                match matchValue with
                | Some matchValue when matchValue.Equals(var) ->
                    Some(matchValue,Some unionType,idx,bindings,case,elseExpr)
                | None when not var.IsMemberThisValue && not(isInline var) ->
                    match typ with
                    | DiscriminatedUnion _ -> Some(var,Some unionType,idx,bindings,case,elseExpr)
                    | OptionUnion _ | ListUnion _ | ErasedUnion _ | StringEnum _ -> None
                | _ -> None
            | _ -> None
            |> function
                | Some(matchValue,isUnionType,idx,bindings,case,elseExpr) ->
                    let map =
                        match Map.tryFind idx map with
                        | None -> Map.add idx (bindings, [case]) map |> Some
                        | Some([],cases) when List.isEmpty bindings -> Map.add idx (bindings, cases@[case]) map |> Some
                        | Some _ -> None // Multiple case with multiple var bindings, cannot optimize
                    match map, elseExpr with
                    | Some map, DecisionTreeSuccess(idx, bindings) ->
                        Some(matchValue, isUnionType, size + 1, map, (idx, bindings))
                    | Some map, elseExpr -> makeSwitch (size + 1) map (Some matchValue) elseExpr
                    | None, _ -> None
                | None -> None
        match fsExpr with
        | DecisionTree(decisionExpr, decisionTargets) ->
            match makeSwitch 0 Map.empty None decisionExpr with
            // For small sizes it's better not to convert to switch so
            // the match is still a expression and not a statement
            | Some(matchValue, isUnionType, size, cases, defaultCase) when size > 3 ->
                Some(matchValue, isUnionType, cases, defaultCase, decisionTargets)
            | _ -> None
        | _ -> None

    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList)

module TypeHelpers =
    open Helpers
    open Patterns

    let rec makeGenArgs com ctxTypeArgs genArgs =
        Seq.map (makeType com ctxTypeArgs) genArgs |> Seq.toList

    and makeTypeFromDef (com: IFableCompiler) ctxTypeArgs (genArgs: seq<FSharpType>) (tdef: FSharpEntity) =
        let getSingleGenericArg genArgs =
            Seq.tryHead genArgs
            |> Option.map (makeType com ctxTypeArgs)
            |> Option.defaultValue Fable.Any // Raise error if not found?
        let fullName = getEntityFullName tdef
        if tdef.IsArrayType
        then getSingleGenericArg genArgs |> Fable.Array
        elif tdef.IsEnum
        then Fable.EnumType(Fable.NumberEnumType, fullName)
        elif tdef.IsDelegate
        then
            if fullName.StartsWith("System.Action")
            then
                let argTypes =
                    if Seq.length genArgs = 1
                    then [Seq.head genArgs |> makeType com ctxTypeArgs]
                    else [Fable.Unit]
                Fable.FunctionType(Fable.DelegateType argTypes, Fable.Unit)
            elif fullName.StartsWith("System.Func")
            then
                let argTypes, returnType =
                    match Seq.length genArgs with
                    | 0 -> [Fable.Unit], Fable.Unit
                    | 1 -> [Fable.Unit], Seq.head genArgs |> makeType com ctxTypeArgs
                    | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com ctxTypeArgs) |> Seq.toList,
                            Seq.last genArgs |> makeType com ctxTypeArgs
                Fable.FunctionType(Fable.DelegateType argTypes, returnType)
            else
            try
                let argTypes =
                    tdef.FSharpDelegateSignature.DelegateArguments
                    |> Seq.map (snd >> makeType com ctxTypeArgs) |> Seq.toList
                let returnType =
                    makeType com ctxTypeArgs tdef.FSharpDelegateSignature.DelegateReturnType
                Fable.FunctionType(Fable.DelegateType argTypes, returnType)
            with _ ->
                // TODO: Log error here?
                Fable.FunctionType(Fable.DelegateType [Fable.Any], Fable.Any)
        elif fullName = Types.object
        then Fable.Any
        else
        match fullName with
        // Fable "primitives"
        | Types.unit -> Fable.Unit
        | Types.bool -> Fable.Boolean
        | Types.char -> Fable.Char
        | Types.string -> Fable.String
        | Types.regex -> Fable.Regex
        | Types.option -> getSingleGenericArg genArgs |> Fable.Option
        | Types.resizeArray -> getSingleGenericArg genArgs |> Fable.Array
        | Types.list -> getSingleGenericArg genArgs |> Fable.List
        | NumberKind kind -> Fable.Number kind
        | ExtendedNumberKind kind -> Fable.ExtendedNumber kind
        | _ ->
            tdef.Attributes |> Seq.tryPick (fun att ->
                match att.AttributeType.TryFullName with
                | Some Atts.stringEnum ->
                    Fable.EnumType(Fable.StringEnumType, fullName) |> Some
                | Some Atts.erase ->
                    makeGenArgs com ctxTypeArgs genArgs
                    |> Fable.ErasedUnion |> Some
                | _ -> None)
            |> Option.defaultWith (fun () ->
                Fable.DeclaredType(tdef, makeGenArgs com ctxTypeArgs genArgs))

    and makeType (com: IFableCompiler) ctxTypeArgs (NonAbbreviatedType t) =
        let resolveGenParam (genParam: FSharpGenericParameter) =
            match Map.tryFind genParam.Name ctxTypeArgs with
            // Clear typeArgs to prevent infinite recursion
            | Some typ -> makeType com Map.empty typ
            | None -> Fable.GenericParam genParam.Name
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
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

    let getBaseClass (com: IFableCompiler) (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.TryFullName <> Some Types.object ->
            Some (getEntityFullName tdef)
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
       hasAtt Atts.abstractClass ent.Attributes

    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.typeArgs t

module Identifiers =
    open Helpers
    open TypeHelpers

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with scope = (Some fsRef, expr)::ctx.scope}

    let private bindIdentPrivate (com: IFableCompiler) (ctx: Context) typ
                  (fsRef: FSharpMemberOrFunctionOrValue option) force name =
        let sanitizedName = name |> Naming.sanitizeIdent (fun x ->
            not force && ctx.varNames.Contains x)
        ctx.varNames.Add sanitizedName |> ignore
        // Track all used var names in the file so they're not used for imports
        com.AddUsedVarName sanitizedName
        let isMutable, range =
            match fsRef with
            | Some x -> x.IsMutable, Some(makeRange x.DeclarationLocation)
            | None -> false, None
        let ident: Fable.Ident =
            { Name = sanitizedName
              Type = typ
              IsMutable = isMutable
              Range = range }
        let identValue = Fable.IdentExpr ident
        { ctx with scope = (fsRef, identValue)::ctx.scope}, ident

    let bindIdentWithExactName com ctx typ name =
        bindIdentPrivate com ctx typ None true name

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        let typ = makeType com ctx.typeArgs fsRef.FullType
        bindIdentPrivate com ctx typ (Some fsRef) false fsRef.CompiledName

    let bindIdentWithTentativeName com ctx (fsRef: FSharpMemberOrFunctionOrValue) tentativeName: Context*Fable.Ident =
        let typ = makeType com ctx.typeArgs fsRef.FullType
        bindIdentPrivate com ctx typ (Some fsRef) false tentativeName

    let (|BindIdent|) com ctx fsRef = bindIdentFrom com ctx fsRef

    /// Get corresponding identifier to F# value in current scope
    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fst >> function Some fsRef' -> obj.Equals(fsRef, fsRef') | None -> false)
        |> function
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
        let ctx = { ctx with varNames = HashSet(ctx.varNames) }
        /// TODO: Remove unit arg if single or with `this` arg
        (args, (ctx, [])) ||> List.foldBack (fun tupledArg (ctx, accArgs) ->
            // The F# compiler "untuples" the args in methods
            let ctx, untupledArg = makeFunctionArgs com ctx tupledArg
            ctx, untupledArg@accArgs)

    let makeTryCatch com ctx (fsExpr: FSharpExpr) (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                Some (catchVar, com.Transform catchContext catchBody)
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer)

    let (|Replaced|_|) com ctx r typ genArgs (info: Fable.CallInfo) callee args
                                        (memb: FSharpMemberOrFunctionOrValue) =
        let fullName = memb.FullName
        if Replacements.isReplaceCandidate fullName then
            match Replacements.tryFirstPass r typ args fullName with
            | Some replaced -> Some replaced
            | None ->
                let extraInfo: Fable.ExtraCallInfo =
                  { FullName = fullName
                    CompiledName = memb.CompiledName
                    GenericArgs = List.map (makeType com ctx.typeArgs) genArgs }
                let unresolved = Fable.UnresolvedCall(callee, args, info, extraInfo)
                Fable.Operation(unresolved, typ, r) |> Some
        else None

    let matchGenericParams (ctx: Context) (memb: FSharpMemberOrFunctionOrValue) genArgs =
        (ctx.typeArgs, memb.GenericParameters, genArgs)
        |||> Seq.fold2 (fun acc genPar t -> Map.add genPar.Name t acc)

    let (|Emitted|_|) r typ argsAndCallInfo (memb: FSharpMemberOrFunctionOrValue) =
        match memb.Attributes with
        | ContainsAtt Atts.emit attArgs ->
            match attArgs with
            | [:? string as macro] ->
                Fable.Operation(Fable.Emit(macro, argsAndCallInfo), typ, r) |> Some
            | _ -> "EmitAttribute must receive a string argument" |> attachRange r |> failwith
        | _ -> None

    /// Ignores relative imports (e.g. `[<Import("foo","./lib.js")>]`)
    let tryImported typ (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some Atts.global_ ->
                match Seq.tryHead att.ConstructorArguments with
                | Some(_, (:? string as customName)) -> makeTypedIdent typ customName |> Fable.IdentExpr |> Some
                | _ -> makeTypedIdent typ name |> Fable.IdentExpr |> Some
            | Some Atts.import ->
                match Seq.toList att.ConstructorArguments with
                | [(_, (:? string as memb)); (_, (:? string as path))]
                        when not(isNull memb || isNull path || path.StartsWith ".") ->
                    Fable.Import(memb.Trim(), path.Trim(), Fable.CustomImport, typ) |> Some
                | _ -> None
            | _ -> None)

    let (|Imported|_|) r typ argsAndCallInfo (memb: FSharpMemberOrFunctionOrValue) =
        match tryImported typ memb.CompiledName memb.Attributes with
        | Some expr ->
            match argsAndCallInfo with
            | Some(args: Fable.Expr list, info: Fable.CallInfo) ->
                // Allow combination of Import and Emit attributes
                let emittedCallInfo = { info with ArgTypes = expr.Type::info.ArgTypes }
                match memb with
                | Emitted r typ (Some(expr::args, emittedCallInfo)) emitted -> Some emitted
                | _ ->
                    // TODO: Check if owner is an object first
                    match getObjectMemberKind memb with
                    | Fable.Getter -> Some expr
                    | Fable.Setter -> Fable.Set(expr, Fable.VarSet, args.Head, r) |> Some
                    | Fable.Method -> Fable.Operation(Fable.Call(expr, None, args, info), typ, r) |> Some
                    | Fable.Constructor ->
                        let info = { info with IsConstructor = true }
                        Fable.Operation(Fable.Call(expr, None, args, info), typ, r) |> Some
            | None ->
                Some expr
        | None -> None

    let (|Inlined|_|) (com: IFableCompiler) ctx genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        if not(isInline memb)
        then None
        else
            let argIdents, fsExpr = com.GetInlineExpr memb
            let args = match callee with Some c -> c::args | None -> args
            let ctx, bindings =
                ((ctx, []), argIdents, args) |||> List.fold2 (fun (ctx, bindings) argId arg ->
                    let ctx, ident = bindIdentFrom com ctx argId
                    ctx, (ident, arg)::bindings)
            let ctx = { ctx with typeArgs = matchGenericParams ctx memb genArgs }
            match com.Transform ctx fsExpr with
            | Fable.Let(bindings2, expr) -> Fable.Let((List.rev bindings) @ bindings2, expr) |> Some
            | expr -> Fable.Let(List.rev bindings, expr) |> Some

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

    let memberRef (com: IFableCompiler) typ argTypes (memb: FSharpMemberOrFunctionOrValue) =
        let memberName = getMemberDeclarationName com argTypes memb
        let file =
            match memb.EnclosingEntity with
            | Some ent -> (getEntityLocation ent).FileName |> Path.normalizePath
            // Cases when .EnclosingEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> com.CurrentFile
        if file = com.CurrentFile
        then makeTypedIdent typ memberName |> Fable.IdentExpr
        else Fable.Import(memberName, file, Fable.Internal, typ)

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        let call info callee memb args =
            Fable.Operation(Fable.Call(callee, memb, args, info), typ, r)
        let args = removeOmittedOptionalArguments memb args
        let info: Fable.CallInfo =
          { ArgTypes = getArgTypes com memb
            IsConstructor = false
            IsDynamic = false
            HasSpread = hasSpread memb
            HasThisArg = false }
        let argsAndCallInfo =
            match callee with
            | Some c -> Some(c::args, { info with HasThisArg = true })
            | None -> Some(args, info)
        match memb, memb.EnclosingEntity with
        | Imported r Fable.Any argsAndCallInfo imported, _ -> imported
        | Emitted r Fable.Any argsAndCallInfo emitted, _ -> emitted
        | Replaced com ctx r typ genArgs info callee args replaced, _ -> replaced
        | Inlined com ctx genArgs callee args expr, _ -> expr
        | Try (tryGetBoundExpr ctx r) expr, _ ->
            match callee with
            | Some c -> call { info with HasThisArg = true } expr None (c::args)
            | None ->
                if isModuleValueForCalls memb
                then expr
                else call info expr None args
        // Check if this is an interface or abstract/overriden method
        | _, Some owner when owner.IsInterface
                        || memb.IsOverrideOrExplicitInterfaceImplementation
                        || isAbstract owner ->
            match callee with
            | Some callee ->
                match getObjectMemberKind memb with
                | Fable.Getter -> Fable.Get(callee, Fable.FieldGet memb.DisplayName, typ, r)
                | Fable.Setter -> Fable.Set(callee, Fable.FieldSet memb.DisplayName, args.Head, r)
                // Constructor is unexpected (abstract class cons calls are resolved in transformConstructor)
                | Fable.Constructor
                | Fable.Method -> call info callee (Some memb.DisplayName) args
            | None -> "Unexpected static interface/override call" |> attachRange r |> failwith
        | _ ->
            match callee with
            | Some callee ->
                let info = { info with HasThisArg = true }
                call info (memberRef com Fable.Any info.ArgTypes memb) None (callee::args)
            | None ->
                if isModuleValueForCalls memb
                then memberRef com typ [] memb
                else call info (memberRef com Fable.Any info.ArgTypes memb) None args

    let makeValueFrom com (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType com ctx.typeArgs v.FullType
        match v with
        | _ when typ = Fable.Unit -> Fable.Value Fable.UnitConstant
        | Imported r typ None imported -> imported
        | Emitted r typ None emitted -> emitted
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr -> expr
        | _ -> memberRef com typ [] v
