namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Transforms

type Context =
    { scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
      scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      /// Some expressions that create scope in F# don't do it in JS (like let bindings)
      /// so we need a mutable registry to prevent duplicated var names.
      varNames: HashSet<string>
      typeArgs: Map<string, FSharpType>
    }
    static member Create() =
        { scope = []
          scopedInlines = []
          varNames = HashSet()
          typeArgs = Map.empty }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context * FSharpExpr -> Fable.Expr
    abstract TryGetInternalFile: FSharpEntity -> string option
    abstract TryReplace: thisArg: Fable.Expr option * args: Fable.Expr list * info: Fable.CallInfo
                                    * Fable.Type * SourceLocation option -> Fable.Expr option
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

    let findOverloadIndex (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
        let argsEqual (args1: IList<IList<FSharpParameter>>) (args2: IList<IList<FSharpParameter>>) =
            if args1.Count = args2.Count then
                (args1, args2) ||> Seq.forall2 (fun g1 g2 ->
                // Checking equality of FSharpParameter seems to be fast
                // See https://github.com/fsharp/FSharp.Compiler.Service/blob/0d87c8878c14ab2b283fbd696096e4e4714fab6b/src/fsharp/symbols/Symbols.fs#L2145
                    g1.Count = g2.Count && Seq.forall2 (=) g1 g2)
            else false
        if m.IsImplicitConstructor || m.IsOverrideOrExplicitInterfaceImplementation
        then 0
        else
            // m.Overloads(false) doesn't work
            let name = m.CompiledName
            let isInstance = m.IsInstanceMember
            ((0, false), entity.MembersFunctionsAndValues)
            ||> Seq.fold (fun (i, found) m2 ->
                if not found && m2.IsInstanceMember = isInstance && m2.CompiledName = name then
                    // .Equals() doesn't work. TODO: Compare arg types for trait calls
                    // .IsEffectivelySameAs() doesn't work for constructors
                    if argsEqual m.CurriedParameterGroups m2.CurriedParameterGroups
                    then i, true
                    else i + 1, false
                else i, found)
            |> fst

    let private getMemberDeclarationNamePrivate removeRootModule (com: IFableCompiler) (memb: FSharpMemberOrFunctionOrValue) =
        let removeRootModule' (ent: FSharpEntity) (fullName: string) =
            if not removeRootModule then
                fullName
            else
                let rootMod =
                    (getEntityLocation ent).FileName
                    |> Path.normalizePath
                    |> com.GetRootModule
                fullName.Replace(rootMod, ".").TrimStart('.')
        match memb.DeclaringEntity with
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

    let getMemberDeclarationName (com: IFableCompiler) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberDeclarationNamePrivate true com memb

    let getMemberDeclarationFullname (com: IFableCompiler) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberDeclarationNamePrivate false com memb

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

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Core.Unit"
        else false

    let makeRange (r: Range.range) =
        { start = { line = r.StartLine; column = r.StartColumn }
          ``end``= { line = r.EndLine; column = r.EndColumn } }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt Atts.compiledName
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

    let private isModuleValuePrivate checkPublicMutable (entity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        if entity.IsFSharpModule then
            let preCondition =
                if checkPublicMutable
                // Mutable public values must be compiled as functions (see #986)
                then not memb.IsMutable || not (isPublicMember memb)
                else true
            preCondition
            && memb.CurriedParameterGroups.Count = 0
            && memb.GenericParameters.Count = 0
        else false

    let isModuleValueForCalls (entity: FSharpEntity option) (memb: FSharpMemberOrFunctionOrValue) =
        match entity with
        | Some entity -> isModuleValuePrivate true entity memb
        | None -> false

    let isModuleValueForDeclaration (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        | Some entity -> isModuleValuePrivate false entity memb
        | None -> false

    let getObjectMemberKind (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsImplicitConstructor || memb.IsConstructor then Fable.Constructor
        elif memb.IsPropertyGetterMethod && (getArgCount memb) = 0 then Fable.Getter
        elif memb.IsPropertySetterMethod && (getArgCount memb) = 1 then Fable.Setter
        else Fable.Method

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

    let (|FableCoreDynamicOp|_|) = function
        | BasicPatterns.Let((_, BasicPatterns.Call(None,m,_,_,[e1; e2])),_)
                when m.FullName = "Fable.Core.JsInterop.( ? )" -> Some(e1, e2)
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
        | "System.Decimal" -> Some Decimal
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.sbyte" _ -> Some Int8
        | Naming.StartsWith "Microsoft.FSharp.Core.int16" _ -> Some Int16
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
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

    let tryFindMember com (entity: FSharpEntity) membCompiledName isInstance (argTypes: Fable.Type list) =
        let argsEqual (args1: Fable.Type list) =
            let args1Len = List.length argTypes
            fun (args2: IList<IList<FSharpParameter>>) ->
                let args2Len = args2 |> Seq.sumBy (fun g -> g.Count)
                if args1Len = args2Len then
                    let args2 = args2 |> Seq.collect (fun g ->
                        g |> Seq.map (fun p -> makeType com Map.empty p.Type) |> Seq.toList)
                    listEquals typeEquals args1 (Seq.toList args2)
                else false
        // TODO: Check record fields
        getOwnAndInheritedFsharpMembers entity |> Seq.tryFind (fun m2 ->
            let argsEqual = argsEqual argTypes
            if m2.IsInstanceMember = isInstance && m2.CompiledName = membCompiledName then
                argsEqual m2.CurriedParameterGroups
            else false)

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

    let makeTryCatch com ctx (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                Some (catchVar, com.Transform(catchContext, catchBody))
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer)

    let matchGenericParams (memb: FSharpMemberOrFunctionOrValue) genArgs =
        (Map.empty, memb.GenericParameters, genArgs)
        |||> Seq.fold2 (fun acc genPar t -> Map.add genPar.Name t acc)

    let (|Replaced|_|) com ctx r typ argTypes genArgs thisArg args
            (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let isCandidate (entityFullName: string) =
            entityFullName.StartsWith("System.")
                || entityFullName.StartsWith("Microsoft.FSharp.")
                || entityFullName.StartsWith("Fable.Core.")

        let tryPipesAndComposition (com: ICompiler) r t args entityFullName compiledName  =
            let rec curriedApply r t applied args =
                Fable.Operation(Fable.CurriedApply(applied, args), t, r)
            let compose r t f1 f2 =
                let argType, retType =
                    match t with
                    | Fable.FunctionType(Fable.LambdaType argType, retType) -> argType, retType
                    | _ -> Fable.Any, Fable.Any
                let tempVar = com.GetUniqueVar() |> makeTypedIdent argType
                let body =
                    [Fable.IdentExpr tempVar]
                    |> curriedApply None Fable.Any f1
                    |> List.singleton
                    |> curriedApply r retType f2
                Fable.Function(Fable.Lambda tempVar, body)
            if entityFullName = "Microsoft.FSharp.Core.Operators" then
                match compiledName, args with
                | "op_PipeRight", [x; f]
                | "op_PipeLeft", [f; x] -> curriedApply r t f [x] |> Some
                | "op_PipeRight2", [x; y; f]
                | "op_PipeLeft2", [f; x; y] -> curriedApply r t f [x; y] |> Some
                | "op_PipeRight3", [x; y; z; f]
                | "op_PipeLeft3", [f; x; y; z] -> curriedApply r t f [x; y; z] |> Some
                | "op_ComposeRight", [f1; f2] -> compose r t f1 f2 |> Some
                | "op_ComposeLeft", [f2; f1] -> compose r t f1 f2 |> Some
                // Deal with reraise here too as we need the caught exception
                | "Reraise", _ -> failwith "TODO: Reraise"
                    // match info.caughtException with
                    // | Some ex ->
                    //     let ex = Fable.IdentValue ex |> Fable.Value
                    //     Fable.Throw (ex, typ, r) |> Some
                    // | None ->
                    //     "`reraise` used in context where caught exception is not available, please report"
                    //     |> addError com info.fileName info.range
                    //     Fable.Throw (newError None Fable.Any [], typ, r) |> Some
                | _ -> None
            else None

        match entity |> Option.bind (fun e -> e.TryFullName) with
        | Some entityFullName when isCandidate entityFullName ->
            let compiledName = memb.CompiledName
            match tryPipesAndComposition com r typ args entityFullName compiledName with
            | Some replaced -> Some replaced
            | None ->
                let info: Fable.CallInfo =
                  { ArgTypes = argTypes
                    DeclaringEntityFullName = entityFullName
                    CompiledName = memb.CompiledName
                    GenericArgs = List.map (makeType com ctx.typeArgs) genArgs
                                  |> matchGenericParams memb }
                match com.TryReplace(thisArg, args, info, typ, r) with
                | Some e -> Some e
                | None -> sprintf "Cannot resolve %s.%s" info.DeclaringEntityFullName info.CompiledName
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

    let (|Imported|_|) r typ argInfo (memb: FSharpMemberOrFunctionOrValue) =
        let importValueType = if Option.isSome argInfo then Fable.Any else typ
        match tryImported importValueType memb.CompiledName memb.Attributes with
        | Some importExpr ->
            match argInfo with
            | Some argInfo ->
                let emittedArgInfo = { argInfo with Fable.ThisArg = Some importExpr }
                match memb with
                // Allow combination of Import and Emit attributes
                | Emitted r typ (Some emittedArgInfo) emitted -> Some emitted
                | _ ->
                    if isModuleValueForCalls memb.DeclaringEntity memb
                    then Some importExpr
                    else staticCall r typ argInfo importExpr |> Some
            | None ->
                Some importExpr
        | None -> None

    let (|Inlined|_|) (com: IFableCompiler) ctx genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        if not(isInline memb)
        then None
        else
            // TODO: Log error if the inline function is called recursively
            let argIdents, fsExpr = com.GetInlineExpr(memb)
            let args = match callee with Some c -> c::args | None -> args
            let ctx, bindings =
                ((ctx, []), argIdents, args) |||> List.fold2 (fun (ctx, bindings) argId arg ->
                    let ctx, ident = bindIdentFrom com ctx argId
                    ctx, (ident, arg)::bindings)
            let ctx = { ctx with typeArgs = matchGenericParams memb genArgs }
            match com.Transform(ctx, fsExpr) with
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

    let memberRefTyped (com: IFableCompiler) typ (memb: FSharpMemberOrFunctionOrValue) =
        let memberName = getMemberDeclarationName com memb
        let file =
            match memb.DeclaringEntity with
            | Some ent -> (getEntityLocation ent).FileName |> Path.normalizePath
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> com.CurrentFile
        if file = com.CurrentFile
        then makeTypedIdent typ memberName |> Fable.IdentExpr
        else Fable.Import(memberName, file, Fable.Internal, typ)

    let memberRef (com: IFableCompiler) (memb: FSharpMemberOrFunctionOrValue) =
        memberRefTyped com Fable.Any memb

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        let call kind args =
            Fable.Operation(Fable.Call(kind, args), typ, r)
        let args = removeOmittedOptionalArguments memb args
        let argTypes = getArgTypes com memb
        let argInfo: Fable.ArgInfo =
          { ThisArg = callee
            Args = args
            ArgTypes = Some argTypes
            Spread = if hasSeqSpread memb then Fable.SeqSpread else Fable.NoSpread }
        match memb, memb.DeclaringEntity with
        | Imported r typ (Some argInfo) imported, _ -> imported
        | Emitted r typ (Some argInfo) emitted, _ -> emitted
        | Replaced com ctx r typ argTypes genArgs callee args replaced -> replaced
        | Inlined com ctx genArgs callee args expr, _ -> expr
        | Try (tryGetBoundExpr ctx r) funcExpr, entity ->
            if isModuleValueForCalls entity memb
            then funcExpr
            else staticCall r typ argInfo funcExpr
        // Check if this is an interface or abstract/overriden method
        | _, Some entity
                when entity.IsInterface
                || memb.IsOverrideOrExplicitInterfaceImplementation
                || isAbstract entity ->
            match callee with
            | Some callee ->
                match getObjectMemberKind memb, args with
                | Fable.Getter, _ -> get r typ callee memb.DisplayName
                | Fable.Setter, [arg] -> Fable.Set(callee, makeStrConst memb.DisplayName |> Fable.ExprSet, arg, r)
                // Constructor is unexpected (abstract class cons calls are resolved in transformConstructor)
                | _ -> makeStrConst memb.DisplayName |> Some |> instanceCall r typ argInfo
            | None -> "Unexpected static interface/override call" |> attachRange r |> failwith
        | _, entity ->
            if isModuleValueForCalls entity memb
            then memberRefTyped com typ memb
            else memberRef com memb |> staticCall r typ argInfo

    let makeValueFrom com (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType com ctx.typeArgs v.FullType
        match v with
        | _ when typ = Fable.Unit -> Fable.Value Fable.UnitConstant
        | Imported r typ None imported -> imported
        | Emitted r typ None emitted -> emitted
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr -> expr
        | _ -> memberRefTyped com typ v
