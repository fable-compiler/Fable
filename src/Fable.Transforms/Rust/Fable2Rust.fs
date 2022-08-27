module rec Fable.Transforms.Rust.Fable2Rust

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.Rust
open Fable.Transforms.Rust.AST.Helpers

module Rust = Fable.Transforms.Rust.AST.Types

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type Import = {
    Selector: string
    LocalIdent: string
    ModuleName: string
    ModulePath: string
    Path: string
    mutable Depths: int list
}

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: Fable.Ident list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames = {
    RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string>
}

type ScopedVarAttrs = {
    IsArm: bool
    IsRef: bool
    IsBox: bool
    // HasMultipleUses: bool
    mutable UsageCount: int
}

type Context = {
    File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    // HoistVars: Fable.Ident list -> bool
    // OptimizeTailCall: unit -> unit
    TailCallOpportunity: ITailCallOpportunity option
    ScopedTypeParams: Set<string>
    ScopedSymbols: FSharp.Collections.Map<string, ScopedVarAttrs>
    IsInPluralizedExpr: bool //this could be a closure in a map, or or a for loop. The point is anything leaving the scope cannot be assumed to be the only reference
    IsParamAnyType: bool
    IsParamByRefPreferred: bool
    RequiresSendSync: bool // a way to implicitly propagate Arc's down the hierarchy when it is not possible to explicitly tag
    ModuleDepth: int
}

type IRustCompiler =
    inherit Fable.Compiler
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit
    abstract GetAllImports: Context -> Import list
    abstract ClearAllImports: Context -> unit
    abstract GetAllModules: unit -> (string * string) list
    abstract GetImportName: Context * selector: string * path: string * SourceLocation option -> string
    abstract TransformExpr: Context * Fable.Expr -> Rust.Expr
    abstract GetEntity: entRef: Fable.EntityRef -> Fable.Entity

// TODO: Centralise and find a home for this
module Helpers =
    module Map =
        let except excludedKeys source =
            source |> Map.filter (fun key _v -> not (excludedKeys |> Set.contains key))
        let merge a b =
            (a, b) ||> Map.fold (fun acc key t -> acc |> Map.add key t)
        let mergeAndAggregate aggregateFn a b =
            (a, b) ||> Map.fold (fun acc key value ->
                match acc |> Map.tryFind key with
                | Some old -> acc |> Map.add key (aggregateFn old value)
                | None -> acc |> Map.add key value)

module UsageTracking =

    let calcIdentUsages expr =
        let mutable usages = Map.empty
        let mutable shadowed = Set.empty
        do FableTransforms.deepExists
            (function
                // Leaving this trivial nonshadowing impl here for debugging purposes!
                // | Fable.IdentExpr ident ->
                //         let count = usages |> Map.tryFind ident.Name |> Option.defaultValue 0
                //         usages <- usages |> Map.add ident.Name (count + 1)
                //         false
                | Fable.IdentExpr ident ->
                    if not (shadowed |> Set.contains ident.Name) then //if something is shadowed, no longer track it
                        let count = usages |> Map.tryFind ident.Name |> Option.defaultValue 0
                        usages <- usages |> Map.add ident.Name (count + 1)
                    false
                | Fable.Let(identPotentiallyShadowing, _, body) ->
                    //need to also count a shadowed
                    match body with
                    | Fable.IdentExpr ident when ident.Name = identPotentiallyShadowing.Name ->
                        //if an ident is shadowed by a self-binding (a = a), it will not be counted above, so need to explicitly handle here
                        //Why ever would we do this? This is a Rust scoping trick to foce cloning when taking ownership within a scope
                        let count = usages |> Map.tryFind ident.Name |> Option.defaultValue 0
                        usages <- usages |> Map.add ident.Name (count + 1)
                    | _ -> ()
                    if usages |> Map.containsKey identPotentiallyShadowing.Name then
                        shadowed <- shadowed |> Set.add identPotentiallyShadowing.Name
                    false
                | Fable.DecisionTree _
                | Fable.IfThenElse _ ->
                    shadowed <- Set.empty //for all conditional control flow, cannot reason about branches in shadow so just be conservative and assume no shadowing
                    false
                | _ -> false) expr
            |> ignore
        usages

    let isArmScoped ctx name =
        ctx.ScopedSymbols |> Map.tryFind name |> Option.map (fun s -> s.IsArm) |> Option.defaultValue false

    let isValueScoped ctx name =
        ctx.ScopedSymbols |> Map.tryFind name |> Option.map (fun s -> not s.IsRef) |> Option.defaultValue false

    let isRefScoped ctx name =
        ctx.ScopedSymbols |> Map.tryFind name |> Option.map (fun s -> s.IsRef) |> Option.defaultValue false

    let isBoxScoped ctx name =
        ctx.ScopedSymbols |> Map.tryFind name |> Option.map (fun s -> s.IsBox) |> Option.defaultValue false

    let usageCount name usages =
        Map.tryFind name usages |> Option.defaultValue 0

    // let hasMultipleUses (name: string) =
    //     Map.tryFind name >> Option.map (fun x -> x > 1) >> Option.defaultValue true

module TypeInfo =

    let splitName (fullName: string) =
        let i = fullName.LastIndexOf('.')
        if i < 0 then "", fullName
        else fullName.Substring(0, i), fullName.Substring(i + 1)

    let splitLast (fullName: string) =
        let i = fullName.LastIndexOf('.')
        if i < 0 then fullName
        else fullName.Substring(i + 1)

    let makeFullNamePath fullName genArgs =
        let parts = splitNameParts fullName
        mkGenericPath parts genArgs

    let makeFullNamePathExpr fullName genArgs =
        makeFullNamePath fullName genArgs
        |> mkPathExpr

    let makeFullNamePathTy fullName genArgs =
        makeFullNamePath fullName genArgs
        |> mkPathTy

    let makeFullNameIdentPat (fullName: string) =
        let fullName = fullName.Replace(".", "::")
        mkIdentPat fullName false false

    let primitiveType (name: string): Rust.Ty =
        mkGenericPathTy [name] None

    let getLibraryImportName (com: IRustCompiler) ctx moduleName typeName =
        let selector = moduleName + "_::" + typeName
        let libPath = getLibPath com moduleName
        com.GetImportName(ctx, selector, libPath, None)

    let makeImportType com ctx moduleName typeName tys: Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        tys |> mkGenericTy (splitNameParts importName)

    let makeLrcTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Lrc"

    let makeRcTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Rc"

    let makeArcTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Arc"

    let makeBoxTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Box"

    // TODO: emit Lazy or SyncLazy depending on threading.
    let makeLazyTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Lazy"

    // TODO: emit MutCell or AtomicCell depending on threading.
    let makeMutTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "MutCell"

    let makeOptionTy (ty: Rust.Ty): Rust.Ty =
        [ty] |> mkGenericTy [rawIdent "Option"]

    let hasAttribute fullName (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let hasInterface fullName (ent: Fable.Entity) =
        ent |> FSharp2Fable.Util.hasInterface fullName

    let hasStructuralEquality (ent: Fable.Entity) =
        not (ent |> hasAttribute Atts.noEquality)
            && (ent.IsFSharpRecord
            || (ent.IsFSharpUnion)
            || (ent.IsValueType)
            || (ent |> hasInterface Types.iStructuralEquatable))

    let hasStructuralComparison (ent: Fable.Entity) =
        not (ent |> hasAttribute Atts.noComparison)
            && (ent.IsFSharpRecord
            || (ent.IsFSharpUnion)
            || (ent.IsValueType)
            || (ent |> hasInterface Types.iStructuralComparable))

    let hasReferenceEquality (com: IRustCompiler) typ =
        match typ with
        | Fable.LambdaType _ -> true
        | Fable.DelegateType _ -> true
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            not (ent |> hasStructuralEquality)
        | _ -> false

    let hasMutableFields (com: IRustCompiler) (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            ent.UnionCases |> Seq.exists (fun uci ->
                uci.UnionCaseFields |> List.exists (fun fi -> fi.IsMutable)
            )
        else
            ent.FSharpFields |> Seq.exists (fun fi -> fi.IsMutable)

    let isEntityOfType (com: IRustCompiler) isTypeOf entNames (ent: Fable.Entity) =
        if Set.contains ent.FullName entNames then
            true // already checked, avoids circular checks
        else
            let entNames = Set.add ent.FullName entNames
            if ent.IsFSharpUnion then
                ent.UnionCases |> Seq.forall (fun uci ->
                    uci.UnionCaseFields |> List.forall (fun field ->
                        isTypeOf com entNames field.FieldType
                    )
                )
            else
                ent.FSharpFields |> Seq.forall (fun fi ->
                    isTypeOf com entNames fi.FieldType
                )

    let isTypeOfType (com: IRustCompiler) isTypeOf isEntityOf entNames typ =
        match typ with
        | Fable.Option(genArg, _) -> isTypeOf com entNames genArg
        | Fable.Array(genArg, _) -> isTypeOf com entNames genArg
        | Fable.List genArg -> isTypeOf com entNames genArg
        | Fable.Tuple(genArgs, _) ->
            List.forall (isTypeOf com entNames) genArgs
        | Fable.AnonymousRecordType(_, genArgs, _isStruct) ->
            List.forall (isTypeOf com entNames) genArgs
        | Replacements.Util.Builtin (Replacements.Util.FSharpSet genArg) ->
            isTypeOf com entNames genArg
        | Replacements.Util.Builtin (Replacements.Util.FSharpMap(k, v)) ->
            isTypeOf com entNames k && isTypeOf com entNames v
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            isEntityOf com entNames ent
        | _ ->
            true

    let isPrintableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more unprintable types?
        | Fable.LambdaType _
        | Fable.DelegateType _
            -> false
        | _ ->
            isTypeOfType com isPrintableType isPrintableEntity entNames typ

    let isPrintableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && (isEntityOfType com isPrintableType entNames ent)

    let isDefaultableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more undefaultable types?
        | Fable.String
        | Fable.LambdaType _
        | Fable.DelegateType _
            -> false
        | _ ->
            isTypeOfType com isDefaultableType isDefaultableEntity entNames typ

    let isDefaultableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && not (ent.IsFSharpUnion) // deriving 'Default' on enums is experimental
        && (isEntityOfType com isDefaultableType entNames ent)

    let isHashableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more unhashable types?
        | Fable.Number((Float32|Float64), _)
        | Fable.LambdaType _
        | Fable.DelegateType _
            -> false
        | _ ->
            isTypeOfType com isHashableType isHashableEntity entNames typ

    let isHashableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && (isEntityOfType com isHashableType entNames ent)

    let isCopyableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more uncopyable types?
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any
        | Fable.Unit
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.GenericParam _
        | Fable.String
        | Fable.Regex
            -> false
        | Fable.Tuple(genArgs, isStruct) ->
            isStruct && (List.forall (isCopyableType com entNames) genArgs)
        | Fable.AnonymousRecordType(_, genArgs, isStruct) ->
            isStruct && (List.forall (isCopyableType com entNames) genArgs)
        | _ ->
            isTypeOfType com isCopyableType isCopyableEntity entNames typ

    let isCopyableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && ent.IsValueType
        && not (hasMutableFields com ent)
        && (isEntityOfType com isCopyableType entNames ent)

    let isEquatableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more unequatable types?
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any
        | Fable.Unit
        | Fable.LambdaType _
        | Fable.DelegateType _
            -> false
        // | Fable.GenericParam(_, _, constraints) ->
        //     constraints |> List.contains Fable.Constraint.HasEquality
        | _ ->
            isTypeOfType com isEquatableType isEquatableEntity entNames typ

    let isEquatableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && (hasStructuralEquality ent)
        && (isEntityOfType com isEquatableType entNames ent)

    let isComparableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more uncomparable types?
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any
        | Fable.Unit
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.Regex
            -> false
        // | Fable.GenericParam(_, _, constraints) ->
        //     constraints |> List.contains Fable.Constraint.HasComparison
        | _ ->
            isTypeOfType com isComparableType isComparableEntity entNames typ

    let isComparableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && (hasStructuralComparison ent)
        && (isEntityOfType com isComparableType entNames ent)

    // Checks whether the type needs a ref counted wrapper
    // such as Rc<T> (or Arc<T> in a multithreaded context)
    let shouldBeRefCountWrapped (com: IRustCompiler) ctx typ =
        match typ with
        // passed by reference, no need to Rc-wrap
        | t when isInRefType com t
            -> None

        // always not Rc-wrapped
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any
        | Fable.Unit
        | Fable.Boolean
        | Fable.Char
        | Fable.Number _
        | Fable.String
        | Fable.GenericParam _
        // struct containers, no need to Rc-wrap
        | Fable.List _
        | Fable.Option _
        | Replacements.Util.Builtin (Replacements.Util.FSharpResult _)
        | Replacements.Util.Builtin (Replacements.Util.FSharpSet _)
        | Replacements.Util.Builtin (Replacements.Util.FSharpMap _)
            -> None

        // should be Rc or Arc-wrapped
        | Fable.LambdaType _
        | Fable.DelegateType _
            -> if ctx.RequiresSendSync then Some Arc else Some Lrc

        // should be Rc-wrapped
        | Fable.Regex
        | Fable.Array _
        | Replacements.Util.Builtin (Replacements.Util.BclHashSet _)
        | Replacements.Util.Builtin (Replacements.Util.BclDictionary _)
        | Replacements.Util.Builtin (Replacements.Util.FSharpReference _)
        | Replacements.Util.IsEntity (Types.keyCollection) _
        | Replacements.Util.IsEntity (Types.valueCollection) _
        | Replacements.Util.IsEnumerator _
            -> Some Lrc

        // should be Arc-wrapped
        | Replacements.Util.IsEntity (Types.fsharpAsyncGeneric) _
        | Replacements.Util.IsEntity (Types.task) _
        | Replacements.Util.IsEntity (Types.taskGeneric) _ ->
            Some Arc

        // conditionally Rc-wrapped
        | Fable.Tuple(_, isStruct) ->
            if isStruct then None else Some Lrc
        | Fable.AnonymousRecordType(_, _, isStruct) ->
            if isStruct then None else Some Lrc
        | Fable.DeclaredType(entRef, _) ->
            match com.GetEntity(entRef) with
            | HasEmitAttribute _ -> None
            // do not make custom types Rc-wrapped by default. This prevents inconsistency between type and implementation emit
            | HasReferenceTypeAttribute ptrType ->
                Some ptrType
            | ent ->
                if ent.IsValueType then None else Some Lrc

    let shouldBeDyn (com: IRustCompiler) typ=
        match typ with
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            ent.IsInterface && not ent.IsValueType
        | _ -> false

    let TypeImplementsCloneTrait (com: IRustCompiler) typ =
        match typ with
        | Fable.String
        | Fable.GenericParam _
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.Option _
        | Fable.List _
        | Fable.Array _
            -> true

        | Fable.AnonymousRecordType _ -> true
        | Fable.DeclaredType(entRef, _) -> true

        | _ -> false

    let TypeImplementsCopyTrait (com: IRustCompiler) typ =
        match typ with
        | Fable.Unit
        | Fable.Boolean
        | Fable.Char _
        | Fable.Number _ -> true
        | _ -> false

    let rec tryGetIdent = function
        | Fable.IdentExpr i -> i.Name |> Some
        | Fable.Get (expr, Fable.OptionValue, _, _) -> tryGetIdent expr
        | Fable.Get (expr, Fable.UnionField _, _, _) -> tryGetIdent expr
        | Fable.Operation (Fable.Unary(UnaryOperator.UnaryAddressOf, expr), _, _) -> tryGetIdent expr
        | _ -> None

    let getIdentName expr =
        tryGetIdent expr |> Option.defaultValue ""

    let transformImport (com: IRustCompiler) ctx r t (info: Fable.ImportInfo) genArgs =
        if info.Selector.Contains("*") || info.Selector.Contains("{") then
            let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
            mkUnitExpr () // just an import without a body
        else
            match info.Kind with
            | Fable.MemberImport(isInstance, _) when isInstance = true ->
                // no import needed
                makeFullNamePathExpr info.Selector genArgs
            | Fable.MemberImport(isInstance, _) when isInstance = false ->
                // for constructors or static members, import just the type
                let selector, membName = splitName info.Selector
                let importName = com.GetImportName(ctx, selector, info.Path, r)
                makeFullNamePathExpr (importName + "::" + membName) genArgs
            | _ ->
                let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
                makeFullNamePathExpr importName genArgs

    let makeLibCall com ctx genArgs moduleName memberName (args: Rust.Expr list) =
        let importName = getLibraryImportName com ctx moduleName memberName
        let callee = makeFullNamePathExpr importName genArgs
        mkCallExpr callee args

    let libCall com ctx r types moduleName memberName (args: Fable.Expr list) =
        let path = getLibPath com moduleName
        let selector = moduleName + "_::" + memberName
        let info: Fable.ImportInfo =
            { Selector = selector; Path = path; Kind = Fable.LibraryImport }
        let genArgs = transformGenArgs com ctx types
        let callee = transformImport com ctx r Fable.Any info genArgs
        Util.callFunction com ctx r callee args

    let genArgsUnitsFilter = function
        | Fable.Measure _ | Fable.GenericParam(_, true, _)
        | Replacements.Util.IsEntity (Types.measureProduct2) _ -> false
        | _ -> true

    let transformGenArgs com ctx genArgs: Rust.GenericArgs option =
        genArgs
        |> List.filter genArgsUnitsFilter
        |> List.map (transformType com ctx)
        |> mkGenericTypeArgs

    let transformGenericType com ctx genArgs typeName: Rust.Ty =
        genArgs
        |> List.filter genArgsUnitsFilter
        |> List.map (transformType com ctx)
        |> mkGenericTy (splitNameParts typeName)

    let transformImportType com ctx genArgs moduleName typeName: Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        transformGenericType com ctx genArgs importName

    let transformDecimalType com ctx: Rust.Ty =
        transformImportType com ctx [] "Decimal" "decimal"

    let transformListType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "List" "List"

    let transformSetType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Set" "Set"

    let transformMapType com ctx genArgs: Rust.Ty =
        transformImportType com ctx genArgs "Map" "Map"

    let transformArrayType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Native" "MutArray"

    let transformHashSetType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Native" "MutHashSet"

    let transformHashMapType com ctx genArgs: Rust.Ty =
        transformImportType com ctx genArgs "Native" "MutHashMap"

    let transformGuidType com ctx: Rust.Ty =
        transformImportType com ctx [] "Guid" "Guid"

    let transformDateTimeType com ctx: Rust.Ty =
        transformImportType com ctx [] "DateTime" "DateTime"

    let transformTimeSpanType com ctx: Rust.Ty =
        transformImportType com ctx [] "TimeSpan" "TimeSpan"

    let transformDateTimeOffsetType com ctx: Rust.Ty =
        transformImportType com ctx [] "DateTimeOffset" "DateTimeOffset"

    let transformAsyncType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Async" "Async"

    let transformTaskType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Task" "Task"

    let transformTaskBuilderType com ctx: Rust.Ty =
        transformImportType com ctx [] "TaskBuilder" "TaskBuilder"

    let transformThreadType com ctx: Rust.Ty =
        transformImportType com ctx [] "Thread" "Thread"

    let transformTupleType com ctx isStruct genArgs: Rust.Ty =
        genArgs
        |> List.map (transformType com ctx)
        |> mkTupleTy

    let transformOptionType com ctx genArg: Rust.Ty =
        transformGenericType com ctx [genArg] (rawIdent "Option")

    let transformParamType com ctx typ: Rust.Ty =
        let ty = transformType com ctx typ
        if isByRefType com typ || ctx.IsParamByRefPreferred
        then ty |> mkRefTy
        else ty

    // let transformLambdaType com ctx argTypes returnType: Rust.Ty =
    //     let fnRetTy =
    //         if returnType = Fable.Unit then VOID_RETURN_TY
    //         else transformType com ctx returnType |> mkFnRetTy
    //     let pat = makeFullNameIdentPat "a"
    //     let inputs = argTypes |> List.map (fun tInput ->
    //         mkParam [] (transformParamType com ctx tInput) pat false)
    //     let fnDecl = mkFnDecl inputs fnRetTy
    //     let genParams = [] // TODO:
    //     mkFnTy genParams fnDecl

    let transformClosureType com ctx argTypes returnType: Rust.Ty =
        let inputs =
            match argTypes with
            | [Fable.Unit] -> []
            | _ -> argTypes |> List.filter genArgsUnitsFilter |> List.map (transformParamType com ctx)
        let output =
            if returnType = Fable.Unit then VOID_RETURN_TY
            else
                returnType |> transformType com ctx |> mkParenTy |> mkFnRetTy
        let bounds = [
            mkFnTraitGenericBound inputs output
            mkLifetimeGenericBound "'static"
        ]
        mkDynTraitTy bounds

    let transformNumberType com ctx kind: Rust.Ty =
        match kind with
        | Int8 -> "i8" |> primitiveType
        | UInt8 -> "u8" |> primitiveType
        | Int16 -> "i16" |> primitiveType
        | UInt16 -> "u16" |> primitiveType
        | Int32 -> "i32" |> primitiveType
        | UInt32 -> "u32" |> primitiveType
        | Int64 -> "i64" |> primitiveType
        | UInt64 -> "u64" |> primitiveType
        | NativeInt -> "isize" |> primitiveType
        | UNativeInt -> "usize" |> primitiveType
        | Float32 -> "f32" |> primitiveType
        | Float64 -> "f64" |> primitiveType
        | Decimal -> transformDecimalType com ctx
        | BigInt -> makeFullNamePathTy Types.bigint None

    let getEntityFullName (com: IRustCompiler) ctx (entRef: Fable.EntityRef) =
        match entRef.SourcePath with
        | Some path when path <> com.CurrentFile ->
            // entity is imported from another file
            let importPath = Fable.Path.getRelativeFileOrDirPath false com.CurrentFile false path
            let importName = com.GetImportName(ctx, entRef.FullName, importPath, None)
            importName
        | _ ->
            entRef.FullName

    let declaredInterfaces =
        Set.ofList [
            Types.icollection
            Types.icollectionGeneric
            Types.idictionary
            Types.ireadonlydictionary
            Types.idisposable
            Types.ienumerable
            Types.ienumerableGeneric
            Types.ienumerator
            Types.ienumeratorGeneric
            Types.iequatableGeneric
            Types.icomparable
            Types.icomparableGeneric
            Types.iStructuralEquatable
            Types.iStructuralComparable
        ]

    let isDeclaredInterface fullName =
        Set.contains fullName declaredInterfaces

    let getInterfaceEntityName (com: IRustCompiler) ctx (entRef: Fable.EntityRef) =
        if isDeclaredInterface entRef.FullName
        then getLibraryImportName com ctx "Interfaces" entRef.FullName
        else getEntityFullName com ctx entRef

    let tryFindInterface (com: IRustCompiler) fullName (entRef: Fable.EntityRef): Fable.DeclaredType option =
        let ent = com.GetEntity(entRef)
        ent.AllInterfaces |> Seq.tryFind (fun ifc -> ifc.Entity.FullName = fullName)

    let transformInterfaceType (com: IRustCompiler) ctx (entRef: Fable.EntityRef) genArgs: Rust.Ty =
        let nameParts = getInterfaceEntityName com ctx entRef |> splitNameParts
        let genArgs = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgs
        mkDynTraitTy [traitBound]

    let (|HasEmitAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes |> Seq.tryPick (fun att ->
            if att.Entity.FullName.StartsWith(Atts.emit) then
                match att.ConstructorArgs with
                | [:? string as macro] -> Some macro
                | _ -> None
            else None)

    type PointerType =
        | Lrc
        | Rc
        | Arc
        | Box

    let (|HasReferenceTypeAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes |> Seq.tryPick (fun att ->
            if att.Entity.FullName.StartsWith(Atts.referenceType) then
                match att.ConstructorArgs with
                | [:? int as ptrType] ->
                    match ptrType with
                    | 0 -> Some Lrc
                    | 1 -> Some Rc
                    | 2 -> Some Arc
                    | 3 -> Some Box
                    | _ -> None
                | _ -> None
            else None)

    let (|IsNonErasedInterface|_|) (com: Compiler) = function
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)
            if ent.IsInterface && not (ent |> hasAttribute Atts.erase)
            then Some(entRef, genArgs)
            else None
        | _ -> None

    let transformDeclaredType (com: IRustCompiler) ctx entRef genArgs: Rust.Ty =
        match com.GetEntity(entRef) with
        | HasEmitAttribute value ->
            let genArgs = genArgs |> List.map (transformType com ctx)
            mkEmitTy value genArgs
        | ent when ent.IsInterface ->
            transformInterfaceType com ctx entRef genArgs
        | ent ->
            let genArgs =
                genArgs
                |> List.zip ent.GenericParameters
                |> List.choose (fun (p, a) -> if not p.IsMeasure then Some a else None)
            let genArgs = transformGenArgs com ctx genArgs
            let entName = getEntityFullName com ctx entRef
            makeFullNamePathTy entName genArgs

    let transformResultType com ctx genArgs: Rust.Ty =
        transformGenericType com ctx genArgs (rawIdent "Result")

    let transformChoiceType com ctx genArgs: Rust.Ty =
        let argCount = string (List.length genArgs)
        transformImportType com ctx genArgs "Choice" ("Choice`" + argCount)

    let transformRefCellType com ctx genArg: Rust.Ty =
        let ty = transformType com ctx genArg
        ty |> makeMutTy com ctx

    let isAddrOfExpr (expr: Fable.Expr) =
        match expr with
        | Fable.Operation(Fable.Unary (UnaryOperator.UnaryAddressOf, e), _, _) -> true
        | _ -> false

    let isByRefType (com: IRustCompiler) = function
        | Replacements.Util.IsByRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let isInRefType (com: IRustCompiler) = function
        | Replacements.Util.IsInRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let isInterface (com: IRustCompiler) = function
        | IsNonErasedInterface com _ -> true
        | _ -> false

    // let inferredType = Fable.GenericParam(rawIdent "_", false, [])

    // let inferIfAny t = match t with | Fable.Any -> inferredType | _ -> t

    let transformAnyType com ctx: Rust.Ty =
        if ctx.IsParamAnyType then
            let importName = getLibraryImportName com ctx "Native" "Any"
            let traitBound = mkTypeTraitGenericBound [importName] None
            mkDynTraitTy [traitBound]
        else mkInferTy ()

    let transformMetaType com ctx: Rust.Ty =
        transformImportType com ctx [] "Native" "TypeId"

    let transformStringType com ctx: Rust.Ty =
        transformImportType com ctx [] "String" "string"

    let transformType (com: IRustCompiler) ctx (typ: Fable.Type): Rust.Ty =
        let ty =
            match typ with
            | Fable.Any -> transformAnyType com ctx
            | Fable.Unit -> mkUnitTy ()
            | Fable.Measure _ -> mkInferTy ()
            | Fable.Char -> primitiveType "char"
            | Fable.Boolean -> primitiveType "bool"
            | Fable.String -> transformStringType com ctx
            | Fable.MetaType -> transformMetaType com ctx
            | Fable.Number(kind, _) -> transformNumberType com ctx kind
            | Fable.GenericParam(name, _, _) -> primitiveType name
            | Fable.LambdaType(argType, returnType) ->
                let argTypes, returnType = [argType], returnType
                transformClosureType com ctx argTypes returnType
            | Fable.DelegateType(argTypes, returnType) ->
                transformClosureType com ctx argTypes returnType
            | Fable.Tuple(genArgs, isStruct) -> transformTupleType com ctx isStruct genArgs
            | Fable.Option(genArg, _isStruct) -> transformOptionType com ctx genArg
            | Fable.Array(genArg, _kind) -> transformArrayType com ctx genArg
            | Fable.List genArg -> transformListType com ctx genArg
            | Fable.Regex ->
                // nonGenericTypeInfo Types.regex
                TODO_TYPE $"%A{typ}" //TODO:
            | Fable.AnonymousRecordType(fieldNames, genArgs, isStruct) ->
                transformTupleType com ctx isStruct genArgs

            // pre-defined declared types
            | Replacements.Util.IsEntity (Types.iset) (entRef, [genArg]) -> transformHashSetType com ctx genArg
            | Replacements.Util.IsEntity (Types.idictionary) (entRef, [k; v]) -> transformHashMapType com ctx [k; v]
            | Replacements.Util.IsEntity (Types.ireadonlydictionary) (entRef, [k; v]) -> transformHashMapType com ctx [k; v]
            | Replacements.Util.IsEntity (Types.keyCollection) (entRef, [k; v]) -> transformArrayType com ctx k
            | Replacements.Util.IsEntity (Types.valueCollection) (entRef, [k; v]) -> transformArrayType com ctx v
            | Replacements.Util.IsEntity (Types.icollectionGeneric) (entRef, [t]) -> transformArrayType com ctx t
            | Replacements.Util.IsEntity (Types.fsharpAsyncGeneric) (_, [t]) -> transformAsyncType com ctx t
            | Replacements.Util.IsEntity (Types.taskGeneric) (_, [t]) -> transformTaskType com ctx t
            | Replacements.Util.IsEntity (Types.taskBuilder) (_, []) -> transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.taskBuilderModule) (_, []) -> transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.thread) (_, []) -> transformThreadType com ctx
            | Replacements.Util.IsEnumerator (entRef, genArgs) ->
                // get IEnumerator interface from enumerator object
                match tryFindInterface com Types.ienumeratorGeneric entRef with
                | Some ifc -> transformInterfaceType com ctx ifc.Entity [Fable.Any]
                | _ -> failwith "Cannot find IEnumerator interface, should not happen."

            // other declared types
            | Fable.DeclaredType(entRef, genArgs) ->
                match entRef.FullName, genArgs with
                | Replacements.Util.BuiltinEntity kind ->
                    match kind with
                    | Replacements.Util.BclDateOnly
                    | Replacements.Util.BclTimeOnly
                    | Replacements.Util.BclTimer
                        -> transformDeclaredType com ctx entRef genArgs
                    | Replacements.Util.BclGuid -> transformGuidType com ctx
                    | Replacements.Util.BclDateTime -> transformDateTimeType com ctx
                    | Replacements.Util.BclTimeSpan -> transformTimeSpanType com ctx
                    | Replacements.Util.BclDateTimeOffset -> transformDateTimeOffsetType com ctx
                    | Replacements.Util.BclHashSet(genArg) -> transformHashSetType com ctx genArg
                    | Replacements.Util.BclDictionary(k, v) -> transformHashMapType com ctx [k; v]
                    | Replacements.Util.FSharpSet(genArg) -> transformSetType com ctx genArg
                    | Replacements.Util.FSharpMap(k, v) -> transformMapType com ctx [k; v]
                    | Replacements.Util.BclKeyValuePair(k, v) -> transformTupleType com ctx true [k; v]
                    | Replacements.Util.FSharpResult(ok, err) -> transformResultType com ctx [ok; err]
                    | Replacements.Util.FSharpChoice genArgs -> transformChoiceType com ctx genArgs
                    | Replacements.Util.FSharpReference(genArg) ->
                        if isInRefType com typ
                        then transformType com ctx genArg
                        else transformRefCellType com ctx genArg
                | _ ->
                    transformDeclaredType com ctx entRef genArgs

                    // // let generics = generics |> List.map (transformTypeInfo com ctx r genMap) |> List.toArray
                    // // Check if the entity is actually declared in JS code
                    // if ent.IsInterface
                    //     || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    //     || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                    //     || FSharp2Fable.Util.isReplacementCandidate ent then
                    //     genericEntity ent.FullName generics
                    // else
                    //     let reflectionMethodExpr = FSharp2Fable.Util.entityIdentWithSuffix com ent Naming.reflectionSuffix
                    //     let callee = com.TransformExpr(ctx, reflectionMethodExpr)
                    //     Expression.callExpression(callee, generics)

        match shouldBeRefCountWrapped com ctx typ with
        | Some Lrc -> ty |> makeLrcTy com ctx
        | Some Rc ->  ty |> makeRcTy com ctx
        | Some Arc -> ty |> makeArcTy com ctx
        | Some Box -> ty |> makeBoxTy com ctx
        | _ -> ty

(*
    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            [|
                yield Expression.stringLiteral(fullname)
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield Expression.arrayExpression(generics)
                match tryJsConstructor com ctx ent with
                | Some cons -> yield cons
                | None -> ()
                match ent.BaseType with
                | Some d ->
                    let genMap =
                        Seq.zip ent.GenericParameters generics
                        |> Seq.map (fun (p, e) -> p.Name, e)
                        |> Map
                    yield Fable.DeclaredType(d.Entity, d.GenericArgs)
                          |> transformTypeInfo com ctx r genMap
                | None -> ()
            |]
            |> libReflectionCall com ctx r "class"

    let private ofString s = Expression.stringLiteral(s)
    let private ofArray rustExprs = Expression.arrayExpression(List.toArray rustExprs)
*)

module Util =

    // open Lib
    // open Reflection
    open UsageTracking
    open TypeInfo

    let (|TransformExpr|) (com: IRustCompiler) ctx e =
        com.TransformExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, info) -> Some([arg], body, info)
        | Fable.Delegate(args, body, info, []) -> Some(args, body, info)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let (|IDisposable|_|) = function
        | Replacements.Util.IsEntity (Types.idisposable) _ -> Some()
        | _ -> None

    let (|IFormattable|_|) = function
        | Replacements.Util.IsEntity (Types.iformattable) _ -> Some()
        | _ -> None

    let (|IEquatable|_|) = function
        | Replacements.Util.IsEntity (Types.iequatableGeneric) (_, [genArg]) -> Some(genArg)
        | _ -> None

    let (|IEnumerable|_|) = function
        | Replacements.Util.IsEntity (Types.ienumerableGeneric) (_, [genArg]) -> Some(genArg)
        | _ -> None

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgument && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    /// Fable doesn't currently sanitize attached members/fields so we do a simple sanitation here.
    /// Should this be done in FSharp2Fable step?
    let sanitizeMember (name: string) =
        FSharp2Fable.Helpers.cleanNameAsRustIdentifier name

    let makeUniqueName name (usedNames: Set<string>) =
        name |> Fable.Naming.preventConflicts (usedNames.Contains)

    let getUniqueNameInRootScope (ctx: Context) name =
        let name = name |> Fable.Naming.preventConflicts (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.DeclarationScopes.Contains(name))
        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name = name |> Fable.Naming.preventConflicts (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.CurrentDeclarationScope.Contains(name))
        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: IRustCompiler, ctx, name, args: Fable.Ident list) =
        let args = args |> List.filter (fun arg -> not (arg.IsThisArgument))
        let label = splitLast name
        interface ITailCallOpportunity with
            member _.Label = label
            member _.Args = args
            member _.IsRecursiveRef(e) =
                match e with
                | Fable.IdentExpr id -> name = id.Name
                | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let transformIdent com ctx r (ident: Fable.Ident) =
        match ctx.ScopedSymbols |> Map.tryFind ident.Name with
        | Some varAttrs ->
            //ident has been seen, subtract 1
            varAttrs.UsageCount <- varAttrs.UsageCount - 1
        | None -> ()
        if ident.IsThisArgument
        then mkGenericPathExpr [rawIdent "self"] None
        else mkGenericPathExpr (splitNameParts ident.Name) None

    // let transformExprMaybeIdentExpr (com: IRustCompiler) ctx (expr: Fable.Expr) =
    //     match expr with
    //     | Fable.IdentExpr id when id.IsThisArgument ->
    //         // avoids the extra Lrc wrapping for self that transformIdentGet does
    //         transformIdent com ctx None id
    //     | _ -> com.TransformExpr(ctx, expr)

    let transformIdentGet com ctx r (ident: Fable.Ident) =
        let expr = transformIdent com ctx r ident
        if ident.IsMutable && not (isInRefType com ident.Type) then
            expr |> mutableGet
        elif isBoxScoped ctx ident.Name then
            expr |> makeLrcValue com ctx
        // elif isRefScoped ctx ident.Name then
        //     expr |> makeClone // |> mkDerefExpr |> mkParenExpr
        else expr

    let transformIdentSet com ctx r (ident: Fable.Ident) (value: Rust.Expr) =
        let expr = transformIdent com ctx r ident
        // assert(ident.IsMutable)
        mutableSet expr value

    let memberFromName (memberName: string): Rust.Expr * bool =
        match memberName with
        | "ToString" -> (mkGenericPathExpr ["ToString"] None), false
        // | n when n.StartsWith("Symbol.") ->
        //     Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n.[7..]), false), true
        // | n when Naming.hasIdentForbiddenChars n -> Expression.stringLiteral(n), true
        | n -> (mkGenericPathExpr [n] None), false

    let getField r (expr: Rust.Expr) (fieldName: string) =
        mkFieldExpr expr (fieldName |> sanitizeMember) // ?loc=r)

    let getExpr r (expr: Rust.Expr) (index: Rust.Expr) =
        mkIndexExpr expr index // ?loc=r)

    let callFunction com ctx range (callee: Rust.Expr) (args: Fable.Expr list) =
        let trArgs = transformCallArgs com ctx args [] []
        mkCallExpr callee trArgs //?loc=range)

    // /// Immediately Invoked Function Expression
    // let iife (com: IRustCompiler) ctx (expr: Fable.Expr) =
    //     let fnExpr = transformLambda com ctx None [] expr
    //     let range = None // TODO:
    //     callFunction com ctx range fnExpr []

    let getGenericParams (ctx: Context) (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam (_, false, _) as p -> [p]
            | t -> t.Generics |> List.collect getGenParams
        let mutable dedupSet = ctx.ScopedTypeParams
        types
        |> List.collect getGenParams
        |> List.filter (function
            | Fable.GenericParam(name=name) ->
                if Set.contains name dedupSet then false
                else dedupSet <- Set.add name dedupSet; true
            | _ -> false)

(*
    type MemberKind =
        | ClassConstructor
        | NonAttached of funcName: string
        | Attached of isStatic: bool

    let getMemberArgsAndBody (com: IRustCompiler) ctx kind hasSpread (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, genTypeParams, args, body =
            match kind, args with
            | Attached(isStatic=false), (thisArg::args) ->
                let genTypeParams = Set.difference (getGenericTypeParams [thisArg.Type]) ctx.ScopedTypeParams
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if FableTransforms.isIdentUsed thisArg.Name body then
                        let thisKeyword = Fable.IdentExpr { thisArg with Name = "this" }
                        Fable.Let(thisArg, thisKeyword, body)
                    else body
                None, genTypeParams, args, body
            | Attached(isStatic=true), _
            | ClassConstructor, _ -> None, ctx.ScopedTypeParams, args, body
            | NonAttached funcName, _ -> Some funcName, Set.empty, args, body
            | _ -> None, Set.empty, args, body

        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams }
        let args, body, returnType, typeParamDecl = transformFunctionWithAnnotations com ctx funcName args body

        let typeParamDecl =
            if com.Options.Typescript then
                makeTypeParamDecl genTypeParams |> mergeTypeParamDecls typeParamDecl
            else typeParamDecl

        let args =
            let len = Array.length args
            if not hasSpread || len = 0 then args
            else [|
                if len > 1 then
                    yield! args.[..len-2]
                yield restElement args.[len-1]
            |]

        args, body, returnType, typeParamDecl

    let getUnionCaseName (uci: Fable.UnionCase) =
        // match uci.CompiledName with Some cname -> cname | None -> uci.Name
        uci.FullName

    let getUnionExprTag (com: IRustCompiler) ctx range (fableExpr: Fable.Expr) =
        let expr = com.TransformExpr(ctx, fableExpr)
        // getExpr range expr (Expression.stringLiteral("tag"))
        expr

    /// Wrap int expressions with '| 0' to help optimization of JS VMs
    let wrapIntExpression typ (e: Rust.Expr) =
        match e, typ with
        | Literal(NumericLiteral(_)), _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32) ->
            Expression.binaryExpression(BinaryOrBitwise, e, Expression.numericLiteral(0.))
        | _ -> e

    let wrapExprInBlockWithReturn e =
        BlockStatement([| Statement.returnStatement(e)|])

    let makeArrowFunctionExpression _name (args, (body: BlockStatement), returnType, typeParamDecl): Rust.Expr =
        Expression.arrowFunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let makeFunctionExpression name (args, (body: Rust.Expr), returnType, typeParamDecl): Rust.Expr =
        let id = name |> Option.map Identifier.identifier
        let body = wrapExprInBlockWithReturn body
        Expression.functionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)
*)
    let getCellType = function
        | Replacements.Util.Builtin (Replacements.Util.FSharpReference t) -> t
        | t -> t

    let optimizeTailCall com ctx r (tc: ITailCallOpportunity) (args: Fable.Expr list): Rust.Expr =
        let tempArgs = tc.Args |> List.map (fun arg ->
            { arg with Name = arg.Name + "_temp"; IsMutable = false; Type = getCellType arg.Type })
        let bindings = List.zip tempArgs args
        let tempLetStmts, ctx = makeLetStmts com ctx bindings Map.empty
        let setArgStmts =
            List.zip tc.Args tempArgs
            |> List.map (fun (id, idTemp) ->
                let value = transformIdentGet com ctx r idTemp
                transformIdentSet com ctx r id value |> mkExprStmt)
        let continueStmt = mkContinueExpr (Some tc.Label) |> mkExprStmt
        tempLetStmts @ setArgStmts @ [continueStmt]
        |> mkStmtBlockExpr

    let transformCast (com: IRustCompiler) (ctx: Context) typ (fableExpr: Fable.Expr): Rust.Expr =
        // search the typecast chain for a matching type
        let rec getNestedExpr typ expr =
            match expr with
            | Fable.TypeCast(e, t) when t <> typ -> getNestedExpr t e
            | _ -> expr
        let nestedExpr = getNestedExpr typ fableExpr
        let fableExpr =
            // optimization to eliminate unnecessary casts
            if nestedExpr.Type = typ then nestedExpr else fableExpr
        let fromType, toType = fableExpr.Type, typ
        let expr = transformLeaveContext com ctx (Some typ) fableExpr
        let ty = transformType com ctx typ

        match fromType, toType with
        | t1, t2 when t1 = t2 ->
            expr // no cast needed if types are the same
        | Fable.Number _, Fable.Number _ ->
            expr |> mkCastExpr ty
        | Fable.Char, Fable.Number(UInt32, Fable.NumberInfo.Empty) ->
            expr |> mkCastExpr ty
        | Fable.Tuple(ga1, false), Fable.Tuple(ga2, true) when ga1 = ga2 ->
            expr |> makeAsRef |> makeClone  //.ToValueTuple()
        | Fable.Tuple(ga1, true), Fable.Tuple(ga2, false) when ga1 = ga2 ->
            expr |> makeLrcValue com ctx            //.ToTuple()

        // casts to IEnumerable
        | Replacements.Util.IsEntity (Types.keyCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.valueCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.icollectionGeneric) _, IEnumerable _
        | Fable.Array _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofArray" [expr]
        | Fable.List _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofList" [expr]
        | Replacements.Util.IsEntity (Types.hashset) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.iset) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "Native" "hashSetEntries" [expr]
            makeLibCall com ctx None "Seq" "ofArray" [ar]
        | Replacements.Util.IsEntity (Types.dictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.idictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.ireadonlydictionary) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "Native" "hashMapEntries" [expr]
            makeLibCall com ctx None "Seq" "ofArray" [ar]

        // casts to generic param
        | _, Fable.GenericParam(name, _isMeasure, _constraints) ->
            makeCall [name; "from"] None [expr] // e.g. T::from(value)
        // casts to interface
        | Replacements.Util.IsEntity (Types.dictionary) _, Replacements.Util.IsEntity (Types.idictionary) _ ->
            expr
        | t1, t2 when not (shouldBeDyn com t1) && shouldBeDyn com t2 ->
            expr |> makeClone |> makeLrcValue com ctx |> mkCastExpr ty
        | _, t when isInterface com t ->
            expr |> makeClone |> mkCastExpr ty
        // // casts to object
        // | _, Fable.Any ->
        //     let ctx = { ctx with IsParamAnyType = true }
        //     let ty = transformType com ctx toType
        //     expr |> mkCastExpr (ty |> mkRefTy)
        // TODO: other casts?
        | _ ->
            //TODO: add warning?
            expr // no cast is better than error

(*
    let transformCast (com: IRustCompiler) (ctx: Context) t tag e: Rust.Expr =
        // HACK: Try to optimize some patterns after FableTransforms
        let optimized =
            match tag with
            | Some(Naming.StartsWith "optimizable:" optimization) ->
                match optimization, e with
                | "array", Fable.Call(_,info,_,_) ->
                    match info.Args with
                    | [Replacements.Util.ArrayOrListLiteral(vals,_)] -> Fable.Value(Fable.NewArray(vals, Fable.Any), e.Range) |> Some
                    | _ -> None
                | "pojo", Fable.Call(_,info,_,_) ->
                    match info.Args with
                    | keyValueList::caseRule::_ -> Replacements.makePojo com (Some caseRule) keyValueList
                    | keyValueList::_ -> Replacements.makePojo com None keyValueList
                    | _ -> None
                | _ -> None
            | _ -> None

        match optimized, t with
        | Some e, _ -> com.TransformExpr(ctx, e)
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | None, Fable.DeclaredType(ent,[_]) ->
            match ent.FullName, e with
            | Types.ienumerableGeneric, Replacements.Util.ArrayOrListLiteral(exprs, _) ->
                makeArray com ctx exprs
            | _ -> com.TransformExpr(ctx, e)
        | _ -> com.TransformExpr(ctx, e)
*)
    let transformCurry (com: IRustCompiler) (ctx: Context) expr arity: Rust.Expr =
        com.TransformExpr(ctx, Replacements.Api.curryExprAtRuntime com arity expr)

    /// This guarantees a new owned Rc<T>
    let makeClone expr = mkMethodCallExprOnce "clone" None expr []

    /// Calling this on an rc guarantees a &T, regardless of if the Rc is a ref or not
    let makeAsRef expr = mkMethodCallExpr "as_ref" None expr []

    let makeCall pathNames genArgs (args: Rust.Expr list) =
        let callee = mkGenericPathExpr pathNames genArgs
        mkCallExpr callee args

    let makeLrcValue com ctx (value: Rust.Expr) =
        let name = getLibraryImportName com ctx "Native" "Lrc"
        makeCall [name; "new"] None [value]

    let makeRcValue com ctx (value: Rust.Expr) =
        let name = getLibraryImportName com ctx "Native" "Rc"
        makeCall [name; "new"] None [value]

    let makeArcValue com ctx (value: Rust.Expr) =
        let name = getLibraryImportName com ctx "Native" "Arc"
        makeCall [name; "new"] None [value]

    let makeBoxValue com ctx (value: Rust.Expr) =
        let name = getLibraryImportName com ctx "Native" "Box"
        makeCall [name; "new"] None [value]

    let maybeWrapSmartPtr com ctx ent expr =
        match ent with
        | HasReferenceTypeAttribute a ->
            match a with
            | Lrc -> expr |> makeLrcValue com ctx
            | Rc -> expr |> makeRcValue com ctx
            | Arc -> expr |> makeArcValue com ctx
            | Box -> expr |> makeBoxValue com ctx
        | _ ->
            match ent.FullName with
            | Types.fsharpAsyncGeneric
            | Types.task
            | Types.taskGeneric ->
                expr |> makeArcValue com ctx
            | _ ->
                expr |> makeLrcValue com ctx

    let makeMutValue (value: Rust.Expr) =
        makeCall ["MutCell";"new"] None [value]

    let makeLazyValue (value: Rust.Expr) =
        makeCall ["Lazy";"new"] None [value]

    let parameterIsByRefPreferred idx (parameters: Fable.Parameter list) =
        parameters
        |> List.tryItem idx
        |> Option.map (fun p -> p.Attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef))
        |> Option.defaultValue false

    let transformCallArgs (com: IRustCompiler) ctx (args: Fable.Expr list) (argTypes: Fable.Type list) (parameters: Fable.Parameter list) =
        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant, _))] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformExpr(ctx, last))]
        | args ->
            let argsWithTypes =
                if argTypes.Length = args.Length
                then args |> List.zip argTypes |> List.map(fun (t, a) -> Some t, a)
                else args |> List.map (fun a -> None, a)
            argsWithTypes |> List.mapi (fun i (argType, arg) ->
                let isByRefPreferred =
                    parameterIsByRefPreferred i parameters
                let ctx = { ctx with IsParamByRefPreferred = isByRefPreferred || ctx.IsParamByRefPreferred }
                transformLeaveContext com ctx argType arg)

    let prepareRefForPatternMatch (com: IRustCompiler) ctx typ name fableExpr =
        let expr = com.TransformExpr(ctx, fableExpr)
        if isRefScoped ctx name || (isInRefType com typ)
        then expr
        elif shouldBeRefCountWrapped com ctx typ |> Option.isSome
        then expr |> makeAsRef
        else expr |> mkAddrOfExpr

    let makeNumber com ctx r t kind (x: obj) =
        match kind, x with

        | Int8, (:? int8 as x) when x = System.SByte.MinValue ->
            mkGenericPathExpr ["i8";"MIN"] None
        | Int8, (:? int8 as x) when x = System.SByte.MaxValue ->
            mkGenericPathExpr ["i8";"MAX"] None
        | Int16, (:? int16 as x) when x = System.Int16.MinValue ->
            mkGenericPathExpr ["i16";"MIN"] None
        | Int16, (:? int16 as x) when x = System.Int16.MaxValue ->
            mkGenericPathExpr ["i16";"MAX"] None
        | Int32, (:? int32 as x) when int32 x = System.Int32.MinValue ->
            mkGenericPathExpr ["i32";"MIN"] None
        | Int32, (:? int32 as x) when int32 x = System.Int32.MaxValue ->
            mkGenericPathExpr ["i32";"MAX"] None
        | Int64, (:? int64 as x) when int64 x = System.Int64.MinValue ->
            mkGenericPathExpr ["i64";"MIN"] None
        | Int64, (:? int64 as x) when int64 x = System.Int64.MaxValue ->
            mkGenericPathExpr ["i64";"MAX"] None

        // | UInt8, (:? uint8 as x) when x = System.Byte.MinValue ->
        //     mkGenericPathExpr ["u8";"MIN"] None
        | UInt8, (:? uint8 as x) when x = System.Byte.MaxValue ->
            mkGenericPathExpr ["u8";"MAX"] None
        // | UInt16, (:? uint16 as x) when x = System.UInt16.MinValue ->
        //     mkGenericPathExpr ["u16";"MIN"] None
        | UInt16, (:? uint16 as x) when x = System.UInt16.MaxValue ->
            mkGenericPathExpr ["u16";"MAX"] None
        // | UInt32, (:? uint32 as x) when x = System.UInt32.MinValue ->
        //     mkGenericPathExpr ["u32";"MIN"] None
        | UInt32, (:? uint32 as x) when x = System.UInt32.MaxValue ->
            mkGenericPathExpr ["u32";"MAX"] None
        // | UInt64, (:? uint64 as x) when x = System.UInt64.MinValue ->
        //     mkGenericPathExpr ["u64";"MIN"] None
        | UInt64, (:? uint64 as x) when x = System.UInt64.MaxValue ->
            mkGenericPathExpr ["u64";"MAX"] None

        | Float32, (:? float32 as x) when System.Single.IsNaN(x) ->
            mkGenericPathExpr ["f32";"NAN"] None
        | Float64, (:? float as x) when System.Double.IsNaN(x) ->
            mkGenericPathExpr ["f64";"NAN"] None
        | Float32, (:? float32 as x) when System.Single.IsPositiveInfinity(x) ->
            mkGenericPathExpr ["f32";"INFINITY"] None
        | Float64, (:? float as x) when System.Double.IsPositiveInfinity(x) ->
            mkGenericPathExpr ["f64";"INFINITY"] None
        | Float32, (:? float32 as x) when System.Single.IsNegativeInfinity(x) ->
            mkGenericPathExpr ["f32";"NEG_INFINITY"] None
        | Float64, (:? float as x) when System.Double.IsNegativeInfinity(x) ->
            mkGenericPathExpr ["f64";"NEG_INFINITY"] None

        | NativeInt, (:? nativeint as x) ->
            let expr = mkIsizeLitExpr (abs x |> uint64)
            if x < 0n then expr |> mkNegExpr else expr
        | Int8, (:? int8 as x) ->
            let expr = mkInt8LitExpr (abs x |> uint64)
            if x < 0y then expr |> mkNegExpr else expr
        | Int16, (:? int16 as x) ->
            let expr = mkInt16LitExpr (abs x |> uint64)
            if x < 0s then expr |> mkNegExpr else expr
        | Int32, (:? int32 as x) ->
            let expr = mkInt32LitExpr (abs x |> uint64)
            if x < 0 then expr |> mkNegExpr else expr
        | Int64, (:? int64 as x) ->
            let expr = mkInt64LitExpr (abs x |> uint64)
            if x < 0 then expr |> mkNegExpr else expr
        | UNativeInt, (:? unativeint as x) ->
            mkUsizeLitExpr (x |> uint64)
        | UInt8, (:? uint8 as x) ->
            mkUInt8LitExpr (x |> uint64)
        | UInt16, (:? uint16 as x) ->
            mkUInt16LitExpr (x |> uint64)
        | UInt32, (:? uint32 as x) ->
            mkUInt32LitExpr (x |> uint64)
        | UInt64, (:? uint64 as x) ->
            mkUInt64LitExpr (x |> uint64)
        | Float32, (:? float32 as x) ->
            let expr = mkFloat32LitExpr (abs x)
            if x < 0.0f then expr |> mkNegExpr else expr
        | Float64, (:? float as x) ->
            let expr = mkFloat64LitExpr (abs x)
            if x < 0.0 then expr |> mkNegExpr else expr
        | Decimal, (:? decimal as x) ->
            Replacements.makeDecimal com r t x |> transformExpr com ctx
        | kind, x ->
            $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
            |> addError com [] r
            mkFloat64LitExpr 0.

    let makeString com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "string" [value]

    let makeDefaultOf com ctx (typ: Fable.Type) =
        let genArgs = transformGenArgs com ctx [typ]
        makeLibCall com ctx genArgs "Native" "defaultOf" []

    let makeOption (com: IRustCompiler) ctx r typ value isStruct =
        let expr =
            match value with
            | Some arg ->
                let callee = mkGenericPathExpr [rawIdent "Some"] None
                callFunction com ctx r callee [arg]
            | None ->
                let ty = transformType com ctx typ
                let genArgs = mkGenericTypeArgs [ty]
                mkGenericPathExpr [rawIdent "None"] genArgs
        // if isStruct
        // then expr
        // else expr |> makeLrcValue com ctx
        expr // all options are value options

    let makeArray (com: IRustCompiler) ctx r typ (exprs: Fable.Expr list) =
        match exprs with
        | [] ->
            let genArgs = transformGenArgs com ctx [typ]
            makeLibCall com ctx genArgs "Native" "arrayEmpty" []
        | _ ->
            let arrayExpr =
                exprs
                |> List.map (transformExpr com ctx)
                |> mkArrayExpr
                |> mkAddrOfExpr
            makeLibCall com ctx None "Native" "arrayFrom" [arrayExpr]

    let makeArrayFrom (com: IRustCompiler) ctx r typ fableExpr =
        match fableExpr with
        | Fable.Value(Fable.NewTuple([valueExpr; sizeExpr], isStruct), _) ->
            let size = transformExpr com ctx sizeExpr |> mkAddrOfExpr
            let value = transformExpr com ctx valueExpr |> mkAddrOfExpr
            makeLibCall com ctx None "Native" "arrayCreate" [size; value]
        | expr ->
            // this assumes expr converts to a slice
            // TODO: this may not always work, make it work
            let sequence = transformExpr com ctx expr |> mkAddrOfExpr
            makeLibCall com ctx None "Native" "arrayFrom" [sequence]

    let makeList (com: IRustCompiler) ctx r typ headAndTail =
        // list contruction with cons
        match headAndTail with
        | None ->
            libCall com ctx r [typ] "List" "empty" []
        | Some(head, Fable.Value(Fable.NewList(None, _), _)) ->
            libCall com ctx r [] "List" "singleton" [head]
        | Some(head, tail) ->
            libCall com ctx r [] "List" "cons" [head; tail]

        // // convert list construction to List.ofArray
        // let rec getItems acc = function
        //     | None -> List.rev acc, None
        //     | Some(head, Fable.Value(Fable.NewList(tail, _),_)) -> getItems (head::acc) tail
        //     | Some(head, tail) -> List.rev (head::acc), Some tail
        // let makeNewArray r typ exprs =
        //     Fable.Value(Fable.NewArray(exprs, typ), r)
        // match getItems [] headAndTail with
        // | [], None ->
        //     libCall com ctx r [] "List" "empty" []
        // | [expr], None ->
        //     libCall com ctx r [] "List" "singleton" [expr]
        // | exprs, None ->
        //     [makeNewArray r typ exprs]
        //     |> libCall com ctx r [] "List" "ofArray"
        // | [head], Some tail ->
        //     libCall com ctx r [] "List" "cons" [head; tail]
        // | exprs, Some tail ->
        //     [makeNewArray r typ exprs; tail]
        //     |> libCall com ctx r [] "List" "ofArrayWithTail"

    let makeTuple (com: IRustCompiler) ctx r isStruct (exprs: (Fable.Expr) list) =
        let expr =
            exprs
            |> List.map (transformLeaveContext com ctx None)
            |> mkTupleExpr
        if isStruct
        then expr
        else expr |> makeLrcValue com ctx

    let makeRecord (com: IRustCompiler) ctx r values entRef genArgs =
        let ent = com.GetEntity(entRef)
        let idents = getEntityFieldsAsIdents com ent
        let fields =
            List.zip idents values
            |> List.map (fun (ident, value) ->
                let expr =
                    let expr = transformLeaveContext com ctx None value
                    if ident.IsMutable
                    then expr |> makeMutValue
                    else expr
                let attrs = []
                mkExprField attrs ident.Name expr false false
            )
        let genArgs =
            genArgs
            |> List.zip ent.GenericParameters
            |> List.choose (fun (p, a) -> if not p.IsMeasure then Some a else None)
        let genArgs = transformGenArgs com ctx genArgs
        let entName = getEntityFullName com ctx entRef
        let path = makeFullNamePath entName genArgs
        let expr = mkStructExpr path fields // TODO: range
        if ent.IsValueType
        then expr
        else expr |> maybeWrapSmartPtr com ctx ent

    let tryUseKnownUnionCaseNames fullName =
        match fullName with
        | "FSharp.Core.FSharpResult`2.Ok" -> rawIdent "Ok" |> Some
        | "FSharp.Core.FSharpResult`2.Error" -> rawIdent "Err" |> Some
        | _ ->
            if fullName.StartsWith("FSharp.Core.FSharpChoice`") then
                fullName |> Fable.Naming.replacePrefix "FSharp.Core.FSharp" "" |> Some
            else
                None

    let getUnionCaseName com ctx entRef (unionCase: Fable.UnionCase) =
        tryUseKnownUnionCaseNames unionCase.FullName
        |> Option.defaultWith (fun () ->
            let entName = getEntityFullName com ctx entRef
            entName + "::" + unionCase.Name
        )

    let makeUnion (com: IRustCompiler) ctx r values tag entRef genArgs =
        let ent = com.GetEntity(entRef)
        // let genArgs = transformGenArgs com ctx genArgs

        let unionCase = ent.UnionCases |> List.item tag
        let unionCaseName = getUnionCaseName com ctx entRef unionCase

        let callee = makeFullNamePathExpr unionCaseName None //genArgs
        let expr =
            if List.isEmpty values
            then callee
            else callFunction com ctx None callee values
        if ent.IsValueType || ent.FullName = Types.result
        then expr
        else expr |> maybeWrapSmartPtr com ctx ent

    let makeThis (com: IRustCompiler) ctx r typ =
        let expr = mkGenericPathExpr [rawIdent "self"] None
        expr

    let makeFormat (parts: string list) =
        let sb = System.Text.StringBuilder()
        sb.Append(List.head parts) |> ignore
        List.tail parts |> List.iteri (fun i part ->
            sb.Append($"{{{i}}}" + part) |> ignore)
        sb.ToString()

    let formatString (com: IRustCompiler) ctx parts values: Rust.Expr =
        let fmt = makeFormat parts
        let args = transformCallArgs com ctx values [] []
        let expr = mkMacroExpr "format" ((mkStrLitExpr fmt)::args)
        expr |> mkAddrOfExpr |> makeString com ctx

    let transformTypeInfo (com: IRustCompiler) ctx r (typ: Fable.Type): Rust.Expr =
        let importName = getLibraryImportName com ctx "Native" "TypeId"
        let genArgs = transformGenArgs com ctx [typ]
        makeFullNamePathExpr importName genArgs

    let transformValue (com: IRustCompiler) (ctx: Context) r value: Rust.Expr =
        let unimplemented () =
            $"Value %A{value} is not implemented yet"
            |> addWarning com [] None
            TODO_EXPR $"%A{value}"
        match value with
        | Fable.BaseValue (None, _) ->
            // Super(None)
            unimplemented ()
        | Fable.BaseValue(Some boundIdent, _) ->
            // identAsExpr boundIdent
            unimplemented ()
        | Fable.ThisValue typ -> makeThis com ctx r typ
        | Fable.TypeInfo(typ, _tags) -> transformTypeInfo com ctx r typ
        | Fable.Null t ->
            //TODO: some other representation perhaps?
            makeDefaultOf com ctx t
        | Fable.UnitConstant -> mkUnitExpr ()
        | Fable.BoolConstant b -> mkBoolLitExpr b //, ?loc=r)
        | Fable.CharConstant c -> mkCharLitExpr c //, ?loc=r)
        | Fable.StringConstant s -> mkStrLitExpr s |> makeString com ctx
        | Fable.StringTemplate(_tag, parts, values) -> formatString com ctx parts values
        | Fable.NumberConstant(x, kind, _) -> makeNumber com ctx r value.Type kind x
        | Fable.RegexConstant(source, flags) ->
            // Expression.regExpLiteral(source, flags, ?loc=r)
            unimplemented ()
        | Fable.NewArray(Fable.ArrayValues values, typ, _isMutable) -> makeArray com ctx r typ values
        | Fable.NewArray((Fable.ArrayFrom expr | Fable.ArrayAlloc expr), typ, _isMutable) -> makeArrayFrom com ctx r typ expr
        | Fable.NewTuple(values, isStruct) -> makeTuple com ctx r isStruct values
        | Fable.NewList(headAndTail, typ) -> makeList com ctx r typ headAndTail
        | Fable.NewOption(value, typ, isStruct) -> makeOption com ctx r typ value isStruct
        | Fable.NewRecord(values, entRef, genArgs) -> makeRecord com ctx r values entRef genArgs
        | Fable.NewAnonymousRecord(values, fieldNames, genArgs, isStruct) -> makeTuple com ctx r isStruct values
        | Fable.NewUnion(values, tag, entRef, genArgs) -> makeUnion com ctx r values tag entRef genArgs

    let calcVarAttrsAndOnlyRef com ctx (e: Fable.Expr) =
        let t = e.Type
        let name = getIdentName e
        let varAttrs =
            ctx.ScopedSymbols   // todo - cover more than just root level idents
            |> Map.tryFind name
            |> Option.defaultValue {
                IsArm = false
                IsRef = false
                IsBox = false
                // HasMultipleUses = true
                UsageCount = 9999 }
        let isOnlyReference =
            match e with
            | Fable.Let _ -> true
            | Fable.Call _ ->
                //if the source is the returned value of a function, it is never bound, so we can assume this is the only reference
                true
            | Fable.CurriedApply _ -> true
            | Fable.Value(kind, r) ->
                //an inline value kind is also never bound, so can assume this is the only reference also
                true
            | Fable.Operation(Fable.Binary _, _, _) ->
                true //Anything coming out of an operation is as good as being returned from a function
            | Fable.Lambda _
            | Fable.Delegate _ ->
                true
            | Fable.IfThenElse _
            | Fable.DecisionTree _
            | Fable.DecisionTreeSuccess _
            | Fable.Sequential _
            | Fable.ForLoop _ ->
                true //All control constructs in f# return expressions, and as return statements are always take ownership, we can assume this is already owned, and not bound
            //| Fable.Sequential _ -> true    //this is just a wrapper, so do not need to clone, passthrough only. (currently breaks some stuff, needs looking at)
            | _ ->
                if ctx.IsInPluralizedExpr then
                    false
                    // If an owned value is captured, it must be cloned or it will turn a closure into a FnOnce (as value is consumed on first call).
                    // If an owned value leaves scope inside a for loop, it can also not be assumed to be the only usage, as there are multiple instances of that expression invocation at runtime
                else varAttrs.UsageCount < 2
        varAttrs, isOnlyReference

    let transformLeaveContext (com: IRustCompiler) ctx (t: Fable.Type option) (e: Fable.Expr): Rust.Expr =
        let varAttrs, isOnlyReference = calcVarAttrsAndOnlyRef com ctx e
        // Careful moving this, as idents mutably subtract their count as they are seen, so ident transforming must happen AFTER checking
        let expr = com.TransformExpr (ctx, e)

        let targetExpectsRef =
            ctx.IsParamByRefPreferred
            || Option.exists (isByRefType com) t
            || isAddrOfExpr e
        let sourceIsRef =
            varAttrs.IsRef
        let implClone = TypeImplementsCloneTrait com e.Type
        let implCopy = TypeImplementsCopyTrait com e.Type
        let exprResultIsUnreachable =
            match e with
            | Fable.Extended(Fable.Throw(_, _), _) -> true
            | _ -> false

        match implClone,    implCopy,   sourceIsRef,    targetExpectsRef,   isOnlyReference,    exprResultIsUnreachable with
        | _,                _,          true,           true,               _,                  false ->                    expr
        | _,                true,       false,          false,              _,                  false ->                    expr
        | _,                _,          true,           false,              _,                  false ->                    expr |> makeClone
        //| _,                _,          true,           false,              true,               false ->                    expr |> mkDerefExpr // should be able to just deref but sourceIsRef is not always correct for root union ident
        | _,                _,          false,          true,               _,                  false ->                    expr |> mkAddrOfExpr
        | true,             false,      _,              false,              false,              false ->                    expr |> makeClone
        | _ ->                                                                                                              expr
        //|> BLOCK_COMMENT_SUFFIX (sprintf "implClone: %b, implCopy: %b, sourceIsRef; %b, targetExpRef: %b, isOnlyRef: %b, unreachable: %b" implClone implCopy sourceIsRef targetExpectsRef isOnlyReference exprResultIsUnreachable)

(*
    let enumerator2iterator com ctx =
        let enumerator = Expression.callExpression(get None (Expression.identifier("this")) "GetEnumerator", [||])
        BlockStatement([| Statement.returnStatement(libCall com ctx None [] "Util" "toIterator" [|enumerator|])|])

    let extractBaseExprFromBaseCall (com: IRustCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
        match baseCall, baseType with
        | Some(Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr =
                match baseRef with
                | Fable.IdentExpr id -> typedIdent com ctx id |> Expression.Identifier
                | _ -> transformExpr com ctx baseRef
            let args = transformCallArgs com ctx info.Args
            Some(baseExpr, args)
        | Some(Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some(baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)
            $"Ignoring base call for %s{baseType.Entity.FullName}" |> addWarning com [] range
            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)
            "Unexpected base call expression, please report" |> addError com [] range
            None
        | None, _ ->
            None
*)
    let transformObjectExpr (com: IRustCompiler) ctx typ (members: Fable.ObjectExprMember list) baseCall: Rust.Expr =
        if members |> List.isEmpty then
            mkUnitExpr () // object constructors sometimes generate this
        else
            let makeEntRef fullName assemblyName: Fable.EntityRef =
                { FullName = fullName; Path = Fable.CoreAssemblyName assemblyName }
            let entRef, genArgs =
                match typ with
                | Fable.DeclaredType(entRef, genArgs) -> entRef, genArgs
                | Fable.Any ->
                    makeEntRef "System.Object" "System.Runtime", []
                | _ ->
                    "Unsupported object expression" |> addWarning com [] None
                    makeEntRef "System.Object" "System.Runtime", []
            //TODO: properly handle non-interface types with constructors
            let entName = "ObjectExpr"
            let members: Fable.MemberDecl list =
                members |> List.map (fun memb -> {
                    Name = memb.Name
                    Args = memb.Args
                    Body = memb.Body
                    MemberRef = memb.MemberRef
                    IsMangled = memb.IsMangled
                    ImplementedSignatureRef = None
                    UsedNames = Set.empty
                    XmlDoc = None
                    Tags = []
                })
            let decl: Fable.ClassDecl = {
                Name = entName
                Entity = entRef
                Constructor = None
                BaseCall = baseCall
                AttachedMembers = members
                XmlDoc = None
                Tags = []
            }
            let attrs = []
            let fields = []
            let generics = genArgs |> makeGenerics com ctx
            let structItems =
                if baseCall.IsSome then [] // if base type is not an interface
                else [mkStructItem attrs entName fields generics]
            let memberItems = transformClassMembers com ctx decl
            let genArgs = transformGenArgs com ctx genArgs
            let path = makeFullNamePath entName genArgs
            let objExpr =
                match baseCall with
                | Some fableExpr -> com.TransformExpr(ctx, fableExpr)
                | None -> mkStructExpr path fields |> makeLrcValue com ctx |> makeLrcValue com ctx
            let objStmt = objExpr |> mkExprStmt
            let declStmts = structItems @ memberItems |> List.map mkItemStmt
            declStmts @ [objStmt] |> mkBlock |> mkBlockExpr

    let maybeAddParens fableExpr expr: Rust.Expr =
        match fableExpr with
        | Fable.IfThenElse _ -> mkParenExpr expr
        // TODO: add more expressions that need parens
        | _ -> expr

    let transformOperation com ctx range typ opKind: Rust.Expr =
        match opKind with
        | Fable.Unary(UnaryOperator.UnaryAddressOf, Fable.IdentExpr ident) ->
            transformIdent com ctx range ident // |> mkAddrOfExpr
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            match op with
            | UnaryOperator.UnaryMinus -> mkNegExpr expr //?loc=range)
            | UnaryOperator.UnaryPlus -> expr // no unary plus
            | UnaryOperator.UnaryNot -> mkNotExpr expr //?loc=range)
            | UnaryOperator.UnaryNotBitwise -> mkNotExpr expr //?loc=range)
            | UnaryOperator.UnaryAddressOf -> expr // |> mkAddrOfExpr// already handled above

        | Fable.Binary(op, leftExpr, rightExpr) ->
            let kind =
                match op with
                | BinaryOperator.BinaryEqual -> Rust.BinOpKind.Eq
                | BinaryOperator.BinaryUnequal -> Rust.BinOpKind.Ne
                | BinaryOperator.BinaryLess -> Rust.BinOpKind.Lt
                | BinaryOperator.BinaryLessOrEqual -> Rust.BinOpKind.Le
                | BinaryOperator.BinaryGreater -> Rust.BinOpKind.Gt
                | BinaryOperator.BinaryGreaterOrEqual -> Rust.BinOpKind.Ge
                | BinaryOperator.BinaryShiftLeft -> Rust.BinOpKind.Shl
                | BinaryOperator.BinaryShiftRightSignPropagating -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryShiftRightZeroFill -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryMinus -> Rust.BinOpKind.Sub
                | BinaryOperator.BinaryPlus -> Rust.BinOpKind.Add
                | BinaryOperator.BinaryMultiply -> Rust.BinOpKind.Mul
                | BinaryOperator.BinaryDivide -> Rust.BinOpKind.Div
                | BinaryOperator.BinaryModulus -> Rust.BinOpKind.Rem
                | BinaryOperator.BinaryExponent -> failwithf "BinaryExponent not supported. TODO: implement with pow."
                | BinaryOperator.BinaryOrBitwise -> Rust.BinOpKind.BitOr
                | BinaryOperator.BinaryXorBitwise -> Rust.BinOpKind.BitXor
                | BinaryOperator.BinaryAndBitwise -> Rust.BinOpKind.BitAnd

            let left = transformLeaveContext com ctx None leftExpr |> maybeAddParens leftExpr
            let right = transformLeaveContext com ctx None rightExpr |> maybeAddParens rightExpr

            match leftExpr.Type, kind with
            | Fable.String, Rust.BinOpKind.Add ->
                makeLibCall com ctx None "String" "append" [left; right]
            | typ, (Rust.BinOpKind.Eq | Rust.BinOpKind.Ne) when hasReferenceEquality com typ ->
                makeLibCall com ctx None "Native" "referenceEquals" [makeAsRef left; makeAsRef right]
            | _ ->
                mkBinaryExpr (mkBinOp kind) left right //?loc=range)

        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            let kind =
                match op with
                | LogicalOperator.LogicalOr -> Rust.BinOpKind.Or
                | LogicalOperator.LogicalAnd -> Rust.BinOpKind.And
            mkBinaryExpr (mkBinOp kind) left right //?loc=range)

    let transformMacro (com: IRustCompiler) ctx range (emitInfo: Fable.EmitInfo) =
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro |> Fable.Naming.replaceSuffix "!" ""
        let args = transformCallArgs com ctx info.Args info.SignatureArgTypes []
        let args =
            // for certain macros, use unwrapped format string as first argument
            match macro with
            | "print" |"println" |"format" ->
                match info.Args with
                | [arg] -> (mkStrLitExpr "{0}")::args
                | Fable.Value(Fable.StringConstant formatStr, _)::restArgs ->
                    (mkStrLitExpr formatStr)::(List.tail args)
                | _ -> args
            | _ -> args
        let expr = mkMacroExpr macro args
        if macro = "format"
        then expr |> mkAddrOfExpr |> makeString com ctx
        else expr

    let transformEmit (com: IRustCompiler) ctx range (emitInfo: Fable.EmitInfo) =
        // for now only supports macro calls or function calls
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro
        // if it ends with '!', it's a Rust macro
        if macro.EndsWith("!") then
            transformMacro com ctx range emitInfo
        else // otherwise it's an Emit
            let thisArg = info.ThisArg |> Option.map (fun e -> com.TransformExpr(ctx, e)) |> Option.toList
            let args = transformCallArgs com ctx info.Args info.SignatureArgTypes []
            let args = args |> List.append thisArg
            mkEmitExpr macro args

    let transformCallee (com: IRustCompiler) ctx calleeExpr =
        let expr = transformExpr com ctx calleeExpr
        match calleeExpr with
        | Fable.IdentExpr id -> expr
        | _ -> expr |> mkParenExpr // if not an identifier, wrap it in parentheses

    let isDeclEntityKindOf (com: IRustCompiler) isKindOf (callInfo: Fable.CallInfo) =
        callInfo.MemberRef
        |> Option.bind com.TryGetMember
        |> Option.bind (fun mi -> mi.DeclaringEntity)
        |> Option.bind com.TryGetEntity
        |> Option.map isKindOf
        |> Option.defaultValue false

    let isModuleMember (com: IRustCompiler) (callInfo: Fable.CallInfo) =
        isDeclEntityKindOf com (fun ent -> ent.IsFSharpModule) callInfo

    let isNativeCall (callInfo: Fable.CallInfo) =
        callInfo.Tags |> List.contains "native"

    let transformCall (com: IRustCompiler) ctx range typ calleeExpr (callInfo: Fable.CallInfo) =
        let isByRefPreferred =
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> Option.map (fun memberInfo ->
                memberInfo.Attributes
                |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef))
            |> Option.defaultValue false
        let argParams =
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> Option.map (fun memberInfo ->
                memberInfo.CurriedParameterGroups |> List.concat
            ) |> Option.defaultValue []

        let ctx =
            let isSendSync =
                match calleeExpr with
                | Fable.Import(info, t, r) ->
                    match info.Selector with
                    | "AsyncBuilder_::delay"
                    | "AsyncBuilder_::bind"
                    | "Task_::bind"
                    | "Task_::delay"
                    | "TaskBuilder_::bind"
                    | "TaskBuilder_::delay" -> true
                    | _ -> false
                | _ -> false
            { ctx with RequiresSendSync = isSendSync
                       IsParamByRefPreferred = isByRefPreferred }

        let args = transformCallArgs com ctx callInfo.Args callInfo.SignatureArgTypes argParams
        match calleeExpr with
        // mutable module values (transformed as function calls)
        | Fable.IdentExpr id when id.IsMutable && isModuleMember com callInfo ->
            let expr = transformIdent com ctx range id
            mutableGet (mkCallExpr expr [])

        | Fable.Get(calleeExpr, (Fable.FieldGet info as kind), t, _r) ->
            // this is an instance call
            match t with
            | Fable.LambdaType _ | Fable.DelegateType _ ->
                // if the field type is a function, wrap in parentheses
                let callee = transformGet com ctx None t calleeExpr kind
                mkCallExpr (callee |> mkParenExpr) args
            | _ ->
                match calleeExpr.Type with
                | IsNonErasedInterface com (entRef, genArgs) when not (isNativeCall callInfo) ->
                    // interface instance call
                    let ifcName = getInterfaceEntityName com ctx entRef
                    let membName = splitLast info.Name
                    let parts = (ifcName + "::" + membName) |> splitNameParts
                    let callee = com.TransformExpr(ctx, calleeExpr)
                    (callee |> makeAsRef)::args |> makeCall parts None
                | _ ->
                    // normal instance call
                    let membName = splitLast info.Name
                    let callee = com.TransformExpr(ctx, calleeExpr)
                    mkMethodCallExpr membName None callee args

        | Fable.Import(info, t, r) ->
            // imports without args need to have type added to path.
            // this is for imports like Array.empty, Seq.empty etc.
            // TODO: a more general way of doing this in Replacements
            let genArgs =
                match info.Selector, typ with
                | "Native_::arrayEmpty", Fable.Array(genArg, _) ->
                    transformGenArgs com ctx [genArg]
                | "Native_::arrayWithCapacity", Fable.Array(genArg, _) ->
                    transformGenArgs com ctx [genArg]
                | ("Native_::defaultOf" | "Native_::getZero"), genArg ->
                    transformGenArgs com ctx [genArg]
                | "Set_::empty", Replacements.Util.Builtin (Replacements.Util.FSharpSet(genArg)) ->
                    transformGenArgs com ctx [genArg]
                | "Map_::empty", Replacements.Util.Builtin (Replacements.Util.FSharpMap(k, v)) ->
                    transformGenArgs com ctx [k; v]
                | "Seq_::empty", IEnumerable genArg ->
                    transformGenArgs com ctx [genArg]
                | "Native_::hashSetEmpty", Replacements.Util.Builtin (Replacements.Util.BclHashSet(genArg)) ->
                    transformGenArgs com ctx [genArg]
                | "Native_::hashSetWithCapacity", Replacements.Util.Builtin (Replacements.Util.BclHashSet(genArg)) ->
                    transformGenArgs com ctx [genArg]
                | "Native_::hashMapEmpty", Replacements.Util.Builtin (Replacements.Util.BclDictionary(k, v)) ->
                    transformGenArgs com ctx [k; v]
                | "Native_::hashMapWithCapacity", Replacements.Util.Builtin (Replacements.Util.BclDictionary(k, v)) ->
                    transformGenArgs com ctx [k; v]
                | _ -> None
            match callInfo.ThisArg, info.Kind with
            |  Some thisArg, Fable.MemberImport(isInstance, _) when isInstance = true ->
                let callee = transformCallee com ctx thisArg
                mkMethodCallExpr info.Selector genArgs callee args
            | _ ->
                let callee = transformImport com ctx r t info genArgs
                mkCallExpr callee args

        | _ ->
            match ctx.TailCallOpportunity with
            | Some tc when tc.IsRecursiveRef(calleeExpr)
                && List.length tc.Args = List.length callInfo.Args ->
                optimizeTailCall com ctx range tc callInfo.Args
            | _ ->
                match callInfo.ThisArg, calleeExpr with
                |  Some thisArg, Fable.IdentExpr id ->
                    let callee = transformCallee com ctx thisArg
                    mkMethodCallExpr id.Name None callee args
                // | None, Fable.IdentExpr id ->
                //     let callee = makeFullNamePathExpr id.Name None
                //     mkCallExpr callee args
                | _ ->
                    let callee = transformCallee com ctx calleeExpr
                    mkCallExpr callee args

(*
    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause.catchClause(identAsPattern param, transformBlock com ctx returnStrategy body))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=r)|]
*)
    let mutableGet expr =
        mkMethodCallExpr "get" None expr []

    let mutableGetMut expr =
        mkMethodCallExpr "get_mut" None expr []

    let mutableSet expr value =
        mkMethodCallExpr "set" None expr [value]

    let transformGet (com: IRustCompiler) ctx range typ (fableExpr: Fable.Expr) kind =
        match kind with
        | Fable.ExprGet idx ->
            let expr = transformCallee com ctx fableExpr
            let prop = transformExpr com ctx idx
            match fableExpr.Type, idx.Type with
            | Fable.Array(t,_), Fable.Number(Int32, Fable.NumberInfo.Empty) ->
                // // when indexing an array, cast index to usize
                // let expr = expr |> mutableGetMut
                // let prop = prop |> mkCastExpr (primitiveType "usize")
                getExpr range expr prop |> makeClone
            | _ ->
                getExpr range expr prop

        | Fable.FieldGet info ->
            let fieldName = info.Name
            match fableExpr.Type with
            | Fable.AnonymousRecordType (fields, _genArgs, isStruct) ->
                // anonimous records are tuples
                let idx = fields |> Array.findIndex (fun f -> f = fieldName)
                (Fable.TupleIndex (idx))
                |> transformGet com ctx range typ fableExpr
            | t when isInterface com t ->
                // for interfaces, transpile property_get as instance call
                let callee = transformCallee com ctx fableExpr
                mkMethodCallExpr fieldName None callee []
            | _ ->
                let expr = transformCallee com ctx fableExpr
                let field = getField range expr fieldName
                if info.IsMutable
                then field |> mutableGet
                else field

        | Fable.ListHead ->
            // get range (com.TransformExpr(ctx, fableExpr)) "head"
            libCall com ctx range [] "List" "head" [fableExpr]

        | Fable.ListTail ->
            // get range (com.TransformExpr(ctx, fableExpr)) "tail"
            libCall com ctx range [] "List" "tail" [fableExpr]

        | Fable.TupleIndex index ->
            let expr = transformCallee com ctx fableExpr
            mkFieldExpr expr (index.ToString())
            |> makeClone

        | Fable.OptionValue ->
            match fableExpr with
            | Fable.IdentExpr id when isArmScoped ctx id.Name ->
                // if arm scoped, just output the ident value
                let name = $"{id.Name}_{0}_{0}"
                mkGenericPathExpr [name] None
            | _ ->
                libCall com ctx range [] "Option" "getValue" [fableExpr]

        | Fable.UnionTag ->
            let expr = com.TransformExpr(ctx, fableExpr)
            // TODO: range
            expr

        | Fable.UnionField info ->
            match fableExpr with
            | Fable.IdentExpr id when isArmScoped ctx id.Name ->
                // if arm scoped, just output the ident value
                let name = $"{id.Name}_{info.CaseIndex}_{info.FieldIndex}"
                mkGenericPathExpr [name] None
            | _ ->
                // compile as: "if let MyUnion::Case(x, _) = opt { x } else { unreachable!() }"
                let ent = com.GetEntity(info.Entity)
                assert(ent.IsFSharpUnion)
                // let genArgs = transformGenArgs com ctx genArgs // TODO:
                let unionCase = ent.UnionCases |> List.item info.CaseIndex
                let fieldName = "x"
                let fields =
                    unionCase.UnionCaseFields |> List.mapi (fun i _field ->
                        if i = info.FieldIndex
                        then makeFullNameIdentPat fieldName
                        else WILD_PAT
                    )
                let unionCaseName = getUnionCaseName com ctx info.Entity unionCase
                let pat = makeUnionCasePat unionCaseName fields
                let expr =
                    fableExpr
                    |> prepareRefForPatternMatch com ctx fableExpr.Type ""
                let thenExpr =
                    mkGenericPathExpr [fieldName] None |> makeClone

                let arms = [
                    mkArm [] pat None thenExpr
                ]
                let arms =
                    if (List.length ent.UnionCases) > 1 then
                        // only add a default arm if needed
                        let defaultArm = mkArm [] WILD_PAT None (mkMacroExpr "unreachable" [])
                        arms @ [defaultArm]
                    else arms

                mkMatchExpr expr arms
                // TODO : Cannot use if let because it moves references out of their Rc's, which breaks borrow checker. We cannot bind
                // let ifExpr = mkLetExpr pat expr
                // let thenExpr = mkGenericPathExpr [fieldName] None
                // let elseExpr = mkMacroExpr "unreachable" []
                // mkIfThenElseExpr ifExpr thenExpr elseExpr

    let transformSet (com: IRustCompiler) ctx range fableExpr typ (fableValue: Fable.Expr) kind =
        let expr = transformCallee com ctx fableExpr
        let value = transformLeaveContext com ctx None fableValue
        match kind with
        | Fable.ValueSet ->
            match fableExpr with
            // mutable values
            | Fable.IdentExpr id when id.IsMutable ->
                transformIdentSet com ctx range id value
            // mutable module values (transformed as function calls)
            | Fable.Call(Fable.IdentExpr id, info, _, _)
                when id.IsMutable && isModuleMember com info ->
                let expr = transformIdent com ctx range id
                mutableSet (mkCallExpr expr []) value
            | _ ->
                match fableExpr.Type with
                | Replacements.Util.Builtin (Replacements.Util.FSharpReference _)
                    -> mutableSet expr value
                | _ -> mkAssignExpr expr value
        | Fable.ExprSet idx ->
            let prop = transformExpr com ctx idx
            match fableExpr.Type, idx.Type with
            | Fable.Array(t,_), Fable.Number(Int32, Fable.NumberInfo.Empty) ->
                // when indexing an array, cast index to usize
                let expr = expr |> mutableGetMut
                let prop = prop |> mkCastExpr (primitiveType "usize")
                let left = getExpr range expr prop
                mkAssignExpr left value
            | _ ->
                let left = getExpr range expr prop
                mkAssignExpr left value //?loc=range)
        | Fable.FieldSet(fieldName) ->
            let field = getField None expr fieldName
            mutableSet field value

    let transformAsStmt (com: IRustCompiler) ctx (e: Fable.Expr): Rust.Stmt =
        let expr = transformLeaveContext com ctx None e
        mkExprStmt expr

    // flatten nested Let binding expressions
    let rec flattenLet acc (expr: Fable.Expr) =
        match expr with
        | Fable.Let(ident, value, body) ->
            flattenLet ((ident, value)::acc) body
        | _ -> List.rev acc, expr

    // flatten nested Sequential expressions (depth first)
    let rec flattenSequential (expr: Fable.Expr) =
        match expr with
        | Fable.Sequential exprs ->
            List.collect flattenSequential exprs
        | _ -> [expr]

    let makeLetStmt com ctx usages (ident: Fable.Ident) (value: Fable.Expr) =
        let tyOpt =
            match ident.Type with
            | Fable.Any
            | Fable.LambdaType _
            | Fable.DelegateType _
                -> None
            | _ ->
                transformType com ctx ident.Type
                |> Some
        let tyOpt =
            tyOpt |> Option.map (fun ty ->
                if ident.IsMutable
                then ty |> makeMutTy com ctx |> makeLrcTy com ctx
                else ty)
        let initOpt =
            match value with
            | Fable.Value(Fable.Null _t, _) ->
                None // just a declaration, to be initialized later
            | Function (args, body, _name) ->
                transformLambda com ctx (Some ident.Name) args body
                |> Some
            | _ ->
                transformLeaveContext com ctx None value
                |> Some
        let initOpt =
            initOpt |> Option.map (fun init ->
                if ident.IsMutable
                then init |> makeMutValue |> makeLrcValue com ctx
                else init)
        let local = mkIdentLocal [] ident.Name tyOpt initOpt
        // TODO : traverse body and follow references to decide on if this should be wrapped or not]
        let scopedVarAttrs = {
            IsArm = false
            IsRef = false
            IsBox = false
            // HasMultipleUses = hasMultipleUses ident.Name usages
            UsageCount = usageCount ident.Name usages
        }
        let scopedSymbols = ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs
        let ctxNext = { ctx with ScopedSymbols = scopedSymbols }
        mkLocalStmt local, ctxNext

    let makeLetStmts (com: IRustCompiler) ctx bindings usages =
        // Context will to be threaded through all let bindings, appending itself to ScopedSymbols each time
        let ctx, letStmtsRev =
            ((ctx, []), bindings)
            ||> List.fold (fun (ctx, lst) (ident: Fable.Ident, expr) ->
                let stmt, ctxNext =
                    match expr with
                    | Function (args, body, _name) ->
                        let isCapturing = hasCapturedNames com ctx ident.Name args body
                        if isCapturing then makeLetStmt com ctx usages ident expr
                        else transformInnerFunction com ctx ident.Name args body
                    | _ ->
                        makeLetStmt com ctx usages ident expr
                (ctxNext, stmt::lst) )
        letStmtsRev |> List.rev, ctx

    let transformLet (com: IRustCompiler) ctx bindings body =
        let usages =
            let bodyUsages = calcIdentUsages body
            let bindingsUsages = bindings |> List.map (snd >> calcIdentUsages)
            (Map.empty, bodyUsages::bindingsUsages)
            ||> List.fold (Helpers.Map.mergeAndAggregate (+))
        let letStmts, ctx = makeLetStmts com ctx bindings usages
        let bodyStmts =
            match body with
            | Fable.Sequential exprs ->
                let exprs = flattenSequential body
                List.map (transformAsStmt com ctx) exprs
            | _ ->
                [transformAsStmt com ctx body]
        letStmts @ bodyStmts |> mkStmtBlockExpr

    let transformSequential (com: IRustCompiler) ctx exprs =
        exprs
        |> List.map (transformAsStmt com ctx)
        |> mkStmtBlockExpr

    let transformIfThenElse (com: IRustCompiler) ctx range guard thenBody elseBody =
        let guardExpr = transformExpr com ctx guard
        let thenExpr = transformLeaveContext com ctx None thenBody
        match elseBody with
        | Fable.Value(Fable.UnitConstant, _) ->
            mkIfThenExpr guardExpr thenExpr //?loc=range)
        | _ ->
            let elseExpr = transformLeaveContext com ctx None elseBody
            mkIfThenElseExpr guardExpr thenExpr elseExpr //?loc=range)

    let transformWhileLoop (com: IRustCompiler) ctx range guard body =
        let guardExpr = transformExpr com ctx guard
        let bodyExpr = com.TransformExpr(ctx, body)
        mkWhileExpr None guardExpr bodyExpr //?loc=range)

    let transformForLoop (com: IRustCompiler) ctx range isUp (var: Fable.Ident) start limit body =
        let startExpr = transformExpr com ctx start
        let limitExpr = transformExpr com ctx limit
        let ctx = { ctx with IsInPluralizedExpr = true }
        let bodyExpr = com.TransformExpr(ctx, body)
        let varPat = makeFullNameIdentPat var.Name
        let rangeExpr =
            if isUp then
                mkRangeExpr (Some startExpr) (Some limitExpr) true
            else
                // downward loop
                let rangeExpr =
                    mkRangeExpr (Some limitExpr) (Some startExpr) true
                    |> mkParenExpr
                mkMethodCallExpr "rev" None rangeExpr []
        mkForLoopExpr None varPat rangeExpr bodyExpr //?loc=range)

    let transformTryCatch (com: IRustCompiler) ctx range body catch finalizer =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        // TODO: use panic::catch_unwind
        // TODO: transform catch
        match finalizer with
        | Some finBody ->
            // TODO: Temporary, transforms try/finally as sequential
            let letIdent = getUniqueNameInDeclarationScope ctx "try_result" |> makeIdent
            let letValue = body
            let letBody = Fable.Sequential [finBody; Fable.IdentExpr letIdent]
            let letExpr = Fable.Let(letIdent, letValue, letBody)
            letExpr
        | _ ->
            body // no finalizer
        |> transformExpr com ctx
        // |> mkTryBlockExpr // TODO: nightly only, enable when stable

    let transformCurriedApply (com: IRustCompiler) ctx range calleeExpr args =
        match ctx.TailCallOpportunity with
        | Some tc when tc.IsRecursiveRef(calleeExpr)
            && List.length tc.Args = List.length args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            let callee = transformCallee com ctx calleeExpr
            match args with
            | [] -> callFunction com ctx range callee args
            | args -> (callee, args) ||> List.fold (fun c arg -> callFunction com ctx range c [arg])

    let makeUnionCasePat unionCaseName fields =
        if List.isEmpty fields then
            makeFullNameIdentPat unionCaseName
        else
            let path = makeFullNamePath unionCaseName None
            mkTupleStructPat path fields

    let transformTypeTest (com: IRustCompiler) ctx range typ (expr: Fable.Expr): Rust.Expr =
        // let testOpt =
        //     match expr with
        //     | Fable.TypeCast(e, Fable.Any) ->
        //         match typ, e.Type with
        //         | Fable.DeclaredType(entRef, _), Fable.DeclaredType(entRef2, _) ->
        //             // TODO: somehow test if entRef2 implements or inherits entRef
        //             // for now the test is just an exact match
        //             let sameEnt = (entRef.FullName = entRef2.FullName)
        //             Some sameEnt
        //         | _ -> None
        //     | _ -> None
        // match testOpt with
        // | Some b -> mkBoolLitExpr b
        // | _ ->
            match expr.Type with
            | Fable.Any ->
                let callee = transformCallee com ctx expr
                let genArgs = transformGenArgs com ctx [typ]
                let ctx = { ctx with IsParamAnyType = true }
                let ty = transformType com ctx expr.Type
                let callee = callee |> mkCastExpr (ty |> mkRefTy)
                mkMethodCallExpr "is" genArgs callee []
            | _ ->
                addWarning com [] range "Cannot type test (evals to false)"
                mkBoolLitExpr false

    let transformTest (com: IRustCompiler) ctx range kind (fableExpr: Fable.Expr): Rust.Expr =
        match kind with
        | Fable.TypeTest typ ->
            transformTypeTest com ctx range typ fableExpr
        | Fable.OptionTest isSome ->
            let test = if isSome then "is_some" else "is_none"
            let expr = com.TransformExpr(ctx, fableExpr)
            mkMethodCallExpr test None expr []
        | Fable.ListTest nonEmpty ->
            let expr = libCall com ctx range [] "List" "isEmpty" [fableExpr]
            if nonEmpty then mkNotExpr expr else expr //, ?loc=range
        | Fable.UnionCaseTest tag ->
            match fableExpr.Type with
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                assert(ent.IsFSharpUnion)
                // let genArgs = transformGenArgs com ctx genArgs // TODO:
                let unionCase = ent.UnionCases |> List.item tag
                let fields =
                    match fableExpr with
                    | Fable.IdentExpr id ->
                        unionCase.UnionCaseFields |> List.mapi (fun i _field ->
                            let fieldName = $"{id.Name}_{tag}_{i}"
                            makeFullNameIdentPat fieldName
                        )
                    | _ ->
                        if List.isEmpty unionCase.UnionCaseFields
                        then []
                        else [WILD_PAT]
                let unionCaseName = getUnionCaseName com ctx entRef unionCase
                let pat = makeUnionCasePat unionCaseName fields
                let expr =
                    fableExpr
                    |> prepareRefForPatternMatch com ctx fableExpr.Type (getIdentName fableExpr)
                mkLetExpr pat expr
            | _ ->
                failwith "Should not happen"

    let transformSwitch (com: IRustCompiler) ctx (evalExpr: Fable.Expr) cases defaultCase targets: Rust.Expr =
        let namesForIndex evalType evalName caseIndex = //todo refactor with below
            match evalType with
            | Fable.Option(genArg, _) ->
                match evalName with
                | Some idName ->
                    let fieldName = $"{idName}_{caseIndex}_{0}"
                    [(fieldName, idName, genArg)]
                | _ -> []
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsFSharpUnion then
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    match evalName with
                    | Some idName ->
                        unionCase.UnionCaseFields |> List.mapi (fun i field ->
                            let fieldName = $"{idName}_{caseIndex}_{i}"
                            let fieldType = FableTransforms.uncurryType field.FieldType
                            (fieldName, idName, fieldType)
                        )
                    | _ -> []
                else []
            | _ -> []

        let makeArm pat targetIndex boundValues (extraVals: (string * string * Fable.Type) list)=
            let attrs = []
            let guard = None // TODO:
            let idents, (bodyExpr: Fable.Expr) = targets |> List.item targetIndex // TODO:
            let vars = idents |> List.map (fun (id: Fable.Ident) -> id.Name)
            // TODO: vars, boundValues
            let body =
                //com.TransformExpr(ctx, bodyExpr)
                let usages = calcIdentUsages bodyExpr
                let getScope name =
                    name, { IsArm = true
                            IsRef = true
                            IsBox = false
                            // HasMultipleUses = hasMultipleUses name usages
                            UsageCount = usageCount name usages }
                let symbolsAndNames =
                    let fromIdents =
                        idents
                        |> List.map (fun id -> getScope id.Name)
                    let fromExtra =
                        extraVals
                        |> List.map (fun (_name, friendlyName, _t) -> getScope friendlyName)
                    fromIdents @ fromExtra
                let scopedSymbols =
                    Helpers.Map.merge ctx.ScopedSymbols (symbolsAndNames |> Map.ofList)
                let ctx = { ctx with ScopedSymbols = scopedSymbols }
                transformLeaveContext com ctx None bodyExpr
            mkArm attrs pat guard body

        let makeUnionCasePatOpt evalType evalName caseIndex =
            match evalType with
            | Fable.Option(genArg, _) ->
                // let genArgs = transformGenArgs com ctx [genArg]
                let unionCaseFullName =
                    ["Some"; "None"] |> List.item caseIndex |> rawIdent
                let fields =
                    match evalName with
                    | Some idName ->
                        match caseIndex with
                        | 0 ->
                            let fieldName = $"{idName}_{caseIndex}_{0}"
                            [makeFullNameIdentPat fieldName]
                        | _ -> []
                    | _ ->
                        [WILD_PAT]
                let unionCaseName =
                    tryUseKnownUnionCaseNames unionCaseFullName
                    |> Option.defaultValue unionCaseFullName
                Some(makeUnionCasePat unionCaseName fields)
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsFSharpUnion then
                    // let genArgs = transformGenArgs com ctx genArgs
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    let fields =
                        match evalName with
                        | Some idName ->
                            unionCase.UnionCaseFields |> List.mapi (fun i _field ->
                                let fieldName = $"{idName}_{caseIndex}_{i}"
                                makeFullNameIdentPat fieldName
                            )
                        | _ ->
                            if List.isEmpty unionCase.UnionCaseFields
                            then []
                            else [WILD_PAT]
                    let unionCaseName = getUnionCaseName com ctx entRef unionCase
                    Some(makeUnionCasePat unionCaseName fields)
                else
                    None
            | _ ->
                None

        let evalType, evalName =
            match evalExpr with
            | Fable.Get (Fable.IdentExpr id, Fable.UnionTag, _, _) ->
                id.Type, Some id.Name
            | _ -> evalExpr.Type, None

        let arms =
            cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                let patOpt =
                    match caseExpr with
                    | Fable.Value (Fable.NumberConstant (:? int as tag, Int32, Fable.NumberInfo.Empty), r) ->
                        makeUnionCasePatOpt evalType evalName tag
                    | _ -> None
                let pat =
                    match patOpt with
                    | Some pat -> pat
                    | _ -> com.TransformExpr(ctx, caseExpr) |> mkLitPat
                let extraVals = namesForIndex evalType evalName targetIndex
                makeArm pat targetIndex (boundValues) extraVals
            )

        let defaultArm =
            let targetIndex, boundValues = defaultCase
            // To see if the default arm should actually be a union case pattern, we have to
            // examine its body to see if it starts with union field get. // TODO: look deeper
            // If it does, we'll replace the wildcard "_" with a union case pattern
            let idents, bodyExpr = targets |> List.item targetIndex
            let patOpt =
                let rec getUnionPat expr =
                    match expr with
                    | Fable.Get (Fable.IdentExpr id, Fable.OptionValue, _, _)
                        when Some id.Name = evalName && id.Type = evalType ->
                        makeUnionCasePatOpt evalType evalName 0
                    | Fable.Get (Fable.IdentExpr id, Fable.UnionField info, _, _)
                        when Some id.Name = evalName && id.Type = evalType ->
                        makeUnionCasePatOpt evalType evalName info.CaseIndex
                    | _ ->
                        //need to recurse or this only works for trivial expressions
                        let subExprs = FableTransforms.getSubExpressions expr
                        subExprs |> List.tryPick getUnionPat
                getUnionPat bodyExpr
            let pat = patOpt |> Option.defaultValue WILD_PAT
            let extraVals = namesForIndex evalType evalName targetIndex
            makeArm pat targetIndex boundValues extraVals

        let expr =
            evalExpr
            |> prepareRefForPatternMatch com ctx evalType (evalName |> Option.defaultValue "")

        mkMatchExpr expr (arms @ [defaultArm])

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.length idents = List.length values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues
        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr)::bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements)
            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccess (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] ->
            transformExpr com ctx target
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformExpr(ctx, target)

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Test(expr, Fable.OptionTest isSome, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), None)
                let right = makeIntConst (if isSome then 0 else 1)
                Some(evalExpr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | Fable.Get(Fable.IdentExpr i1, Fable.FieldGet fieldInfo1,_,_), Fable.Get(Fable.IdentExpr i2, Fable.FieldGet fieldInfo2,_,_) ->
                i1.Name = i2.Name && fieldInfo1.Name = fieldInfo2.Name
            | _ -> false
        let rec checkInner cases evalExpr treeExpr =
            match treeExpr with
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _)
                                    when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases
                    Some(evalExpr, List.rev cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases
                    checkInner cases evalExpr treeExpr
            | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                Some(evalExpr, List.rev cases, (defaultTargetIndex, defaultBoundValues))
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            let cases = [(caseExpr, targetIndex, boundValues)]
            checkInner cases evalExpr treeExpr
        | _ -> None

    // let simplifyDecisionTree (treeExpr: Fable.Expr) =
    //     treeExpr |> visitFromInsideOut (function
    //         | Fable.IfThenElse(
    //             guardExpr1,
    //             Fable.IfThenElse(
    //                 guardExpr2,
    //                 thenExpr,
    //                 Fable.DecisionTreeSuccess(index2,[],_),_),
    //             Fable.DecisionTreeSuccess(index1,[],t),r)
    //             when index1 = index2 ->
    //             Fable.IfThenElse(
    //                 makeLogOp None guardExpr1 guardExpr2 LogicalAnd,
    //                 thenExpr,
    //                 Fable.DecisionTreeSuccess(index2,[],t),r)
    //         | e -> e)

    let transformDecisionTree (com: IRustCompiler) ctx targets (expr: Fable.Expr): Rust.Expr =
        // let expr = simplifyDecisionTree expr
        match transformDecisionTreeAsSwitch expr with
        | Some(evalExpr, cases, defaultCase) ->
            transformSwitch com ctx evalExpr cases defaultCase targets
        | None ->
            let ctx = { ctx with DecisionTargets = targets }
            com.TransformExpr(ctx, expr)

    let rec transformExpr (com: IRustCompiler) ctx (fableExpr: Fable.Expr): Rust.Expr =
        match fableExpr with
        | Fable.Unresolved(_,_,r) ->
            addError com [] r "Unexpected unresolved expression"
            mkUnitExpr ()

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> transformIdentGet com ctx None id

        | Fable.Import(info, t, r) ->
            transformImport com ctx r t info None

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformLambda com ctx name [arg] body

        | Fable.Delegate(args, body, name, _) ->
            transformLambda com ctx name args body

        | Fable.ObjectExpr(members, typ, baseCall) ->
            transformObjectExpr com ctx typ members baseCall

        | Fable.Call(callee, info, typ, range) ->
            transformCall com ctx range typ callee info

        | Fable.CurriedApply(callee, args, t, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, typ, range) ->
            transformOperation com ctx range typ kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            transformIfThenElse com ctx r guardExpr thenExpr elseExpr

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTree com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccess com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            // flatten nested let binding expressions
            let bindings, body = flattenLet [] fableExpr
            transformLet com ctx bindings body
            // if ctx.HoistVars [ident] then
            //     let assignment = transformBindingAsExpr com ctx ident value
            //     Expression.sequenceExpression([|assignment; com.TransformExpr(ctx, body)|])
            // else iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            transformLet com ctx bindings body
        //     let idents = List.map fst bindings
        //     if ctx.HoistVars(idents) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformExpr(ctx, body)|])
        //     else iife com ctx expr

        | Fable.Sequential exprs ->
            // flatten nested sequential expressions
            let exprs = flattenSequential fableExpr
            transformSequential com ctx exprs

        | Fable.Emit(info, _t, range) ->
            transformEmit com ctx range info

        | Fable.WhileLoop(guard, body, range) ->
            transformWhileLoop com ctx range guard body

        | Fable.ForLoop (var, start, limit, body, isUp, range) ->
            transformForLoop com ctx range isUp var start limit body

        | Fable.TryCatch (body, catch, finalizer, range) ->
            transformTryCatch com ctx range body catch finalizer

        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(e, arity) ->
                // transformCurry com ctx e arity //TODO: check arity, if curry is needed
                transformExpr com ctx e
            | Fable.Throw(expr, _) ->
                match expr with
                | None -> failwith "TODO: rethrow"
                | Some(TransformExpr com ctx msg) -> mkMacroExpr "panic" [mkStrLitExpr "{}"; msg]
            | Fable.Debugger ->
                // TODO:
                $"Unimplemented Extended expression: %A{kind}"
                |> addError com [] r
                mkUnitExpr ()

    let rec tryFindEntryPoint (com: IRustCompiler) decl: string list option =
        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members
            |> List.tryPick (tryFindEntryPoint com)
            |> Option.map (fun name -> decl.Name :: name)
        | Fable.MemberDeclaration decl ->
            let memb = com.GetMember(decl.MemberRef)
            memb.Attributes
            |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint)
            |> Option.map (fun _ -> [splitLast decl.Name])
        | Fable.ActionDeclaration decl -> None
        | Fable.ClassDeclaration decl -> None

    let isLastFileInProject (com: IRustCompiler) =
        (Array.last com.SourceFiles) = com.CurrentFile

    let getModuleItems (com: IRustCompiler) ctx =
        if isLastFileInProject com then
            // add all other project files as module imports
            com.SourceFiles |> Array.iter (fun filePath ->
                if filePath <> com.CurrentFile then
                    let relPath = Fable.Path.getRelativeFileOrDirPath false com.CurrentFile false filePath
                    com.GetImportName(ctx, "*", relPath, None) |> ignore
            )
            let makeModItems (modulePath, moduleName) =
                let relPath = Fable.Path.getRelativePath com.CurrentFile modulePath
                let attrs = [mkEqAttr "path" relPath]
                let modItem = mkUnloadedModItem attrs moduleName
                let useItem = mkGlobUseItem [] [moduleName]
                if isFableLibrary com
                then [modItem; useItem |> mkPublicItem] // export modules at top level
                else [modItem]
            let modItems =
                com.GetAllModules()
                |> List.sortBy fst
                |> List.collect makeModItems
            modItems
        else []

    let getEntryPointItems (com: IRustCompiler) ctx decls =
        let entryPoint = decls |> List.tryPick (tryFindEntryPoint com)
        match entryPoint with
        | Some path ->
            // add some imports for main function
            let asStr = getLibraryImportName com ctx "String" "string"
            let asArr = getLibraryImportName com ctx "Native" "array"
            let tyLrc = getLibraryImportName com ctx "Native" "Lrc"

            // main entrypoint
            let mainName = String.concat "::" path
            let strBody = [
                $"let args: Vec<String> = std::env::args().collect()"
                $"let args: Vec<{tyLrc}<str>> = args[1..].iter().map(|s| {asStr}(s)).collect()"
                $"{mainName}({asArr}(args))"
            ]
            let fnBody = strBody |> Seq.map mkEmitSemiStmt |> mkBlock |> Some

            let attrs = []
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let fnItem = mkFnItem attrs "main" fnKind
            [fnItem |> mkPublicItem]

        | None -> []

(*
    let makeEntityTypeParamDecl (com: IRustCompiler) _ctx (ent: Fable.Entity) =
        if com.Options.Typescript then
            getEntityGenParams ent |> makeTypeParamDecl
        else
            None

    let getClassImplements com ctx (ent: Fable.Entity) =
        let mkNative genArgs typeName =
            let id = Identifier.identifier(typeName)
            let typeParamInst = makeGenTypeParamInst com ctx genArgs
            ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some
//        let mkImport genArgs moduleName typeName =
//            let id = makeImportTypeId com ctx moduleName typeName
//            let typeParamInst = makeGenTypeParamInst com ctx genArgs
//            ClassImplements(id, ?typeParameters=typeParamInst) |> Some
        ent.AllInterfaces |> Seq.choose (fun ifc ->
            match ifc.Entity.FullName with
            | "Fable.Core.JS.Set`1" -> mkNative ifc.GenericArgs "Set"
            | "Fable.Core.JS.Map`2" -> mkNative ifc.GenericArgs "Map"
            | _ -> None
        )

    let getUnionFieldsAsIdents (_com: IRustCompiler) _ctx (_ent: Fable.Entity) =
        let tagId = makeTypedIdent (Fable.Number Int32) "tag"
        let fieldsId = makeTypedIdent (Fable.Array Fable.Any) "fields"
        [| tagId; fieldsId |]
*)
    let getEntityFieldsAsIdents _com (ent: Fable.Entity): Fable.Ident list =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> sanitizeMember
            let typ = FableTransforms.uncurryType field.FieldType
            let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
            id)
        |> Seq.toList
(*
    let getEntityFieldsAsProps (com: IRustCompiler) ctx (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            getUnionFieldsAsIdents com ctx ent
            |> Array.map (fun id ->
                let prop = identAsExpr id
                let ta = typeAnnotation com ctx id.Type
                ObjectTypeProperty.objectTypeProperty(prop, ta))
        else
            ent.FSharpFields
            |> Seq.map (fun field ->
                let prop, computed = memberFromName field.Name
                let ta = typeAnnotation com ctx field.FieldType
                let isStatic = if field.IsStatic then Some true else None
                ObjectTypeProperty.objectTypeProperty(prop, ta, computed_=computed, ?``static``=isStatic))
            |> Seq.toArray

    let declareClassType (com: IRustCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) (baseExpr: Rust.Expr option) classMembers =
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let implements =
            if com.Options.Typescript then
                let implements = Util.getClassImplements com ctx ent |> Seq.toArray
                if Array.isEmpty implements then None else Some implements
            else None
        let classCons = makeClassConstructor consArgs consBody
        let classFields =
            if com.Options.Typescript then
                getEntityFieldsAsProps com ctx ent
                |> Array.map (fun (ObjectTypeProperty(key, value, _, _, ``static``, _, _, _)) ->
                    let ta = value |> TypeAnnotation |> Some
                    ClassMember.classProperty(key, ``static``=``static``, ?typeAnnotation=ta))
            else Array.empty
        let classMembers = Array.append [| classCons |] classMembers
        let classBody = ClassBody.classBody([| yield! classFields; yield! classMembers |])
        let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr, ?typeParameters=typeParamDecl, ?implements=implements)
        classExpr |> declareModuleMember ent.IsPublic entName false

    let declareType (com: IRustCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr classMembers: ModuleDeclaration list =
        let typeDeclaration = declareClassType com ctx ent entName consArgs consBody baseExpr classMembers
        let reflectionDeclaration =
            let ta =
                if com.Options.Typescript then
                    makeImportTypeAnnotation com ctx [] "Reflection" "TypeInfo"
                    |> TypeAnnotation |> Some
                else None
            let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent)
            let generics = genArgs |> Array.map identAsExpr
            let body = transformReflectionInfo com ctx None ent generics
            let args = genArgs |> Array.map (fun x -> Pattern.identifier(x.Name, ?typeAnnotation=ta))
            let returnType = ta
            makeFunctionExpression None (args, body, returnType, None)
            |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false
        [typeDeclaration; reflectionDeclaration]
*)
    let makeTypedParam (com: IRustCompiler) ctx (ident: Fable.Ident) returnType =
        if ident.IsThisArgument then
            // is it a fluent API
            match ident.Type, shouldBeRefCountWrapped com ctx ident.Type with
            | Fable.DeclaredType(entRef, genArgs), Some ptrType when ident.Type = returnType ->
                // for fluent APIs, the type of thisArg is (self: &Lrc<Self>)
                let ty = mkImplSelfTy()
                let ty =
                    match ptrType with
                    | Rc -> ty |> makeRcTy com ctx
                    | Arc -> ty |> makeArcTy com ctx
                    | Lrc -> ty |> makeLrcTy com ctx
                    | Box -> ty |> makeBoxTy com ctx
                    |> mkRefTy
                mkParamFromType (rawIdent "self") ty false false
            | _ ->
                mkImplSelfParam false false
        else
            let ty = transformParamType com ctx ident.Type
            mkParamFromType ident.Name ty false false

    // let inferredParam (com: IRustCompiler) ctx (ident: Fable.Ident) =
    //     mkInferredParam ident.Name false false

    let transformFunctionDecl (com: IRustCompiler) ctx args (parameters: Fable.Parameter list) returnType =
        let ctx = { ctx with IsParamAnyType = true }
        let inputs =
            args
            |> discardUnitArg
            |> List.mapi (fun idx ident ->
                            let isByRefPreferred = parameterIsByRefPreferred idx parameters
                            let ctx = { ctx with IsParamByRefPreferred = isByRefPreferred || ctx.IsParamByRefPreferred }
                            makeTypedParam com ctx ident returnType)
        let output =
            if returnType = Fable.Unit then VOID_RETURN_TY
            else
                let ty = returnType |> transformType com ctx
                ty |> mkFnRetTy
        mkFnDecl inputs output

    let isClosedOverIdent com ctx (ignoredNames: HashSet<string>) expr =
        match expr with
        | Fable.IdentExpr ident ->
            if not (ignoredNames.Contains(ident.Name))
                && not (ident.IsCompilerGenerated && ident.Name = "matchValue")
                && (ident.IsMutable ||
                    (isValueScoped ctx ident.Name) ||
                    (isRefScoped ctx ident.Name) ||
                    (match ident.Type with Fable.GenericParam _ -> true | _ -> false) ||
                    // Closures may capture Ref counted vars, so by cloning
                    // the actual closure, all attached ref counted var are cloned too
                    (shouldBeRefCountWrapped com ctx ident.Type |> Option.isSome)
                )
            then Some ident
            else None
        // ignore local names declared in the closure
        // TODO: not perfect, local name shadowing will ignore captured names
        | Fable.ForLoop(ident, _, _, _, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.Lambda(arg, _, _) ->
            ignoredNames.Add(arg.Name) |> ignore
            None
        | Fable.Delegate(args, body, name, _) ->
            args |> List.iter (fun arg ->
                ignoredNames.Add(arg.Name) |> ignore)
            None
        | Fable.Let(ident, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.LetRec(bindings, _) ->
            bindings |> List.iter (fun (ident, _) ->
                ignoredNames.Add(ident.Name) |> ignore)
            None
        | Fable.DecisionTree(_, targets) ->
            targets |> List.iter (fun (idents, _) ->
                idents |> List.iter (fun ident ->
                    ignoredNames.Add(ident.Name) |> ignore))
            None
        | _ ->
            None

    let getIgnoredNames (name: string option) (args: Fable.Ident list) =
        let argNames = args |> List.map (fun arg -> arg.Name)
        let allNames = name |> Option.fold (fun xs x -> x :: xs) argNames
        allNames |> Set.ofList

    let hasCapturedNames com ctx (name: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames (Some name) args)
        let isClosedOver expr =
            isClosedOverIdent com ctx ignoredNames expr
            |> Option.isSome
        FableTransforms.deepExists isClosedOver body

    let getCapturedNames com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames name args)
        let capturedNames = HashSet<string>()
        let addClosedOver expr =
            isClosedOverIdent com ctx ignoredNames expr
            |> Option.iter (fun ident -> capturedNames.Add(ident.Name) |> ignore)
            false
        // collect all closed over names that are not arguments
        FableTransforms.deepExists addClosedOver body |> ignore
        capturedNames |> Set.ofSeq // remove duplicates in some contexts

    let getFunctionCtx com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) isTailRec =
        let usages = calcIdentUsages body
        let scopedSymbols =
            (ctx.ScopedSymbols, args)
            ||> List.fold (fun acc arg ->
                //TODO: optimizations go here
                let scopedVarAttrs = {
                    IsArm = false
                    IsRef = arg.IsThisArgument || isByRefType com arg.Type || ctx.IsParamByRefPreferred
                    IsBox = false
                    // HasMultipleUses = hasMultipleUses arg.Name usages
                    UsageCount = usageCount arg.Name usages
                }
                acc |> Map.add arg.Name scopedVarAttrs)
        let tco =
            if isTailRec then
                Some(NamedTailCallOpportunity(com, ctx, name.Value, args) :> ITailCallOpportunity)
            else None
        { ctx with
            ScopedSymbols = scopedSymbols
            IsParamByRefPreferred = false
            TailCallOpportunity = tco }

    let isTailRecursive (name: string option) (body: Fable.Expr) =
        if name.IsNone then false, false
        else FableTransforms.isTailRecursive name.Value body

    let transformFunctionBody com ctx (args: Fable.Ident list) (body: Fable.Expr) =
        match ctx.TailCallOpportunity with
        | Some tc ->
            // tail call elimination setup (temp vars, loop, break)
            let label = tc.Label
            let args = args |> List.filter (fun arg -> not (arg.IsMutable || arg.IsThisArgument))
            let mutArgs = args |> List.map (fun arg -> { arg with IsMutable = true })
            let idExprs = args |> List.map (fun arg -> Fable.IdentExpr arg)
            let bindings = List.zip mutArgs idExprs
            let argMap = mutArgs |> List.map (fun arg -> arg.Name, Fable.IdentExpr arg) |> Map.ofList
            let body = FableTransforms.replaceValues argMap body
            let letStmts, ctx = makeLetStmts com ctx bindings Map.empty
            let loopBody = transformLeaveContext com ctx None body
            let loopExpr = mkBreakExpr (Some label) (Some(mkParenExpr loopBody))
            let loopStmt = mkLoopExpr (Some label) loopExpr |> mkExprStmt
            letStmts @ [loopStmt] |> mkStmtBlockExpr
        | _ ->
            transformLeaveContext com ctx None body

    let transformFunc com ctx (name: string option) (parameters: Fable.Parameter list) (args: Fable.Ident list) (body: Fable.Expr) =
        //if name |> Option.exists (fun n -> n.Contains("byrefAttrIntFn")) then System.Diagnostics.Debugger.Break()
        let isRecursive, isTailRec = isTailRecursive name body
        let argTypes = args |> List.map (fun arg -> arg.Type)
        let genParams = getGenericParams ctx (argTypes @ [body.Type])
        let fnDecl = transformFunctionDecl com ctx args parameters body.Type
        let ctx = getFunctionCtx com ctx name args body isTailRec
        let fnBody = transformFunctionBody com ctx args body
        fnDecl, fnBody, genParams

    let transformLambda com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let isRecursive, isTailRec = isTailRecursive name body
        let fixedArgs = if isRecursive && not isTailRec then (makeIdent name.Value) :: args else args
        let fnDecl = transformFunctionDecl com ctx fixedArgs [] Fable.Unit
        let ctx = getFunctionCtx com ctx name args body isTailRec
        // remove captured names from scoped symbols, as they will be cloned
        let closedOverCloneableNames = getCapturedNames com ctx name args body
        let scopedSymbols = ctx.ScopedSymbols |> Helpers.Map.except closedOverCloneableNames
        let ctx = { ctx with IsInPluralizedExpr = true; ScopedSymbols = scopedSymbols }
        let fnBody = transformFunctionBody com ctx args body
        let closureExpr = mkClosureExpr fnDecl fnBody
        let closureExpr =
            if isRecursive && not isTailRec then
                // make it recursive with fixed-point combinator
                let fixName = "fix" + string (List.length args)
                makeLibCall com ctx None "Func" fixName [closureExpr]
            else closureExpr
        let closureExpr =
            if not (Set.isEmpty closedOverCloneableNames) then
                mkStmtBlockExpr [
                    for name in closedOverCloneableNames do
                        let pat = makeFullNameIdentPat name
                        let identExpr = com.TransformExpr(ctx, makeIdentExpr name)
                        let cloneExpr = makeClone identExpr
                        let letExpr = mkLetExpr pat cloneExpr
                        yield letExpr |> mkSemiStmt
                    yield closureExpr |> mkExprStmt
                ]
            else closureExpr
        if ctx.RequiresSendSync
        then closureExpr |> makeArcValue com ctx
        else closureExpr |> makeLrcValue com ctx

    let makeTypeBounds (com: IRustCompiler) ctx argName (constraints: Fable.Constraint list) =
        let makeGenBound names tyNames =
            // makes gen type bound, e.g. T: From(i32), or T: Default
            let tys = tyNames |> List.map (fun tyName ->
                mkGenericPathTy [tyName] None)
            let genArgs = mkConstraintArgs tys []
            mkTypeTraitGenericBound names genArgs

        let makeRawBound id =
            makeGenBound [rawIdent id] []

        let makeOpBound op =
            // makes ops type bound, e.g. T: Add(Output=T)
            let ty = mkGenericPathTy [argName] None
            let genArgs = mkConstraintArgs [] ["Output", ty]
            mkTypeTraitGenericBound ["core";"ops"; op] genArgs

        let makeConstraint = function
            | Fable.Constraint.HasMember(membName, isStatic) ->
                match membName, isStatic with
                | Operators.addition, true -> [makeOpBound "Add"]
                | Operators.subtraction, true -> [makeOpBound "Sub"]
                | Operators.multiply, true -> [makeOpBound "Mul"]
                | Operators.division, true -> [makeOpBound "Div"]
                | Operators.modulus, true -> [makeOpBound "Rem"]
                | Operators.unaryNegation, true -> [makeOpBound "Neg"]
                | Operators.divideByInt, true ->
                    [makeOpBound "Div"; makeGenBound [rawIdent "From"] ["i32"]]
                | "get_Zero", true -> [makeRawBound "Default"]
                | _ -> []
            | Fable.Constraint.CoercesTo(targetType) ->
                match targetType with
                | IFormattable ->
                    [ makeGenBound ["core";"fmt";"Debug"] [] ]
                | IEquatable _ ->
                    [ makeRawBound "Eq"
                    ; makeGenBound ["core";"hash";"Hash"] [] ]
                | Fable.DeclaredType(entRef, genArgs) ->
                    let ent = com.GetEntity(entRef)
                    if ent.IsInterface then
                        let nameParts = getInterfaceEntityName com ctx entRef |> splitNameParts
                        let genArgs = transformGenArgs com ctx genArgs
                        let traitBound = mkTypeTraitGenericBound nameParts genArgs
                        [traitBound]
                    else []
                | _ -> []
            | Fable.Constraint.IsNullable -> []
            | Fable.Constraint.IsValueType -> []
            | Fable.Constraint.IsReferenceType -> []
            | Fable.Constraint.HasDefaultConstructor -> []
            | Fable.Constraint.HasComparison -> [makeRawBound "PartialOrd"]
            | Fable.Constraint.HasEquality -> //[makeRawBound "PartialEq"]
                [ makeRawBound "Eq"
                ; makeGenBound ["core";"hash";"Hash"] [] ]
            | Fable.Constraint.IsUnmanaged -> []
            | Fable.Constraint.IsEnum -> []

        constraints
        |> List.distinct
        |> List.collect makeConstraint

    let makeGenerics com ctx (genParams: Fable.Type list) =
        let defaultBounds = [
            mkTypeTraitGenericBound [rawIdent "Clone"] None
            mkLifetimeGenericBound "'static" //TODO: add it only when needed
        ]
        genParams
        |> List.choose (function
            | Fable.GenericParam(name, _isMeasure, constraints) ->
                let bounds = makeTypeBounds com ctx name constraints
                let p = mkGenericParamFromName [] name (bounds @ defaultBounds)
                Some p
            | _ -> None)
        |> mkGenerics

    let transformInnerFunction com ctx (name: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let fnDecl, fnBody, fnGenParams =
            transformFunc com ctx (Some name) [] args body
        let fnBodyBlock =
            if body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics com ctx fnGenParams
        let fnKind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = []
        let fnItem = mkFnItem attrs name fnKind
        let scopedVarAttrs = {
            IsArm = false
            IsRef = false
            IsBox = true
            // HasMultipleUses = true
            UsageCount = 9999
        }
        let scopedSymbols = ctx.ScopedSymbols |> Map.add name scopedVarAttrs
        let ctxNext = { ctx with ScopedSymbols = scopedSymbols }
        mkItemStmt fnItem, ctxNext

    let transformAttributes (com: IRustCompiler) ctx (attributes: Fable.Attribute seq) =
        attributes
        |> Seq.collect (fun att ->
            // translate test methods attributes
            // TODO: support more test frameworks
            if att.Entity.FullName.EndsWith(".FactAttribute") then
                [mkAttr "test" []]
            // custom outer attributes
            elif att.Entity.FullName = Atts.rustOuterAttr then
                match att.ConstructorArgs with
                | [:? string as name] -> [mkAttr name []]
                | [:? string as name; :? string as value] -> [mkEqAttr name value]
                | [:? string as name; :? (obj[]) as items] -> [mkAttr name (items |> Array.map string)]
                | _ -> []
            // custom inner attributes
            elif att.Entity.FullName = Atts.rustInnerAttr then
                match att.ConstructorArgs with
                | [:? string as name] -> [mkInnerAttr name []]
                | [:? string as name; :? string as value] -> [mkInnerEqAttr name value]
                | [:? string as name; :? (obj[]) as items] -> [mkInnerAttr name (items |> Array.map string)]
                | _ -> []
            else []
        )
        |> Seq.toList

    let transformModuleAction (com: IRustCompiler) ctx (body: Fable.Expr) =
        // uses startup::on_startup! for static execution (before main)
        let expr = transformExpr com ctx body
        let attrs = []
        let macroName = getLibraryImportName com ctx "Native" "on_startup"
        let macroItem = mkMacroItem attrs macroName [expr]
        [macroItem]

    let transformModuleFunction (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        let name = splitLast decl.Name
        let isByRefPreferred =
            memb.Attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef)
        let fnDecl, fnBody, fnGenParams =
            let ctx = { ctx with IsParamByRefPreferred = isByRefPreferred }
            let parameters = memb.CurriedParameterGroups |> List.concat
            // let returnType = memb.ReturnParameter.Type
            transformFunc com ctx (Some memb.FullName) parameters decl.Args decl.Body
        let fnBodyBlock =
            if decl.Body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics com ctx fnGenParams
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = transformAttributes com ctx memb.Attributes
        let fnItem = mkFnItem attrs name kind
        // let fnItem =
        //     if memb.IsPublic
        //     then fnItem |> mkPublicItem
        //     else fnItem
        [fnItem |> mkPublicItem]

    let transformModuleMember (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        // Module let bindings look like this:
        // pub fn value() -> T {
        //     static value: MutCell<Option<T>> = MutCell::new(None);
        //     value.get_or_init(|| initValue)
        // }
        let name = splitLast decl.Name
        let typ = decl.Body.Type
        let initNone =
            mkGenericPathExpr [rawIdent "None"] None
            |> makeMutValue
        let value = transformLeaveContext com ctx None decl.Body
        let value =
            if memb.IsMutable
            then value |> makeMutValue |> makeLrcValue com ctx
            else value
        let ty = transformType com ctx typ
        let ty =
            if memb.IsMutable
            then ty |> makeMutTy com ctx |> makeLrcTy com ctx
            else ty
        let staticTy = ty |> makeOptionTy |> makeMutTy com ctx
        let staticStmt =
            mkStaticItem [] name staticTy (Some initNone)
            |> mkItemStmt
        let callee = com.TransformExpr(ctx, makeIdentExpr name)
        let closureExpr =
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            mkClosureExpr fnDecl value
        let valueStmt =
            mkMethodCallExpr "get_or_init" None callee [closureExpr]
            |> mkExprStmt

        let attrs = transformAttributes com ctx memb.Attributes
        let fnBody = [staticStmt; valueStmt] |> mkBlock |> Some
        let fnDecl = mkFnDecl [] (mkFnRetTy ty)
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnItem attrs name fnKind
        // let fnItem =
        //     if memb.IsPublic
        //     then fnItem |> mkPublicItem
        //     else fnItem
        [fnItem |> mkPublicItem]

    // // is the member return type the same as the entity
    // let isFluentMemberType (ent: Fable.Entity) = function
    //     | Fable.DeclaredType(entRef, _) -> entRef.FullName = ent.FullName
    //     | _ -> false

    // // does the member body return "this"
    // let isFluentMemberBody (body: Fable.Expr) =
    //     let rec loop = function
    //         | Fable.IdentExpr id when id.IsThisArgument -> true
    //         | Fable.Sequential exprs -> loop (List.last exprs)
    //         | Fable.Let(_, value, body) -> loop body
    //         | Fable.LetRec(bindings, body) -> loop body
    //         | Fable.IfThenElse(cond, thenExpr, elseExpr, _) ->
    //             loop thenExpr || loop elseExpr
    //         | Fable.DecisionTree(expr, targets) ->
    //             List.map snd targets |> List.exists loop
    //         | _ -> false
    //     loop body

    let makeAssocMemberItem (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (membName: string) (args: Fable.Ident list) =
        let parameters = memb.CurriedParameterGroups |> List.concat
        let returnType = memb.ReturnParameter.Type
        let fnDecl = transformFunctionDecl com ctx args parameters returnType
        let generics = getMemberGenArgs memb |> makeGenerics com ctx
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics None
        let attrs = transformAttributes com ctx memb.Attributes
        mkFnAssocItem attrs membName fnKind

    let transformAssocMemberFunction (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (membName: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let name = splitLast membName
        let fnDecl, fnBody, fnGenParams =
            let parameters = memb.CurriedParameterGroups |> List.concat
            // let returnType = memb.ReturnParameter.Type
            transformFunc com ctx (Some membName) parameters args body
        let fnBody =
            if body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let generics = makeGenerics com ctx fnGenParams
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics (Some fnBody)
        let attrs = transformAttributes com ctx memb.Attributes
        let fnItem = mkFnAssocItem attrs name fnKind
        fnItem

    let getEntityGenArgs (ent: Fable.Entity) =
        ent.GenericParameters
        |> List.map (fun p -> Fable.Type.GenericParam(p.Name, p.IsMeasure, Seq.toList p.Constraints))

    let getMemberGenArgs (memb: Fable.MemberFunctionOrValue) =
        memb.GenericParameters
        |> List.choose (fun p ->
            if not p.IsMeasure then
                Fable.Type.GenericParam(p.Name, p.IsMeasure, Seq.toList p.Constraints)
                |> Some
            else None)

    let getInterfaceMemberNamesSet (com: IRustCompiler) (entRef: Fable.EntityRef) =
        let ent = com.GetEntity(entRef)
        assert(ent.IsInterface)
        ent.AllInterfaces
        |> Seq.collect (fun i ->
            let e = com.GetEntity(i.Entity)
            e.MembersFunctionsAndValues)
        |> Seq.map (fun m -> m.DisplayName)
        |> Set.ofSeq

    let makeDerivedFrom com (ent: Fable.Entity) =
        let isCopyable = ent |> isCopyableEntity com Set.empty
        let isPrintable = ent |> isPrintableEntity com Set.empty
        let isDefaultable = ent |> isDefaultableEntity com Set.empty
        let isComparable = ent |> isComparableEntity com Set.empty
        let isEquatable = ent |> isEquatableEntity com Set.empty
        let isHashable = ent |> isHashableEntity com Set.empty

        let derivedFrom = [
            rawIdent "Clone"
            if isCopyable then rawIdent "Copy"
            if isPrintable then rawIdent "Debug"
            if isDefaultable then rawIdent "Default"
            if isEquatable then rawIdent "PartialEq"
            if isComparable then rawIdent "PartialOrd"
            if isHashable then rawIdent "Hash"
            if isEquatable && isHashable then rawIdent "Eq"
            if isComparable && isHashable then rawIdent "Ord"
        ]
        derivedFrom

    let transformAbbrev (com: IRustCompiler) ctx (ent: Fable.Entity) =
        // TODO: this is unfinished and untested
        let entName = splitLast ent.FullName
        let genArgs = getEntityGenArgs ent
        let ty =
            let genArgs = transformGenArgs com ctx genArgs
            let traitBound = mkTypeTraitGenericBound [entName] genArgs
            mkTraitTy [traitBound]
        let path =
            let genArgTys = genArgs |> List.map (transformType com ctx)
            mkGenericPath (splitNameParts ent.FullName) (mkGenericTypeArgs genArgTys)
        let generics = genArgs |> makeGenerics com ctx
        let bounds = [] //TODO:
        let tyItem = mkTyAliasItem [] entName ty generics bounds
        [tyItem]

    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entName = splitLast ent.FullName
        let generics = getEntityGenArgs ent
                        |> List.filter genArgsUnitsFilter
                        |> makeGenerics com ctx
        let variants =
            ent.UnionCases |> Seq.map (fun uci ->
                let name = uci.Name
                let isPublic = false
                let fields =
                    uci.UnionCaseFields |> List.map (fun field ->
                        let typ = FableTransforms.uncurryType field.FieldType
                        let fieldTy = transformType com ctx typ
                        let fieldName = field.Name |> sanitizeMember
                        mkField [] fieldName fieldTy isPublic
                    )
                if List.isEmpty uci.UnionCaseFields
                then mkUnitVariant [] name
                else mkTupleVariant [] name fields
            )
        let attrs = transformAttributes com ctx ent.Attributes
        let attrs = attrs @ [mkAttr "derive" (makeDerivedFrom com ent)]
        let enumItem = mkEnumItem attrs entName variants generics
        [enumItem |> mkPublicItem] // TODO: add traits for attached members

    let transformClass (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entName = splitLast ent.FullName
        let generics = getEntityGenArgs ent
                        |> List.filter genArgsUnitsFilter
                        |> makeGenerics com ctx
        let isPublic = ent.IsFSharpRecord
        let idents = getEntityFieldsAsIdents com ent
        let fields =
            idents |> List.map (fun ident ->
                let ty = transformType com ctx ident.Type
                let ty =
                    if ident.IsMutable
                    then ty |> makeMutTy com ctx
                    else ty
                mkField [] ident.Name ty isPublic
            )
        let attrs = transformAttributes com ctx ent.Attributes
        let attrs = attrs @ [mkAttr "derive" (makeDerivedFrom com ent)]
        let structItem = mkStructItem attrs entName fields generics
        [structItem |> mkPublicItem] // TODO: add traits for attached members

    let transformCompilerGeneratedConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) =
        // let ctor = ent.MembersFunctionsAndValues |> Seq.tryFind (fun q -> q.CompiledName = ".ctor")
        // ctor |> Option.map (fun ctor -> ctor.CurriedParameterGroups)
        let idents = getEntityFieldsAsIdents com ent
        let fields = idents |> List.map Fable.IdentExpr
        let genArgs = getEntityGenArgs ent
        let body = Fable.Value(Fable.NewRecord(fields, ent.Ref, genArgs), None)
        let entName = getEntityFullName com ctx ent.Ref
        let paramTypes = idents |> List.map (fun id -> id.Type)
        let memberRef = Fable.GeneratedMember.Function(entName, paramTypes, body.Type, entRef = ent.Ref)
        let memb = com.GetMember(memberRef)
        transformAssocMemberFunction com ctx memb entName idents body

    let transformPrimaryConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (ctor: Fable.MemberDecl) =
        let body =
            match ctor.Body with
            | Fable.Sequential exprs ->
                let idents = getEntityFieldsAsIdents com ent
                let argNames = ctor.Args |> List.map (fun arg -> arg.Name) |> Set.ofList
                let identMap = idents |> List.map (fun id ->
                    let uniqueName = makeUniqueName id.Name argNames
                    id.Name, { id with Name = uniqueName; IsMutable = false }) |> Map.ofList
                let fieldIdents = idents |> List.map (fun id -> Map.find id.Name identMap)
                let fields = fieldIdents |> List.map Fable.IdentExpr
                let genArgs = getEntityGenArgs ent
                let body = Fable.Value(Fable.NewRecord(fields, ent.Ref, genArgs), None)

                // add return value after the body
                let body = Fable.Sequential (exprs @ [body])
                // replace 'this.field' with just 'field' in body
                let body =
                    body |> visitFromInsideOut (function
                        | Fable.Set(Fable.Value(Fable.ThisValue _, _), Fable.SetKind.FieldSet(fieldName), t, value, r)
                                when not (fieldName.Contains("@")) ->
                            let identExpr = identMap |> Map.find fieldName |> Fable.IdentExpr
                            Fable.Set(identExpr, Fable.ValueSet, t, value, r)
                        | Fable.Get(Fable.Value(Fable.ThisValue _, _), Fable.GetKind.FieldGet info, t, r)
                                when not (info.Name.Contains("@")) ->
                            let identExpr = identMap |> Map.find info.Name |> Fable.IdentExpr
                            identExpr
                        | e -> e)
                // add field declarations before body
                let body =
                    (body, fieldIdents |> List.rev)
                    ||> List.fold (fun acc ident ->
                        let nullOfT = Fable.Value(Fable.Null ident.Type, None)
                        Fable.Let(ident, nullOfT, acc)) // will be transformed as declaration only
                body
            | e -> e
        let ctor = { ctor with Body = body }
        let ctx = { ctx with ScopedTypeParams =
                                ent.GenericParameters
                                |> List.map (fun p -> p.Name)
                                |> Set.ofList }
        let memb = com.GetMember(ctor.MemberRef)
        transformAssocMemberFunction com ctx memb ctor.Name ctor.Args ctor.Body

    let transformInterface (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let assocItems =
            ent.AllInterfaces
            |> Seq.collect (fun ifc ->
                let ifcTyp = Fable.DeclaredType(ifc.Entity, ifc.GenericArgs)
                let ifcEnt = com.GetEntity(ifc.Entity)
                ifcEnt.MembersFunctionsAndValues
                |> Seq.filter (fun memb -> not memb.IsProperty)
                |> Seq.map (fun memb ->
                    let thisArg = { makeTypedIdent ifcTyp "this" with IsThisArgument = true }
                    let memberArgs =
                        memb.CurriedParameterGroups
                        |> List.collect id
                        |> List.mapi (fun i p ->
                            let name = defaultArg p.Name $"arg{i}"
                            makeTypedIdent p.Type name)
                    let membName = memb.DisplayName
                    let args = (thisArg::memberArgs)
                    makeAssocMemberItem com ctx memb membName args
                )
            )
        let generics = getEntityGenArgs ent
                        |> List.filter genArgsUnitsFilter
                        |> makeGenerics com ctx
        let entName = splitLast ent.FullName
        let traitItem = mkTraitItem [] entName assocItems [] generics
        [traitItem |> mkPublicItem]

    let makeDisplayTraitImpl com ctx self_ty genArgs =
        // expected output:
        // impl core::fmt::Display for {self_ty} {
        //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        //         write!(f, "{}", self.ToString_())
        //     }
        // }
        let bodyStmt =
            "write!(f, \"{}\", self.ToString_())"
            |> mkEmitExprStmt
        let fnBody = [bodyStmt] |> mkBlock |> Some
        let fnDecl =
            let inputs =
                let ty = mkGenericPathTy ["core";"fmt";"Formatter"] None
                let p1 = mkImplSelfParam false false
                let p2 = mkParamFromType "f" (ty |> mkMutRefTy) false false
                [p1; p2]
            let output =
                let ty = mkGenericPathTy ["core";"fmt";rawIdent "Result"] None
                ty |> mkFnRetTy
            mkFnDecl inputs output
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnAssocItem [] "fmt" fnKind
        let implItem =
            let generics = genArgs |> makeGenerics com ctx
            let path = mkGenericPath ["core";"fmt";"Display"] None
            let ofTrait = mkTraitRef path |> Some
            mkImplItem [] "" self_ty generics [fnItem] ofTrait
        [implItem]

    let objectMethodsSet =
        set ["Equals"; "GetHashCode"; "GetType"; "ToString"] //MemberwiseClone, ReferenceEquals

    let transformClassMembers (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        let isCtorOrStaticOrObject (m: Fable.MemberDecl) =
            let memb = com.GetMember(m.MemberRef)
            memb.IsConstructor
            || not (memb.IsOverrideOrExplicitInterfaceImplementation)
            || Set.contains memb.CompiledName objectMethodsSet

        let makeMemberItem ctx (m: Fable.MemberDecl) =
            withCurrentScope ctx m.UsedNames <| fun ctx ->
                let memb = com.GetMember(m.MemberRef)
                let memberItem = transformAssocMemberFunction com ctx memb m.Name m.Args m.Body
                memberItem

        let entRef = decl.Entity
        let ent = com.GetEntity(entRef)
        let entName =
            if ent.IsInterface then decl.Name // for interface object expressions
            else getEntityFullName com ctx entRef
            |> splitLast
        let genArgs = getEntityGenArgs ent
        let self_ty = transformDeclaredType com ctx entRef genArgs

        let ctx = { ctx with ScopedTypeParams =
                                ent.GenericParameters
                                |> List.map (fun p -> p.Name)
                                |> Set.ofList }

        let ctorOrStaticOrObjectImpls =
            let ctorItems =
                if ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsInterface then
                    []
                else
                    let ctorItem =
                        match decl.Constructor with
                        | Some ctor ->
                            withCurrentScope ctx ctor.UsedNames <| fun ctx ->
                                transformPrimaryConstructor com ctx ent ctor
                        | _ ->
                            transformCompilerGeneratedConstructor com ctx ent
                    [ctorItem]
            let ctorOrStaticOrObjectItems =
                decl.AttachedMembers
                |> List.filter isCtorOrStaticOrObject
                |> List.map (makeMemberItem ctx)
                |> List.append ctorItems
                |> List.map mkPublicAssocItem
            if List.isEmpty ctorOrStaticOrObjectItems then
                []
            else
                let generics = genArgs |> List.filter genArgsUnitsFilter |> makeGenerics com ctx
                let implItem =
                    mkImplItem [] "" self_ty generics ctorOrStaticOrObjectItems None
                [implItem]

        let interfaces =
            ent.AllInterfaces
            |> Seq.map (fun i -> i.Entity, i.Entity |> getInterfaceMemberNamesSet com)
            // throw out anything on the declaredInterfaces list such as IComparable etc.
            |> Seq.filter (fun (entRef, _) -> not (isDeclaredInterface entRef.FullName))
            |> Seq.toList

        let allInterfaceMembersSet =
            interfaces
            |> Seq.map (fun (_, members) -> members)
            |> Seq.fold Set.union Set.empty

        let allCtorOrStaticOrObjectMembersSet =
            decl.AttachedMembers
            |> List.filter isCtorOrStaticOrObject
            |> List.map (fun m -> m.Name)
            |> Set.ofList

        let allOtherMembersSet =
            decl.AttachedMembers
            |> List.filter (isCtorOrStaticOrObject >> not)
            |> List.map (fun m -> m.Name)
            |> Set.ofList

        let nonInterfaceMembersSet =
            Set.difference allOtherMembersSet allInterfaceMembersSet

        let displayTraitImpls =
            if Set.contains "ToString" allCtorOrStaticOrObjectMembersSet
            then makeDisplayTraitImpl com ctx self_ty genArgs
            else []

        let nonInterfaceMembersTrait =
            let assocItems =
                decl.AttachedMembers
                |> List.filter (fun m -> Set.contains m.Name nonInterfaceMembersSet)
                |> List.map (fun m ->
                    let memb = com.GetMember(m.MemberRef)
                    makeAssocMemberItem com ctx memb m.Name m.Args
                )
            if List.isEmpty assocItems then
                []
            else
                let traitItem =
                    let generics = genArgs |> makeGenerics com ctx
                    mkTraitItem [] (entName + "Methods") assocItems [] generics
                [traitItem |> mkPublicItem]

        let traitsToRender =
            let complTraits =
                if Set.isEmpty nonInterfaceMembersSet then []
                else [entRef, nonInterfaceMembersSet]
            interfaces @ complTraits

        let memberTraitImpls =
            traitsToRender
            |> List.collect (fun (tEntRef, tMethods) ->
                let tEnt = com.GetEntity(tEntRef)
                let memberItems =
                    decl.AttachedMembers
                    |> List.filter (fun m -> Set.contains m.Name tMethods)
                    |> List.map (makeMemberItem ctx)
                let ty =
                    let genArgTys = genArgs |> transformGenArgs com ctx
                    let bounds = mkTypeTraitGenericBound [entName] genArgTys
                    let ty = mkTraitTy [bounds]
                    ty
                let ty =
                    if tEnt.IsValueType
                    then ty
                    else ty |> makeLrcTy com ctx
                let genArgs = getEntityGenArgs tEnt
                let generics = genArgs |> makeGenerics com ctx
                let implItem =
                    let nameParts =
                        if tEnt.IsInterface
                        then getInterfaceEntityName com ctx tEntRef
                        else entName + "Methods"
                        |> splitNameParts
                    let path =
                        let genArgs = transformGenArgs com ctx genArgs
                        mkGenericPath nameParts genArgs
                    let ofTrait = mkTraitRef path |> Some
                    mkImplItem [] "" ty generics memberItems ofTrait
                [implItem]
            )

        ctorOrStaticOrObjectImpls
        @ displayTraitImpls
        @ nonInterfaceMembersTrait
        @ memberTraitImpls

    let transformClassDecl (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let ent = com.GetEntity(decl.Entity)
        if ent.IsFSharpAbbreviation then
            transformAbbrev com ctx ent
        elif ent.IsInterface then
            if isDeclaredInterface ent.FullName
            then []
            else transformInterface com ctx ent
        else
            let memberDecls = transformClassMembers com ctx decl
            let entityDecls =
                if ent.IsFSharpUnion
                then transformUnion com ctx ent
                else transformClass com ctx ent
            entityDecls @ memberDecls

(*
    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getUnionFieldsAsIdents com ctx ent
        let args =
            [| typedIdent com ctx fieldIds.[0] |> Pattern.Identifier
               typedIdent com ctx fieldIds.[1] |> Pattern.Identifier |> restElement |]
        let body =
            BlockStatement([|
                yield callSuperAsStatement []
                yield! fieldIds |> Array.map (fun id ->
                    let left = get None thisExpr id.Name
                    let right =
                        match id.Type with
                        | Fable.Number _ ->
                            Expression.binaryExpression(BinaryOrBitwise, identAsExpr id, Expression.numericLiteral(0.))
                        | _ -> identAsExpr id
                    assign None left right |> ExpressionStatement)
            |])
        let cases =
            let body =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeArray com ctx
                |>  Statement.returnStatement
                |> Array.singleton
                |> BlockStatement
            ClassMember.classMethod(ClassFunction, Expression.identifier("cases"), [||], body)

        let baseExpr = libValue com ctx "Types" "Union" |> Some
        let classMembers = Array.append [|cases|] classMembers
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getEntityFieldsAsIdents com ent
        let args = fieldIds |> Array.map identAsExpr
        let baseExpr =
            if ent.IsFSharpExceptionDeclaration
            then libValue com ctx "Types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType
            then libValue com ctx "Types" "Record" |> Some
            else None
        let body =
            BlockStatement([|
                if Option.isSome baseExpr then
                    yield callSuperAsStatement []
                yield! ent.FSharpFields |> Seq.mapi (fun i field ->
                    let left = get None thisExpr field.Name
                    let right = wrapIntExpression field.FieldType args.[i]
                    assign None left right |> ExpressionStatement)
                |> Seq.toArray
            |])
        let typedPattern x = typedIdent com ctx x
        let args = fieldIds |> Array.map (typedPattern >> Pattern.Identifier)
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithPrimaryConstructor (com: IRustCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let classEnt = com.GetEntity(classDecl.Entity)
        let classIdent = Expression.identifier(classDecl.Name)
        let consArgs, consBody, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx ClassConstructor cons.Info.HasSpread cons.Args cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.Typescript then
                let genParams = getEntityGenericTypeNames classEnt
                let returnType = getGenericTypeAnnotation com ctx classDecl.Name genParams
                let typeParamDecl = makeTypeParamDecl genParams |> mergeTypeParamDecls typeParamDecl
                returnType, typeParamDecl
            else
                returnType, typeParamDecl

        let exposedCons =
            let argExprs = consArgs |> Array.map (fun p -> Expression.identifier(p.Name))
            let exposedConsBody = Expression.newExpression(classIdent, argExprs)
            makeFunctionExpression None (consArgs, exposedConsBody, returnType, typeParamDecl)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then Some(libValue com ctx "Types" "Record", [])
                else None)
            |> Option.map (fun (baseExpr, baseArgs) ->
                let consBody =
                    consBody.Body
                    |> Array.append [|callSuperAsStatement baseArgs|]
                    |> BlockStatement
                Some baseExpr, consBody)
            |> Option.defaultValue (None, consBody)

        [
            yield! declareType com ctx classEnt classDecl.Name consArgs consBody baseExpr classMembers
            yield declareModuleMember cons.Info.IsPublic cons.Name false exposedCons
        ]
*)

    let transformModuleDecl (com: IRustCompiler) ctx (decl: Fable.ModuleDecl) =
        let ctx = { ctx with ModuleDepth = ctx.ModuleDepth + 1 }
        let memberDecls =
            // Instead of transforming declarations depth-first, i.e.
            // (decl.Members |> List.collect (transformDecl com ctx)),
            // this prioritizes non-module declaration transforms first,
            // so module imports can be properly deduped top to bottom.
            decl.Members
            |> List.map (fun decl ->
                let lazyDecl = lazy (transformDecl com ctx decl)
                match decl with
                | Fable.ModuleDeclaration _ -> () // delay module decl transform
                | _ -> lazyDecl.Force() |> ignore // transform other decls first
                lazyDecl)
            |> List.collect (fun lazyDecl -> lazyDecl.Force())
        if List.isEmpty memberDecls then
            [] // don't output empty modules
        else
            let ent = com.GetEntity(decl.Entity)
            // if ent.IsNamespace then // maybe do something different
            let useDecls =
                let useItem = mkGlobUseItem [] ["super"]
                let importItems = com.GetAllImports(ctx) |> transformImports com ctx
                com.ClearAllImports(ctx)
                useItem :: importItems
            let attrs = transformAttributes com ctx ent.Attributes
            let modDecls = useDecls @ memberDecls
            let modItem = modDecls |> mkModItem attrs decl.Name
            [modItem |> mkPublicItem]

    let transformDecl (com: IRustCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            withCurrentScope ctx (Set.singleton decl.Name) <| fun ctx ->
                transformModuleDecl (com: IRustCompiler) ctx decl

        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                transformModuleAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                let memb = com.GetMember(decl.MemberRef)
                if memb.IsValue
                then transformModuleMember com ctx memb decl
                else transformModuleFunction com ctx memb decl

        | Fable.ClassDeclaration decl ->
            transformClassDecl com ctx decl

    // F# hash function is unstable and gives different results in different runs
    // Taken from fable-library/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
    let stableStringHash (s: string) =
        let mutable h = 5381
        for i = 0 to s.Length - 1 do
            h <- (h * 33) ^^^ (int s.[i])
        h

    let isFableLibrary (com: IRustCompiler) =
        List.contains "FABLE_LIBRARY" com.Options.Define //TODO: look in project defines too

    let isFableLibraryPath (com: IRustCompiler) (path: string) =
        not (isFableLibrary com) && (path.StartsWith(com.LibraryDir) || path = "fable_library_rust")

    let getImportModulePath (com: IRustCompiler) (path: string) =
        let isAbsolutePath =
            path.StartsWith("/") || path.StartsWith("\\") || path.IndexOf(":") = 1
        let modulePath =
            if isAbsolutePath || (isFableLibraryPath com path) then
                Fable.Path.normalizePath path
            else
                let currentDir = Fable.Path.GetDirectoryName(com.CurrentFile)
                Fable.Path.Combine(currentDir, path)
                |> Fable.Path.normalizeFullPath
        modulePath

    let getImportModuleName (com: IRustCompiler) (modulePath: string) =
        System.String.Format("module_{0:x}", stableStringHash modulePath)

    let transformImports (com: IRustCompiler) ctx (imports: Import list): Rust.Item list =
        imports
        |> List.groupBy (fun import -> import.ModulePath)
        |> List.sortBy (fun (modulePath, _) -> modulePath)
        |> List.collect (fun (_modulePath, moduleImports) ->
            moduleImports
            |> List.sortBy (fun import -> import.Selector)
            |> List.map (fun import ->
                let modPath =
                    if import.Path.Length = 0
                    then [] // empty path, means direct import of the selector
                    else
                        if isFableLibraryPath com import.Path
                        then ["fable_library_rust"]
                        else ["crate"; import.ModuleName]
                match import.Selector with
                | "" | "*" | "default" ->
                    mkGlobUseItem [] modPath
                | _ ->
                    let parts = splitNameParts import.Selector
                    let alias =
                        if List.last parts <> import.LocalIdent
                        then Some(import.LocalIdent)
                        else None
                    mkSimpleUseItem [] (modPath @ parts) alias
            )
        )

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        match selector with
        | "" | "*" | "default" -> Fable.Path.GetFileNameWithoutExtension(path)
        | _ -> splitNameParts selector |> List.last
        |> getUniqueNameInRootScope ctx


module Compiler =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open Util

    // global list of import modules (across files)
    let importModules = ConcurrentDictionary<string, string>()

    // per file
    type RustCompiler (com: Fable.Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IRustCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member self.GetImportName(ctx, selector, path, r) =
                if selector = Fable.Naming.placeholder then
                    "`importMember` must be assigned to a variable"
                    |> addError com [] r
                let path =
                    if path.EndsWith(".fs") then
                        let fileExt = (self :> Compiler).Options.FileExtension
                        Fable.Path.ChangeExtension(path, fileExt)
                    else path
                let cacheKey =
                    if (isFableLibraryPath self path)
                    then "fable_library_rust::" + selector
                    elif path.Length = 0 then selector
                    else path + "::" + selector
                let import =
                    match imports.TryGetValue(cacheKey) with
                    | true, import ->
                        if not (import.Depths |> List.contains ctx.ModuleDepth) then
                            import.Depths <- ctx.ModuleDepth :: import.Depths
                        import
                    | false, _ ->
                        let localIdent = getIdentForImport ctx path selector
                        let modulePath = getImportModulePath self path
                        let moduleName = getImportModuleName self modulePath
                        let import = {
                            Selector = selector
                            LocalIdent = localIdent
                            ModuleName = moduleName
                            ModulePath = modulePath
                            Path = path
                            Depths = [ctx.ModuleDepth]
                        }
                        // add import module to a global list (across files)
                        if path.Length > 0 && not (isFableLibraryPath self path) then
                            importModules.TryAdd(modulePath, moduleName) |> ignore

                        imports.Add(cacheKey, import)
                        import
                $"{import.LocalIdent}"

            member _.GetAllImports(ctx) =
                imports.Values
                |> Seq.filter (fun import ->
                    // return only imports at the current module depth level
                    import.Depths |> List.forall (fun d -> d = ctx.ModuleDepth))
                |> Seq.toList

            member _.ClearAllImports(ctx) =
                for import in imports do
                    import.Value.Depths <-
                        // remove all import depths at this module level or deeper
                        import.Value.Depths |> List.filter (fun d -> d < ctx.ModuleDepth)
                    if import.Value.Depths.Length = 0 then
                        imports.Remove(import.Key) |> ignore
                        ctx.UsedNames.RootScope.Remove(import.Value.LocalIdent) |> ignore

            member _.GetAllModules() =
                importModules |> Seq.map (fun p -> p.Key, p.Value) |> Seq.toList

            member com.TransformExpr(ctx, e) = transformExpr com ctx e

            member _.GetEntity(fullName) =
                match com.TryGetEntity(fullName) with
                | Some ent -> ent
                | None -> failwith $"Missing entity {fullName}"

        interface Fable.Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.SourceFiles = com.SourceFiles
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = RustCompiler(com)

    let transformFile (com: Fable.Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IRustCompiler
        let declScopes =
            let hs = HashSet()
            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)
            hs

        let ctx = {
            File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = HashSet [] }
            DecisionTargets = []
            // HoistVars = fun _ -> false
            // OptimizeTailCall = fun () -> ()
            TailCallOpportunity = None
            ScopedTypeParams = Set.empty
            ScopedSymbols = Map.empty

            IsInPluralizedExpr = false
            IsParamAnyType = false
            IsParamByRefPreferred = false
            RequiresSendSync = false
            ModuleDepth = 0
        }

        let topAttrs = [
            // adds "no_std" for fable library crate if feature is enabled
            if isFableLibrary com && isLastFileInProject com then
                mkInnerAttr "cfg_attr" ["feature = \"no_std\""; "no_std"]

            // TODO: make some of those conditional on compiler options
            mkInnerAttr "allow" ["dead_code"]
            mkInnerAttr "allow" ["non_snake_case"]
            mkInnerAttr "allow" ["non_camel_case_types"]
            mkInnerAttr "allow" ["non_upper_case_globals"]
            mkInnerAttr "allow" ["unused_parens"]
            mkInnerAttr "allow" ["unused_imports"]
            mkInnerAttr "allow" ["unused_variables"]
            mkInnerAttr "allow" ["unused_attributes"]

            // these require nightly
            // mkInnerAttr "feature" ["once_cell"]
            // mkInnerAttr "feature" ["stmt_expr_attributes"]
            // mkInnerAttr "feature" ["destructuring_assignment"]
        ]

        let entryPointItems = getEntryPointItems com ctx file.Declarations
        let importItems = com.GetAllImports(ctx) |> transformImports com ctx
        let declItems = List.collect (transformDecl com ctx) file.Declarations
        let moduleItems = getModuleItems com ctx // global module imports
        let crateItems = importItems @ declItems @ moduleItems @ entryPointItems

        let crate = mkCrate topAttrs crateItems
        crate
