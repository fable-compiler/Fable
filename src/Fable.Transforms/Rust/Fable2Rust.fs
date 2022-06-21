module rec Fable.Transforms.Rust.Fable2Rust

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.Rust
open Fable.Transforms.Rust.AST.Helpers

module Rust = Fable.Transforms.Rust.AST.Types

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type Import =
  { Selector: string
    LocalIdent: string
    ModuleName: string
    FullPath: string
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: Fable.Ident list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type TypegenContext = {
    IsParamType: bool
    IsParamByRefPreferred: bool
    IsRawType: bool // prevents rc-wrap
}

type ScopedVarAttrs = {
    IsArm: bool
    IsRef: bool
    IsBox: bool
    HasMultipleUses: bool
}

type Context =
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string>
    ScopedSymbols: FSharp.Collections.Map<string, ScopedVarAttrs>
    IsInPluralizedExpr: bool //this could be a closure in a map, or or a for loop. The point is anything leaving the scope cannot be assumed to be the only reference
    IsCallingFunction: bool
    Typegen: TypegenContext }

type IRustCompiler =
    inherit Fable.Compiler
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit
    abstract GetAllImports: unit -> Import list
    abstract GetAllModules: unit -> (string * string) list
    abstract GetImportName: Context * selector: string * path: string * SourceLocation option -> string
    abstract TransformAsExpr: Context * Fable.Expr -> Rust.Expr
    // abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Rust.Stmt array
    // abstract TransformImport: Context * selector:string * path:string -> Rust.Expr
    // abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement
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

    let hasMultipleUses (name: string) =
        Map.tryFind name >> Option.map (fun x -> x > 1) >> Option.defaultValue true
        //fun _ -> true

module TypeInfo =

    let cleanNameAsRustIdentifier (name: string) =
        FSharp2Fable.Helpers.cleanNameAsRustIdentifier name

    let splitNameSpace (fullName: string) =
        let i = fullName.LastIndexOf('.')
        if i < 0 then "", fullName
        else fullName.Substring(0, i), fullName.Substring(i + 1)

    let splitLast (fullName: string) =
        let i = fullName.LastIndexOf('.')
        if i < 0 then fullName
        else fullName.Substring(i + 1)

    let makeFullNamePath fullName genArgs =
        let parts = splitFullName fullName
        mkGenericPath parts genArgs

    let makeFullNamePathExpr fullName genArgs =
        makeFullNamePath fullName genArgs
        |> mkPathExpr

    let makeFullNamePathTy fullName genArgs =
        makeFullNamePath fullName genArgs
        |> mkPathTy

    let primitiveType (name: string): Rust.Ty =
        mkGenericPathTy [name] None

    let getLibraryImportName (com: IRustCompiler) ctx moduleName typeName =
        let selector = moduleName + "_::" + typeName
        let libPath = getLibPath com moduleName
        com.GetImportName(ctx, selector, libPath, None)

    let makeImportType com ctx moduleName typeName tys: Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        tys |> mkGenericTy (splitFullName importName)

    // TODO: emit Rc or Arc depending on threading.
    // Could also support Gc<T> in the future - https://github.com/Manishearth/rust-gc
    let makeRcTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Rc"

    // TODO: emit Lazy or SyncLazy depending on threading.
    let makeLazyTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "Lazy"

    // TODO: emit MutCell or AtomicCell depending on threading.
    let makeMutTy com ctx (ty: Rust.Ty): Rust.Ty =
        [ty] |> makeImportType com ctx "Native" "MutCell"

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

    let isEntityOfType (com: IRustCompiler) isTypeOf entNames (ent: Fable.Entity) =
        if Set.contains ent.FullName entNames then
            true // already checked, avoids circular checks
        else
            let entNames = Set.add ent.FullName entNames
            if ent.IsFSharpUnion then
                ent.UnionCases |> Seq.forall (fun uci ->
                    uci.UnionCaseFields |> List.forall (fun fi ->
                        isTypeOf com entNames fi.FieldType))
            else
                ent.FSharpFields |> Seq.forall (fun fi ->
                    isTypeOf com entNames fi.FieldType)

    let isTypeOfType (com: IRustCompiler) isTypeOf isEntityOf entNames typ =
        match typ with
        | Fable.Option(genArg, _) -> isTypeOf com entNames genArg
        | Fable.Array(genArg, _) -> isTypeOf com entNames genArg
        | Fable.List genArg -> isTypeOf com entNames genArg
        | Fable.Tuple(genArgs, _) ->
            List.forall (isTypeOf com entNames) genArgs
        | Fable.AnonymousRecordType(_, genArgs) ->
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
        | _ ->
            isTypeOfType com isCopyableType isCopyableEntity entNames typ

    let isCopyableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && ent.IsValueType
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
    let shouldBeRefCountWrapped (com: IRustCompiler) typ =
        match typ with
        // passed by reference, no need to Rc-wrap
        | Replacements.Util.IsInRefType com _
        // always not Rc-wrapped
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any
        | Fable.Unit
        | Fable.Boolean
        | Fable.Char
        | Fable.Number _
        | Fable.GenericParam _
        // already RC-wrapped
        | Fable.LambdaType _
        | Fable.DelegateType _
        // containers, no need to Rc-wrap
        | Fable.List _
        | Fable.Option _
        | Replacements.Util.Builtin (Replacements.Util.FSharpResult _)
        | Replacements.Util.Builtin (Replacements.Util.FSharpSet _)
        | Replacements.Util.Builtin (Replacements.Util.FSharpMap _)
        | Fable.Tuple _
        | Fable.AnonymousRecordType _
            -> false

        // always Rc-wrapped
        | Fable.String
        | Fable.Regex
        | Fable.Array _
        | Replacements.Util.IsEntity (Types.keyCollection) _
        | Replacements.Util.IsEntity (Types.valueCollection) _
        | Replacements.Util.IsEnumerator _
        | Replacements.Util.Builtin (Replacements.Util.FSharpReference _)
            -> true

        // conditionally Rc-wrapped
        | Replacements.Util.Builtin (Replacements.Util.FSharpChoice _) ->
            not (isCopyableType com Set.empty typ)
        | Fable.DeclaredType (entRef, _) ->
            match com.GetEntity(entRef) with
            | HasEmitAttribute _ -> false // do not make custom types Rc-wrapped by default. This prevents inconsistency between type and implementation emit
            | _ ->
                not (isCopyableType com Set.empty typ)

    let isCloneableType (com: IRustCompiler) typ =
        match typ with
        | Fable.String
        | Fable.GenericParam _
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.Option _
        | Fable.List _
            -> true

        | Fable.DeclaredType(entRef, _) -> true
        | Fable.AnonymousRecordType _ -> true

        | _ -> false

    let rec isCloneableExpr (com: IRustCompiler) typ e =
        match e with
        | Fable.Get _
        | Fable.IdentExpr _
            -> true
        // | Fable.TypeCast(e, t) -> isCloneableExpr com t e
        | Fable.Operation (Fable.Unary(UnaryOperator.UnaryAddressOf, e), _, _) ->
            isCloneableExpr com typ e
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
        let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
        if info.Selector = "*"
        then mkUnitExpr ()
        else makeFullNamePathExpr importName genArgs

    let makeNativeCall com ctx genArgs moduleName memberName (args: Rust.Expr list) =
        let importName = getLibraryImportName com ctx moduleName memberName
        let callee = makeFullNamePathExpr importName genArgs
        mkCallExpr callee args

    let makeLibCall com ctx genArgs moduleName memberName passByRef (args: Rust.Expr list) =
        let args = if passByRef then args |> List.map mkAddrOfExpr else args
        makeNativeCall com ctx genArgs moduleName memberName args

    let libCall com ctx r types moduleName memberName (args: Fable.Expr list) =
        let path = getLibPath com moduleName
        let selector = moduleName + "::" + memberName
        let info: Fable.ImportInfo =
            { Selector = selector; Path = path; Kind = Fable.LibraryImport }
        let genArgs = transformGenArgs com ctx types
        let callee = transformImport com ctx r Fable.Any info genArgs
        Util.callFunction com ctx r callee args

    let transformGenArgs com ctx genArgs: Rust.GenericArgs option =
        genArgs
        |> List.map (transformType com ctx)
        |> mkGenericTypeArgs

    let transformGenericType com ctx genArgs typeName: Rust.Ty =
        genArgs
        |> List.map (transformType com ctx)
        |> mkGenericTy (splitFullName typeName)

    let transformImportType com ctx genArgs moduleName typeName: Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        transformGenericType com ctx genArgs importName

    let transformArrayType com ctx genArg: Rust.Ty =
        let ty = transformType com ctx genArg
        [ty]
        |> mkGenericTy [rawIdent "Vec"]
        |> makeMutTy com ctx
        // transformImportType com ctx [genArg] "Native" "Array`1"

    let transformListType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Native" "List`1"

    let transformSetType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Native" "Set`1"

    let transformMapType com ctx genArgs: Rust.Ty =
        transformImportType com ctx genArgs "Native" "Map`2"

    let transformHashSetType com ctx genArg: Rust.Ty =
        transformImportType com ctx [genArg] "Native" "HashSet"
        |> makeMutTy com ctx
        // transformImportType com ctx [genArg] "Native" "HashSet`1"

    let transformHashMapType com ctx genArgs: Rust.Ty =
        transformImportType com ctx genArgs "Native" "HashMap"
        |> makeMutTy com ctx
        // transformImportType com ctx genArgs "Native" "HashMap`2"

    let transformTupleType com ctx genArgs: Rust.Ty =
        genArgs
        |> List.map (transformType com ctx)
        |> mkTupleTy

    let transformOptionType com ctx genArg: Rust.Ty =
        transformGenericType com ctx [genArg] (rawIdent "Option")

    let transformParamType com ctx typ: Rust.Ty =
        let ty = transformType com ctx typ
        if isByRefType com typ || ctx.Typegen.IsParamByRefPreferred
        then ty |> mkRefTy
        else ty

    // let transformLambdaType com ctx argTypes returnType: Rust.Ty =
    //     let fnRetTy =
    //         if returnType = Fable.Unit then VOID_RETURN_TY
    //         else transformType com ctx returnType |> mkFnRetTy
    //     let pat = mkIdentPat "a" false false
    //     let inputs = argTypes |> List.map (fun tInput ->
    //         mkParam [] (transformParamType com ctx tInput) pat false)
    //     let fnDecl = mkFnDecl inputs fnRetTy
    //     let genParams = [] // TODO:
    //     mkFnTy genParams fnDecl

    let transformClosureType com ctx argTypes returnType: Rust.Ty =
        let inputs =
            match argTypes with
            | [Fable.Unit] -> []
            | _ ->
                argTypes
                |> List.map (transformParamType com ctx)
        let output =
            if returnType = Fable.Unit then VOID_RETURN_TY
            else
                let ctx = { ctx with Typegen = { ctx.Typegen with IsParamType = false } }
                returnType |> transformType com ctx |> mkParenTy |> mkFnRetTy
        let bounds = [
            mkFnTraitGenericBound inputs output
            mkLifetimeGenericBound "'static"
        ]
        if ctx.Typegen.IsParamType
        then mkImplTraitTy bounds
        else mkDynTraitTy bounds
        |> makeRcTy com ctx

    let numberType kind: Rust.Ty =
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
        | Decimal -> makeFullNamePathTy Types.decimal None
        | BigInt -> makeFullNamePathTy Types.bigint None

    let getInterfaceEntityName (com: IRustCompiler) ctx (entRef: Fable.EntityRef) =
        match entRef.FullName with
        | Types.icollection
        | Types.icollectionGeneric
            -> getLibraryImportName com ctx "Interfaces" "ICollection`1"
        | Types.ienumerable
        | Types.ienumerableGeneric
            -> getLibraryImportName com ctx "Interfaces" "IEnumerable`1"
        | Types.ienumerator
        | Types.ienumeratorGeneric
            -> getLibraryImportName com ctx "Interfaces" "IEnumerator`1"
        | Types.idictionary
        | Types.ireadonlydictionary
            -> getLibraryImportName com ctx "Interfaces" "IDictionary`2"
        | _ ->
            match entRef.SourcePath with
            | Some path when path <> com.CurrentFile ->
                // interface is imported from another file
                let importPath = Fable.Path.getRelativeFileOrDirPath false com.CurrentFile false path
                let importName = com.GetImportName(ctx, entRef.FullName, importPath, None)
                importName
            | _ ->
                entRef.FullName

    let tryFindInterface (com: IRustCompiler) fullName (entRef: Fable.EntityRef): Fable.DeclaredType option =
        let ent = com.GetEntity(entRef)
        ent.AllInterfaces |> Seq.tryFind (fun ifc -> ifc.Entity.FullName = fullName)

    let transformInterfaceType (com: IRustCompiler) ctx (entRef: Fable.EntityRef) genArgs: Rust.Ty =
        let nameParts = getInterfaceEntityName com ctx entRef |> splitFullName
        let genArgs = transformGenArgs com ctx genArgs
        let bounds = [mkTypeTraitGenericBound nameParts genArgs]
        mkDynTraitTy bounds

    let (|HasEmitAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes |> Seq.tryPick (fun att ->
            if att.Entity.FullName.StartsWith(Atts.emit) then
                match att.ConstructorArgs with
                | [:? string as macro] -> Some macro
                | _ -> None
            else None)

    let transformDeclaredType (com: IRustCompiler) ctx entRef genArgs: Rust.Ty =
        match com.GetEntity(entRef) with
        | HasEmitAttribute value ->
            let genArgs = genArgs |> List.map (transformType com ctx)
            mkEmitTy value genArgs
        | ent when ent.IsInterface ->
            transformInterfaceType com ctx entRef genArgs
        | ent ->
            let genArgs = transformGenArgs com ctx genArgs
            makeFullNamePathTy ent.FullName genArgs

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
        | _ -> false

    let isInRefType (com: IRustCompiler) = function
        | Replacements.Util.IsInRefType com _ -> true
        | _ -> false

    let isInterface (com: IRustCompiler) = function
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)
            ent.IsInterface
        | _ -> false

    let inferredType = Fable.GenericParam(rawIdent "_", false, [])

    let inferIfAny t = match t with | Fable.Any -> inferredType | _ -> t

    // let transformTypeInfo (com: IRustCompiler) ctx r (genMap: Map<string, Rust.Expr>) (t: Fable.Type): Rust.Ty =
    // TODO: use a genMap?
    let transformType (com: IRustCompiler) ctx (typ: Fable.Type): Rust.Ty =
        // let nonGenericTypeInfo fullname =
        //     [| Expression.stringLiteral(fullname) |]
        //     |> libReflectionCall com ctx None "class"
        // let resolveGenerics generics: Rust.Expr[] =
        //     generics |> Array.map (transformTypeInfo com ctx r genMap)
        // let genericTypeInfo name genArgs =
        //     let resolved = resolveGenerics genArgs
        //     libReflectionCall com ctx None name resolved
        // let genericEntity (fullname: string) generics =
        //     libReflectionCall com ctx None "class" [|
        //         Expression.stringLiteral(fullname)
        //         if not(Array.isEmpty generics) then
        //             Expression.arrayExpression(generics)
        //     |]
        let ty =
            match typ with
            | Fable.Measure _ -> mkInferTy ()
            | Fable.Any -> mkInferTy ()
            | Fable.GenericParam(name=name) ->
                mkGenericPathTy [name] None
                // match Map.tryFind name genMap with
                // | Some t -> t
                // | None ->
                //     Replacements.Util.genericTypeInfoError name |> addError com [] r
                //     Expression.nullLiteral()
            | Fable.Unit    -> mkUnitTy ()
            | Fable.Boolean -> primitiveType "bool"
            | Fable.Char    -> primitiveType "char"
            | Fable.String  -> primitiveType "str"
            | Fable.Number(kind, _) -> numberType kind
            | Fable.LambdaType(argType, returnType) ->
                let argTypes, returnType = FableTransforms.uncurryLambdaType [argType] returnType
                transformClosureType com ctx argTypes returnType
                // if true //ctx.Typegen.FavourClosureTraitOverFunctionPointer
                // then transformClosureType com ctx argTypes returnType
                // else transformLambdaType com ctx argTypes returnType
            | Fable.DelegateType(argTypes, returnType) ->
                transformClosureType com ctx argTypes returnType
            | Fable.Tuple(genArgs, _) -> transformTupleType com ctx genArgs
            | Fable.Option(genArg, _) -> transformOptionType com ctx genArg
            | Fable.Array(genArg, _) -> transformArrayType com ctx genArg
            | Fable.List genArg -> transformListType com ctx genArg
            | Fable.Regex ->
                // nonGenericTypeInfo Types.regex
                TODO_TYPE $"%A{typ}" //TODO:
            | Fable.MetaType ->
                // nonGenericTypeInfo Types.type_
                TODO_TYPE $"%A{typ}" //TODO:
            | Fable.AnonymousRecordType(fieldNames, genArgs) ->
                transformTupleType com ctx genArgs //TODO: temporary - uses tuples for now!
                // let genArgs = resolveGenerics (List.toArray genArgs)
                // Array.zip fieldNames genArgs
                // |> Array.map (fun (k, t) -> Expression.arrayExpression[|Expression.stringLiteral(k); t|])
                // |> libReflectionCall com ctx None "anonRecord"

            // pre-defined declared types
            | Replacements.Util.IsEntity (Types.iset) (entRef, [genArg]) -> transformHashSetType com ctx genArg
            | Replacements.Util.IsEntity (Types.idictionary) (entRef, [k; v]) -> transformHashMapType com ctx [k; v]
            | Replacements.Util.IsEntity (Types.ireadonlydictionary) (entRef, [k; v]) -> transformHashMapType com ctx [k; v]
            | Replacements.Util.IsEntity (Types.keyCollection) (entRef, [k; v]) -> transformArrayType com ctx k
            | Replacements.Util.IsEntity (Types.valueCollection) (entRef, [k; v]) -> transformArrayType com ctx v
            | Replacements.Util.IsEntity (Types.icollectionGeneric) (entRef, [t]) -> transformArrayType com ctx (inferIfAny t)
            | Replacements.Util.IsEnumerator (entRef, genArgs) ->
                // get IEnumerator interface from enumerator object
                match tryFindInterface com Types.ienumeratorGeneric entRef with
                | Some ifc -> transformInterfaceType com ctx ifc.Entity [inferredType]
                | _ -> failwith "Cannot find IEnumerator interface, should not happen."

            // other declared types
            | Fable.DeclaredType(entRef, genArgs) ->
                match entRef.FullName, genArgs with
                | Replacements.Util.BuiltinEntity kind ->
                    match kind with
                    | Replacements.Util.BclGuid
                    | Replacements.Util.BclTimeSpan
                    | Replacements.Util.BclDateTime
                    | Replacements.Util.BclDateTimeOffset
                    | Replacements.Util.BclDateOnly
                    | Replacements.Util.BclTimeOnly
                    | Replacements.Util.BclTimer
                        -> transformDeclaredType com ctx entRef genArgs

                    | Replacements.Util.BclHashSet(genArg) -> transformHashSetType com ctx genArg
                    | Replacements.Util.BclDictionary(k, v) -> transformHashMapType com ctx [k; v]
                    | Replacements.Util.FSharpSet(genArg) -> transformSetType com ctx genArg
                    | Replacements.Util.FSharpMap(k, v) -> transformMapType com ctx [k; v]
                    | Replacements.Util.BclKeyValuePair(k, v) -> transformTupleType com ctx [k; v]
                    | Replacements.Util.FSharpResult(ok, err) -> transformResultType com ctx [ok; err]
                    | Replacements.Util.FSharpChoice genArgs -> transformChoiceType com ctx genArgs
                    | Replacements.Util.FSharpReference(genArg) ->
                        match typ with
                        | Replacements.Util.IsInRefType com genArg -> transformType com ctx genArg
                        | _ -> transformRefCellType com ctx genArg
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
                    //     let callee = com.TransformAsExpr(ctx, reflectionMethodExpr)
                    //     Expression.callExpression(callee, generics)

        if shouldBeRefCountWrapped com typ && not ctx.Typegen.IsRawType
        then makeRcTy com ctx ty
        else ty

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
    let transformTypeTest (com: IRustCompiler) ctx range expr (typ: Fable.Type): Rust.Expr =
        // no runtime type tests in Rust, unfortunately
        let testOpt =
            match expr with
            | Fable.TypeCast(e, Fable.Any) ->
                match typ, e.Type with
                | Fable.DeclaredType(entRef, _), Fable.DeclaredType(entRef2, _) ->
                    // TODO: somehow test if entRef2 implements or inherits entRef
                    // for now the test is just an exact match
                    let sameEnt = (entRef.FullName = entRef2.FullName)
                    Some sameEnt
                | _ -> None
            | _ -> None

        match testOpt with
        | Some b -> mkBoolLitExpr b
        | _ ->
            addWarning com [] range "Cannot type test (evals to false)"
            mkBoolLitExpr false

module Util =

    // open Lib
    // open Reflection
    open UsageTracking
    open TypeInfo

    let (|TransformExpr|) (com: IRustCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, info) -> Some([arg], body, info)
        | Fable.Delegate(args, body, info, Fable.Tag.empty) -> Some(args, body, info)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
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
        if ident.IsThisArgument
        then mkGenericPathExpr [rawIdent "self"] None
        else mkGenericPathExpr (splitFullName ident.Name) None

    let transformExprMaybeIdentExpr (com: IRustCompiler) ctx (expr: Fable.Expr) =
        match expr with
        | Fable.IdentExpr id -> //when id.IsThisArgument ->
            // avoids the extra Rc wrapping for self that transformIdentGet does
            transformIdent com ctx None id
        | _ -> com.TransformAsExpr(ctx, expr)

    let transformIdentGet com ctx r (ident: Fable.Ident) =
        let expr = transformIdent com ctx r ident
        if ident.IsThisArgument then
            expr |> makeClone |> makeRcValue
        elif ident.IsMutable && not (isInRefType com ident.Type) then
            expr |> mutableGet
        else
            if isBoxScoped ctx ident.Name
            then expr |> makeRcValue
            elif isRefScoped ctx ident.Name
            then expr |> makeClone // |> mkDerefExpr |> mkParenExpr
            else expr

    let transformIdentSet com ctx r (ident: Fable.Ident) (value: Rust.Expr) =
        let expr = transformIdent com ctx r ident
        // assert(ident.IsMutable)
        mutableSet expr value

(*
    let thisExpr =
        Expression.thisExpression()

    let ofInt i =
        mkIntLitExpr (uint64 (abs i))
        // Expression.numericLiteral(float i)

    let ofString s =
        mkStrLitExpr s
        // Expression.stringLiteral(s)
*)
    let memberFromName (memberName: string): Rust.Expr * bool =
        match memberName with
        | "ToString" -> (mkGenericPathExpr ["ToString"] None), false
        // | n when n.StartsWith("Symbol.") ->
        //     Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n.[7..]), false), true
        // | n when Naming.hasIdentForbiddenChars n -> Expression.stringLiteral(n), true
        | n -> (mkGenericPathExpr [n] None), false
(*
    let memberFromExpr (com: IRustCompiler) ctx memberExpr: Rust.Expr * bool =
        match memberExpr with
        | Fable.Value(Fable.StringConstant name, _) -> memberFromName name
        | e -> com.TransformAsExpr(ctx, e), true
*)
    let getField r (expr: Rust.Expr) (fieldName: string) =
        mkFieldExpr expr fieldName // ?loc=r)

    let getExpr r (expr: Rust.Expr) (index: Rust.Expr) =
        mkIndexExpr expr index // ?loc=r)
(*
    let rec getParts (parts: string list) (expr: Rust.Expr) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    let makeTypedArray (com: IRustCompiler) ctx t (args: Fable.Expr list) =
        match t with
        | Fable.Number kind when com.Options.TypedArrays ->
            let jsName = getTypedArrayName com kind
            let args = [|makeArray com ctx args|]
            Expression.newExpression(Expression.identifier(jsName), args)
        | _ -> makeArray com ctx args

    let makeTypedAllocatedFrom (com: IRustCompiler) ctx typ (fableExpr: Fable.Expr) =
        let getArrayCons t =
            match t with
            | Fable.Number kind when com.Options.TypedArrays ->
                getTypedArrayName com kind |> Expression.identifier
            | _ -> Expression.identifier("Array")

        match fableExpr with
        | ExprType(Fable.Number _) ->
            let cons = getArrayCons typ
            let expr = com.TransformAsExpr(ctx, fableExpr)
            Expression.newExpression(cons, [|expr|])
        | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
            makeTypedArray com ctx typ exprs
        | _ ->
            let cons = getArrayCons typ
            let expr = com.TransformAsExpr(ctx, fableExpr)
            Expression.callExpression(get None cons "from", [|expr|])

    let makeStringArray strings =
        strings
        |> List.mapToArray (fun x -> Expression.stringLiteral(x))
        |> Expression.arrayExpression

    let makeJsObject pairs =
        pairs |> Seq.map (fun (name, value) ->
            let prop, computed = memberFromName name
            ObjectMember.objectProperty(prop, value, computed_=computed))
        |> Seq.toArray
        |> Expression.objectExpression

    let assign range left right =
        Expression.assignmentExpression(AssignEqual, left, right, ?loc=range)

    let multiVarDeclaration kind (variables: (Identifier * Expression option) list) =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> List.distinctBy (fun (Identifier(name=name), _value) -> name)
            |> List.mapToArray (fun (id, value) ->
                VariableDeclarator(id |> Pattern.Identifier, value))
        Statement.variableDeclaration(kind, varDeclarators)

    let varDeclaration (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration.variableDeclaration(var, value, kind)

    let restElement (var: Pattern) =
        Pattern.restElement(var)

    let callSuper (args: Rust.Expr list) =
        Expression.callExpression(Super(None), List.toArray args)

    let callSuperAsStatement (args: Rust.Expr list) =
        ExpressionStatement(callSuper args)

    let makeClassConstructor args body =
        ClassMember.classMethod(ClassImplicitConstructor, Expression.identifier("constructor"), args, body)
*)
    let callFunction com ctx range (callee: Rust.Expr) (args: Fable.Expr list) =
        let trArgs = transformCallArgs com ctx args []
        mkCallExpr callee trArgs //?loc=range)

    /// Immediately Invoked Function Expression
    let iife (com: IRustCompiler) ctx (expr: Fable.Expr) =
        let fnExpr = transformLambda com ctx None [] expr
        let range = None // TODO:
        callFunction com ctx range fnExpr []
        // let _, body = com.TransformFunction(ctx, None, [], expr)
        // // Use an arrow function in case we need to capture 'this'
        // Expression.callExpression(Expression.arrowFunctionExpression([||], body), [||])

    let getGenericParams (ctx: Context) (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam _ as p -> [p]
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
        let expr = com.TransformAsExpr(ctx, fableExpr)
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

        // casts to IEnumerable
        | Replacements.Util.IsEntity (Types.keyCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.valueCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.icollectionGeneric) _, IEnumerable _
        | Fable.Array _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofArray" false [expr]
        | Fable.List _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofList" false [expr]
        | Replacements.Util.IsEntity (Types.hashset) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.iset) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "Native" "hashSetEntries" false [expr]
            makeLibCall com ctx None "Seq" "ofArray" false [ar]
        | Replacements.Util.IsEntity (Types.dictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.idictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.ireadonlydictionary) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "Native" "hashMapEntries" false [expr]
            makeLibCall com ctx None "Seq" "ofArray" false [ar]

        // casts to generic param
        | _, Fable.GenericParam(name, _isMeasure, _constraints) ->
            makeCall [name; "from"] None [expr] // e.g. T::from(value)
        // casts to interface
        | _, t when isInterface com t ->
            expr |> makeClone |> mkCastExpr ty
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
        | Some e, _ -> com.TransformAsExpr(ctx, e)
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | None, Fable.DeclaredType(ent,[_]) ->
            match ent.FullName, e with
            | Types.ienumerableGeneric, Replacements.Util.ArrayOrListLiteral(exprs, _) ->
                makeArray com ctx exprs
            | _ -> com.TransformAsExpr(ctx, e)
        | _ -> com.TransformAsExpr(ctx, e)
*)
    let transformCurry (com: IRustCompiler) (ctx: Context) expr arity: Rust.Expr =
        com.TransformAsExpr(ctx, Replacements.Api.curryExprAtRuntime com arity expr)

    /// This guarantees a new owned Rc<T>
    let makeClone expr = mkMethodCallExprOnce "clone" None expr []

    /// Calling this on an rc guarantees a &T, regardless of if the Rc is a ref or not
    let makeAsRef expr = mkMethodCallExpr "as_ref" None expr []

    let makeCall pathNames genArgs (args: Rust.Expr list) =
        let callee = mkGenericPathExpr pathNames genArgs
        mkCallExpr callee args

    let makeRcValue (value: Rust.Expr) =
        makeCall ["Rc";"from"] None [value]

    let makeMutValue (value: Rust.Expr) =
        makeCall ["MutCell";"from"] None [value]

    let makeLazyValue (value: Rust.Expr) =
        makeCall ["Lazy";"new"] None [value]

    let transformCallArgs (com: IRustCompiler) ctx (args: Fable.Expr list) (argTypes: Fable.Type list) =
        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant, _))] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
        | args ->
            let argsWithTypes =
                if argTypes.Length = args.Length
                then args |> List.zip argTypes |> List.map(fun (t, a) -> Some t, a)
                else args |> List.map (fun a -> None, a)
            argsWithTypes |> List.map (fun (argType, arg) ->
                transformLeaveContext com ctx argType arg)

    let transformExprMaybeUnwrapRef (com: IRustCompiler) ctx fableExpr =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        expr

    let prepareRefForPatternMatch (com: IRustCompiler) ctx typ name fableExpr =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        if shouldBeRefCountWrapped com typ
        then makeAsRef expr
        else
            if isRefScoped ctx name
            then expr
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
            Replacements.makeDecimal com r t x |> transformAsExpr com ctx
        | kind, x ->
            $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
            |> addError com [] r
            mkFloat64LitExpr 0.

    let makeString com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "string" false [value]

    let makeDefaultOf com ctx (typ: Fable.Type) =
        let genArgs = transformGenArgs com ctx [typ]
        makeLibCall com ctx genArgs "Native" "defaultOf" true []

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
        // else expr |> makeRcValue
        expr // all options are value options

    let makeArray (com: IRustCompiler) ctx r typ (exprs: Fable.Expr list) =
        match exprs with
        | [] ->
            let genArgs = transformGenArgs com ctx [typ]
            makeLibCall com ctx genArgs "Native" "arrayEmpty" true []
        | _ ->
            let arrayExpr =
                exprs
                |> List.map (transformExprMaybeUnwrapRef com ctx)
                |> mkArrayExpr
            makeLibCall com ctx None "Native" "arrayFrom" true [arrayExpr]

    let makeArrayFrom (com: IRustCompiler) ctx r typ fableExpr =
        match fableExpr with
        | Fable.Value(Fable.NewTuple([valueExpr; sizeExpr], isStruct), _) ->
            let size = transformExprMaybeUnwrapRef com ctx sizeExpr
            let value = transformExprMaybeUnwrapRef com ctx valueExpr
            makeLibCall com ctx None "Native" "arrayCreate" true [size; value]
        | expr ->
            // this assumes expr converts to a slice
            // TODO: this may not always work, make it work
            let sequence = transformExprMaybeUnwrapRef com ctx expr
            makeLibCall com ctx None "Native" "arrayFrom" true [sequence]

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

    let makeTuple (com: IRustCompiler) ctx r (exprs: (Fable.Expr) list) isStruct =
        let expr =
            exprs
            |> List.map (transformLeaveContext com ctx None)
            |> mkTupleExpr
        // if isStruct
        // then expr
        // else expr |> makeRcValue
        expr // all tuples are value tuples

    let makeRecord (com: IRustCompiler) ctx r values entRef genArgs =
        let ent = com.GetEntity(entRef)
        let fields =
            Seq.zip ent.FSharpFields values
            |> Seq.map (fun (fi, value) ->
                let attrs = []
                let expr =
                    let expr = transformLeaveContext com ctx None value
                    if fi.IsMutable
                    then expr |> makeMutValue
                    else expr
                mkExprField attrs fi.Name expr false false)
        let genArgs = transformGenArgs com ctx genArgs
        let path = makeFullNamePath ent.FullName genArgs
        let expr = mkStructExpr path fields // TODO: range
        if isCopyableEntity com Set.empty ent
        then expr
        else expr |> makeRcValue

    let mapKnownUnionCaseNames fullName =
        match fullName with
        | "FSharp.Core.FSharpResult`2.Ok" -> rawIdent "Ok"
        | "FSharp.Core.FSharpResult`2.Error" -> rawIdent "Err"
        | _ ->
            if fullName.StartsWith("FSharp.Core.FSharpChoice`") then
                fullName |> Fable.Naming.replacePrefix "FSharp.Core.FSharp" ""
            else
                fullName

    let makeUnion (com: IRustCompiler) ctx r values tag entRef genArgs =
        let ent = com.GetEntity(entRef)
        // let genArgs = transformGenArgs com ctx genArgs
        let unionCase = ent.UnionCases |> List.item tag
        let unionCaseName = mapKnownUnionCaseNames unionCase.FullName
        let callee = makeFullNamePathExpr unionCaseName None //genArgs
        let expr = callFunction com ctx None callee values
        if isCopyableEntity com Set.empty ent || ent.FullName = Types.result
        then expr
        else expr |> makeRcValue

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
        let args = transformCallArgs com ctx values []
        let expr = mkMacroExpr "format" ((mkStrLitExpr fmt)::args) |> mkAddrOfExpr
        makeString com ctx expr

    let transformValue (com: IRustCompiler) (ctx: Context) r value: Rust.Expr =
        let unimplemented () =
            $"Value %A{value} is not implemented yet"
            |> addWarning com [] None
            TODO_EXPR $"%A{value}"
        match value with
        | Fable.BaseValue (None, _) ->
            // Super(None)
            unimplemented ()
        | Fable.BaseValue (Some boundIdent, _) ->
            // identAsExpr boundIdent
            unimplemented ()
        | Fable.ThisValue typ -> makeThis com ctx r typ
        | Fable.TypeInfo _ ->
            // transformTypeInfo com ctx r Map.empty t
            unimplemented ()
        | Fable.Null t ->
            //TODO: some other representation perhaps?
            makeDefaultOf com ctx t
        | Fable.UnitConstant -> mkUnitExpr ()
        | Fable.BoolConstant b -> mkBoolLitExpr b //, ?loc=r)
        | Fable.CharConstant c -> mkCharLitExpr c //, ?loc=r)
        | Fable.StringConstant s -> mkStrLitExpr s |> makeString com ctx
        | Fable.StringTemplate (_tag, parts, values) -> formatString com ctx parts values
        | Fable.NumberConstant (x, kind, _) -> makeNumber com ctx r value.Type kind x
        | Fable.RegexConstant (source, flags) ->
            // Expression.regExpLiteral(source, flags, ?loc=r)
            unimplemented ()
        | Fable.NewArray (Fable.ArrayValues values, typ, _isMutable) -> makeArray com ctx r typ values
        | Fable.NewArray ((Fable.ArrayFrom expr | Fable.ArrayAlloc expr), typ, _isMutable) -> makeArrayFrom com ctx r typ expr
        | Fable.NewTuple (values, isStruct) -> makeTuple com ctx r values isStruct
        | Fable.NewList (headAndTail, typ) -> makeList com ctx r typ headAndTail
        | Fable.NewOption (value, typ, isStruct) -> makeOption com ctx r typ value isStruct
        | Fable.NewRecord (values, entRef, genArgs) -> makeRecord com ctx r values entRef genArgs
        | Fable.NewAnonymousRecord (values, fieldNames, genArgs) -> makeTuple com ctx r values false // temporary
        | Fable.NewUnion (values, tag, entRef, genArgs) -> makeUnion com ctx r values tag entRef genArgs

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
                HasMultipleUses = true }
        let isOnlyReference =
            if varAttrs.IsRef then false
            else
                match e with
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
                | Fable.DecisionTreeSuccess _ ->
                    true //All control constructs in f# return expressions, and as return statements are always take ownership, we can assume this is already owned, and not bound
                //| Fable.Sequential _ -> true    //this is just a wrapper, so do not need to clone, passthrough only. (currently breaks some stuff, needs looking at)
                | _ ->
                    if ctx.IsInPluralizedExpr then
                        false
                        // If an owned value is captured, it must be cloned or it will turn a closure into a FnOnce (as value is consumed on first call).
                        // If an owned value leaves scope inside a for loop, it can also not be assumed to be the only usage, as there are multiple instances of that expression invocation at runtime
                    else not varAttrs.HasMultipleUses
        varAttrs, isOnlyReference

    let transformLeaveContext (com: IRustCompiler) ctx (t: Fable.Type option) (e: Fable.Expr): Rust.Expr =
        let expr = com.TransformAsExpr (ctx, e)
        if ctx.IsCallingFunction && isAddrOfExpr e then //explicit syntax. Only functions supply types, so if & is used with a function, we skip checks
            expr |> mkAddrOfExpr
        else
        if isCloneableExpr com t e then
            let varAttrs, isOnlyReference = calcVarAttrsAndOnlyRef com ctx e
            if ctx.Typegen.IsParamByRefPreferred && not varAttrs.IsRef then
                expr |> mkAddrOfExpr
            elif Option.exists (isByRefType com) t && not varAttrs.IsRef then //implicit syntax
                expr |> mkAddrOfExpr
            elif shouldBeRefCountWrapped com e.Type && not isOnlyReference then
                expr |> makeClone
            elif isCloneableType com e.Type && not isOnlyReference then
                expr |> makeClone // TODO: can this clone be removed somehow?
            elif varAttrs.IsRef then
                expr |> makeClone
            else expr
        else expr
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
                | _ -> transformAsExpr com ctx baseRef
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
    let transformObjectExpr (com: IRustCompiler) ctx (members: Fable.ObjectExprMember list) baseCall: Rust.Expr =
        // let makeMethod kind prop computed hasSpread args body =
        //     let args, body, returnType, typeParamDecl =
        //         getMemberArgsAndBody com ctx (Attached(isStatic=false)) hasSpread args body
        //     ObjectMember.objectMethod(kind, prop, args, body, computed_=computed,
        //         ?returnType=returnType, ?typeParameters=typeParamDecl)

        // let members =
        //     members |> List.collect (fun memb ->
        //         let info = info
        //         let prop, computed = memberFromName memb.Name
        //         [prop, computed]
        //         // if info.IsValue || (info.IsGetter) then
        //         //     [ObjectMember.objectProperty(prop, com.TransformAsExpr(ctx, memb.Body), computed_=computed)]
        //         // elif info.IsGetter then
        //         //     [makeMethod ObjectGetter prop computed false memb.Args memb.Body]
        //         // elif info.IsSetter then
        //         //     [makeMethod ObjectSetter prop computed false memb.Args memb.Body]
        //         // elif info.FullName = "System.Collections.Generic.IEnumerable`1.GetEnumerator" then
        //         //     let method = makeMethod ObjectMeth prop computed memb.Args memb.Body
        //         //     let iterator =
        //         //         let prop, computed = memberFromName "Symbol.iterator"
        //         //         let body = enumerator2iterator com ctx
        //         //         ObjectMember.objectMethod(ObjectMeth, prop, [||], body, computed_=computed)
        //         //     [method; iterator]
        //         // else
        //         //     [makeMethod ObjectMeth prop computed memb.Args memb.Body]
        //     )
        //Expression.objectExpression(List.toArray  members)

        // TODO:
        if members |> List.isEmpty then
            mkUnitExpr () // object constructors may be adding this?
        else
            "Object expressions are not implemented yet"
            |> addWarning com [] None
            TODO_EXPR $"%A{members}"

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

        | Fable.Binary(op, left, right) ->
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

            let left = transformLeaveContext com ctx None left |> maybeAddParens left
            let right = transformLeaveContext com ctx None right |> maybeAddParens right

            match typ, kind with
            | Fable.String, Rust.BinOpKind.Add ->
                //proprietary string concatenation - String + &String = String
                let left = mkMethodCallExprOnce "to_string" None left []
                let strTy = primitiveType "str" |> makeRcTy com ctx
                mkBinaryExpr (mkBinOp kind) left (mkAddrOfExpr right)
                |> makeRcValue
                |> mkCastExpr strTy
            // | _, (Rust.BinOpKind.Eq | Rust.BinOpKind.Ne) when hasReferenceEquality com typ ->
            //         // reference equality
            //         //TODO: implement
            //         // mkBinaryExpr (mkBinOp kind) (mkAddrOfExpr left) (mkAddrOfExpr right)
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
        let args = transformCallArgs com ctx info.Args info.SignatureArgTypes
        let args =
            // for certain macros, use unwrapped format string as first argument
            match macro, info.Args with
            | ("print"|"println"|"format"), (Fable.Value(Fable.StringConstant formatStr, _)::restArgs) ->
                (mkStrLitExpr formatStr)::(List.tail args)
            | _ -> args
        let expr = mkMacroExpr macro args
        if macro = "format"
        then expr |> makeString com ctx
        else expr

    let transformEmit (com: IRustCompiler) ctx range (emitInfo: Fable.EmitInfo) =
        // for now only supports macro calls or function calls
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro
        // if it ends with '!', it's a Rust macro
        if macro.EndsWith("!") then
            transformMacro com ctx range emitInfo
        else // otherwise it's an Emit
            let thisArg = info.ThisArg |> Option.map (fun e -> com.TransformAsExpr(ctx, e)) |> Option.toList
            let args = transformCallArgs com ctx info.Args info.SignatureArgTypes
            let args = args |> List.append thisArg
            mkEmitExpr macro args

    let transformCallee (com: IRustCompiler) ctx calleeExpr =
        let expr = transformExprMaybeIdentExpr com ctx calleeExpr
        match calleeExpr with
        | Fable.IdentExpr id -> expr
        | _ -> expr |> mkParenExpr // if not an identifier, wrap it in parentheses

    let isDeclEntityKindOf (com: IRustCompiler) isKindOf (callInfo: Fable.CallInfo) =
        let mi = com.GetMember(callInfo.MemberRef)
        match mi.DeclaringEntity with
        | Some entRef ->
            let ent = com.GetEntity(entRef)
            ent |> isKindOf
        | None -> false

    let isModuleMember (com: IRustCompiler) (callInfo: Fable.CallInfo) =
        isDeclEntityKindOf com (fun ent -> ent.IsFSharpModule) callInfo

    let transformCall (com: IRustCompiler) ctx range typ calleeExpr (callInfo: Fable.CallInfo) =
        let memberInfo = com.GetMember(callInfo.MemberRef)
        let ctx =
            let issParamByRefPreferred = memberInfo.Attributes |> Seq.exists (fun a -> a.Entity.FullName.Contains("ByRefAttr"))
            { ctx with IsCallingFunction = true; Typegen = { ctx.Typegen with IsParamByRefPreferred = issParamByRefPreferred } }

        let args = transformCallArgs com ctx callInfo.Args callInfo.SignatureArgTypes
        match calleeExpr with
        // mutable module values (transformed as function calls)
        | Fable.IdentExpr id when id.IsMutable && isModuleMember com callInfo ->
            let expr = transformIdent com ctx range id
            mutableGet (mkCallExpr expr [])

        | Fable.Get(calleeExpr, (Fable.FieldGet info as kind), t, _r) ->
            // this is an instance call
            match t with
            | Fable.LambdaType _ | Fable.DelegateType _ ->
                // if the field itself is a function, wrap in parentheses
                let callee = transformGet com ctx None t calleeExpr kind
                mkCallExpr (callee |> mkParenExpr) args
            | _ ->
                // normal instance call
                let namesp, name = splitNameSpace info.Name
                let callee = com.TransformAsExpr(ctx, calleeExpr)
                mkMethodCallExpr name None callee args

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
    let transformCurriedApply com ctx range (TransformExpr com ctx applied) args =
        match transformCallArgs com ctx false args with
        | [] -> callFunction range applied []
        | args -> (applied, args) ||> List.fold (fun e arg -> callFunction range e [arg])

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
            let expr = transformExprMaybeIdentExpr com ctx fableExpr
            let prop = transformExprMaybeUnwrapRef com ctx idx
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
            | Fable.AnonymousRecordType (fields, args) ->
                // temporary - redirect anon to tuple calls
                let idx = fields |> Array.findIndex (fun f -> f = fieldName)
                (Fable.TupleIndex (idx))
                |> transformGet com ctx range typ fableExpr
            | t when isInterface com t ->
                // for interfaces, transpile property_get as instance call
                let callee = transformExprMaybeIdentExpr com ctx fableExpr
                mkMethodCallExpr fieldName None callee []
            | _ ->
                let expr = transformExprMaybeIdentExpr com ctx fableExpr
                let field = getField range expr fieldName
                if info.IsMutable
                then field |> mutableGet
                else field

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            libCall com ctx range [] "List" "head" [fableExpr]

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            libCall com ctx range [] "List" "tail" [fableExpr]

        | Fable.TupleIndex index ->
            let expr = transformExprMaybeIdentExpr com ctx fableExpr
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
            let expr = com.TransformAsExpr(ctx, fableExpr)
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
                    unionCase.UnionCaseFields
                    |> Seq.mapi (fun i field ->
                        if i = info.FieldIndex then
                            mkIdentPat fieldName false false
                        else WILD_PAT)
                let unionCaseName = mapKnownUnionCaseNames unionCase.FullName
                let path = makeFullNamePath unionCaseName None
                let pat = mkTupleStructPat path fields
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
        let expr = transformExprMaybeIdentExpr com ctx fableExpr
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
            let prop = transformExprMaybeUnwrapRef com ctx idx
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
        let expr =
            // com.TransformAsExpr(ctx, e)
            transformLeaveContext com ctx None e
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
                let typegen =
                    { ctx.Typegen with
                        IsParamType = false
                        IsRawType = false }
                let ctx = { ctx with Typegen = typegen }
                transformType com ctx ident.Type
                |> Some
        let tyOpt =
            tyOpt |> Option.map (fun ty ->
                if ident.IsMutable
                then ty |> makeMutTy com ctx |> makeRcTy com ctx
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
                then init |> makeMutValue |> makeRcValue
                else init)
        let local = mkIdentLocal [] ident.Name tyOpt initOpt
        // TODO : traverse body and follow references to decide on if this should be wrapped or not]
        let scopedVarAttrs = {
            IsArm = false
            IsRef = false
            IsBox = false
            HasMultipleUses = hasMultipleUses ident.Name usages
        }
        let ctxNext = { ctx with ScopedSymbols = ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs }
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
        let guardExpr = transformExprMaybeUnwrapRef com ctx guard
        let thenExpr = transformLeaveContext com ctx None thenBody
        match elseBody with
        | Fable.Value(Fable.UnitConstant, _) ->
            mkIfThenExpr guardExpr thenExpr //?loc=range)
        | _ ->
            let elseExpr = transformLeaveContext com ctx None elseBody
            mkIfThenElseExpr guardExpr thenExpr elseExpr //?loc=range)

    let transformWhileLoop (com: IRustCompiler) ctx range guard body =
        let guardExpr = transformExprMaybeUnwrapRef com ctx guard
        let bodyExpr = com.TransformAsExpr(ctx, body)
        mkWhileExpr None guardExpr bodyExpr //?loc=range)

    let transformForLoop (com: IRustCompiler) ctx range isUp (var: Fable.Ident) start limit body =
        let startExpr = transformExprMaybeUnwrapRef com ctx start
        let limitExpr = transformExprMaybeUnwrapRef com ctx limit
        let ctx = { ctx with IsInPluralizedExpr = true }
        let bodyExpr = com.TransformAsExpr(ctx, body)
        let varPat = mkIdentPat var.Name false false
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
        |> transformAsExpr com ctx
        // |> mkTryBlockExpr // TODO: nightly only, enable when stable

    let transformCurriedApply (com: IRustCompiler) ctx range calleeExpr args =
        match ctx.TailCallOpportunity with
        | Some tc when tc.IsRecursiveRef(calleeExpr)
            && List.length tc.Args = List.length args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            let callee = transformCallee com ctx calleeExpr
            callFunction com ctx range callee args
        // let handler =
        //     catch |> Option.map (fun (param, body) ->
        //         CatchClause.catchClause(identAsPattern param, transformBlock com ctx returnStrategy body))
        // let finalizer =
        //     finalizer |> Option.map (transformBlock com ctx None)
        // [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
        //     ?handler=handler, ?finalizer=finalizer, ?loc=r)|]

(*
    let transformBindingExprBody (com: IRustCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        | Function(args, body) ->
            let name = Some var.Name
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name
        | _ ->
            if var.IsMutable then
                com.TransformAsExpr(ctx, value)
            else
                com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IRustCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (identAsExpr var)

    let transformBindingAsStatements (com: IRustCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isJsStatement ctx false value then
            let varPattern, varExpr = identAsPattern var, identAsExpr var
            let decl = Statement.variableDeclaration(varPattern)
            let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            Array.append [|decl|] body
        else
            let value = transformBindingExprBody com ctx var value
            let decl = varDeclaration (identAsPattern var) var.IsMutable value |> Declaration.VariableDeclaration |> Declaration
            [|decl|]
*)
    let transformTest (com: IRustCompiler) ctx range kind (fableExpr: Fable.Expr): Rust.Expr =
        match kind with
        | Fable.TypeTest t ->
            transformTypeTest com ctx range fableExpr t
        | Fable.OptionTest isSome ->
            let test = if isSome then "is_some" else "is_none"
            let expr = com.TransformAsExpr(ctx, fableExpr)
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
                let unionCaseName = mapKnownUnionCaseNames unionCase.FullName
                let path = makeFullNamePath unionCaseName None
                let fields =
                    match fableExpr with
                    | Fable.IdentExpr id ->
                        unionCase.UnionCaseFields
                        |> Seq.mapi (fun i _field ->
                            let fieldName = $"{id.Name}_{tag}_{i}"
                            mkIdentPat fieldName false false
                        )
                        |> Seq.toList
                    | _ ->
                        [WILD_PAT]
                let pat = mkTupleStructPat path fields
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
                        unionCase.UnionCaseFields
                        |> Seq.mapi (fun i field ->
                            let fieldName = $"{idName}_{caseIndex}_{i}"
                            (fieldName, idName, field.FieldType)
                        )
                        |> Seq.toList
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
                //com.TransformAsExpr(ctx, bodyExpr)
                let usages = calcIdentUsages bodyExpr
                let getScope name =
                    name, { IsArm = true
                            IsRef = true
                            IsBox = false
                            HasMultipleUses = hasMultipleUses name usages }
                let symbolsAndNames =
                    let fromIdents =
                        idents
                        |> List.map (fun id -> getScope id.Name)
                    let fromExtra =
                        extraVals
                        |> List.map (fun (_name, friendlyName, _t) -> getScope friendlyName)
                    fromIdents @ fromExtra
                let scopedSymbolsNext =
                    Helpers.Map.merge ctx.ScopedSymbols (symbolsAndNames |> Map.ofList)
                let ctx = { ctx with ScopedSymbols = scopedSymbolsNext }
                transformLeaveContext com ctx None bodyExpr
            mkArm attrs pat guard body

        let makeUnionCasePat evalType evalName caseIndex =
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
                            [mkIdentPat fieldName false false]
                        | _ -> []
                    | _ ->
                        [WILD_PAT]
                if List.isEmpty fields then
                    Some(mkIdentPat unionCaseFullName false false)
                else
                    let path = makeFullNamePath unionCaseFullName None
                    Some(mkTupleStructPat path fields)
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsFSharpUnion then
                    // let genArgs = transformGenArgs com ctx genArgs
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    let fields =
                        match evalName with
                        | Some idName ->
                            unionCase.UnionCaseFields
                            |> Seq.mapi (fun i _field ->
                                let fieldName = $"{idName}_{caseIndex}_{i}"
                                mkIdentPat fieldName false false
                            )
                            |> Seq.toList
                        | _ ->
                            [WILD_PAT]
                    let unionCaseName = mapKnownUnionCaseNames unionCase.FullName
                    if List.isEmpty fields then
                        Some(mkIdentPat unionCaseName false false)
                    else
                        let path = makeFullNamePath unionCaseName None
                        Some(mkTupleStructPat path fields)
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
                        makeUnionCasePat evalType evalName tag
                    | _ -> None
                let pat =
                    match patOpt with
                    | Some pat -> pat
                    | _ -> com.TransformAsExpr(ctx, caseExpr) |> mkLitPat
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
                        makeUnionCasePat evalType evalName 0
                    | Fable.Get (Fable.IdentExpr id, Fable.UnionField info, _, _)
                        when Some id.Name = evalName && id.Type = evalType ->
                        makeUnionCasePat evalType evalName info.CaseIndex
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

    let transformDecisionTreeSuccessAsExpr (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] ->
            transformExprMaybeUnwrapRef com ctx target
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)
(*
    let transformDecisionTreeSuccessAsStatements (com: IRustCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Rust.Stmt[] =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex
            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
                    assign None (identAsExpr id) value |> ExpressionStatement)
            let targetAssignment = assign None (targetId |> Expression.Identifier) (ofInt targetIndex) |> ExpressionStatement
            Array.append [|targetAssignment|] assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (com.TransformAsStatements(ctx, ret, target))
*)
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
                    let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                    Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr ->
                    checkInner ((caseExpr, targetIndex, boundValues)::cases) evalExpr treeExpr
            | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            checkInner [(caseExpr, targetIndex, boundValues)] evalExpr treeExpr
        | _ -> None

    let transformDecisionTreeAsExpr (com: IRustCompiler) ctx targets (expr: Fable.Expr): Rust.Expr =
        match transformDecisionTreeAsSwitch expr with
        | Some(evalExpr, cases, defaultCase) ->
            transformSwitch com ctx evalExpr cases defaultCase targets
        | None ->
            let ctx = { ctx with DecisionTargets = targets }
            com.TransformAsExpr(ctx, expr)

    let rec transformAsExpr (com: IRustCompiler) ctx (fableExpr: Fable.Expr): Rust.Expr =
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

        | Fable.ObjectExpr (members, _, baseCall) ->
            transformObjectExpr com ctx members baseCall

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
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            // flatten nested let binding expressions
            let bindings, body = flattenLet [] fableExpr
            transformLet com ctx bindings body
            // if ctx.HoistVars [ident] then
            //     let assignment = transformBindingAsExpr com ctx ident value
            //     Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
            // else iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            transformLet com ctx bindings body
        //     let idents = List.map fst bindings
        //     if ctx.HoistVars(idents) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
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
                transformAsExpr com ctx e
            | Fable.Throw(expr, _) ->
                match expr with
                | None -> failwith "TODO: rethrow"
                | Some(TransformExpr com ctx msg) -> mkMacroExpr "panic" [mkStrLitExpr "{}"; msg]
            | Fable.Debugger
            | Fable.RegionStart _ ->
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
            let info = com.GetMember(decl.MemberRef)
            info.Attributes
            |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint)
            |> Option.map (fun _ -> [splitLast decl.Name])
        | Fable.ActionDeclaration decl -> None
        | Fable.ClassDeclaration decl -> None

    let getModuleItems (com: IRustCompiler) ctx =
        let isLastFileInProject =
            (Array.last com.SourceFiles) = com.CurrentFile
        if isLastFileInProject then
            // add all other project files as module imports
            com.SourceFiles |> Array.iter (fun filePath ->
                if filePath <> com.CurrentFile then
                    let relPath = Fable.Path.getRelativeFileOrDirPath false com.CurrentFile false filePath
                    com.GetImportName(ctx, "*", relPath, None) |> ignore
            )
            let makeModItems (fullPath, moduleName) =
                let importPath = Fable.Path.getRelativePath com.CurrentFile fullPath
                let attrs = [mkEqAttr "path" ("\"" + importPath  + "\"")]
                let modItem = mkUnloadedModItem attrs moduleName
                let useItem = mkGlobUseItem [] [moduleName]
                if isFableLibrary com
                then [modItem; useItem |> mkPublicItem] // export modules at top level
                else [modItem]
            let modItems = com.GetAllModules() |> List.sortBy fst |> List.collect makeModItems
            modItems
        else []

    let getEntryPointItems (com: IRustCompiler) ctx decls =
        let entryPoint = decls |> List.tryPick (tryFindEntryPoint com)
        match entryPoint with
        | Some path ->
            // add some imports for main
            let asStr = getLibraryImportName com ctx "String" "string"
            let asArr = getLibraryImportName com ctx "Native" "arrayFrom"

            // main entrypoint
            let mainName = String.concat "::" path
            let strBody = [
                $"let args: Vec<String> = std::env::args().collect()"
                $"let args: Vec<Rc<str>> = args[1..].iter().map(|s| {asStr}(s)).collect()"
                $"{mainName}({asArr}(&args))"
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
    let getEntityFieldsAsIdents _com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> cleanNameAsRustIdentifier
            let typ = field.FieldType
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
    let typedParam (com: IRustCompiler) ctx (ident: Fable.Ident) =
        let typegen =
            { ctx.Typegen with
                IsParamType = true
                IsRawType = false }
        let ctx = { ctx with Typegen = typegen }
        let ty = transformParamType com ctx ident.Type
        let isRef = false
        let isMut = false
        if ident.IsThisArgument
        then mkImplSelfParam isRef isMut
        else mkParamFromType ident.Name ty isRef isMut //?loc=id.Range)

    let inferredParam (com: IRustCompiler) ctx (ident: Fable.Ident) =
        let isRef = false
        let isMut = false
        mkInferredParam ident.Name isRef isMut //?loc=id.Range)

    let transformFunctionDecl (com: IRustCompiler) ctx args returnType =
        let inputs =
            args
            |> discardUnitArg
            |> List.map (typedParam com ctx)
        let output =
            if returnType = Fable.Unit then VOID_RETURN_TY
            else
                let ctx = { ctx with Typegen = { ctx.Typegen with IsParamType = false } }
                returnType |> transformType com ctx |> mkFnRetTy
        mkFnDecl inputs output

    let isClosedOverIdent com ctx (ignoredNames: HashSet<string>) expr =
        match expr with
        | Fable.IdentExpr ident ->
            if not (ignoredNames.Contains(ident.Name))
                && (ident.IsMutable ||
                    (isValueScoped ctx ident.Name) ||
                    (isRefScoped ctx ident.Name) ||
                    (shouldBeRefCountWrapped com ident.Type) ||
                    // Closures may capture Ref counted vars, so by cloning
                    // the actual closure, all attached ref counted var are cloned too
                    (match ident.Type with
                        | Fable.LambdaType _
                        | Fable.DelegateType _ -> true
                        | _ -> false)
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
        let fixedNames = ["matchValue"] //TODO: find better way to exclude this
        let allNames = name |> Option.fold (fun xs x -> x :: xs) (argNames @ fixedNames)
        allNames |> Set.ofList

    let hasCapturedNames com ctx (name: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames (Some name) args)
        let isClosedOver expr =
            isClosedOverIdent com ctx ignoredNames expr |> Option.isSome
        FableTransforms.deepExists isClosedOver body

    let getCapturedNames com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames name args)
        let capturedNames = HashSet<string>()
        let addClosedOverIdent expr =
            isClosedOverIdent com ctx ignoredNames expr
            |> Option.iter (fun ident -> capturedNames.Add(ident.Name) |> ignore)
            false
        // collect all closed over names that are not arguments
        FableTransforms.deepExists addClosedOverIdent body |> ignore
        capturedNames |> Set.ofSeq // remove duplicates in some contexts

    let getFunctionCtx com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) isTailRec =
        let usages = calcIdentUsages body
        let scopedSymbols =
            (ctx.ScopedSymbols, args)
            ||> List.fold (fun acc arg ->
                //TODO: optimizations go here
                let scopedVarAttrs = {
                    IsArm = false
                    IsRef = isByRefType com arg.Type || ctx.Typegen.IsParamByRefPreferred
                    IsBox = false
                    HasMultipleUses = hasMultipleUses arg.Name usages
                }
                acc |> Map.add arg.Name scopedVarAttrs)
        let tco =
            if isTailRec then
                Some(NamedTailCallOpportunity(com, ctx, name.Value, args) :> ITailCallOpportunity)
            else None
        { ctx with
            ScopedSymbols = scopedSymbols
            Typegen = {ctx.Typegen with IsParamByRefPreferred = false}
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

    let transformFunction com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        //if name |> Option.exists (fun n -> n.Contains("byrefAttrIntFn")) then System.Diagnostics.Debugger.Break()
        let isRecursive, isTailRec = isTailRecursive name body
        let argTypes = args |> List.map (fun arg -> arg.Type)
        let genParams = getGenericParams ctx (argTypes @ [body.Type])
        let fnDecl = transformFunctionDecl com ctx args body.Type
        let ctx = getFunctionCtx com ctx name args body isTailRec
        let fnBody = transformFunctionBody com ctx args body
        fnDecl, fnBody, genParams

    let transformLambda com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let isRecursive, isTailRec = isTailRecursive name body
        let fixedArgs = if isRecursive && not isTailRec then (makeIdent name.Value) :: args else args
        let fnDecl = transformFunctionDecl com ctx fixedArgs Fable.Unit
        let ctx = getFunctionCtx com ctx name args body isTailRec
        // remove captured names from scoped symbols, as they will be cloned
        let closedOverCloneableNames = getCapturedNames com ctx name args body
        let ctx = { ctx with    IsInPluralizedExpr = true
                                ScopedSymbols = ctx.ScopedSymbols |> Helpers.Map.except closedOverCloneableNames }
        let fnBody = transformFunctionBody com ctx args body
        let closureExpr = mkClosureExpr fnDecl fnBody
        let closureExpr =
            if isRecursive && not isTailRec then
                // make it recursive with fixed-point combinator
                let fixName = "fix" + string (List.length args)
                makeNativeCall com ctx None "Func" fixName [closureExpr]
            else closureExpr
        if not (Set.isEmpty closedOverCloneableNames) then
            mkStmtBlockExpr [
                for name in closedOverCloneableNames do
                    let pat = mkIdentPat name false false
                    let identExpr = com.TransformAsExpr(ctx, makeIdentExpr name)
                    let cloneExpr = makeClone identExpr
                    let letExpr = mkLetExpr pat cloneExpr
                    yield letExpr |> mkSemiStmt
                yield closureExpr |> mkExprStmt
            ]
        else closureExpr
        |> makeRcValue

    let makeTypeBounds argName (constraints: Fable.Constraint list) =
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

    let makeGenerics (genParams: Fable.Type list) =
        let defaultBounds = [
            mkTypeTraitGenericBound [rawIdent "Clone"] None
            mkLifetimeGenericBound "'static" //TODO: add it only when needed
        ]
        genParams
        |> List.choose (function
            | Fable.GenericParam(name, _isMeasure, constraints) ->
                let bounds = makeTypeBounds name constraints
                let p = mkGenericParamFromName [] name (bounds @ defaultBounds)
                Some p
            | _ -> None)
        |> mkGenerics

    let transformInnerFunction com ctx (name: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let fnDecl, fnBody, fnGenParams =
            transformFunction com ctx (Some name) args body
        let fnBodyBlock =
            if body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics fnGenParams
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = []
        let fnItem = mkFnItem attrs name kind
        let scopedVarAttrs = {
            IsArm = false
            IsRef = false
            IsBox = true
            HasMultipleUses = true
        }
        let ctxNext = { ctx with ScopedSymbols = ctx.ScopedSymbols |> Map.add name scopedVarAttrs }
        mkItemStmt fnItem, ctxNext

    let transformModuleFunction (com: IRustCompiler) ctx (info: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        let name = splitLast decl.Name
        let info = com.GetMember(decl.MemberRef)
        let fnDecl, fnBody, fnGenParams =
            let ctx = { ctx with Typegen = { ctx.Typegen with
                                                IsParamByRefPreferred = info.Attributes
                                                                        |> Seq.exists (fun q -> q.Entity.FullName.Contains("ByRefAttr")) } }
            transformFunction com ctx (Some info.FullName) decl.Args decl.Body
        let fnBodyBlock =
            if decl.Body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics fnGenParams
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs =
            // translate test methods attributes
            // TODO: support more test frameworks
            info.Attributes
            |> Seq.filter (fun att -> att.Entity.FullName.EndsWith(".FactAttribute"))
            |> Seq.map (fun _ -> mkAttr "test" [])
        let fnItem = mkFnItem attrs name kind
        // let fnItem =
        //     if info.IsPublic
        //     then fnItem |> mkPublicItem
        //     else fnItem
        [fnItem |> mkPublicItem]

    let transformModuleMember (com: IRustCompiler) ctx (info: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        // uses thread_local for static initialization
        let name = splitLast decl.Name
        let fableExpr = decl.Body
        let value = transformAsExpr com ctx fableExpr
        let value =
            if info.IsMutable
            then value |> makeMutValue |> makeRcValue
            else value
        let attrs = []
        let ty = transformType com ctx fableExpr.Type
        let ty =
            if info.IsMutable
            then ty |> makeMutTy com ctx |> makeRcTy com ctx
            else ty

        let staticItem = mkStaticItem attrs name ty (Some value)
        let macroStmt = mkMacroStmt "thread_local" [mkItemToken staticItem]
        let valueStmt = mkEmitExprStmt $"{name}.with(|value| value.clone())"

        let attrs = []
        let fnBody = [macroStmt; valueStmt] |> mkBlock |> Some
        let fnDecl = mkFnDecl [] (mkFnRetTy ty)
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnItem attrs name fnKind
        // let fnItem =
        //     if info.IsPublic
        //     then fnItem |> mkPublicItem
        //     else fnItem
        [fnItem |> mkPublicItem]

    let transformAssocMemberFunction (com: IRustCompiler) ctx (info: Fable.MemberFunctionOrValue) (membName: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let name = splitLast membName
        let fnDecl, fnBody, fnGenParams =
            transformFunction com ctx (Some membName) args body
        let fnBodyBlock =
            if body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics fnGenParams
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs =
            info.Attributes
            |> Seq.filter (fun att -> att.Entity.FullName.EndsWith(".FactAttribute"))
            |> Seq.map (fun _ -> mkAttr "test" [])
        let fnItem = mkFnAssocItem attrs name kind
        fnItem

(*
    let transformAction (com: IRustCompiler) ctx expr =
        // let statements = transformAsStatements com ctx None expr
        // let hasVarDeclarations =
        //     statements |> Array.exists (function
        //         | Declaration(Declaration.VariableDeclaration(_)) -> true
        //         | _ -> false)
        // if hasVarDeclarations then
        //     [ Expression.callExpression(Expression.functionExpression([||], BlockStatement(statements)), [||])
        //       |> ExpressionStatement |> PrivateModuleDeclaration ]
        // else statements |> Array.mapToList (fun x -> PrivateModuleDeclaration(x))

    let transformAttachedProperty (com: IRustCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not info.IsInstance
        let kind = if info.IsGetter then ClassGetter else ClassSetter
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) false memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        ClassMember.classMethod(kind, key, args, body, computed_=computed, ``static``=isStatic)
        |> Array.singleton

    let transformAttachedMethod (com: IRustCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not info.IsInstance
        let makeMethod name args body =
            let key, computed = memberFromName name
            ClassMember.classMethod(ClassFunction, key, args, body, computed_=computed, ``static``=isStatic)
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) memb.Args memb.Body
        [|
            yield makeMethod memb.Name args body
            if info.FullName = "System.Collections.Generic.IEnumerable`1.GetEnumerator" then
                yield makeMethod "Symbol.iterator" [||] (enumerator2iterator com ctx)
        |]
*)
    let getEntityGenArgs (ent: Fable.Entity) =
        ent.GenericParameters
        |> List.map (fun p -> Fable.Type.GenericParam(p.Name, p.IsMeasure, Seq.toList p.Constraints))

    let getInterfaceMemberNamesSet (com: IRustCompiler) (entRef: Fable.EntityRef) =
        let ent = com.GetEntity(entRef)
        assert(ent.IsInterface)
        ent.AllInterfaces
        |> Seq.collect (fun i ->
            let e = com.GetEntity(i.Entity)
            Map.values e.MembersFunctionsAndValues)
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
        let entNamesp, entName = splitNameSpace ent.FullName
        let genArgs = getEntityGenArgs ent
        let ty =
            let genArgs = genArgs |> transformGenArgs com ctx
            let bounds = mkTypeTraitGenericBound [entName] genArgs
            mkTraitTy [bounds] // |> makeRcTy com ctx
        let path =
            let genArgTys = genArgs |> List.map (transformType com ctx)
            mkGenericPath (splitFullName ent.FullName) (mkGenericTypeArgs genArgTys)
        let generics = genArgs |> makeGenerics
        let bounds = [] //TODO:
        let tyItem = mkTyAliasItem [] entName ty generics bounds
        [tyItem]

    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entNamesp, entName = splitNameSpace ent.FullName
        let generics = getEntityGenArgs ent |> makeGenerics
        let variants =
            ent.UnionCases |> Seq.map (fun uci ->
                let name = uci.Name
                let isPublic = false
                let fields =
                    uci.UnionCaseFields |> List.map (fun fi ->
                        let ty = transformType com ctx fi.FieldType
                        mkField [] fi.Name ty isPublic)
                mkTupleVariant [] name fields
            )
        let attrs = [mkAttr "derive" (makeDerivedFrom com ent)]
        let enumItem = mkEnumItem attrs entName variants generics
        [enumItem |> mkPublicItem] // TODO: add traits for attached members

    let transformClass (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entNamesp, entName = splitNameSpace ent.FullName
        let generics = getEntityGenArgs ent |> makeGenerics
        let isPublic = ent.IsFSharpRecord
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let ty = transformType com ctx fi.FieldType
                let ty =
                    if fi.IsMutable
                    then ty |> makeMutTy com ctx
                    else ty
                mkField [] fi.Name ty isPublic
            )
        let attrs = [mkAttr "derive" (makeDerivedFrom com ent)]
        let structItem = mkStructItem attrs entName fields generics
        [structItem |> mkPublicItem] // TODO: add traits for attached members

    let transformCompilerGeneratedConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (declName: string) =
        // let ctor = ent.MembersFunctionsAndValues |> Seq.tryFind (fun q -> q.CompiledName = ".ctor")
        // ctor |> Option.map (fun ctor -> ctor.CurriedParameterGroups)
        let fields = getEntityFieldsAsIdents com ent
        //let exprs = ent.FSharpFields |> List.map (fun f -> f.)
        //ctor |> Option.map (fun x -> transformModuleFunction com ctx x. )
        // let fields = ent.FSharpFields
        //                 |> List.map (fun f -> makeIdent f.Name, f.FieldType)
        // let fieldIdents = fields |> List.map (fst)
        let body =
            Fable.Value(
                Fable.NewRecord(
                    fields |> List.map Fable.IdentExpr,
                    ent.Ref,
                    fields |> List.map (fun ident -> ident.Type)
                ), None)
        let info = Fable.GeneratedMember.Function() |> com.GetMember
        let name = declName //TODO: is this correct?
        transformAssocMemberFunction com ctx info name fields body

    let interfacesToIgnore =
        Set.ofList [
            Types.ienumerable
            Types.ienumerableGeneric
            Types.ienumerator
            Types.ienumeratorGeneric
            Types.iequatableGeneric
            Types.icomparable
            Types.icomparableGeneric
            Types.iStructuralEquatable
            Types.iStructuralComparable
            Types.idisposable
        ]

    let transformInterface (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let assocItems =
            ent.AllInterfaces
            |> Seq.collect (fun iface ->
                let ifaceEnt = com.GetEntity(iface.Entity)
                ifaceEnt.MembersFunctionsAndValues
                |> Map.values
                |> Seq.map (fun memb ->
                    let thisArg = { makeIdent "this" with IsThisArgument = true }
                    let memberArgs =
                        memb.CurriedParameterGroups
                        |> Seq.collect id
                        |> Seq.mapi (fun i p ->
                            let name = defaultArg p.Name $"arg{i}"
                            makeTypedIdent p.Type name)
                        |> Seq.toList
                    let returnType = memb.ReturnParameter.Type
                    let fnName = memb.DisplayName
                    let fnDecl = transformFunctionDecl com ctx (thisArg::memberArgs) returnType
                    let generics = makeGenerics [] //TODO: add generics?
                    let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics None
                    mkFnAssocItem [] fnName fnKind
                )
            )
        let generics = getEntityGenArgs ent |> makeGenerics
        let entNamesp, entName = splitNameSpace ent.FullName
        let traitItem = mkTraitItem [] entName assocItems [] generics
        [traitItem |> mkPublicItem]

    let makeUniqueName name (usedNames: Set<string>) =
        name |> Fable.Naming.preventConflicts (usedNames.Contains)

    let transformImplicitConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (ctor: Fable.MemberDecl) =
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
                let returnValue = Fable.Value(Fable.NewRecord(fields, ent.Ref, genArgs), None)
                // add return value after the body
                let body = Fable.Sequential (exprs @ [returnValue])
                // replace 'this.field' with just 'field' in body
                let body =
                    body |> visitFromInsideOut (function
                        | Fable.Set(Fable.Value(Fable.ThisValue _, _), Fable.SetKind.FieldSet(fieldName), t, value, r) ->
                            let identExpr = identMap |> Map.find fieldName |> Fable.IdentExpr
                            Fable.Set(identExpr, Fable.ValueSet, t, value, r)
                        | Fable.Get(Fable.Value(Fable.ThisValue _, _), Fable.GetKind.FieldGet info, t, r) ->
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
        let ctx = { ctx with ScopedTypeParams = ent.GenericParameters |> List.map (fun g -> g.Name) |> Set.ofList }
        let info = com.GetMember(ctor.MemberRef)
        transformAssocMemberFunction com ctx info ctor.Name ctor.Args ctor.Body

    let transformClassMembers (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        let ent = com.GetEntity(decl.Entity)
        let entNamesp, entName = splitNameSpace ent.FullName
        let ctorImpls =
            if ent.IsFSharpUnion || ent.IsFSharpRecord then
                []
            else
                let ctorItem =
                    match decl.Constructor with
                    | Some ctor ->
                        withCurrentScope ctx ctor.UsedNames <| fun ctx ->
                            transformImplicitConstructor com ctx ent ctor
                    | _ ->
                        transformCompilerGeneratedConstructor com ctx ent decl.Name
                    |> mkPublicAssocItem
                let genArgs = getEntityGenArgs ent
                let ty =
                    let ctx = { ctx with Typegen = { ctx.Typegen with IsRawType = true } }
                    Fable.Type.DeclaredType(ent.Ref, genArgs) |> transformType com ctx
                let generics = genArgs |> makeGenerics
                let implItem = mkImplItem [] "" ty generics [ctorItem] None
                [implItem]

        let interfaces =
            ent.AllInterfaces
            |> Seq.map (fun i ->
                let members = i.Entity |> getInterfaceMemberNamesSet com
                false, members, i.Entity)
            // temporary, throw out anything not defined such as IComparable etc
            |> Seq.filter (fun (_, _, entRef) -> not (interfacesToIgnore |> Set.contains entRef.FullName))
            |> Seq.toList

        let allInterfaceMembersSet =
            interfaces
            |> Seq.map (fun (_, members, _) -> members)
            |> Seq.fold Set.union Set.empty

        let allAttachedMembersSet =
            decl.AttachedMembers
            |> List.map (fun m -> m.Name)
            |> Set.ofList

        let nonInterfaceMembersSet =
            Set.difference allAttachedMembersSet allInterfaceMembersSet

        let nonInterfaceMembersTrait =
            if Set.isEmpty nonInterfaceMembersSet then
                []
            else
                let assocItems = [
                    let membersNotDeclared =
                        decl.AttachedMembers |> List.filter (fun m ->
                            Set.contains m.Name nonInterfaceMembersSet)
                    for m in membersNotDeclared ->
                        let fnDecl = transformFunctionDecl com ctx m.Args m.Body.Type
                        let generics = makeGenerics [] //TODO: add generics?
                        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics None
                        mkFnAssocItem [] m.Name fnKind
                ]
                let generics = getEntityGenArgs ent |> makeGenerics
                let traitItem = mkTraitItem [] (entName + "Methods") assocItems [] generics
                [traitItem |> mkPublicItem]

        let traitsToRender =
            let complTraits =
                if Set.isEmpty nonInterfaceMembersSet then []
                else [true, nonInterfaceMembersSet, decl.Entity]
            interfaces @ complTraits

        let memberTraitImpls =
            traitsToRender
            |> List.collect (fun (isNonInterface, tMethods, tEntRef) ->
                let tEnt = com.GetEntity(tEntRef)
                let ctx = { ctx with ScopedTypeParams = tEnt.GenericParameters |> List.map (fun g -> g.Name) |> Set.ofList }
                let decls =
                    let makeDecl (decl: Fable.MemberDecl) =
                        withCurrentScope ctx decl.UsedNames <| fun ctx ->
                            let info = com.GetMember(decl.MemberRef)
                            let name = if info.IsInstance then decl.Name else info.FullName
                            transformAssocMemberFunction com ctx info name decl.Args decl.Body
                    decl.AttachedMembers
                    |> List.filter (fun m -> tMethods |> Set.contains m.Name)
                    |> List.map makeDecl
                if List.isEmpty decls then
                    []
                else
                    let ty =
                        let genArgs = getEntityGenArgs ent |> transformGenArgs com ctx
                        let bounds = mkTypeTraitGenericBound [entName] genArgs
                        mkTraitTy [bounds] // |> makeRcTy com ctx
                    let genArgs = getEntityGenArgs tEnt
                    let nameParts =
                        if isNonInterface then tEntRef.FullName + "Methods"
                        else getInterfaceEntityName com ctx tEntRef
                        |> splitFullName
                    let path =
                        let genArgTys = genArgs |> List.map (transformType com ctx)
                        mkGenericPath nameParts (mkGenericTypeArgs genArgTys)
                    let generics = genArgs |> makeGenerics
                    let ofTrait = mkTraitRef path |> Some
                    let implItem = mkImplItem [] "" ty generics decls ofTrait
                    [implItem]
            )

        ctorImpls @ nonInterfaceMembersTrait @ memberTraitImpls

    let transformClassDecl (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let ent = com.GetEntity(decl.Entity)
        if ent.IsFSharpAbbreviation then
            transformAbbrev com ctx ent
        elif ent.IsInterface then
            if interfacesToIgnore |> Set.contains ent.FullName
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

    let transformClassWithImplicitConstructor (com: IRustCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
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

    let rec transformDecl (com: IRustCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            // TODO: perhaps collect other use decls from usage in body
            let useItem = mkGlobUseItem [] ["super"]
            let useDecls = [useItem]
            let memberDecls = decl.Members |> List.collect (transformDecl com ctx)
            let attrs =  []
            let modDecls = useDecls @ memberDecls
            let modItem = modDecls |> mkModItem attrs decl.Name
            [modItem |> mkPublicItem]

        | Fable.ActionDeclaration decl ->
            // TODO: use ItemKind.Static with IIFE closure?
            [TODO_ITEM "module_do_bindings_not_implemented_yet"]
            // withCurrentScope ctx decl.UsedNames <| fun ctx ->
            //     transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                let info = com.GetMember(decl.MemberRef)
                if info.IsValue
                then transformModuleMember com ctx info decl
                else transformModuleFunction com ctx info decl

        | Fable.ClassDeclaration decl ->
            transformClassDecl com ctx decl

    let getImportFullPath (com: IRustCompiler) (path: string) =
        let isAbsolutePath =
            path.StartsWith("/") || path.StartsWith("\\") || path.IndexOf(":") = 1
        let isLibraryPath =
            path.StartsWith(com.LibraryDir)
        if isAbsolutePath || isLibraryPath then
            Fable.Path.normalizePath path
        else
            let currentDir = Fable.Path.GetDirectoryName(com.CurrentFile)
            Fable.Path.Combine(currentDir, path)
            |> Fable.Path.normalizeFullPath

    let isFableLibrary (com: IRustCompiler) =
        List.contains "FABLE_LIBRARY" com.Options.Define //TODO: look in project defines too

    let isFableLibraryImport (com: IRustCompiler) (path: string) =
        not (isFableLibrary com) && path.StartsWith(com.LibraryDir)

    let transformImports (com: IRustCompiler) ctx (imports: Import list): Rust.Item list =
        imports
        |> List.groupBy (fun import -> import.FullPath)
        |> List.collect (fun (_fullPath, moduleImports) ->
            moduleImports
            |> List.map (fun import ->
                let modPath =
                    if isFableLibraryImport com import.Path
                    then ["fable_library_rust"]
                    else ["crate"; import.ModuleName]
                match import.Selector with
                | "" | "*" | "default" ->
                    mkGlobUseItem [] modPath
                | _ ->
                    let parts = splitFullName import.Selector
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
        | _ -> splitFullName selector |> List.last
        |> getUniqueNameInRootScope ctx

    // F# hash function is unstable and gives different results in different runs
    // Taken from fable-library/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
    let stableStringHash (s: string) =
        let mutable h = 5381
        for i = 0 to s.Length - 1 do
            h <- (h * 33) ^^^ (int s.[i])
        h

    let getModuleName fullPath =
        System.String.Format("module_{0:x}", stableStringHash fullPath)


module Compiler =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open Util

    // global level (across files)
    let importModules = ConcurrentDictionary<string, string>()

    // per file
    type RustCompiler (com: Fable.Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IRustCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member this.GetImportName(ctx, selector, path, r) =
                if selector = Fable.Naming.placeholder then
                    "`importMember` must be assigned to a variable"
                    |> addError com [] r
                let path = path |> Fable.Naming.replaceSuffix ".fs" ".rs"
                if path.Contains("::") then
                    path + "::" + selector // direct Rust import
                else
                    let cacheKey = path + "::" + selector
                    let import =
                        match imports.TryGetValue(cacheKey) with
                        | true, import -> import
                        | false, _ ->
                            let fullPath = getImportFullPath this path
                            let localIdent = getIdentForImport ctx path selector
                            let moduleName = getModuleName fullPath
                            let import = {
                                Selector = selector
                                LocalIdent = localIdent
                                ModuleName = moduleName
                                FullPath = fullPath
                                Path = path
                            }
                            if not (isFableLibraryImport this path) then
                                importModules.TryAdd(fullPath, moduleName) |> ignore
                            imports.Add(cacheKey, import)
                            import
                    $"{import.LocalIdent}"

            member _.GetAllImports() = imports.Values |> Seq.toList
            member _.GetAllModules() = importModules |> Seq.map (fun p -> p.Key, p.Value) |> Seq.toList

            member this.TransformAsExpr(ctx, e) = transformAsExpr this ctx e
            // member this.TransformAsStatements(ctx, ret, e) = transformAsStatements this ctx ret e
            // member this.TransformFunction(ctx, name, args, body) = transformFunction this ctx name args body
            // member this.TransformImport(ctx, selector, path) = transformImport this ctx None selector path

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

        let ctx =
          { File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = Unchecked.defaultof<_> }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty
            ScopedSymbols = Map.empty
            IsInPluralizedExpr = false
            IsCallingFunction = true
            Typegen = { IsParamType = false
                        IsParamByRefPreferred = false
                        IsRawType = false } }

        let topAttrs = [
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

        let declItems = List.collect (transformDecl com ctx) file.Declarations
        let entryPointItems = getEntryPointItems com ctx file.Declarations
        let importItems = com.GetAllImports() |> transformImports com ctx
        let moduleItems = getModuleItems com ctx // adds imports for project files
        let crateItems = importItems @ declItems @ moduleItems @ entryPointItems

        let crate = mkCrate topAttrs crateItems
        crate
