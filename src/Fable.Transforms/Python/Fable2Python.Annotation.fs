module rec Fable.Transforms.Python.Annotation

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.Py
open Fable.Transforms
open Fable.Transforms.Python.AST
open Fable.Transforms.Python.Types
open Fable.Transforms.Python.Util

/// Check if type is an inref (in-reference) or Any type.
/// In F#, struct instance method's `this` parameter is represented as inref<StructType>,
/// but in Python the struct is passed directly, not wrapped in FSharpRef.
let isInRefOrAnyType (com: IPythonCompiler) =
    function
    | Replacements.Util.IsInRefType com _ -> true
    | Fable.Any -> true
    | _ -> false

let tryPyConstructor (com: IPythonCompiler) ctx ent =
    match Py.Replacements.tryConstructor com ent with
    | Some e -> com.TransformAsExpr(ctx, e) |> Some
    | None -> None

let libCall (com: IPythonCompiler) ctx r moduleName memberName args =
    Expression.call (com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc = r)

let libValue (com: IPythonCompiler) ctx moduleName memberName =
    com.TransformImport(ctx, memberName, getLibPath com moduleName)

let removeNamespace (fullName: string) =
    fullName.Split('.')
    |> Array.last
    |> (fun name -> name.Replace("`", "_"))
    |> Helpers.clean

// Returns type parameters that is used more than once
let getRepeatedGenericTypeParams ctx (types: Fable.Type list) =
    types
    |> List.collect (fun x -> FSharp2Fable.Util.getGenParamNames [ x ]) // Pass one at a time to avoid deduping
    |> List.append (ctx.ScopedTypeParams |> Set.toList)
    |> List.countBy id
    |> List.choose (fun (param, count) ->
        if count > 1 then
            Some param
        else
            None
    )
    |> Set.ofList

let getGenericTypeParams (types: Fable.Type list) =
    types |> FSharp2Fable.Util.getGenParamNames |> Set.ofList

/// Helper function to extract generic arguments from a type.
/// Used for type narrowing casts and other type analysis.
let getGenericArgs (typ: Fable.Type) : Fable.Type list =
    match typ with
    | Fable.DeclaredType(_, genArgs) -> genArgs
    | Fable.Array(elementType, _) -> [ elementType ]
    | Fable.List elementType -> [ elementType ]
    | Fable.Option(elementType, _) -> [ elementType ]
    | _ -> []

/// Check if a type contains any generic parameters (recursively).
/// Used to determine if Option<T> should use Option[T] annotation vs T | None.
/// If the inner type contains generics, we need Option[T] because runtime may wrap.
let containsGenericParams (t: Fable.Type) =
    FSharp2Fable.Util.getGenParamNames [ t ] |> List.isEmpty |> not

/// Check if a type is a callable type (Lambda or Delegate)
let isCallableType (t: Fable.Type) =
    match t with
    | Fable.LambdaType _
    | Fable.DelegateType _ -> true
    | _ -> false

/// Get the final (non-callable) return type from a nested callable type.
/// For A -> B -> C -> int, returns int.
let rec getFinalReturnType (t: Fable.Type) =
    match t with
    | Fable.LambdaType(_, returnType) -> getFinalReturnType returnType
    | Fable.DelegateType(_, returnType) -> getFinalReturnType returnType
    | _ -> t

/// Get the immediate return type of a callable (one level deep).
let getImmediateReturnType (t: Fable.Type) =
    match t with
    | Fable.LambdaType(_, returnType) -> returnType
    | Fable.DelegateType(_, returnType) -> returnType
    | _ -> t

/// Generate type annotation for a callable (lambda) type.
/// For nested callables:
/// - If returned callable returns another callable: Callable[..., Any]
/// - If returned callable returns concrete type: Callable[..., Callable[..., ConcreteType]]
/// For simple callables (depth 1), preserves full type information.
let makeLambdaTypeAnnotation
    (com: IPythonCompiler)
    ctx
    (repeatedGenerics: Set<string> option)
    (argType: Fable.Type)
    (returnType: Fable.Type)
    : Expression * Statement list
    =
    if isCallableType returnType then
        // Check if the returned callable also returns a callable
        let innerReturnType = getImmediateReturnType returnType

        if isCallableType innerReturnType then
            // Deeply nested: Callable[..., Any]
            let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
            stdlibModuleAnnotation com ctx "collections.abc" "Callable" [ Expression.ellipsis; any ], stmts
        else
            // Returned callable returns concrete type: Callable[..., Callable[..., ConcreteType]]
            let concreteReturnExpr, stmts =
                typeAnnotation com ctx repeatedGenerics innerReturnType

            let innerCallable =
                stdlibModuleAnnotation com ctx "collections.abc" "Callable" [ Expression.ellipsis; concreteReturnExpr ]

            stdlibModuleAnnotation com ctx "collections.abc" "Callable" [ Expression.ellipsis; innerCallable ], stmts
    else
        // Simple case: Callable[[A], B] where B is not a callable - preserve full types
        stdlibModuleTypeHint com ctx "collections.abc" "Callable" [ argType; returnType ] repeatedGenerics

let getEntityGenParams (ent: Fable.Entity) =
    ent.GenericParameters |> Seq.map (fun x -> x.Name) |> Set.ofSeq

/// Extract generic parameter names from a member's explicit generic parameters.
/// Used for abstract interface methods and other cases with explicit type parameters.
let getMemberGenParams (genParams: Fable.GenericParam list) =
    genParams |> List.map (fun p -> p.Name) |> Set.ofList

/// Create type parameters from a member's explicit generic parameters.
/// Returns empty list if member has no generic parameters.
/// Preserves type constraint bounds (e.g., 'T :> IDisposable becomes T: IDisposable in Python).
let makeMemberTypeParams (com: IPythonCompiler) ctx (genParams: Fable.GenericParam list) : TypeParam list =
    if genParams.Length > 0 then
        makeTypeParamsFromGenParams com ctx genParams
    else
        []

/// Try to convert a generic constraint type to its non-generic base type.
/// Python 3.12+ TypeVar bounds cannot use parameterized generic types,
/// so we map e.g., IEnumerable<'T> to IEnumerable (non-generic).
let private tryGetNonGenericBase (target: Fable.Type) : Fable.Type option =
    match target with
    | Fable.DeclaredType(entRef, _genArgs) ->
        match entRef.FullName with
        // IEnumerable<T> -> IEnumerable (non-generic)
        | Types.ienumerableGeneric ->
            let nonGenericRef: Fable.EntityRef =
                {
                    FullName = Types.ienumerable
                    Path = Fable.CoreAssemblyName "System.Runtime"
                }

            Some(Fable.DeclaredType(nonGenericRef, []))
        // Add other mappings here as needed:
        // Types.icomparableGeneric -> Types.icomparable, etc.
        | _ -> None
    | _ -> None

/// Extract bound type from CoercesTo constraint if present.
/// Returns the first CoercesTo constraint target type, or None if no such constraint exists.
/// For bounds with generic parameters, attempts to use a non-generic base type instead,
/// since Python 3.12+ TypeVar bounds cannot be parameterized.
let tryGetCoercesToBound (constraints: Fable.Constraint list) : Fable.Type option =
    constraints
    |> List.tryPick (
        function
        | Fable.Constraint.CoercesTo target ->
            // Python 3.12+ doesn't support parameterized generic types as bounds
            // e.g., T: IEnumerable[U] is invalid, only T: SomeNonGenericType works
            if containsGenericParams target then
                // Try to use a non-generic base type instead
                tryGetNonGenericBase target
            else
                Some target
        | _ -> None
    )

/// Create a TypeParam with optional bound from a GenericParam's constraints.
let makeTypeParamWithBound (com: IPythonCompiler) ctx (genParam: Fable.GenericParam) : TypeParam =
    let name = genParam.Name.ToUpperInvariant() |> Helpers.clean |> Identifier

    match tryGetCoercesToBound genParam.Constraints with
    | Some boundType ->
        let boundExpr, _stmts = typeAnnotation com ctx None boundType
        TypeParam.typeVar (name, bound = boundExpr)
    | None -> TypeParam.typeVar name

let makeTypeParams (com: IPythonCompiler) ctx (genParams: Set<string>) : TypeParam list =
    // Python 3.12+ syntax: create TypeParam list for class/function declaration
    genParams
    |> Set.toList
    |> List.map (fun genParam -> TypeParam.typeVar (Identifier(genParam.ToUpperInvariant() |> Helpers.clean)))

/// Create type parameters from GenericParam list, preserving constraint bounds.
/// Use this when you have full GenericParam objects with constraint information.
let makeTypeParamsFromGenParams (com: IPythonCompiler) ctx (genParams: Fable.GenericParam list) : TypeParam list =
    genParams |> List.map (makeTypeParamWithBound com ctx)

let makeFunctionTypeParams (com: IPythonCompiler) ctx (repeatedGenerics: Set<string>) : TypeParam list =
    // Python 3.12+ syntax: create TypeParam list for function declaration from repeated generics
    // Ensure deduplication by normalizing case and then converting back
    repeatedGenerics
    |> Set.map (fun genParam -> genParam.ToUpperInvariant() |> Helpers.clean)
    |> Set.toList
    |> List.map (fun genParam -> TypeParam.typeVar (Identifier(genParam)))

/// Create type parameters for a function, filtering by repeated generics and preserving constraint bounds.
/// This version accepts the full GenericParam list to extract constraint information.
let makeFunctionTypeParamsWithConstraints
    (com: IPythonCompiler)
    ctx
    (genParams: Fable.GenericParam list)
    (repeatedGenerics: Set<string>)
    : TypeParam list
    =
    // Filter to only the generic params that are in the repeated set
    let filteredParams =
        genParams |> List.filter (fun p -> repeatedGenerics.Contains p.Name)

    // For params that exist in genParams, use the constraint-aware version
    // For params only known by name (from signature), create without bounds
    let paramsWithConstraints =
        filteredParams |> List.map (makeTypeParamWithBound com ctx)

    // Find names that are in repeatedGenerics but not in genParams (signature-only generics)
    let explicitNames = genParams |> List.map (fun p -> p.Name) |> Set.ofList

    let signatureOnlyParams =
        repeatedGenerics
        |> Set.filter (fun name -> not (explicitNames.Contains name))
        |> Set.toList
        |> List.map (fun genParam -> genParam.ToUpperInvariant() |> Helpers.clean |> Identifier |> TypeParam.typeVar)

    paramsWithConstraints @ signatureOnlyParams

/// Calculate type parameters for a method from its Fable argument and return types.
/// This is used for object expression methods and other cases where we need to derive
/// type parameters from the method signature.
let calculateMethodTypeParams
    (com: IPythonCompiler)
    ctx
    (argTypes: Fable.Type list)
    (returnType: Fable.Type)
    : TypeParam list
    =
    let repeatedGenerics =
        getRepeatedGenericTypeParams ctx (argTypes @ [ returnType ]) |> Set.difference
        <| ctx.ScopedTypeParams

    makeFunctionTypeParams com ctx repeatedGenerics

let private libReflectionCall (com: IPythonCompiler) ctx r memberName args =
    libCall com ctx r "reflection" (memberName + "_type") args

let fableModuleAnnotation (com: IPythonCompiler) ctx moduleName memberName args =
    let expr = com.TransformImport(ctx, memberName, getLibPath com moduleName)

    match args with
    | [] -> expr
    | [ arg ] -> Expression.subscript (expr, arg)
    | args -> Expression.subscript (expr, Expression.tuple args)

let stdlibModuleAnnotation (com: IPythonCompiler) ctx moduleName memberName args =
    let expr = com.TransformImport(ctx, memberName, moduleName)

    match memberName, args with
    | "Callable", args ->
        let returnType = List.last args

        let args =
            match args with
            | Expression.Name { Id = Identifier Ellipsis } :: _xs -> Expression.ellipsis
            | _ ->
                args
                |> List.removeAt (args.Length - 1)
                |> List.choose (
                    function
                    | Expression.Name { Id = Identifier "None" } when args.Length = 2 -> None
                    | x -> Some x
                )
                |> Expression.list

        Expression.subscript (expr, Expression.tuple [ args; returnType ])
    | _, [] -> expr
    | _, [ arg ] -> Expression.subscript (expr, arg)
    | _, args -> Expression.subscript (expr, Expression.tuple args)

let fableModuleTypeHint com ctx moduleName memberName genArgs repeatedGenerics =
    let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
    fableModuleAnnotation com ctx moduleName memberName resolved, stmts

let stdlibModuleTypeHint com ctx moduleName memberName genArgs repeatedGenerics =
    let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
    stdlibModuleAnnotation com ctx moduleName memberName resolved, stmts

let makeGenTypeParamInst com ctx (genArgs: Fable.Type list) (repeatedGenerics: Set<string> option) =
    match genArgs with
    | [] -> []
    | _ -> genArgs |> List.map (typeAnnotation com ctx repeatedGenerics) |> List.map fst

let makeGenericTypeAnnotation
    (com: IPythonCompiler)
    ctx
    (id: string)
    (genArgs: Fable.Type list)
    (repeatedGenerics: Set<string> option)
    =
    stdlibModuleAnnotation com ctx "__future__" "annotations" [] |> ignore

    let typeParamInst = makeGenTypeParamInst com ctx genArgs repeatedGenerics

    let name = Expression.name id

    if typeParamInst.IsEmpty then
        name
    else
        Expression.subscript (name, Expression.tuple typeParamInst)

let makeGenericTypeAnnotation'
    (com: IPythonCompiler)
    ctx
    (id: string)
    (genArgs: string list)
    (repeatedGenerics: Set<string> option)
    =
    stdlibModuleAnnotation com ctx "__future__" "annotations" [] |> ignore

    let name = Expression.name id

    if genArgs.IsEmpty then
        name
    else
        let genArgs =
            match repeatedGenerics with
            | Some generics ->
                let genArgs = genArgs |> Set.ofList |> Set.intersect generics |> Set.toList

                if genArgs.IsEmpty then
                    [ stdlibModuleAnnotation com ctx "typing" "Any" [] ]
                else
                    genArgs |> List.map (fun name -> com.AddTypeVar(ctx, name))
            | _ -> genArgs |> List.map (fun name -> com.AddTypeVar(ctx, name))

        Expression.subscript (name, Expression.tuple genArgs)

let resolveGenerics com ctx generics repeatedGenerics : Expression list * Statement list =
    generics
    |> List.map (typeAnnotation com ctx repeatedGenerics)
    |> Helpers.unzipArgs

let typeAnnotation
    (com: IPythonCompiler)
    ctx
    (repeatedGenerics: Set<string> option)
    (t: Fable.Type)
    : Expression * Statement list
    =
    // printfn "typeAnnotation: %A" (t, repeatedGenerics)
    match t with
    | Fable.Measure _
    | Fable.Any -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
    | Fable.GenericParam(name = name) when name.StartsWith("$$", StringComparison.Ordinal) ->
        stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
    | Fable.GenericParam(name = name) ->
        match repeatedGenerics with
        | Some names when names.Contains name ->
            let name = Helpers.clean name
            com.AddTypeVar(ctx, name), []
        | Some _ -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
        | None ->
            let name = Helpers.clean name
            com.AddTypeVar(ctx, name), []
    | Fable.Unit -> Expression.none, []
    | Fable.Boolean -> Expression.name "bool", []
    | Fable.Char -> Expression.name "str", []
    | Fable.String -> Expression.name "str", []
    | Fable.Number(kind, info) -> makeNumberTypeAnnotation com ctx kind info
    | Fable.LambdaType(argType, returnType) -> makeLambdaTypeAnnotation com ctx repeatedGenerics argType returnType
    | Fable.DelegateType(argTypes, returnType) ->
        stdlibModuleTypeHint com ctx "collections.abc" "Callable" (argTypes @ [ returnType ]) repeatedGenerics
    | Fable.Nullable(genArg, isStruct) ->
        if isStruct then
            // For nullable value types, use T | None pattern similar to Option but without special handling
            let innerType, stmts = typeAnnotation com ctx repeatedGenerics genArg
            Expression.binOp (innerType, BinaryOrBitwise, Expression.none), stmts
        else
            typeAnnotation com ctx repeatedGenerics genArg // nullable reference types are erased
    | Fable.Option(Fable.Unit, _) ->
        // unit option -> Option[None] since it can be some(None) or None
        fableModuleAnnotation com ctx "option" "Option" [ Expression.none ], []
    | Fable.Option(genArg, _) ->
        // Must match mustWrapOption logic in Transforms.Util.fs
        // Wrap when: Any, Unit, GenericParam, or nested Option
        match genArg with
        | Fable.Option _
        | Fable.Any
        | Fable.Unit
        | Fable.GenericParam _ ->
            // Use full Option type annotation (code will use SomeWrapper)
            let resolved, stmts = resolveGenerics com ctx [ genArg ] repeatedGenerics
            fableModuleAnnotation com ctx "option" "Option" resolved, stmts
        | _ ->
            // For concrete types, erase to T | None (simpler, no wrapper needed)
            let resolved, stmts = typeAnnotation com ctx repeatedGenerics genArg
            Expression.binOp (resolved, BitOr, Expression.none), stmts
    | Fable.Tuple(genArgs, _) -> makeGenericTypeAnnotation com ctx "tuple" genArgs repeatedGenerics, []
    | Fable.Array(genArg, Fable.ArrayKind.ResizeArray) ->
        makeGenericTypeAnnotation com ctx "list" [ genArg ] repeatedGenerics, []
    | Fable.Array(genArg, _) -> fableModuleTypeHint com ctx "array_" "Array" [ genArg ] repeatedGenerics
    | Fable.List genArg -> fableModuleTypeHint com ctx "list" "FSharpList" [ genArg ] repeatedGenerics
    | Replacements.Util.Builtin kind as typ -> makeBuiltinTypeAnnotation com ctx typ repeatedGenerics kind
    | Fable.AnonymousRecordType(_, _genArgs, _) ->
        let value = Expression.name "dict"
        let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

        Expression.subscript (value, Expression.tuple [ Expression.name "str"; any ]), stmts
    | Fable.DeclaredType(entRef, genArgs) -> makeEntityTypeAnnotation com ctx entRef genArgs repeatedGenerics
    | _ -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

let makeNumberTypeAnnotation com ctx kind info =
    let numberInfo kind =
        let name =
            match kind with
            | Int8 -> "sbyte"
            | UInt8 -> "byte"
            | Int16 -> "int16"
            | UInt16 -> "uint16"
            | UInt32 -> "uint32"
            | Int64 -> "int64"
            | UInt64 -> "uint64"
            | Int32 -> "int32"
            | BigInt
            | Int128
            | UInt128
            | NativeInt
            | UNativeInt -> "int"
            | Float16
            | Float32 -> "float32"
            | Float64 -> "float64"
            | _ -> failwith $"Unsupported number type: %A{kind}"

        match name with
        | "int"
        | "float" -> Expression.name name
        | _ -> fableModuleAnnotation com ctx "core" name []


    match kind, info with
    | Decimal, _ -> stdlibModuleTypeHint com ctx "decimal" "Decimal" [] None
    | _ -> numberInfo kind, []

let makeImportTypeId (com: IPythonCompiler) ctx moduleName typeName =
    let expr = com.GetImportExpr(ctx, getLibPath com moduleName, typeName)

    match expr with
    | Expression.Name { Id = Identifier id } -> id
    | _ -> typeName

let makeImportTypeAnnotation com ctx genArgs moduleName typeName =
    let id = makeImportTypeId com ctx moduleName typeName
    makeGenericTypeAnnotation com ctx id genArgs None

let makeEntityTypeAnnotation com ctx (entRef: Fable.EntityRef) genArgs repeatedGenerics =
    // printfn "DeclaredType: %A" entRef.FullName
    match entRef.FullName, genArgs with
    | Types.result, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "result" "FSharpResult_2" resolved, stmts
    | Replacements.Util.BuiltinEntity _kind -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
    (*
        | Replacements.Util.BclGuid
        | Replacements.Util.BclTimeSpan
        | Replacements.Util.BclDateTime
        | Replacements.Util.BclDateTimeOffset
        | Replacements.Util.BclDateOnly
        | Replacements.Util.BclTimeOnly
        | Replacements.Util.BclTimer
        | Replacements.Util.BclBigInt -> genericEntity fullName [], []
        | Replacements.Util.BclHashSet gen
        | Replacements.Util.FSharpSet gen ->
            let gens, stmts = transformTypeInfo com ctx r genMap gen
            genericEntity fullName [ gens ], stmts
    | entName when entName.StartsWith(Types.choiceNonGeneric) ->
        makeUnionTypeAnnotation com ctx genArgs
        *)
    | Types.fsharpAsyncGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "async_builder" "Async" resolved, stmts
    | Types.taskGeneric, _ -> stdlibModuleTypeHint com ctx "collections.abc" "Awaitable" genArgs repeatedGenerics
    | Types.icomparable, _ -> libValue com ctx "protocols" "IComparable", []
    | Types.iStructuralEquatable, _ -> libValue com ctx "protocols" "IStructuralEquatable", []
    | Types.iStructuralComparable, _ -> libValue com ctx "protocols" "IStructuralComparable", []
    | Types.icomparerGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "IComparer_1" resolved, stmts
    | Types.iequalityComparer, _ -> libValue com ctx "protocols" "IEqualityComparer", []
    | Types.iequalityComparerGeneric, _ ->
        let resolved, stmts =
            stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

        fableModuleAnnotation com ctx "protocols" "IEqualityComparer_1" [ resolved ], stmts
    | Types.ienumerator, _ ->
        let resolved, stmts =
            stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

        fableModuleAnnotation com ctx "protocols" "IEnumerator" [ resolved ], stmts
    | Types.ienumeratorGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "IEnumerator" resolved, stmts
    | Types.ienumerable, _ -> fableModuleAnnotation com ctx "protocols" "IEnumerable" [], []
    | Types.ienumerableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "IEnumerable_1" resolved, stmts
    | Types.iequatableGeneric, _ ->
        let resolved, stmts =
            stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

        fableModuleAnnotation com ctx "protocols" "IEquatable" [ resolved ], stmts
    | Types.icomparableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "IComparable_1" resolved, stmts
    | Types.icollection, _
    | Types.icollectionGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "ICollection" resolved, stmts
    | Types.ilist, _
    | Types.ilistGeneric, _ ->
        // Map IList<T> to MutableSequence[T] which both list and FSharpArray implement
        stdlibModuleTypeHint com ctx "collections.abc" "MutableSequence" genArgs repeatedGenerics
    | Types.idisposable, _ -> libValue com ctx "protocols" "IDisposable", []
    | Types.iobserverGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "observable" "IObserver" resolved, stmts
    | Types.iobservableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "observable" "IObservable" resolved, stmts
    | Types.idictionary, _ -> stdlibModuleTypeHint com ctx "collections.abc" "Mapping" genArgs repeatedGenerics
    | Types.ievent2, _ ->
        // IEvent<'Delegate, 'Args> - only use Args (second param) since Delegate is phantom in Python
        let argsType = genArgs |> List.tryItem 1 |> Option.defaultValue Fable.Any
        let resolved, stmts = resolveGenerics com ctx [ argsType ] repeatedGenerics
        fableModuleAnnotation com ctx "event" "IEvent" resolved, stmts
    | Types.cancellationToken, _ -> libValue com ctx "async_builder" "CancellationToken", []
    | Types.mailboxProcessor, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "mailbox_processor" "MailboxProcessor" resolved, stmts
    // IFormatProvider is not used in Python, just map to Any
    | "System.IFormatProvider", _ -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
    // Py.Set/Map are used because fable-library-py reuses Set.fs/Map.fs from the ts folder
    | "Fable.Core.Py.Set`1", _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "ISet_1" resolved, stmts
    | "Py.Mapping.IMapping`2", _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "protocols" "IMap" resolved, stmts
    | "Fable.Core.Py.Callable", _ ->
        let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
        let genArgs = [ Expression.ellipsis; any ]

        stdlibModuleAnnotation com ctx "collections.abc" "Callable" genArgs, stmts
    | "Fable.Core.Py.Iterator`1", _ ->
        // Py.Iterator<'T> maps to collections.abc.Iterator[T]
        stdlibModuleTypeHint com ctx "collections.abc" "Iterator" genArgs repeatedGenerics
    | "Fable.Library.Python.Atom", _ ->
        // Atom[T] is a callable wrapper for mutable module-level values
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "Atom" resolved, stmts
    | _ ->
        let ent = com.GetEntity(entRef)
        // printfn "DeclaredType: %A" ent.FullName
        // Erased interfaces (with [<Erase>] attribute) don't exist at runtime, use Any
        let isErased =
            ent.Attributes |> Seq.exists (fun att -> att.Entity.FullName = Atts.erase)

        // Check for [<Global>] attribute - use the global name directly as the type annotation
        match com, ent.Attributes with
        | FSharp2Fable.Util.GlobalAtt(Some customName) ->
            // Use the custom global name (e.g., "list" for [<Global("list")>])
            makeGenericTypeAnnotation com ctx customName genArgs repeatedGenerics, []
        | FSharp2Fable.Util.GlobalAtt None ->
            // Use the entity's display name
            let name = Helpers.removeNamespace ent.FullName
            makeGenericTypeAnnotation com ctx name genArgs repeatedGenerics, []
        | _ when ent.IsInterface && not isErased ->
            let name = Helpers.removeNamespace ent.FullName

            // If the interface is imported then it's erased and we need to add the actual imports
            match com, ent.Attributes with
            | FSharp2Fable.Util.ImportAtt(name, importPath) -> com.GetImportExpr(ctx, importPath, name) |> ignore
            | _ ->
                match entRef.SourcePath with
                | Some path when path <> com.CurrentFile ->
                    // this is just to import the interface
                    let importPath = Path.getRelativeFileOrDirPath false com.CurrentFile false path

                    com.GetImportExpr(ctx, importPath, name) |> ignore
                | _ -> ()

            makeGenericTypeAnnotation com ctx name genArgs repeatedGenerics, []
        | _ when isErased ->
            // Erased types should use Any for type annotations
            stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
        | _ ->
            match tryPyConstructor com ctx ent with
            | Some(entRef, stmts) ->
                match entRef with
                (*
                | Literal(Literal.StringLiteral(StringLiteral(str, _))) ->
                    match str with
                    | "number" -> NumberTypeAnnotation
                    | "boolean" -> BooleanTypeAnnotation
                    | "string" -> StringTypeAnnotation
                    | _ -> AnyTypeAnnotation*)
                | Expression.Name { Id = Identifier id } ->
                    makeGenericTypeAnnotation com ctx id genArgs repeatedGenerics, stmts
                // TODO: Resolve references to types in nested modules
                | _ -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics
            | None -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

let makeBuiltinTypeAnnotation com ctx typ repeatedGenerics kind =
    match kind with
    | Replacements.Util.BclGuid -> stdlibModuleTypeHint com ctx "uuid" "UUID" [] repeatedGenerics
    | Replacements.Util.FSharpReference genArg ->
        let resolved, stmts = resolveGenerics com ctx [ genArg ] repeatedGenerics
        fableModuleAnnotation com ctx "core" "FSharpRef" resolved, stmts
    (*
    | Replacements.Util.BclTimeSpan -> NumberTypeAnnotation
    | Replacements.Util.BclDateTime -> makeSimpleTypeAnnotation com ctx "Date"
    | Replacements.Util.BclDateTimeOffset -> makeSimpleTypeAnnotation com ctx "Date"
    | Replacements.Util.BclDateOnly -> makeSimpleTypeAnnotation com ctx "Date"
    | Replacements.Util.BclTimeOnly -> NumberTypeAnnotation
    | Replacements.Util.BclTimer -> makeImportTypeAnnotation com ctx [] "Timer" "Timer"
    | Replacements.Util.BclDecimal -> makeImportTypeAnnotation com ctx [] "Decimal" "decimal"
    | Replacements.Util.BclBigInt -> makeImportTypeAnnotation com ctx [] "BigInt/z" "BigInteger"
    | Replacements.Util.BclHashSet key -> makeNativeTypeAnnotation com ctx [key] "Set"
    | Replacements.Util.BclDictionary (key, value) -> makeNativeTypeAnnotation com ctx [key; value] "Map"
    | Replacements.Util.BclKeyValuePair (key, value) -> makeTupleTypeAnnotation com ctx [key; value]
    | Replacements.Util.FSharpSet key -> makeImportTypeAnnotation com ctx [key] "Set" "FSharpSet"
    | Replacements.Util.FSharpMap (key, value) -> makeImportTypeAnnotation com ctx [key; value] "Map" "FSharpMap"
    | Replacements.Util.FSharpChoice genArgs ->
        $"FSharpChoice${List.length genArgs}"
        |> makeImportTypeAnnotation com ctx genArgs "Fable.Core"
    *)
    | Replacements.Util.FSharpResult(ok, err) ->
        let resolved, stmts = resolveGenerics com ctx [ ok; err ] repeatedGenerics

        fableModuleAnnotation com ctx "result" "FSharpResult_2" resolved, stmts
    | Replacements.Util.FSharpChoice genArgs ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        let name = $"FSharpChoice_%d{List.length genArgs}"
        fableModuleAnnotation com ctx "choice" name resolved, stmts
    | _ -> stdlibModuleTypeHint com ctx "typing" "Any" [] repeatedGenerics

let transformFunctionWithAnnotations
    (com: IPythonCompiler)
    ctx
    (name: string option)
    (args: Fable.Ident list)
    (body: Fable.Expr)
    =
    let argTypes = args |> List.map _.Type

    // In Python a generic type arg must appear both in the argument and the return type (cannot appear only once)
    let repeatedGenerics = getRepeatedGenericTypeParams ctx (argTypes @ [ body.Type ])

    let args', body' = com.TransformFunction(ctx, name, args, body, repeatedGenerics)

    let returnType, stmts = typeAnnotation com ctx (Some repeatedGenerics) body.Type

    // Calculate type parameters for generic functions (excluding already scoped params)
    let typeParams =
        Set.difference repeatedGenerics ctx.ScopedTypeParams
        |> makeFunctionTypeParams com ctx

    // Return type stays as T (not T | None) since we use Unit as default value
    // which preserves generic constraints: def foo[T](x: T = Unit) -> T
    args', stmts @ body', returnType, typeParams
