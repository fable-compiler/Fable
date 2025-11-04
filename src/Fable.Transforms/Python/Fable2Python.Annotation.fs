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

let getEntityGenParams (ent: Fable.Entity) =
    ent.GenericParameters |> Seq.map (fun x -> x.Name) |> Set.ofSeq

let makeTypeParams (com: IPythonCompiler) ctx (genParams: Set<string>) : TypeParam list =
    // Python 3.12+ syntax: create TypeParam list for class/function declaration
    genParams
    |> Set.toList
    |> List.map (fun genParam -> TypeParam.typeVar (Identifier(genParam.ToUpperInvariant() |> Helpers.clean)))

let makeFunctionTypeParams (com: IPythonCompiler) ctx (repeatedGenerics: Set<string>) : TypeParam list =
    // Python 3.12+ syntax: create TypeParam list for function declaration from repeated generics
    // Ensure deduplication by normalizing case and then converting back
    repeatedGenerics
    |> Set.map (fun genParam -> genParam.ToUpperInvariant() |> Helpers.clean)
    |> Set.toList
    |> List.map (fun genParam -> TypeParam.typeVar (Identifier(genParam)))

let extractGenericParamsFromMethodSignature
    (com: IPythonCompiler)
    ctx
    (args: Arguments)
    (returnType: Expression)
    : Set<string>
    =
    // Extract generic type parameters from method signature by looking for single uppercase letter type names
    let rec extractFromExpression (expr: Expression) : Set<string> =
        match expr with
        | Expression.Name { Id = Identifier name } when name.Length = 1 && System.Char.IsUpper(name.[0]) ->
            Set.singleton name
        | Expression.Subscript {
                                   Value = value
                                   Slice = slice
                               } -> Set.union (extractFromExpression value) (extractFromExpression slice)
        | Expression.Tuple { Elements = elements } -> elements |> List.map extractFromExpression |> Set.unionMany
        | Expression.BinOp {
                               Left = left
                               Right = right
                           } -> Set.union (extractFromExpression left) (extractFromExpression right)
        | _ -> Set.empty

    let argTypes =
        args.Args
        |> List.choose (fun arg -> arg.Annotation)
        |> List.map extractFromExpression
        |> Set.unionMany

    let returnTypes = extractFromExpression returnType

    Set.union argTypes returnTypes

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

let stdlibModuleTypeHint com ctx moduleName memberName genArgs =
    let resolved, stmts = resolveGenerics com ctx genArgs None
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
    | Fable.Any -> stdlibModuleTypeHint com ctx "typing" "Any" []
    | Fable.GenericParam(name = name) when name.StartsWith("$$", StringComparison.Ordinal) ->
        stdlibModuleTypeHint com ctx "typing" "Any" []
    | Fable.GenericParam(name = name) ->
        match repeatedGenerics with
        | Some names when names.Contains name ->
            let name = Helpers.clean name
            com.AddTypeVar(ctx, name), []
        | Some _ -> stdlibModuleTypeHint com ctx "typing" "Any" []
        | None ->
            let name = Helpers.clean name
            com.AddTypeVar(ctx, name), []
    | Fable.Unit -> Expression.none, []
    | Fable.Boolean -> Expression.name "bool", []
    | Fable.Char -> Expression.name "str", []
    | Fable.String -> Expression.name "str", []
    | Fable.Number(kind, info) -> makeNumberTypeAnnotation com ctx kind info
    | Fable.LambdaType(argType, returnType) ->
        let argTypes, returnType = uncurryLambdaType -1 [ argType ] returnType
        stdlibModuleTypeHint com ctx "collections.abc" "Callable" (argTypes @ [ returnType ])
    | Fable.DelegateType(argTypes, returnType) ->
        stdlibModuleTypeHint com ctx "collections.abc" "Callable" (argTypes @ [ returnType ])
    | Fable.Nullable(genArg, isStruct) ->
        if isStruct then
            // For nullable value types, use T | None pattern similar to Option but without special handling
            let innerType, stmts = typeAnnotation com ctx repeatedGenerics genArg
            Expression.binOp (innerType, BinaryOrBitwise, Expression.none), stmts
        else
            typeAnnotation com ctx repeatedGenerics genArg // nullable reference types are erased
    | Fable.Option(Fable.Unit, _) ->
        // unit option -> just None instead of None | None
        Expression.none, []
    | Fable.Option(genArg, _) ->
        // Check if this is a nested option (Option<Option<T>>)
        match genArg with
        | Fable.Option(_, _) ->
            // This is Option<Option<T>>, use the full Option type annotation
            let resolved, stmts = resolveGenerics com ctx [ genArg ] repeatedGenerics
            fableModuleAnnotation com ctx "option" "Option" resolved, []
        | _ ->
            // This is a simple Option<T>, use erased form T | None
            let innerType, stmts = typeAnnotation com ctx repeatedGenerics genArg
            Expression.binOp (innerType, BinaryOrBitwise, Expression.none), stmts
    | Fable.Tuple(genArgs, _) -> makeGenericTypeAnnotation com ctx "tuple" genArgs None, []
    | Fable.Array(genArg, Fable.ArrayKind.ResizeArray) -> makeGenericTypeAnnotation com ctx "list" [ genArg ] None, []
    | Fable.Array(genArg, _) -> fableModuleTypeHint com ctx "array_" "Array" [ genArg ] repeatedGenerics
    | Fable.List genArg -> fableModuleTypeHint com ctx "list" "FSharpList" [ genArg ] repeatedGenerics
    | Replacements.Util.Builtin kind -> makeBuiltinTypeAnnotation com ctx kind repeatedGenerics
    | Fable.AnonymousRecordType(_, _genArgs, _) ->
        let value = Expression.name "dict"
        let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

        Expression.subscript (value, Expression.tuple [ Expression.name "str"; any ]), stmts
    | Fable.DeclaredType(entRef, genArgs) -> makeEntityTypeAnnotation com ctx entRef genArgs repeatedGenerics
    | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []

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
        | _ -> fableModuleAnnotation com ctx "types" name []


    match kind, info with
    | Decimal, _ -> stdlibModuleTypeHint com ctx "decimal" "Decimal" []
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
    | Replacements.Util.BuiltinEntity _kind -> stdlibModuleTypeHint com ctx "typing" "Any" []
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
    | Types.taskGeneric, _ -> stdlibModuleTypeHint com ctx "typing" "Awaitable" genArgs
    | Types.icomparable, _ -> libValue com ctx "util" "IComparable", []
    | Types.iStructuralEquatable, _ -> libValue com ctx "util" "IStructuralEquatable", []
    | Types.iStructuralComparable, _ -> libValue com ctx "util" "IStructuralComparable", []
    | Types.icomparerGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "IComparer_1" resolved, stmts
    | Types.iequalityComparer, _ -> libValue com ctx "util" "IEqualityComparer", []
    | Types.iequalityComparerGeneric, _ ->
        let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []
        fableModuleAnnotation com ctx "util" "IEqualityComparer_1" [ resolved ], stmts
    | Types.ienumerator, _ ->
        let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []
        fableModuleAnnotation com ctx "util" "IEnumerator" [ resolved ], stmts
    | Types.ienumeratorGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "IEnumerator" resolved, stmts
    | Types.ienumerable, _ -> fableModuleAnnotation com ctx "util" "IEnumerable" [], []
    | Types.ienumerableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "IEnumerable_1" resolved, stmts
    | Types.iequatableGeneric, _ ->
        let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []
        fableModuleAnnotation com ctx "util" "IEquatable" [ resolved ], stmts
    | Types.icomparableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "IComparable_1" resolved, stmts
    | Types.icollection, _
    | Types.icollectionGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "ICollection" resolved, stmts
    | Types.idisposable, _ -> libValue com ctx "util" "IDisposable", []
    | Types.iobserverGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "observable" "IObserver" resolved, stmts
    | Types.iobservableGeneric, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "observable" "IObservable" resolved, stmts
    | Types.idictionary, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "util" "IDictionary" resolved, stmts
    | Types.ievent2, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "event" "IEvent_2" resolved, stmts
    | Types.cancellationToken, _ -> libValue com ctx "async_builder" "CancellationToken", []
    | Types.mailboxProcessor, _ ->
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics
        fableModuleAnnotation com ctx "mailbox_processor" "MailboxProcessor" resolved, stmts
    | "Fable.Core.Py.Callable", _ ->
        let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []
        let genArgs = [ Expression.ellipsis; any ]

        stdlibModuleAnnotation com ctx "collections.abc" "Callable" genArgs, stmts
    | _ ->
        let ent = com.GetEntity(entRef)
        // printfn "DeclaredType: %A" ent.FullName
        if ent.IsInterface then
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
        else
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
                | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []
            | None -> stdlibModuleTypeHint com ctx "typing" "Any" []

let makeBuiltinTypeAnnotation com ctx kind repeatedGenerics =
    match kind with
    | Replacements.Util.BclGuid -> Expression.name "str", []
    | Replacements.Util.FSharpReference genArg ->
        let resolved, stmts = resolveGenerics com ctx [ genArg ] repeatedGenerics
        fableModuleAnnotation com ctx "types" "FSharpRef" resolved, stmts
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
    | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []

let transformFunctionWithAnnotations (com: IPythonCompiler) ctx name (args: Fable.Ident list) (body: Fable.Expr) =
    // printfn "transformFunctionWithAnnotations: %A" (name, args, body.Type)
    let argTypes = args |> List.map _.Type

    // In Python a generic type arg must appear both in the argument and the return type (cannot appear only once)
    let repeatedGenerics = getRepeatedGenericTypeParams ctx (argTypes @ [ body.Type ])

    let args', body' = com.TransformFunction(ctx, name, args, body, repeatedGenerics)

    let returnType, stmts = typeAnnotation com ctx (Some repeatedGenerics) body.Type

    // If the only argument is generic, then we make the return type optional as well
    let returnType' =
        // printfn "Generic params: %A" (args, repeatedGenerics, body.Type)
        match args, body.Type with
        | [ { Type = Fable.GenericParam(name = x) } ], Fable.GenericParam(name = y) when
            x = y && Set.contains x repeatedGenerics
            ->
            Expression.binOp (returnType, BinaryOrBitwise, Expression.none)
        | _ -> returnType

    args', stmts @ body', returnType'
