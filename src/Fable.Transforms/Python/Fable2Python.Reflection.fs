module rec Fable.Transforms.Python.Reflection

open System
open Fable
open Fable.AST
open Fable.Py
open Fable.Transforms
open Fable.Transforms.Python.Types
open Fable.Transforms.Python.AST
open Fable.Transforms.Python.Util


open Lib

let private libReflectionCall (com: IPythonCompiler) ctx r memberName args =
    libCall com ctx r "reflection" (memberName + "_type") args

/// Wraps a Python list expression in Array(...) constructor
let private arrayExpr (com: IPythonCompiler) ctx (items: Expression list) =
    Expression.call (libValue com ctx "array_" "Array", [ Expression.list items ])

let private transformRecordReflectionInfo com ctx r (ent: Fable.Entity) generics =
    // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
    let fullname = ent.FullName
    let fullnameExpr = Expression.stringConstant fullname

    let genMap =
        let genParamNames =
            ent.GenericParameters |> List.mapToArray (fun x -> x.Name) |> Seq.toList

        List.zip genParamNames generics |> Map

    let fields, stmts =
        ent.FSharpFields
        |> Seq.map (fun fi ->
            let typeInfo, stmts = transformTypeInfo com ctx r genMap fi.FieldType

            let name =
                if Util.shouldUseRecordFieldNaming ent then
                    fi.Name |> Naming.toRecordFieldSnakeCase |> Helpers.clean
                else
                    fi.Name |> Naming.toSnakeCase |> Helpers.clean

            Expression.tuple [ Expression.stringConstant name; typeInfo ], stmts
        )
        |> Seq.toList
        |> Helpers.unzipArgs

    let fields = Expression.lambda (Arguments.arguments [], Expression.list fields)

    let py, stmts' = pyConstructor com ctx ent

    [ fullnameExpr; arrayExpr com ctx generics; py; fields ]
    |> libReflectionCall com ctx None "record",
    stmts @ stmts'

let private transformUnionReflectionInfo com ctx r (ent: Fable.Entity) generics =
    let fullname = ent.FullName
    let fullnameExpr = Expression.stringConstant fullname

    let genMap =
        let genParamNames =
            ent.GenericParameters |> List.map (fun x -> x.Name) |> Seq.toList

        List.zip genParamNames generics |> Map

    let cases =
        ent.UnionCases
        |> Seq.map (fun uci ->
            uci.UnionCaseFields
            |> List.map (fun fi ->
                Expression.tuple
                    [
                        fi.Name |> Expression.stringConstant
                        let expr, _stmts = transformTypeInfo com ctx r genMap fi.FieldType

                        expr
                    ]
            )
            |> Expression.list
        )
        |> Seq.toList

    let cases = Expression.lambda (Arguments.arguments [], Expression.list cases)

    let py, stmts = pyConstructor com ctx ent

    [ fullnameExpr; arrayExpr com ctx generics; py; cases ]
    |> libReflectionCall com ctx None "union",
    stmts

let transformTypeInfo (com: IPythonCompiler) ctx r (genMap: Map<string, Expression>) t : Expression * Statement list =
    let primitiveTypeInfo name =
        libValue com ctx "Reflection" (name + "_type")

    let numberInfo kind =
        getNumberKindName kind |> primitiveTypeInfo

    let nonGenericTypeInfo fullname =
        [ Expression.stringConstant fullname ] |> libReflectionCall com ctx None "class"

    let resolveGenerics generics : Expression list * Statement list =
        generics
        |> Array.map (transformTypeInfo com ctx r genMap)
        |> List.ofArray
        |> Helpers.unzipArgs

    let genericTypeInfo name genArgs =
        let resolved, stmts = resolveGenerics genArgs
        libReflectionCall com ctx None name resolved, stmts

    let genericEntity (fullname: string) (generics: Expression list) =
        libReflectionCall
            com
            ctx
            None
            "class"
            [
                Expression.stringConstant fullname
                if not (List.isEmpty generics) then
                    arrayExpr com ctx generics
            ]

    match t with
    | Fable.Measure _
    | Fable.Any -> primitiveTypeInfo "obj", []
    | Fable.GenericParam(name = name) ->
        match Map.tryFind name genMap with
        | Some t -> t, []
        | None ->
            Replacements.Util.genericTypeInfoError name |> addError com [] r

            Expression.none, []
    | Fable.Unit -> primitiveTypeInfo "unit", []
    | Fable.Boolean -> primitiveTypeInfo "bool", []
    | Fable.Char -> primitiveTypeInfo "char", []
    | Fable.String -> primitiveTypeInfo "string", []
    | Fable.Number(kind, info) ->
        match info with
        | Fable.NumberInfo.IsEnum entRef ->
            let ent = com.GetEntity(entRef)

            let cases =
                ent.FSharpFields
                |> Seq.choose (fun fi ->
                    match fi.Name with
                    | "value__" -> None
                    | name ->
                        let value =
                            match fi.LiteralValue with
                            | Some v -> Convert.ToInt32 v
                            | None -> 0

                        Expression.tuple [ Expression.stringConstant name; Expression.intConstant value ]
                        |> Some
                )
                |> Seq.toList
                |> Expression.list

            [ Expression.stringConstant entRef.FullName; numberInfo kind; cases ]
            |> libReflectionCall com ctx None "enum",
            []
        | _ -> numberInfo kind, []
    | Fable.LambdaType(argType, returnType) -> genericTypeInfo "lambda" [| argType; returnType |]
    | Fable.DelegateType(argTypes, returnType) -> genericTypeInfo "delegate" [| yield! argTypes; yield returnType |]
    | Fable.Tuple(genArgs, _) -> genericTypeInfo "tuple" (List.toArray genArgs)
    | Fable.Nullable(genArg, true) -> genericTypeInfo "option" [| genArg |]
    | Fable.Nullable(genArg, false) -> transformTypeInfo com ctx r genMap genArg
    | Fable.Option(genArg, _) -> genericTypeInfo "option" [| genArg |]
    | Fable.Array(genArg, Fable.ArrayKind.ResizeArray) -> genericTypeInfo "list" [| genArg |]
    | Fable.Array(genArg, _) -> genericTypeInfo "array" [| genArg |]
    | Fable.List genArg -> genericTypeInfo "list" [| genArg |]
    | Fable.Regex -> nonGenericTypeInfo Types.regex, []
    | Fable.MetaType -> nonGenericTypeInfo Types.type_, []
    | Fable.AnonymousRecordType(fieldNames, genArgs, _isStruct) ->
        let genArgs, stmts = resolveGenerics (List.toArray genArgs)

        List.zip (List.ofArray fieldNames) genArgs
        |> List.map (fun (k, t) -> Expression.tuple [ Expression.stringConstant k; t ])
        |> libReflectionCall com ctx None "anonRecord",
        stmts
    | Fable.DeclaredType(entRef, generics) ->
        let fullName = entRef.FullName

        match fullName, generics with
        | Replacements.Util.BuiltinEntity kind ->
            match kind with
            | Replacements.Util.BclGuid
            | Replacements.Util.BclTimeSpan
            | Replacements.Util.BclDateTime
            | Replacements.Util.BclDateTimeOffset
            | Replacements.Util.BclDateOnly
            | Replacements.Util.BclTimeOnly
            | Replacements.Util.BclTimer -> genericEntity fullName [], []
            | Replacements.Util.BclHashSet gen
            | Replacements.Util.FSharpSet gen ->
                let gens, stmts = transformTypeInfo com ctx r genMap gen
                genericEntity fullName [ gens ], stmts
            | Replacements.Util.BclDictionary(key, value)
            | Replacements.Util.BclKeyValuePair(key, value)
            | Replacements.Util.FSharpMap(key, value) ->
                let keys, stmts = transformTypeInfo com ctx r genMap key

                let values, stmts' = transformTypeInfo com ctx r genMap value

                genericEntity fullName [ keys; values ], stmts @ stmts'
            | Replacements.Util.FSharpResult(ok, err) ->
                let ent = com.GetEntity(entRef)
                let ok', stmts = transformTypeInfo com ctx r genMap ok
                let err', stmts' = transformTypeInfo com ctx r genMap err

                let expr, stmts'' = transformUnionReflectionInfo com ctx r ent [ ok'; err' ]

                expr, stmts @ stmts' @ stmts''
            | Replacements.Util.FSharpChoice gen ->
                let ent = com.GetEntity(entRef)

                let gen, stmts =
                    List.map (transformTypeInfo com ctx r genMap) gen |> Helpers.unzipArgs

                let expr, stmts' = gen |> transformUnionReflectionInfo com ctx r ent

                expr, stmts @ stmts'
            | Replacements.Util.FSharpReference gen ->
                let ent = com.GetEntity(entRef)
                let gen, stmts = transformTypeInfo com ctx r genMap gen
                let expr, stmts' = [ gen ] |> transformRecordReflectionInfo com ctx r ent

                expr, stmts @ stmts'
        | _ ->
            let ent = com.GetEntity(entRef)

            let generics, stmts =
                generics |> List.map (transformTypeInfo com ctx r genMap) |> Helpers.unzipArgs
            // Check if the entity is actually declared in Python code
            if
                ent.IsInterface
                || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                || FSharp2Fable.Util.isReplacementCandidate entRef
            then
                genericEntity ent.FullName generics, stmts
            else
                let reflectionMethodExpr =
                    FSharp2Fable.Util.entityIdentWithSuffix com entRef Naming.reflectionSuffix

                let callee, stmts' = com.TransformAsExpr(ctx, reflectionMethodExpr)

                Expression.call (callee, generics), stmts @ stmts'

let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
    if ent.IsFSharpRecord then
        transformRecordReflectionInfo com ctx r ent generics
    elif ent.IsFSharpUnion then
        transformUnionReflectionInfo com ctx r ent generics
    else
        let fullname = ent.FullName

        let exprs, stmts =
            [
                yield Expression.stringConstant fullname, []
                match generics with
                | [] -> yield Util.undefined None, []
                | generics -> yield arrayExpr com ctx generics, []
                match tryPyConstructor com ctx ent with
                | Some(Expression.Name { Id = name }, stmts) ->
                    yield Expression.name (name.Name |> Naming.toPythonNaming), stmts
                | Some(cons, stmts) -> yield cons, stmts
                | None -> ()
                match ent.BaseType with
                | Some d ->
                    let genMap =
                        Seq.zip ent.GenericParameters generics
                        |> Seq.map (fun (p, e) -> p.Name, e)
                        |> Map

                    yield
                        Fable.DeclaredType(d.Entity, d.GenericArgs)
                        |> transformTypeInfo com ctx r genMap
                | None -> ()
            ]
            |> Helpers.unzipArgs

        exprs |> libReflectionCall com ctx r "class", stmts


let transformTypeTest (com: IPythonCompiler) ctx range expr (typ: Fable.Type) : Expression * Statement list =
    let warnAndEvalToFalse msg =
        "Cannot type test (evals to false): " + msg |> addWarning com [] range

        Expression.boolConstant false

    let pyTypeof (primitiveType: string) (Util.TransformExpr com ctx (expr, stmts)) : Expression * Statement list =
        let typeof =
            let func = Expression.name (Identifier("type"))
            let str = Expression.name (Identifier("str"))
            let typ = Expression.call (func, [ expr ])
            Expression.call (str, [ typ ])

        Expression.compare (typeof, [ Eq ], [ Expression.stringConstant primitiveType ], ?loc = range), stmts

    let pyInstanceof consExpr (Util.TransformExpr com ctx (expr, stmts)) : Expression * Statement list =
        let func = Expression.name (Identifier("isinstance"))

        let args = [ expr; consExpr ]

        Expression.call (func, args), stmts

    match typ with
    | Fable.Measure _ // Dummy, shouldn't be possible to test against a measure type
    | Fable.Any -> Expression.boolConstant true, []
    | Fable.Unit ->
        let expr, stmts = com.TransformAsExpr(ctx, expr)

        Expression.compare (expr, [ Is ], [ Util.undefined None ], ?loc = range), stmts
    | Fable.Boolean -> pyTypeof "<class 'bool'>" expr
    | Fable.Char
    | Fable.String -> pyTypeof "<class 'str'>" expr
    | Fable.Number(kind, _b) ->
        match kind, typ with
        | _, Fable.Type.Number(UInt8, _) -> pyInstanceof (libValue com ctx "core" "uint8") expr
        | _, Fable.Type.Number(Int8, _) -> pyInstanceof (libValue com ctx "core" "int8") expr
        | _, Fable.Type.Number(Int16, _) -> pyInstanceof (libValue com ctx "core" "int16") expr
        | _, Fable.Type.Number(UInt16, _) -> pyInstanceof (libValue com ctx "core" "uint16") expr
        | _, Fable.Type.Number(Int32, _) -> pyInstanceof (libValue com ctx "core" "int32") expr
        | _, Fable.Type.Number(UInt32, _) -> pyInstanceof (libValue com ctx "core" "uint32") expr
        | _, Fable.Type.Number(NativeInt, _)
        | _, Fable.Type.Number(UNativeInt, _) -> pyInstanceof (Expression.name "int") expr
        | _, Fable.Type.Number(Int64, _) -> pyInstanceof (libValue com ctx "core" "int64") expr
        | _, Fable.Type.Number(UInt64, _) -> pyInstanceof (libValue com ctx "core" "uint64") expr
        | _, Fable.Type.Number(Float32, _) -> pyInstanceof (libValue com ctx "core" "float32") expr
        | _, Fable.Type.Number(Float64, _) -> pyInstanceof (libValue com ctx "core" "float64") expr
        | _, Fable.Type.Number(Decimal, _) -> pyTypeof "<class 'decimal.Decimal'>" expr
        | _ -> pyInstanceof (Expression.name "int") expr

    | Fable.Regex -> pyInstanceof (com.GetImportExpr(ctx, "re", "Pattern")) expr
    | Fable.LambdaType _
    | Fable.DelegateType _ -> pyTypeof "<class 'function'>" expr
    | Fable.Array _ ->
        // Use isinstance(x, Array) where Array is from fable_library.types
        pyInstanceof (libValue com ctx "array_" "Array") expr
    | Fable.Tuple _ ->
        // Use isinstance(x, tuple) for Python tuple type test
        pyInstanceof (Expression.name "tuple") expr
    | Fable.List _ -> pyInstanceof (libValue com ctx "List" "FSharpList") expr
    | Fable.AnonymousRecordType _ -> warnAndEvalToFalse "anonymous records", []
    | Fable.MetaType -> pyInstanceof (libValue com ctx "Reflection" "TypeInfo") expr
    | Fable.Nullable(genArg, _isStruct) ->
        // For nullable types, forward to the inner type (same as JS/TS implementation)
        transformTypeTest com ctx range expr genArg
    | Fable.Option _ -> warnAndEvalToFalse "options", [] // TODO
    | Fable.GenericParam _ -> warnAndEvalToFalse "generic parameters", []
    | Fable.DeclaredType(ent, genArgs) ->
        match ent.FullName with
        | Types.idisposable ->
            match expr with
            | MaybeCasted(ExprType(Fable.DeclaredType(ent2, _))) when
                com.GetEntity(ent2) |> FSharp2Fable.Util.hasInterface Types.idisposable
                ->
                Expression.boolConstant true, []
            | _ ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)
                libCall com ctx None "util" "isDisposable" [ expr ], stmts
        | Types.ienumerable ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            [ expr ] |> libCall com ctx None "util" "isIterable", stmts
        | Types.array ->
            // Use isinstance(x, Array) where Array is from fable_library.types
            pyInstanceof (libValue com ctx "array_" "Array") expr
        | Types.exception_ ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            [ expr ] |> libCall com ctx None "exceptions" "is_exception", stmts
        | Types.datetime -> pyInstanceof (com.GetImportExpr(ctx, "datetime", "datetime")) expr
        | _ ->
            let ent = com.GetEntity(ent)

            if ent.IsInterface then
                match FSharp2Fable.Util.tryGlobalOrImportedEntity com ent with
                | Some typeExpr ->
                    let typeExpr, stmts = com.TransformAsExpr(ctx, typeExpr)
                    let expr, stmts' = pyInstanceof typeExpr expr
                    expr, stmts @ stmts'
                | None -> warnAndEvalToFalse "interfaces", []
            else
                match tryPyConstructor com ctx ent with
                | Some(cons, stmts) ->
                    if not (List.isEmpty genArgs) then
                        com.WarnOnlyOnce("Generic args are ignored in type testing", ?range = range)

                    let expr, stmts' = pyInstanceof cons expr
                    expr, stmts @ stmts'
                | None -> warnAndEvalToFalse ent.FullName, []
