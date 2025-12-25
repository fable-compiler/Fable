module rec Fable.Transforms.Python.Transforms

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.Py
open Fable.Transforms.Python.AST
open Fable.Transforms
open Fable.Transforms.Python.Types
open Fable.Transforms.Python.Util
open Fable.Transforms.Python.Annotation
open Fable.Transforms.Python.Reflection

open Lib
open Util


/// Immediately Invoked Function Expression
let iife (com: IPythonCompiler) ctx (expr: Fable.Expr) =
    let afe, stmts =
        Annotation.transformFunctionWithAnnotations com ctx None [] expr
        |||> makeArrowFunctionExpression com ctx None (Some expr.Type)

    Expression.call (afe, []), stmts

let transformImport (com: IPythonCompiler) ctx (_r: SourceLocation option) (name: string) (moduleName: string) =
    let name, parts =
        let parts = Array.toList (name.Split('.'))
        parts.Head, parts.Tail

    com.GetImportExpr(ctx, moduleName, name) |> getParts com ctx parts

let getMemberArgsAndBody (com: IPythonCompiler) ctx kind hasSpread (args: Fable.Ident list) (body: Fable.Expr) =
    // printfn "getMemberArgsAndBody: %A" hasSpread
    let funcName, genTypeParams, args, body =
        match kind, args with
        | Attached(isStatic = false), thisArg :: args ->
            let genTypeParams =
                Set.difference (Annotation.getGenericTypeParams [ thisArg.Type ]) ctx.ScopedTypeParams

            let body =
                // TODO: If ident is not captured maybe we can just replace it with "this"
                if isIdentUsed thisArg.Name body then
                    let thisKeyword = Fable.IdentExpr { thisArg with Name = "self" }

                    Fable.Let(thisArg, thisKeyword, body)
                else
                    body

            None, genTypeParams, args, body
        | Attached(isStatic = true), _
        | ClassConstructor, _ -> None, ctx.ScopedTypeParams, args, body
        | NonAttached funcName, _ -> Some funcName, Set.empty, args, body
        | _ -> None, Set.empty, args, body

    let ctx =
        { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams }

    let args, body, returnType =
        Annotation.transformFunctionWithAnnotations com ctx funcName args body

    let args =
        let len = args.Args.Length

        if not hasSpread || len = 0 then
            args
        else
            { args with
                VarArg = Some { args.Args[len - 1] with Annotation = None }
                Args = args.Args[.. len - 2]
            }

    args, body, returnType

let getUnionCaseName (uci: Fable.UnionCase) =
    match uci.CompiledName with
    | Some cname -> cname
    | None -> uci.Name

let getUnionExprTag (com: IPythonCompiler) ctx r (fableExpr: Fable.Expr) =
    Expression.withStmts {
        let! expr = com.TransformAsExpr(ctx, fableExpr)
        let! finalExpr = getExpr com ctx r expr (Expression.stringConstant "tag")
        return finalExpr
    }

let wrapIntExpression typ (e: Expression) =
    match e, typ with
    | Expression.Constant _, _ -> e
    | _ -> e

let wrapExprInBlockWithReturn (e, stmts) = stmts @ [ Statement.return' e ]

let makeArrowFunctionExpression
    com
    ctx
    (name: string option)
    (bodyType: Fable.Type option)
    (args: Arguments)
    (body: Statement list)
    returnType
    : Expression * Statement list
    =
    let isAsync =
        match bodyType with
        | Some typ -> isTaskType typ
        | None -> false

    let args =
        match args.Args with
        | [] ->
            let ta = com.GetImportExpr(ctx, "typing", "Any")

            Arguments.arguments (args = [ Arg.arg ("__unit", annotation = ta) ], defaults = [ Expression.none ])
        | _ -> args

    let allDefaultsAreNone =
        args.Defaults
        |> List.forall (
            function
            | Expression.Name { Id = Identifier "None" } -> true
            | _ -> false
        )

    let (|ImmediatelyApplied|_|) =
        function
        | Expression.Call {
                              Func = callee
                              Args = appliedArgs
                          } when args.Args.Length = appliedArgs.Length && allDefaultsAreNone ->
            // To be sure we're not running side effects when deleting the function check the callee is an identifier
            match callee with
            | Expression.Name _ ->
                let parameters = args.Args |> List.map (fun a -> (Expression.name a.Arg))

                List.zip parameters appliedArgs
                |> List.forall (
                    function
                    | Expression.Name({ Id = Identifier name1 }), Expression.Name({ Id = Identifier name2 }) ->
                        name1 = name2
                    | _ -> false
                )
                |> function
                    | true -> Some callee
                    | false -> None
            | _ -> None
        | _ -> None

    match body with
    // Check if we can remove the function
    | [ Statement.Return { Value = Some(ImmediatelyApplied(callExpr)) } ] -> callExpr, []
    | _ ->
        let ident =
            name
            |> Option.map Identifier
            |> Option.defaultWith (fun _ -> Helpers.getUniqueIdentifier "_arrow")

        let func =
            if isAsync then
                createAsyncFunction ident args body [] returnType None
            else
                createFunction ident args body [] returnType None

        Expression.name ident, [ func ]

/// Check if a Fable type is a Task type (should generate async def)
let isTaskType (typ: Fable.Type) =
    match typ with
    | Fable.DeclaredType(ent, _) -> ent.FullName = Types.taskGeneric
    | _ -> false

let createFunction name args body decoratorList returnType (comment: string option) =
    createFunctionWithTypeParams name args body decoratorList returnType comment [] false

let createAsyncFunction name args body decoratorList returnType (comment: string option) =
    createFunctionWithTypeParams name args body decoratorList returnType comment [] true

let createFunctionWithTypeParams
    name
    args
    body
    decoratorList
    returnType
    (comment: string option)
    (typeParams: TypeParam list)
    (isAsync: bool)
    =
    if isAsync then
        Statement.asyncFunctionDef (
            name = name,
            args = args,
            body = Helpers.wrapReturnWithAwait body,
            decoratorList = decoratorList,
            returns = Helpers.unwrapTaskType returnType,
            typeParams = typeParams,
            ?comment = comment
        )
    else
        Statement.functionDef (
            name = name,
            args = args,
            body = body,
            decoratorList = decoratorList,
            returns = returnType,
            typeParams = typeParams,
            ?comment = comment
        )

let makeFunction name (args: Arguments, body: Expression, decoratorList, returnType) : Statement =
    // printfn "makeFunction: %A" name
    let body = wrapExprInBlockWithReturn (body, [])
    createFunction name args body decoratorList returnType None

let makeFunctionExpression
    (com: IPythonCompiler)
    ctx
    name
    (args, body: Expression, decoratorList, returnType: Expression)
    : Expression * Statement list
    =
    let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

    let name =
        name
        |> Option.map (fun name -> com.GetIdentifier(ctx, name))
        |> Option.defaultValue (Helpers.getUniqueIdentifier "_expr")

    let func = makeFunction name (args, body, decoratorList, returnType)
    Expression.name name, [ func ]

let optimizeTailCall (com: IPythonCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
    let rec checkCrossRefs tempVars allArgs =
        function
        | [] -> tempVars
        | (argId, _arg) :: rest ->
            let found =
                allArgs
                |> List.exists (
                    deepExists (
                        function
                        | Fable.IdentExpr i -> argId = i.Name
                        | _ -> false
                    )
                )

            let tempVars =
                if found then
                    let tempVarName = Util.getUniqueNameInDeclarationScope ctx (argId + "_tmp")

                    Map.add argId tempVarName tempVars
                else
                    tempVars

            checkCrossRefs tempVars allArgs rest

    ctx.OptimizeTailCall()

    let zippedArgs =
        List.zip (tc.Args |> List.map (fun { Arg = Identifier id } -> id)) args

    let tempVars = checkCrossRefs Map.empty args zippedArgs

    let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)

    [
        // First declare temp variables
        for KeyValue(argId, tempVar) in tempVars do
            yield! varDeclaration ctx (com.GetIdentifierAsExpr(ctx, tempVar)) None (com.GetIdentifierAsExpr(ctx, argId))
        // Then assign argument expressions to the original argument identifiers
        // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
        for argId, arg in zippedArgs do
            let arg = FableTransforms.replaceValues tempVarReplacements arg
            let arg, stmts = com.TransformAsExpr(ctx, arg)

            yield!
                stmts
                @ (assign None (com.GetIdentifierAsExpr(ctx, argId)) arg |> exprAsStatement ctx)
        yield Statement.continue' (?loc = range)
    ]

let transformCast (com: IPythonCompiler) (ctx: Context) t e : Expression * Statement list =
    // printfn "transformCast: %A" (t, e)
    match (t, e) with
    | IEnumerableOfKeyValuePair(kvpEnt) ->
        // Call .items() on the dictionary
        let dictExpr, stmts = com.TransformAsExpr(ctx, e)

        let itemsCall =
            Expression.attribute (value = dictExpr, attr = Identifier "items", ctx = Load)

        Expression.call (itemsCall, []), stmts
    // Optimization for (numeric) array or list literals casted to seq
    // Done at the very end of the compile pipeline to get more opportunities
    // of matching cast and literal expressions after resolving pipes, inlining...
    | Fable.DeclaredType(ent, [ _ ]), _ ->
        match ent.FullName, e with
        | Types.ienumerableGeneric, Replacements.Util.ArrayOrListLiteral(exprs, _typ) ->
            let expr, stmts =
                exprs |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs

            let xs = Expression.list expr
            libCall com ctx None "util" "to_enumerable" [ xs ], stmts

        | _ -> com.TransformAsExpr(ctx, e)
    | Fable.Number(Float32, _), _ ->
        let cons = libValue com ctx "types" "float32"
        let value, stmts = com.TransformAsExpr(ctx, e)
        Expression.call (cons, [ value ], ?loc = None), stmts
    | Fable.Number(Float64, _), _ ->
        let cons = libValue com ctx "types" "float64"
        let value, stmts = com.TransformAsExpr(ctx, e)
        Expression.call (cons, [ value ], ?loc = None), stmts
    | Fable.Number(Int32, _), _ ->
        let cons = libValue com ctx "types" "int32"
        let value, stmts = com.TransformAsExpr(ctx, e)
        Expression.call (cons, [ value ], ?loc = None), stmts
    | _ -> com.TransformAsExpr(ctx, e)

let transformCurry (com: IPythonCompiler) (ctx: Context) expr arity : Expression * Statement list =
    com.TransformAsExpr(ctx, Replacements.Api.curryExprAtRuntime com arity expr)


let transformValue (com: IPythonCompiler) (ctx: Context) r value : Expression * Statement list =
    match value with
    | Fable.BaseValue(None, _) -> Expression.identifier "super()", []
    | Fable.BaseValue(Some boundIdent, _) -> identAsExpr com ctx boundIdent, []
    | Fable.ThisValue _ -> Expression.identifier "self", []
    | Fable.TypeInfo(t, _) -> transformTypeInfo com ctx r Map.empty t
    | Fable.Null _t -> Expression.none, []
    | Fable.UnitConstant -> undefined r, []
    | Fable.BoolConstant x -> Expression.boolConstant (x, ?loc = r), []
    | Fable.CharConstant x -> Expression.stringConstant (string<char> x, ?loc = r), []
    | Fable.StringConstant x -> Expression.stringConstant (x, ?loc = r), []
    | Fable.StringTemplate(_, parts, values) ->
        match parts with
        | [] -> makeStrConst ""
        | [ part ] -> makeStrConst part
        | part :: parts ->
            let acc = makeStrConst part

            (acc, List.zip values parts)
            ||> List.fold (fun acc (MaybeCasted(value), part) ->
                let value =
                    match value.Type with
                    | Fable.String -> value
                    | _ -> Helpers.toString value

                let acc = makeBinOp None Fable.String acc value BinaryPlus

                makeBinOp None Fable.String acc (makeStrConst part) BinaryPlus
            )
        |> transformAsExpr com ctx
    | Fable.NumberConstant(v, _) ->
        match v with
        | Fable.NumberValue.Int8 x -> makeInteger com ctx r value.Type "int8" x
        | Fable.NumberValue.UInt8 x -> makeInteger com ctx r value.Type "uint8" x
        | Fable.NumberValue.Int16 x -> makeInteger com ctx r value.Type "int16" x
        | Fable.NumberValue.UInt16 x -> makeInteger com ctx r value.Type "uint16" x
        | Fable.NumberValue.Int32 x -> makeInteger com ctx r value.Type "int32" x
        | Fable.NumberValue.UInt32 x -> makeInteger com ctx r value.Type "uint32" x
        | Fable.NumberValue.Int64 x -> makeInteger com ctx r value.Type "int64" x
        | Fable.NumberValue.UInt64 x -> makeInteger com ctx r value.Type "uint64" x
        // | Fable.NumberValue.Int128(u,l) -> Expression.intConstant (System.Int128(u,l), ?loc = r), []
        // | Fable.NumberValue.UInt128(u,l) -> Expression.intConstant (System.UInt128(u,l), ?loc = r), []
        | Fable.NumberValue.BigInt x -> Expression.intConstant (x, ?loc = r), []
        | Fable.NumberValue.NativeInt x -> Expression.intConstant (x, ?loc = r), []
        | Fable.NumberValue.UNativeInt x -> Expression.intConstant (x, ?loc = r), []
        // TODO: special consts also need attention
        | Fable.NumberValue.Float64 x when x = infinity -> libValue com ctx "double" "float64.infinity", []
        | Fable.NumberValue.Float64 x when x = -infinity -> libValue com ctx "double" "float64.negative_infinity", []
        | Fable.NumberValue.Float64 x when Double.IsNaN(x) -> libValue com ctx "double" "float64.nan", []
        | Fable.NumberValue.Float32 x when Single.IsNaN(x) ->
            libCall com ctx r "types" "float32" [ Expression.stringConstant "nan" ], []
        | Fable.NumberValue.Float16 x when Single.IsNaN(x) ->
            libCall com ctx r "types" "float32" [ Expression.stringConstant "nan" ], []
        | Fable.NumberValue.Float16 x -> makeFloat com ctx r value.Type "float32" (float x)
        | Fable.NumberValue.Float32 x -> makeFloat com ctx r value.Type "float32" (float x)
        | Fable.NumberValue.Float64 x -> makeFloat com ctx r value.Type "float64" (float x)
        | Fable.NumberValue.Decimal x -> Py.Replacements.makeDecimal com r value.Type x |> transformAsExpr com ctx
        | _ -> addErrorAndReturnNull com r $"Numeric literal is not supported: %A{v}", []
    | Fable.NewArray(newKind, typ, kind) ->
        // printfn "NewArray: %A" (typ)

        match newKind with
        | Fable.ArrayValues values -> makeArray com ctx values kind typ
        | Fable.ArrayAlloc size -> makeArrayAllocated com ctx typ kind size
        | Fable.ArrayFrom expr -> makeArrayFrom com ctx typ kind expr

    | Fable.NewTuple(vals, _) -> makeTuple com ctx vals
    // Optimization for bundle size: compile list literals as List.ofArray
    | Fable.NewList(headAndTail, elementType) ->
        let rec getItems acc =
            function
            | None -> List.rev acc, None
            | Some(head, Fable.Value(Fable.NewList(tail, _), _)) -> getItems (head :: acc) tail
            | Some(head, tail) -> List.rev (head :: acc), Some tail

        match getItems [] headAndTail with
        | [], None -> libCall com ctx r "list" "empty" [], []
        | [ TransformExpr com ctx (expr, stmts) ], None -> libCall com ctx r "list" "singleton" [ expr ], stmts
        | exprs, None ->
            let expr, stmts = makeArray com ctx exprs Fable.MutableArray elementType
            [ expr ] |> libCall com ctx r "list" "ofArray", stmts
        | [ TransformExpr com ctx (head, stmts) ], Some(TransformExpr com ctx (tail, stmts')) ->
            libCall com ctx r "list" "cons" [ head; tail ], stmts @ stmts'
        | exprs, Some(TransformExpr com ctx (tail, stmts)) ->
            let expr, stmts' = makeArray com ctx exprs Fable.MutableArray elementType
            [ expr; tail ] |> libCall com ctx r "list" "ofArrayWithTail", stmts @ stmts'
    | Fable.NewOption(value, t, _) ->
        match value with
        | Some(TransformExpr com ctx (e, stmts)) ->
            if mustWrapOption t then
                libCall com ctx r "option" "some" [ e ], stmts
            else
                e, stmts
        | None -> undefined r, []
    | Fable.NewRecord(values, ent, _genArgs) ->
        let ent = com.GetEntity(ent)

        let values, stmts =
            List.map (fun x -> com.TransformAsExpr(ctx, x)) values |> Helpers.unzipArgs

        let consRef, stmts' = ent |> pyConstructor com ctx
        Expression.call (consRef, values, ?loc = r), stmts @ stmts'
    | Fable.NewAnonymousRecord(values, fieldNames, _genArgs, _isStruct) ->
        let values, stmts =
            values |> List.map (fun x -> com.TransformAsExpr(ctx, x)) |> Helpers.unzipArgs

        List.zip (List.ofArray fieldNames) values |> makePyObject, stmts
    | Fable.NewUnion(values, tag, ent, _genArgs) ->
        let ent = com.GetEntity(ent)

        let values, stmts =
            List.map (fun x -> com.TransformAsExpr(ctx, x)) values |> Helpers.unzipArgs

        let consRef, stmts' = ent |> pyConstructor com ctx
        // let caseName = ent.UnionCases |> List.item tag |> getUnionCaseName |> ofString
        let values = ofInt com ctx tag :: values
        Expression.call (consRef, values, ?loc = r), stmts @ stmts'
    | _ -> failwith $"transformValue: value %A{value} not supported!"

let extractBaseExprFromBaseCall (com: IPythonCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
    // printfn "extractBaseExprFromBaseCall: %A" (baseCall, baseType)
    match baseCall, baseType with
    | Some(Fable.Call(baseRef, info, _, _)), _ ->
        let baseExpr, stmts =
            match baseRef with
            | Fable.IdentExpr id -> com.GetIdentifierAsExpr(ctx, id.Name), []
            | _ -> transformAsExpr com ctx baseRef

        let expr, keywords, stmts' = transformCallArgs com ctx info true

        Some(baseExpr, (expr, keywords, stmts @ stmts'))
    | Some(Fable.ObjectExpr([], Fable.Unit, None)), _ ->
        let range = baseCall |> Option.bind (fun x -> x.Range)

        let name =
            baseType
            |> Option.map (fun t -> t.Entity.FullName)
            |> Option.defaultValue "unknown type"

        $"Ignoring base call for %s{name}" |> addWarning com [] range

        None
    | Some(Fable.Value _), Some baseType ->
        // let baseEnt = com.GetEntity(baseType.Entity)
        // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
        // let entityType = FSharp2Fable.Util.getEntityType baseEnt
        // let baseRefId = makeTypedIdent entityType entityName
        // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
        // Some (baseExpr, []) // default base constructor
        let range = baseCall |> Option.bind (fun x -> x.Range)

        $"Ignoring base call for %s{baseType.Entity.FullName}"
        |> addWarning com [] range

        None
    | Some _, _ ->
        let range = baseCall |> Option.bind (fun x -> x.Range)

        "Unexpected base call expression, please report" |> addError com [] range

        None
    | None, _ -> None

let transformObjectExpr
    (com: IPythonCompiler)
    ctx
    (members: Fable.ObjectExprMember list)
    typ
    baseCall
    : Expression * Statement list
    =
    // printfn "transformObjectExpr: %A" typ

    // A generic class nested in another generic class cannot use same type variables. (PEP-484)
    let ctx = { ctx with TypeParamsScope = ctx.TypeParamsScope + 1 }

    let makeMethod prop hasSpread (fableArgs: Fable.Ident list) (fableBody: Fable.Expr) decorators =
        let args, body, returnType =
            getMemberArgsAndBody com ctx (Attached(isStatic = false)) hasSpread fableArgs fableBody

        let name =
            let name =
                match prop with
                | "ToString" -> "__str__"
                | _ -> prop

            com.GetIdentifier(ctx, Naming.toPythonNaming name)

        let self = Arg.arg "self"

        let args =
            match decorators with
            // Remove extra parameters from getters, i.e __unit=None
            | [ Expression.Name { Id = Identifier "property" } ] ->
                { args with
                    Args = [ self ]
                    Defaults = []
                }
            | _ -> { args with Args = self :: args.Args }

        // Calculate type parameters for generic object expression methods
        let argTypes = fableArgs |> List.map _.Type

        let typeParams =
            Annotation.calculateMethodTypeParams com ctx argTypes fableBody.Type

        createFunctionWithTypeParams name args body decorators returnType None typeParams false

    /// Transform a callable property (delegate) into a method statement
    let transformCallableProperty (memb: Fable.ObjectExprMember) (fableArgs: Fable.Ident list) (fableBody: Fable.Expr) =
        // Transform the function directly without treating first arg as 'this'
        let args, body, returnType =
            Annotation.transformFunctionWithAnnotations com ctx None fableArgs fableBody

        let name = com.GetIdentifier(ctx, Naming.toPythonNaming memb.Name)
        let self = Arg.arg "self"
        let args = { args with Args = self :: args.Args }

        // Calculate type parameters for generic callable properties
        let argTypes = fableArgs |> List.map _.Type

        let typeParams =
            Annotation.calculateMethodTypeParams com ctx argTypes fableBody.Type

        createFunctionWithTypeParams name args body [] returnType None typeParams false

    let interfaces, stmts =
        match typ with
        | Fable.Any -> [], [] // Don't inherit from Any
        | _ ->
            let ta, stmts = Annotation.typeAnnotation com ctx None typ
            [ ta ], stmts

    let members =
        members
        |> List.collect (fun memb ->
            let info = com.GetMember(memb.MemberRef)

            if not memb.IsMangled && (info.IsGetter || info.IsValue) then
                match memb.Body with
                | Fable.Delegate(args, body, _, _) ->
                    // Transform callable property into method
                    [ transformCallableProperty memb args body ]
                | _ ->
                    // Regular property
                    let decorators = [ Expression.name "property" ]
                    [ makeMethod memb.Name false memb.Args memb.Body decorators ]
            elif not memb.IsMangled && info.IsSetter then
                let decorators = [ Expression.name $"%s{memb.Name}.setter" ]

                [ makeMethod memb.Name false memb.Args memb.Body decorators ]
            elif info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" then
                let method = makeMethod memb.Name info.HasSpread memb.Args memb.Body []

                let iterator =
                    let body = enumerator2iterator com ctx
                    let name = com.GetIdentifier(ctx, "__iter__")
                    let args = Arguments.arguments [ Arg.arg "self" ]

                    Statement.functionDef (name = name, args = args, body = body)

                [ method; iterator ]
            else
                [ makeMethod memb.Name info.HasSpread memb.Args memb.Body [] ]
        )

    let _baseExpr, classMembers =
        baseCall
        |> extractBaseExprFromBaseCall com ctx None
        |> Option.map (fun (baseExpr, (baseArgs, _kw, _stmts)) ->
            let consBody = [ callSuperAsStatement baseArgs ]
            let args = Arguments.empty
            let classCons = makeClassConstructor args false None com ctx consBody
            Some baseExpr, classCons @ members
        )
        |> Option.defaultValue (None, members)
        |> (fun (expr, memb) -> expr |> Option.toList, memb)

    let classBody =
        match classMembers with
        | [] -> [ Pass ]
        | _ -> classMembers

    let name = Helpers.getUniqueIdentifier "ObjectExpr"

    let stmt = Statement.classDef (name, body = classBody, bases = interfaces)

    Expression.call (Expression.name name), [ stmt ] @ stmts


let transformCallArgs
    (com: IPythonCompiler)
    ctx
    (callInfo: Fable.CallInfo)
    (isBaseConstructorCall: bool)
    : Expression list * Keyword list * Statement list
    =

    let args =
        FSharp2Fable.Util.dropUnitCallArg com callInfo.Args callInfo.SignatureArgTypes callInfo.MemberRef

    let paramsInfo =
        callInfo.MemberRef |> Option.bind com.TryGetMember |> Option.map getParamsInfo

    // Enhanced handling for constructor calls with named arguments
    let isConstructorCall =
        List.contains "new" callInfo.Tags && not isBaseConstructorCall

    let getCallArgs paramsInfo args =
        paramsInfo
        |> Option.map (splitNamedArgs args)
        |> function
            | None -> args, None, []
            | Some(args, None) -> args, None, []
            | Some(args, Some namedArgs) ->
                let objArg, stmts =
                    namedArgs
                    |> List.choose (fun (param, value) ->
                        match param.Name, value with
                        | Some keyword, Fable.Value(Fable.NewOption(value, _, _), _) ->
                            value |> Option.map (fun value -> keyword, value)
                        | Some keyword, value -> Some(keyword, value)
                        | None, _ -> None
                    )
                    |> List.map (fun (keyword, value) ->
                        let value, stmts = com.TransformAsExpr(ctx, value)
                        (keyword, value), stmts
                    )
                    |> List.unzip
                    |> fun (kv, stmts) ->
                        kv
                        |> List.map (fun (keyword, value) ->
                            Keyword.keyword (Identifier(Naming.toPythonNaming keyword), value)
                        ),
                        stmts |> List.collect id

                args, Some objArg, stmts

    let args, objArg, stmts =
        match paramsInfo, isConstructorCall with
        | Some info, true when args.Length <= info.Parameters.Length && args.Length > 0 ->
            // For constructor calls, check if we should use keyword arguments
            // Only apply if we have parameter names for all arguments
            let relevantParams = List.take args.Length info.Parameters

            let hasAllParameterNames = relevantParams |> List.forall (fun p -> p.Name.IsSome)

            if hasAllParameterNames then
                // Try to create keyword arguments for constructor calls
                let keywordArgs =
                    List.zip relevantParams args
                    |> List.choose (fun (param, arg) ->
                        match param.Name with
                        | Some paramName -> Some(paramName, arg)
                        | None -> None
                    )

                if keywordArgs.Length = args.Length then
                    // All parameters have names, convert to keyword arguments
                    let objArg, stmts =
                        keywordArgs
                        |> List.map (fun (kw, value) ->
                            let value, stmts = com.TransformAsExpr(ctx, value)
                            (kw, value), stmts
                        )
                        |> List.unzip
                        |> fun (kv, stmts) ->
                            kv
                            |> List.map (fun (keyword, value) ->
                                Keyword.keyword (Identifier(Naming.toPythonNaming keyword), value)
                            ),
                            stmts |> List.collect id

                    [], Some objArg, stmts
                else
                    // Fallback to regular handling
                    getCallArgs paramsInfo args
            else
                // Fallback to regular handling when not all parameters have names
                getCallArgs paramsInfo args
        | _ ->
            // Regular handling for non-constructor calls
            getCallArgs paramsInfo args

    let hasSpread =
        paramsInfo |> Option.map (fun i -> i.HasSpread) |> Option.defaultValue false

    let args, stmts' =
        match args with
        | [] -> [], []
        | args when hasSpread ->
            match List.rev args with
            | [] -> [], []
            | Replacements.Util.ArrayOrListLiteral(spreadArgs, _) :: rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))

                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
                |> Helpers.unzipArgs
            | last :: rest ->
                let rest, stmts =
                    List.rev rest
                    |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                    |> Helpers.unzipArgs

                let expr, stmts' = com.TransformAsExpr(ctx, last)
                rest @ [ Expression.starred expr ], stmts @ stmts'
        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args |> Helpers.unzipArgs

    match objArg with
    | None -> args, [], stmts @ stmts'
    | Some objArg -> args, objArg, stmts @ stmts'

let resolveExpr (ctx: Context) _t strategy pyExpr : Statement list =
    // printfn "resolveExpr: %A" (pyExpr, strategy)
    match strategy with
    | None
    | Some ReturnUnit -> exprAsStatement ctx pyExpr
    // TODO: Where to put these int wrappings? Add them also for function arguments?
    | Some(ResourceManager strategy) -> resolveExpr ctx _t strategy pyExpr
    | Some Return -> [ Statement.return' pyExpr ]
    | Some(Assign left) -> exprAsStatement ctx (assign None left pyExpr)
    | Some(Target left) -> exprAsStatement ctx (assign None (left |> Expression.identifier) pyExpr)

let transformOperation com ctx range opKind tags : Expression * Statement list =
    match opKind with
    | Fable.Unary(op, TransformExpr com ctx (expr, stmts)) -> Expression.unaryOp (op, expr, ?loc = range), stmts
    | Fable.Binary(op, left, right: Fable.Expr) ->
        let typ = right.Type
        let left_typ = left.Type
        let left, stmts = com.TransformAsExpr(ctx, left)
        let right, stmts' = com.TransformAsExpr(ctx, right)

        let compare op =
            Expression.compare (left, [ op ], [ right ], ?loc = range), stmts @ stmts'

        let (|IsNone|_|) =
            function
            | Name { Id = Identifier "None" } -> Some()
            | _ -> None

        let strict =
            match tags with
            | Fable.Tags.Contains "strict" -> true
            | _ -> false

        match op, strict with
        | BinaryEqual, true ->
            match left, right with
            // Use == with literals
            | Constant _, _ -> compare Eq
            | _, Constant _ -> compare Eq
            | _ -> compare Is
        | BinaryEqual, false ->
            match left, right with
            // Use == with literals
            | Constant _, _ -> compare Eq
            | _, Constant _ -> compare Eq
            // Use `is` with None (except literals)
            | _, IsNone -> compare Is
            | IsNone, _ -> compare Is
            // Use == for the rest
            | _ -> compare Eq
        | BinaryUnequal, true ->
            match left, right with
            // Use == with literals
            | Constant _, _ -> compare NotEq
            | _, Constant _ -> compare NotEq
            | _ -> compare IsNot
        | BinaryUnequal, false ->
            match left, right with
            // Use != with literals
            | Constant _, _ -> compare NotEq
            | _, Constant _ -> compare NotEq
            // Use `is not` with None (except literals)
            | _, IsNone -> compare IsNot
            | IsNone, _ -> compare IsNot
            // Use != for the rest
            | _ -> compare NotEq
        | BinaryLess, _ -> compare Lt
        | BinaryLessOrEqual, _ -> compare LtE
        | BinaryGreater, _ -> compare Gt
        | BinaryGreaterOrEqual, _ -> compare GtE
        | BinaryDivide, _ ->
            // For integer division, we need to use the // operator
            match typ with
            | Fable.Number(Int8, _)
            | Fable.Number(Int16, _)
            | Fable.Number(Int32, _)
            | Fable.Number(Int64, _)
            | Fable.Number(UInt8, _)
            | Fable.Number(UInt16, _)
            | Fable.Number(UInt32, _)
            | Fable.Number(UInt64, _) ->
                // In .NET we only get floor division for left integers on the left
                match left_typ with
                | Fable.Number(Float32, _)
                | Fable.Number(Float64, _) -> Expression.binOp (left, Div, right, ?loc = range), stmts @ stmts'
                | _ -> Expression.binOp (left, FloorDiv, right, ?loc = range), stmts @ stmts'
            | _ -> Expression.binOp (left, op, right, ?loc = range), stmts @ stmts'
        | _ -> Expression.binOp (left, op, right, ?loc = range), stmts @ stmts'

    | Fable.Logical(op, TransformExpr com ctx (left, stmts), TransformExpr com ctx (right, stmts')) ->
        Expression.boolOp (op, [ left; right ], ?loc = range), stmts @ stmts'

let transformEmit (com: IPythonCompiler) ctx range (info: Fable.EmitInfo) =
    let macro = info.Macro
    let info = info.CallInfo

    let thisArg, stmts =
        info.ThisArg
        |> Option.map (fun e -> com.TransformAsExpr(ctx, e))
        |> Option.toList
        |> Helpers.unzipArgs

    let exprs, kw, stmts' = transformCallArgs com ctx info false

    if macro.StartsWith("functools", StringComparison.Ordinal) then
        com.GetImportExpr(ctx, "functools") |> ignore

    let args = exprs |> List.append thisArg

    // Handle EmitMethod, EmitConstructor and other emit macros that need keywords
    match kw with
    | [] ->
        // No keywords, use regular emit expression
        emitExpression range macro args, stmts @ stmts'
    | _ ->
        // Handle emit patterns with keywords
        match tryParseEmitMethodMacro macro with
        | Some methodName ->
            match thisArg with
            | obj :: _ ->
                let methodCall = Expression.attribute (obj, Identifier methodName)
                callFunction range methodCall exprs kw, stmts @ stmts'
            | [] ->
                // Fallback to emit if no this arg
                emitExpression range macro args, stmts @ stmts'
        | None ->
            // Try EmitConstructor pattern
            match tryParseEmitConstructorMacro macro with
            | Some() ->
                match thisArg with
                | constructorExpr :: _ ->
                    // Handle EmitConstructor with keywords: new Constructor(keywords)
                    callFunction range constructorExpr exprs kw, stmts @ stmts'
                | [] ->
                    // Fallback to emit if no constructor expression
                    emitExpression range macro args, stmts @ stmts'
            | None ->
                // For other emit patterns with keywords, fallback to emit (might lose keywords)
                emitExpression range macro args, stmts @ stmts'

let transformCall (com: IPythonCompiler) ctx range callee (callInfo: Fable.CallInfo) : Expression * Statement list =
    // printfn "transformCall: %A" (callee, callInfo)
    let callee', stmts = com.TransformAsExpr(ctx, callee)

    let args, kw, stmts' = transformCallArgs com ctx callInfo false

    match callee, callInfo.ThisArg with
    | Fable.Get(expr, Fable.FieldGet { Name = "Dispose" }, _, _), _ ->
        let expr, stmts'' = com.TransformAsExpr(ctx, expr)

        libCall com ctx range "util" "dispose" [ expr ], stmts @ stmts' @ stmts''
    | Fable.Get(expr, Fable.FieldGet { Name = "set" }, _, _), _ ->
        // printfn "Type: %A" expr.Type
        Expression.withStmts {
            let! right = com.TransformAsExpr(ctx, callInfo.Args.Head)
            let! arg = com.TransformAsExpr(ctx, callInfo.Args.Tail.Head)
            let! value = com.TransformAsExpr(ctx, expr)
            return! Expression.none, [ Statement.assign ([ Expression.subscript (value, right) ], arg) ]
        }
    | Fable.Get(_, Fable.FieldGet { Name = "sort" }, _, _), _ -> callFunction range callee' [] kw, stmts @ stmts'

    | _, Some(TransformExpr com ctx (thisArg, stmts'')) ->
        callFunction range callee' (thisArg :: args) kw, stmts @ stmts' @ stmts''
    | _, None when List.contains "new" callInfo.Tags ->
        Expression.call (callee', args, kw, ?loc = range), stmts @ stmts'
    | _, None -> callFunction range callee' args kw, stmts @ stmts'

let transformCurriedApply com ctx range (TransformExpr com ctx (applied, stmts)) args =
    ((applied, stmts), args)
    ||> List.fold (fun (applied, stmts) arg ->
        let args, stmts' =
            match arg with
            // TODO: If arg type is unit but it's an expression with potential
            // side-effects, we need to extract it and execute it before the call

            // TODO: discardUnitArg may still be needed in some cases
            | Fable.Value(Fable.UnitConstant, _) -> [], []
            | Fable.IdentExpr ident when ident.Type = Fable.Unit -> [], []
            | TransformExpr com ctx (arg, stmts') -> [ arg ], stmts'

        callFunction range applied args [], stmts @ stmts'
    )

let transformCallAsStatements com ctx range t returnStrategy callee callInfo =
    let argsLen (i: Fable.CallInfo) =
        List.length i.Args
        + (if Option.isSome i.ThisArg then
               1
           else
               0)
    // Warn when there's a recursive call that couldn't be optimized?
    match returnStrategy, ctx.TailCallOpportunity with
    | Some(Return | ReturnUnit), Some tc when tc.IsRecursiveRef(callee) && argsLen callInfo = List.length tc.Args ->
        let args =
            match callInfo.ThisArg with
            | Some thisArg -> thisArg :: callInfo.Args
            | None -> callInfo.Args

        optimizeTailCall com ctx range tc args
    | _ ->
        let expr, stmts = transformCall com ctx range callee callInfo
        stmts @ (expr |> resolveExpr ctx t returnStrategy)

let transformCurriedApplyAsStatements com ctx range t returnStrategy callee args =
    // Warn when there's a recursive call that couldn't be optimized?
    match returnStrategy, ctx.TailCallOpportunity with
    | Some(Return | ReturnUnit), Some tc when tc.IsRecursiveRef(callee) && List.sameLength args tc.Args ->
        optimizeTailCall com ctx range tc args
    | _ ->
        let expr, stmts = transformCurriedApply com ctx range callee args
        stmts @ (expr |> resolveExpr ctx t returnStrategy)

let getNonLocals ctx (body: Statement list) =
    let body, nonLocals =
        body
        |> List.partition (
            function
            | Statement.NonLocal _
            | Statement.Global _ -> false
            | _ -> true
        )

    let nonLocal =
        nonLocals
        |> List.collect (
            function
            | Statement.NonLocal nl -> nl.Names
            | Statement.Global gl -> gl.Names
            | _ -> []
        )
        |> List.distinct
        |> (fun names ->
            match ctx.BoundVars.Inceptions with
            | 1 -> Statement.global' names
            | _ -> Statement.nonLocal names
        )

    [ nonLocal ], body

let transformBody (_com: IPythonCompiler) ctx _ret (body: Statement list) : Statement list =
    match body with
    | [] -> [ Pass ]
    | _ ->
        let nonLocals, body = getNonLocals ctx body
        nonLocals @ body

// When expecting a block, it's usually not necessary to wrap it
// in a lambda to isolate its variable context
let transformBlock (com: IPythonCompiler) ctx ret (expr: Fable.Expr) : Statement list =
    let block =
        com.TransformAsStatements(ctx, ret, expr)
        |> List.choose Helpers.isProductiveStatement

    match block with
    | [] -> [ Pass ]
    | _ -> block |> transformBody com ctx ret

/// Get the Python expression for an exception type
let private getExceptionTypeExpr (com: IPythonCompiler) ctx =
    function
    | Fable.DeclaredType(entRef, _) -> Annotation.tryPyConstructor com ctx (com.GetEntity(entRef))
    | _ -> None

let transformTryCatch com (ctx: Context) r returnStrategy (body, catch: option<Fable.Ident * Fable.Expr>, finalizer) =
    // try .. catch statements cannot be tail call optimized
    let ctx = { ctx with TailCallOpportunity = None }

    let makeHandler exnType handlerBody identifier =
        let transformedBody = transformBlock com ctx returnStrategy handlerBody
        ExceptHandler.exceptHandler (``type`` = Some exnType, name = identifier, body = transformedBody)

    let handlers, handlerStmts =
        match catch with
        | None -> None, []
        | Some(param, catchBody) ->
            let identifier = ident com ctx param

            let extractedHandlers, fallback =
                ExceptionHandling.extractExceptionHandlers catchBody

            match extractedHandlers with
            | [] ->
                // No type tests found, use BaseException to catch all exceptions including
                // KeyboardInterrupt, SystemExit, GeneratorExit which don't inherit from Exception
                let handler =
                    makeHandler (Expression.identifier "BaseException") catchBody identifier

                Some [ handler ], []

            | _ ->
                // Generate separate except clauses for each exception type
                let handlers, stmts =
                    extractedHandlers
                    |> List.choose (fun (typ, handlerBody) ->
                        getExceptionTypeExpr com ctx typ
                        |> Option.map (fun (exnTypeExpr, stmts) ->
                            makeHandler exnTypeExpr handlerBody identifier, stmts
                        )
                    )
                    |> List.unzip

                // Add fallback handler if fallback is not just a reraise
                // Use BaseException to catch all exceptions including KeyboardInterrupt etc.
                let fallbackHandlers =
                    match fallback with
                    | Some fallbackExpr when not (ExceptionHandling.isReraise fallbackExpr) ->
                        [ makeHandler (Expression.identifier "BaseException") fallbackExpr identifier ]
                    | _ -> []

                Some(handlers @ fallbackHandlers), List.concat stmts

    let finalizer, finalizerStmts =
        finalizer
        |> Option.map (fun fin ->
            transformBlock com ctx None fin
            |> List.partition (
                function
                | Statement.NonLocal _
                | Statement.Global _ -> false
                | _ -> true
            )
        )
        |> Option.defaultValue ([], [])

    handlerStmts
    @ finalizerStmts
    @ [
        Statement.try' (
            transformBlock com ctx returnStrategy body,
            ?handlers = handlers,
            finalBody = finalizer,
            ?loc = r
        )
    ]

/// Helper function to generate a cast statement for type narrowing
let makeCastStatement (com: IPythonCompiler) ctx (ident: Fable.Ident) (typ: Fable.Type) =
    // Only add cast for generic types where type checker needs help
    let hasGenerics =
        match typ with
        | Fable.DeclaredType(_, genArgs) when not (List.isEmpty genArgs) -> true
        | Fable.Array _ -> true
        | Fable.List _ -> true
        | _ -> false

    if hasGenerics then
        let cast = com.GetImportExpr(ctx, "typing", "cast")
        let varExpr = identAsExpr com ctx ident
        let typeAnnotation, importStmts = Annotation.typeAnnotation com ctx None typ
        let castExpr = Expression.call (cast, [ typeAnnotation; varExpr ])
        let castStmt = Statement.assign ([ varExpr ], castExpr)
        importStmts @ [ castStmt ]
    else
        []

let rec transformIfStatement (com: IPythonCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
    // printfn "transformIfStatement"

    // Create refined context for then branch if guard is a type test
    let thenCtx =
        match guardExpr with
        | Fable.Test(Fable.IdentExpr ident, Fable.TypeTest typ, _) ->
            { ctx with NarrowedTypes = Map.add ident.Name typ ctx.NarrowedTypes }
        | _ -> ctx

    let expr, stmts = com.TransformAsExpr(ctx, guardExpr)

    match expr with
    | Constant(value = BoolLiteral value) ->
        let e =
            if value then
                thenStmnt
            else
                elseStmnt

        stmts @ com.TransformAsStatements(ctx, ret, e)
    | guardExprPy ->
        let thenStmnt, stmts' =
            let thenBlockStmts = transformBlock com thenCtx ret thenStmnt

            // Add cast statement at the beginning of then branch for type tests
            let thenStmtsWithCast =
                match guardExpr with
                | Fable.Test(Fable.IdentExpr ident, Fable.TypeTest typ, _) ->
                    let castStmts = makeCastStatement com ctx ident typ
                    castStmts @ thenBlockStmts
                | _ -> thenBlockStmts

            thenStmtsWithCast
            |> List.partition (
                function
                | Statement.NonLocal _
                | Statement.Global _ -> false
                | _ -> true
            )

        let ifStatement, stmts'' =
            let block, stmts =
                transformBlock com ctx ret elseStmnt
                |> List.partition (
                    function
                    | Statement.NonLocal _
                    | Statement.Global _ -> false
                    | _ -> true
                )

            match block with
            | [] -> Statement.if' (guardExprPy, thenStmnt, ?loc = r), stmts
            | [ elseStmnt ] -> Statement.if' (guardExprPy, thenStmnt, [ elseStmnt ], ?loc = r), stmts
            | statements -> Statement.if' (guardExprPy, thenStmnt, statements, ?loc = r), stmts

        stmts @ stmts' @ stmts'' @ [ ifStatement ]


let transformGet (com: IPythonCompiler) ctx range typ (fableExpr: Fable.Expr) kind =
    // printfn "transformGet: %A" kind
    // printfn "transformGet: %A" (fableExpr.Type)

    match kind with
    | Fable.ExprGet(TransformExpr com ctx (prop, stmts)) ->
        let expr, stmts' =
            Expression.withStmts {
                let! expr = com.TransformAsExpr(ctx, fableExpr)
                let! finalExpr = getExpr com ctx range expr prop
                return finalExpr
            }

        expr, stmts @ stmts'

    | Fable.FieldGet i ->
        // Use effective type for field naming (considers type refinement)
        let narrowedType =
            match fableExpr with
            | Fable.IdentExpr ident -> getNarrowedType ctx ident
            | _ -> fableExpr.Type

        let fieldName = Util.applyFieldNaming com narrowedType i.Name true

        let fableExpr =
            match fableExpr with
            // If we're accessing a virtual member with default implementation (see #701)
            // from base class, we can use `super` in JS so we don't need the bound this arg
            | Fable.Value(Fable.BaseValue(_, t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
            | _ -> fableExpr

        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

        let subscript =
            match fableExpr.Type with
            | Fable.AnonymousRecordType _ -> true
            | Fable.GenericParam(_, _, [ Fable.Constraint.HasMember(_, false) ]) -> true
            | _ -> false
        // printfn "Fable.FieldGet: %A" (fieldName, fableExpr.Type)
        get com ctx range expr fieldName subscript, stmts

    | Fable.ListHead ->
        // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
        libCall com ctx range "list" "head" [ expr ], stmts

    | Fable.ListTail ->
        // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
        libCall com ctx range "list" "tail" [ expr ], stmts

    | Fable.TupleIndex index ->
        match fableExpr with
        // TODO: Check the erased expressions don't have side effects?
        | Fable.Value(Fable.NewTuple(exprs, _), _) -> com.TransformAsExpr(ctx, List.item index exprs)
        | TransformExpr com ctx (expr, stmts) ->
            let expr, stmts' = getExpr com ctx range expr (ofInt com ctx index)
            expr, stmts @ stmts'

    | Fable.OptionValue ->
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

        if mustWrapOption typ || com.Options.Language = TypeScript then
            libCall com ctx range "option" "value" [ expr ], stmts
        else
            expr, stmts

    | Fable.UnionTag ->
        let expr, stmts = getUnionExprTag com ctx range fableExpr
        expr, stmts

    | Fable.UnionField i ->
        Expression.withStmts {
            let! baseExpr = com.TransformAsExpr(ctx, fableExpr)
            let! fieldsExpr = getExpr com ctx range baseExpr (Expression.stringConstant "fields")
            let! finalExpr = getExpr com ctx range fieldsExpr (ofInt com ctx i.FieldIndex)
            return finalExpr
        }

let transformSet (com: IPythonCompiler) ctx range fableExpr typ (value: Fable.Expr) kind =
    // Normal handling for all assignments - StaticPropertyMeta will handle static properties
    //printfn "transformSet: %A" kind
    let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

    let value, stmts' =
        let value, st = com.TransformAsExpr(ctx, value)
        value |> wrapIntExpression typ, st

    let ret, stmts'' =
        match kind with
        | Fable.ValueSet -> expr, []
        | Fable.ExprSet(TransformExpr com ctx (e, stmts'')) ->
            let expr, stmts''' = getExpr com ctx None expr e
            expr, stmts'' @ stmts'''
        | Fable.FieldSet fieldName ->
            // Try to get the naming convention from the narrowed type (considers type refinement)
            let narrowedType =
                match fableExpr with
                | Fable.IdentExpr ident -> getNarrowedType ctx ident
                | _ -> fableExpr.Type

            let finalFieldName = Util.applyFieldNaming com narrowedType fieldName false
            let cleanFieldName = finalFieldName |> Helpers.clean
            get com ctx None expr cleanFieldName false, []

    assign range ret value, stmts @ stmts' @ stmts''

let transformBindingExprBody (com: IPythonCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
    match value with
    | Function(args, body) ->
        let name = Some var.Name

        Annotation.transformFunctionWithAnnotations com ctx name args body
        |||> makeArrowFunctionExpression com ctx name (Some body.Type)
    | _ ->
        let expr, stmt = com.TransformAsExpr(ctx, value)
        expr |> wrapIntExpression value.Type, stmt

let transformBindingAsExpr (com: IPythonCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    // printfn "transformBindingAsExpr: %A" (var, value)
    let expr, stmts = transformBindingExprBody com ctx var value
    expr |> assign None (identAsExpr com ctx var), stmts

let transformBindingAsStatements (com: IPythonCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    // printfn "transformBindingAsStatements: %A" (var, value)
    let shouldTreatAsStatement = isPyStatement ctx false value

    if shouldTreatAsStatement then
        let varName, varExpr = Expression.name var.Name, identAsExpr com ctx var

        ctx.BoundVars.Bind(var.Name)
        let ta, stmts = Annotation.typeAnnotation com ctx None var.Type
        let decl = Statement.assign (varName, ta)

        let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)

        stmts @ [ decl ] @ body
    else
        let value, stmts = transformBindingExprBody com ctx var value
        let varName = com.GetIdentifierAsExpr(ctx, Naming.toPythonNaming var.Name)
        let ta, stmts' = Annotation.typeAnnotation com ctx None var.Type
        let decl = varDeclaration ctx varName (Some ta) value
        stmts @ stmts' @ decl

let transformTest (com: IPythonCompiler) ctx range kind expr : Expression * Statement list =
    match kind with
    | Fable.TypeTest t -> transformTypeTest com ctx range expr t

    | Fable.OptionTest nonEmpty ->
        let op =
            if nonEmpty then
                IsNot
            else
                Is

        let expr, stmts = com.TransformAsExpr(ctx, expr)

        Expression.compare (expr, [ op ], [ Expression.none ], ?loc = range), stmts

    | Fable.ListTest nonEmpty ->
        let expr, stmts = com.TransformAsExpr(ctx, expr)
        let expr = libCall com ctx range "list" "isEmpty" [ expr ]

        if nonEmpty then
            Expression.unaryOp (UnaryNot, expr, ?loc = range), stmts
        else
            expr, stmts

    | Fable.UnionCaseTest tag ->
        let expected = ofInt com ctx tag
        let actual, stmts = getUnionExprTag com ctx None expr

        Expression.compare (actual, [ Eq ], [ expected ], ?loc = range), stmts

let transformSwitch (com: IPythonCompiler) ctx _useBlocks returnStrategy evalExpr cases defaultCase : Statement list =
    let cases =
        cases
        |> List.collect (fun (guards, expr) ->
            // Remove empty branches
            match returnStrategy, expr, guards with
            | None, Fable.Value(Fable.UnitConstant, _), _
            | _, _, [] -> []
            | _, _, guards ->
                let guards, lastGuard = List.splitLast guards

                let guards =
                    guards
                    |> List.map (fun e ->
                        let expr, stmts = com.TransformAsExpr(ctx, e)
                        (stmts, Some expr)
                    )

                let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)

                let caseBody =
                    match returnStrategy with
                    | Some Return -> caseBody
                    | _ -> List.append caseBody [ Statement.break' () ]

                let expr, stmts = com.TransformAsExpr(ctx, lastGuard)
                guards @ [ (stmts @ caseBody, Some expr) ]
        )

    let cases =
        match defaultCase with
        | Some expr ->
            let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)

            cases @ [ (defaultCaseBody, None) ]
        | None -> cases

    let value, stmts = com.TransformAsExpr(ctx, evalExpr)

    let rec ifThenElse
        (fallThrough: Expression option)
        (cases: (Statement list * Expression option) list)
        : Statement list
        =
        match cases with
        | [] -> []
        | (body, test) :: cases ->
            match test with
            | None -> body
            | Some test ->
                let expr = Expression.compare (left = value, ops = [ Eq ], comparators = [ test ])

                let test =
                    match fallThrough with
                    | Some ft -> Expression.boolOp (op = Or, values = [ ft; expr ])
                    | _ -> expr

                // Check for fallthrough
                if body.IsEmpty then
                    ifThenElse (Some test) cases
                else
                    // Remove any break statements from body
                    let body =
                        body
                        |> List.filter (
                            function
                            | Statement.Break -> false
                            | _ -> true
                        )
                        |> function
                            // Make sure we don't have an empty body
                            | [] -> [ Statement.Pass ]
                            | body -> body

                    let nonLocals, body = getNonLocals ctx body

                    let nonLocals, orElse =
                        ifThenElse None cases |> List.append nonLocals |> getNonLocals ctx

                    nonLocals @ [ Statement.if' (test = test, body = body, orelse = orElse) ]

    let result = cases |> ifThenElse None

    match result with
    | [] -> []
    | ifStmt -> stmts @ ifStmt

let matchTargetIdentAndValues idents values =
    if List.isEmpty idents then
        []
    elif List.sameLength idents values then
        List.zip idents values
    else
        failwith "Target idents/values lengths differ"

let getDecisionTargetAndBoundValues (com: IPythonCompiler) (ctx: Context) targetIndex boundValues =
    let idents, target = getDecisionTarget ctx targetIndex
    let identsAndValues = matchTargetIdentAndValues idents boundValues

    if not com.Options.DebugMode then
        let bindings, replacements =
            (([], Map.empty), identsAndValues)
            ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                if canHaveSideEffects com expr then
                    (ident, expr) :: bindings, replacements
                else
                    bindings, Map.add ident.Name expr replacements
            )

        let target = FableTransforms.replaceValues replacements target
        target, List.rev bindings
    else
        target, identsAndValues

let transformDecisionTreeSuccessAsExpr (com: IPythonCompiler) (ctx: Context) targetIndex boundValues =
    let target, bindings =
        getDecisionTargetAndBoundValues com ctx targetIndex boundValues

    match bindings with
    | [] -> com.TransformAsExpr(ctx, target)
    | bindings ->
        let target =
            List.rev bindings |> List.fold (fun e (i, v) -> Fable.Let(i, v, e)) target

        com.TransformAsExpr(ctx, target)

let exprAsStatement (ctx: Context) (expr: Expression) : Statement list =
    // printfn "exprAsStatement: %A" expr
    match expr with
    // A single None will be removed (i.e transformCall may return None)
    | Name { Id = Identifier "None" } -> []
    | NamedExpr({
                    Target = target
                    Value = value
                    Loc = _
                }) ->
        let nonLocals =
            match target with
            | Expression.Name { Id = id } -> ctx.BoundVars.NonLocals([ id ]) |> Statement.nonLocal |> List.singleton
            | _ -> []

        // printfn "Nonlocals: %A" nonLocals
        nonLocals @ [ Statement.assign ([ target ], value) ]
    | _ -> [ Statement.expr expr ]

let transformDecisionTreeSuccessAsStatements
    (com: IPythonCompiler)
    (ctx: Context)
    returnStrategy
    targetIndex
    boundValues
    : Statement list
    =
    match returnStrategy with
    | Some(Target targetId) as _target ->
        let idents, _ = getDecisionTarget ctx targetIndex

        let assignments =
            matchTargetIdentAndValues idents boundValues
            |> List.collect (fun (id, TransformExpr com ctx (value, stmts)) ->
                let stmts' = assign None (identAsExpr com ctx id) value |> exprAsStatement ctx

                stmts @ stmts'
            )

        let targetAssignment =
            assign None (targetId |> Expression.name) (ofInt com ctx targetIndex)
            |> exprAsStatement ctx

        targetAssignment @ assignments
    | ret ->
        let target, bindings =
            getDecisionTargetAndBoundValues com ctx targetIndex boundValues

        let bindings =
            bindings
            |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            |> Seq.toList

        bindings @ com.TransformAsStatements(ctx, ret, target)

let transformDecisionTreeAsSwitch expr =
    let (|Equals|_|) =
        function
        | Fable.Operation(Fable.Binary(BinaryEqual, expr, right), _, _, _) ->
            match expr with
            | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _), _) ->
                Some(expr, right)
            | _ -> None
        | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
            let evalExpr =
                Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), None)

            let right = makeIntConst tag
            Some(evalExpr, right)
        | _ -> None

    let sameEvalExprs evalExpr1 evalExpr2 =
        match evalExpr1, evalExpr2 with
        | Fable.IdentExpr i1, Fable.IdentExpr i2
        | Fable.Get(Fable.IdentExpr i1, Fable.UnionTag, _, _), Fable.Get(Fable.IdentExpr i2, Fable.UnionTag, _, _) ->
            i1.Name = i2.Name
        | _ -> false

    let rec checkInner cases evalExpr =
        function
        | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                           treeExpr,
                           _) when sameEvalExprs evalExpr evalExpr2 ->
            match treeExpr with
            | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                let cases = (caseExpr, targetIndex, boundValues) :: cases |> List.rev

                Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
            | treeExpr -> checkInner ((caseExpr, targetIndex, boundValues) :: cases) evalExpr treeExpr
        | _ -> None

    match expr with
    | Fable.IfThenElse(Equals(evalExpr, caseExpr), Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
        match checkInner [ caseExpr, targetIndex, boundValues ] evalExpr treeExpr with
        | Some(evalExpr, cases, defaultCase) -> Some(evalExpr, cases, defaultCase)
        | None -> None
    | _ -> None

let transformDecisionTreeAsExpr (com: IPythonCompiler) (ctx: Context) targets expr : Expression * Statement list =
    // TODO: Check if some targets are referenced multiple times
    let ctx = { ctx with DecisionTargets = targets }
    com.TransformAsExpr(ctx, expr)

let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) (defaultIndex, defaultBoundValues) =
    cases
    |> List.groupBy (fun (_, idx, boundValues) ->
        // Try to group cases with some target index and empty bound values
        // If bound values are non-empty use also a non-empty Guid to prevent grouping
        if List.isEmpty boundValues then
            idx, Guid.Empty
        else
            idx, Guid.NewGuid()
    )
    |> List.map (fun ((idx, _), cases) ->
        let caseExprs = cases |> List.map Tuple3.item1
        // If there are multiple cases, it means boundValues are empty
        // (see `groupBy` above), so it doesn't mind which one we take as reference
        let boundValues = cases |> List.head |> Tuple3.item3
        caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t)
    )
    |> function
        | [] -> []
        // Check if the last case can also be grouped with the default branch, see #2357
        | cases when List.isEmpty defaultBoundValues ->
            match List.splitLast cases with
            | cases, (_, Fable.DecisionTreeSuccess(idx, [], _)) when idx = defaultIndex -> cases
            | _ -> cases
        | cases -> cases

let getTargetsWithMultipleReferences expr =
    let rec findSuccess (targetRefs: Map<int, int>) =
        function
        | [] -> targetRefs
        | expr :: exprs ->
            match expr with
            // We shouldn't actually see this, but shortcircuit just in case
            | Fable.DecisionTree _ -> findSuccess targetRefs exprs
            | Fable.DecisionTreeSuccess(idx, _, _) ->
                let count = Map.tryFind idx targetRefs |> Option.defaultValue 0

                let targetRefs = Map.add idx (count + 1) targetRefs
                findSuccess targetRefs exprs
            | expr ->
                let exprs2 = getSubExpressions expr
                findSuccess targetRefs (exprs @ exprs2)

    findSuccess Map.empty [ expr ]
    |> Seq.choose (fun kv ->
        if kv.Value > 1 then
            Some kv.Key
        else
            None
    )
    |> Seq.toList

/// When several branches share target create first a switch to get the target index and bind value
/// and another to execute the actual target
let transformDecisionTreeWithTwoSwitches
    (com: IPythonCompiler)
    ctx
    returnStrategy
    (targets: (Fable.Ident list * Fable.Expr) list)
    treeExpr
    =
    // Declare target and bound idents
    let targetId =
        getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent

    let multiVarDecl =
        let boundIdents =
            targets
            |> List.collect (fun (idents, _) -> idents)
            |> List.map (fun id -> ident com ctx id, None)

        multiVarDeclaration ctx ((ident com ctx targetId, None) :: boundIdents)
    // Transform targets as switch
    let switch2 =
        // TODO: Declare the last case as the default case?
        let cases = targets |> List.mapi (fun i (_, target) -> [ makeIntConst i ], target)

        transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None

    // Transform decision tree
    let targetAssign = Target(ident com ctx targetId)
    let ctx = { ctx with DecisionTargets = targets }

    match transformDecisionTreeAsSwitch treeExpr with
    | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
        let cases =
            groupSwitchCases (Fable.Number(Int32, Fable.NumberInfo.Empty)) cases (defaultIndex, defaultBoundValues)

        let defaultCase =
            Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number(Int32, Fable.NumberInfo.Empty))

        let switch1 =
            transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)

        multiVarDecl @ switch1 @ switch2
    | None ->
        let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)

        multiVarDecl @ decisionTree @ switch2

let transformDecisionTreeAsStatements
    (com: IPythonCompiler)
    (ctx: Context)
    returnStrategy
    (targets: (Fable.Ident list * Fable.Expr) list)
    (treeExpr: Fable.Expr)
    : Statement list
    =
    // If some targets are referenced multiple times, hoist bound idents,
    // resolve the decision index and compile the targets as a switch
    let targetsWithMultiRefs =
        if com.Options.Language = TypeScript then
            [] // no hoisting when compiled with types
        else
            getTargetsWithMultipleReferences treeExpr

    match targetsWithMultiRefs with
    | [] ->
        let ctx = { ctx with DecisionTargets = targets }

        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let t = treeExpr.Type

            let cases =
                cases
                |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                    [ caseExpr ], Fable.DecisionTreeSuccess(targetIndex, boundValues, t)
                )

            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)

            transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)
        | None -> com.TransformAsStatements(ctx, returnStrategy, treeExpr)
    | targetsWithMultiRefs ->
        // If the bound idents are not referenced in the target, remove them
        let targets =
            targets
            |> List.map (fun (idents, expr) ->
                idents
                |> List.exists (fun i -> isIdentUsed i.Name expr)
                |> function
                    | true -> idents, expr
                    | false -> [], expr
            )

        let hasAnyTargetWithMultiRefsBoundValues =
            targetsWithMultiRefs
            |> List.exists (fun idx -> targets[idx] |> fst |> List.isEmpty |> not)

        if not hasAnyTargetWithMultiRefsBoundValues then
            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let t = treeExpr.Type

                let cases = groupSwitchCases t cases (defaultIndex, defaultBoundValues)

                let ctx = { ctx with DecisionTargets = targets }

                let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)

                transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)
            | None -> transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
        else
            transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

let transformSequenceExpr (com: IPythonCompiler) ctx (exprs: Fable.Expr list) : Expression * Statement list =
    // printfn "transformSequenceExpr"
    let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

    let body =
        exprs
        |> List.collecti (fun i e ->
            let expr, stmts = com.TransformAsExpr(ctx, e)
            // Return the last expression
            if i = exprs.Length - 1 then
                stmts @ [ Statement.return' expr ]
            else
                stmts @ exprAsStatement ctx expr
        )
        |> transformBody com ctx None

    let name = Helpers.getUniqueIdentifier "_expr"

    let func =
        Statement.functionDef (name = name, args = Arguments.arguments [], body = body)

    let name = Expression.name name
    Expression.call name, [ func ]

let transformSequenceExpr'
    (_com: IPythonCompiler)
    ctx
    (exprs: Expression list)
    (stmts: Statement list)
    : Expression * Statement list
    =
    // printfn "transformSequenceExpr2', exprs: %A" exprs.Length
    // printfn "ctx: %A" ctx.BoundVars
    let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

    let body =
        exprs
        |> List.collecti (fun i expr ->
            // Return the last expression
            if i = exprs.Length - 1 then
                stmts @ [ Statement.return' expr ]
            else
                exprAsStatement ctx expr
        )

    let name = Helpers.getUniqueIdentifier "_expr"

    let func =
        Statement.functionDef (name = name, args = Arguments.arguments [], body = body)

    let name = Expression.name name
    Expression.call name, [ func ]

let rec transformAsExpr (com: IPythonCompiler) ctx (expr: Fable.Expr) : Expression * Statement list =
    // printfn "transformAsExpr: %A" expr
    match expr with
    | Fable.Unresolved(_, _, r) -> addErrorAndReturnNull com r "Unexpected unresolved expression", []
    | Fable.TypeCast(e, t) -> transformCast com ctx t e
    | Fable.Value(kind, r) -> transformValue com ctx r kind
    | Fable.IdentExpr id -> identAsExpr com ctx id, []
    | Fable.Import({
                       Selector = selector
                       Path = path
                   },
                   _,
                   r) ->
        // printfn "Fable.Import: %A" (selector, path)
        transformImport com ctx r selector path, []

    | Fable.Test(expr, kind, range) -> transformTest com ctx range kind expr

    | Fable.Lambda(arg, body, name) ->
        Annotation.transformFunctionWithAnnotations com ctx name [ arg ] body
        |||> makeArrowFunctionExpression com ctx name (Some body.Type)

    | Fable.Delegate(args, body, name, _) ->
        Annotation.transformFunctionWithAnnotations com ctx name args body
        |||> makeArrowFunctionExpression com ctx name (Some body.Type)

    | Fable.ObjectExpr([], typ, None) ->
        // Check if the type is an interface
        match typ with
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)

            if ent.IsInterface then
                // Use cast with SimpleNamespace for interfaces to avoid type errors
                let cast = com.GetImportExpr(ctx, "typing", "cast")
                let simpleNamespace = com.GetImportExpr(ctx, "types", "SimpleNamespace")
                let typeAnnotation, _ = Annotation.typeAnnotation com ctx None typ
                Expression.call (cast, [ typeAnnotation; Expression.call (simpleNamespace, []) ]), []
            else
                Expression.dict (), []
        | _ -> Expression.dict (), []
    | Fable.ObjectExpr(members, typ, baseCall) ->
        // printfn "members: %A" (members, typ)
        transformObjectExpr com ctx members typ baseCall

    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "has" }, _, _), info, _, _range) ->
        Expression.withStmts {
            let! left = com.TransformAsExpr(ctx, info.Args.Head)
            let! value = com.TransformAsExpr(ctx, expr)
            return Expression.compare (left, [ ComparisonOperator.In ], [ value ])
        }

    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "slice" }, _, _), info, _, _range) ->
        transformAsSlice com ctx expr info

    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "to_array" }, _, _), info, _, _range) ->
        transformAsArray com ctx expr info

    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "Equals" }, _, _), { Args = [ arg ] }, _, _range) ->
        Expression.withStmts {
            let! left = com.TransformAsExpr(ctx, expr)
            let! right = com.TransformAsExpr(ctx, arg)
            return Expression.compare (left, [ Eq ], [ right ])
        }

    | Fable.Call(callee, info, _, range) -> transformCall com ctx range callee info

    | Fable.CurriedApply(callee, args, _, range) -> transformCurriedApply com ctx range callee args

    | Fable.Operation(kind, tags, _, range) -> transformOperation com ctx range kind tags

    | Fable.Get(expr, kind, typ, range) -> transformGet com ctx range typ expr kind

    | Fable.IfThenElse(Fable.Test(expr, Fable.TypeTest typ, r), thenExpr, TransformExpr com ctx (elseExpr, stmts''), _r) ->
        let finalExpr, stmts =
            Expression.withStmts {
                let! guardExpr = transformTest com ctx r (Fable.TypeTest typ) expr

                // Create refined context for then branch with type assertion
                let thenCtx =
                    match expr with
                    | Fable.IdentExpr ident -> { ctx with NarrowedTypes = Map.add ident.Name typ ctx.NarrowedTypes }
                    | _ -> ctx

                let! thenExprCompiled = com.TransformAsExpr(thenCtx, thenExpr)
                return Expression.ifExp (guardExpr, thenExprCompiled, elseExpr)
            }

        finalExpr, stmts @ stmts''

    | Fable.IfThenElse(TransformExpr com ctx (guardExpr, stmts),
                       TransformExpr com ctx (thenExpr, stmts'),
                       TransformExpr com ctx (elseExpr, stmts''),
                       _r) -> Expression.ifExp (guardExpr, thenExpr, elseExpr), stmts @ stmts' @ stmts''

    | Fable.DecisionTree(expr, targets) -> transformDecisionTreeAsExpr com ctx targets expr

    | Fable.DecisionTreeSuccess(idx, boundValues, _) -> transformDecisionTreeSuccessAsExpr com ctx idx boundValues

    | Fable.Set(expr, kind, typ, value, range) ->
        let expr', stmts = transformSet com ctx range expr typ value kind
        // printfn "transformAsExpr: Fable.Set: %A" expr
        match expr' with
        | Expression.NamedExpr {
                                   Target = target
                                   Value = _
                                   Loc = _
                               } ->
            let nonLocals =
                match target with
                | Expression.Name { Id = id } -> [ ctx.BoundVars.NonLocals([ id ]) |> Statement.nonLocal ]
                | _ -> []

            expr', nonLocals @ stmts
        | _ -> expr', stmts

    | Fable.Let(_ident, _value, _body) ->
        // printfn "Fable.Let: %A" (ident, value, body)
        iife com ctx expr

    | Fable.LetRec(bindings, body) ->
        if ctx.HoistVars(List.map fst bindings) then
            Expression.withStmts {
                let values, stmts =
                    bindings
                    |> List.map (fun (id, value) -> transformBindingAsExpr com ctx id value)
                    |> List.unzip
                    |> fun (e, s) -> e, List.collect id s

                let! expr = com.TransformAsExpr(ctx, body)
                let! finalExpr = transformSequenceExpr' com ctx (values @ [ expr ]) []
                return! finalExpr, stmts
            }
        else
            iife com ctx expr

    | Fable.Sequential exprs -> transformSequenceExpr com ctx exprs

    | Fable.Emit(info, _, range) ->
        if info.IsStatement then
            iife com ctx expr
        else
            transformEmit com ctx range info

    // These cannot appear in expression position in Python, must be wrapped in a lambda
    | Fable.WhileLoop _
    | Fable.ForLoop _
    | Fable.TryCatch _ -> iife com ctx expr
    | Fable.Extended(instruction, _) ->
        match instruction with
        | Fable.Curry(e, arity) -> transformCurry com ctx e arity
        | Fable.Throw _
        | Fable.Debugger -> iife com ctx expr

let transformAsSlice (com: IPythonCompiler) ctx expr (info: Fable.CallInfo) : Expression * Statement list =
    Expression.withStmts {
        let! left = com.TransformAsExpr(ctx, expr)

        let args, stmts =
            Expression.mapWith (fun arg -> com.TransformAsExpr(ctx, arg)) info.Args

        let slice =
            match args with
            | [] -> Expression.slice ()
            | [ lower ] -> Expression.slice (lower = lower)
            | [ Expression.Name { Id = Identifier "None" }; upper ] -> Expression.slice (upper = upper)
            | [ lower; upper ] -> Expression.slice (lower = lower, upper = upper)
            | _ -> failwith $"Array slice with %d{args.Length} not supported"

        return! Expression.subscript (left, slice), stmts
    }

let transformAsArray (com: IPythonCompiler) ctx expr (info: Fable.CallInfo) : Expression * Statement list =
    // printfn "transformAsArray: %A" (expr, info)
    let value, stmts = com.TransformAsExpr(ctx, expr)

    match expr.Type with
    | Fable.Type.Array(typ, Fable.ArrayKind.ResizeArray) ->
        // Expression.call (array, Expression.stringConstant l :: [ value ]), stmts
        makeArray com ctx [ expr ] Fable.ArrayKind.ResizeArray typ
    | _ -> transformAsSlice com ctx expr info

/// Active pattern to detect F# `use` statements compiled as TryCatch with Dispose
/// This helps us to transform F# `use` (i.e. TryCatch) as Python `with` statements
let (|UsePattern|_|) (body: Fable.Expr) =
    match body with
    | Fable.TryCatch(tryBody,
                     None,
                     Some(Fable.IfThenElse(_,
                                           Fable.Call(Fable.Get(Fable.TypeCast(Fable.IdentExpr { Name = disposeName }, _),
                                                                Fable.FieldGet { Name = "Dispose" },
                                                                _,
                                                                _),
                                                      _,
                                                      _,
                                                      _),
                                           _,
                                           _)),
                     _) -> Some(disposeName, tryBody)
    | _ -> None

let rec transformAsStatements (com: IPythonCompiler) ctx returnStrategy (expr: Fable.Expr) : Statement list =
    // printfn "transformAsStatements: %A" expr
    match expr with
    | Fable.Unresolved(_, _, r) ->
        addError com [] r "Unexpected unresolved expression"
        []

    | Fable.Extended(kind, _r) ->
        match kind with
        | Fable.Curry(e, arity) ->
            let expr, stmts = transformCurry com ctx e arity

            stmts @ (expr |> resolveExpr ctx e.Type returnStrategy)
        | Fable.Throw(expr, _) ->
            match expr with
            | None -> failwith "TODO: rethrow"
            | Some(TransformExpr com ctx (e, stmts)) -> stmts @ [ Statement.raise e ]
        | Fable.Debugger ->
            [
                Statement.assert' (Expression.boolOp (op = Or, values = [ Expression.boolConstant true ]))
            ]

    | Fable.TypeCast(e, t) ->
        let expr, stmts = transformCast com ctx t e
        stmts @ (expr |> resolveExpr ctx t returnStrategy)

    | Fable.Value(kind, r) ->
        let expr, stmts = transformValue com ctx r kind

        stmts @ (expr |> resolveExpr ctx kind.Type returnStrategy)

    | Fable.IdentExpr id ->
        let narrowedType = getNarrowedType ctx id
        identAsExpr com ctx id |> resolveExpr ctx narrowedType returnStrategy

    | Fable.Import({
                       Selector = selector
                       Path = path
                       Kind = _kind
                   },
                   t,
                   r) -> transformImport com ctx r selector path |> resolveExpr ctx t returnStrategy

    | Fable.Test(expr, kind, range) ->
        let expr, stmts = transformTest com ctx range kind expr

        stmts @ (expr |> resolveExpr ctx Fable.Boolean returnStrategy)

    | Fable.Lambda(arg, body, name) ->
        let expr', stmts =
            transformFunctionWithAnnotations com ctx name [ arg ] body
            |||> makeArrowFunctionExpression com ctx name (Some body.Type)

        stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)

    | Fable.Delegate(args, body, name, _) ->
        let expr', stmts =
            transformFunctionWithAnnotations com ctx name args body
            |||> makeArrowFunctionExpression com ctx name (Some body.Type)

        stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)

    | Fable.ObjectExpr([], _, None) -> [] // Remove empty object expression
    | Fable.ObjectExpr(members, typ, baseCall) ->
        let expr, stmts = transformObjectExpr com ctx members typ baseCall
        stmts @ (expr |> resolveExpr ctx typ returnStrategy)

    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "slice" }, _, _), info, typ, _range) ->
        let expr, stmts = transformAsSlice com ctx expr info
        stmts @ resolveExpr ctx typ returnStrategy expr
    | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "to_array" }, _, _), info, typ, _range) ->
        let expr, stmts = transformAsArray com ctx expr info
        stmts @ resolveExpr ctx typ returnStrategy expr
    | Fable.Call(callee, info, typ, range) -> transformCallAsStatements com ctx range typ returnStrategy callee info

    | Fable.CurriedApply(callee, args, typ, range) ->
        transformCurriedApplyAsStatements com ctx range typ returnStrategy callee args

    | Fable.Emit(info, t, range) ->
        let e, stmts = transformEmit com ctx range info

        if info.IsStatement then
            stmts @ [ Statement.expr e ] // Ignore the return strategy
        else
            stmts @ resolveExpr ctx t returnStrategy e

    | Fable.Operation(kind, tags, t, range) ->
        let expr, stmts = transformOperation com ctx range kind tags
        stmts @ (expr |> resolveExpr ctx t returnStrategy)

    | Fable.Get(expr, kind, t, range) ->
        let expr, stmts = transformGet com ctx range t expr kind
        stmts @ (expr |> resolveExpr ctx t returnStrategy)

    // Transform F# `use` i.e TryCatch as Python `with`
    | Fable.Let(ident, value, UsePattern(disposeName, tryBody)) when ident.Name = disposeName ->
        let id = Identifier ident.Name
        let body = transformBlock com ctx (Some(ResourceManager returnStrategy)) tryBody

        let value, stmts = com.TransformAsExpr(ctx, value)
        let items = [ WithItem.withItem (value, Expression.name id) ]
        stmts @ [ Statement.with' (items, body) ]

    | Fable.Let(ident, value, body) ->
        let binding = transformBindingAsStatements com ctx ident value

        List.append binding (transformAsStatements com ctx returnStrategy body)

    | Fable.LetRec(bindings, body) ->
        let bindings =
            bindings
            |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            |> Seq.toList

        List.append bindings (transformAsStatements com ctx returnStrategy body)

    | Fable.Set(expr, kind, typ, value, range) ->
        let expr', stmts = transformSet com ctx range expr typ value kind
        // printfn "transformAsStatements: Fable.Set: %A" (expr', value)
        match expr' with
        | Expression.NamedExpr({
                                   Target = target
                                   Value = value
                                   Loc = _
                               }) ->
            let nonLocals, ta =
                match target with
                | Expression.Name { Id = id } ->
                    let nonLocals = [ ctx.BoundVars.NonLocals([ id ]) |> Statement.nonLocal ]

                    nonLocals, None
                | Expression.Attribute { Value = Expression.Name { Id = Identifier "self" } } ->
                    let ta, stmts = Annotation.typeAnnotation com ctx None typ
                    stmts, Some ta
                | _ -> [], None

            let assignment =
                match ta with
                | Some ta -> [ Statement.assign (target, ta, value) ]
                | _ -> [ Statement.assign ([ target ], value) ]

            nonLocals @ stmts @ assignment
        | _ -> stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)
    | Fable.IfThenElse(guardExpr,
                       Fable.Expr.Extended(Fable.ExtendedSet.Debugger, _),
                       Fable.Expr.Value(Fable.ValueKind.Null Fable.Type.Unit, None),
                       r) ->
        // Rewrite `if (guard) { Debugger; null }` to assert (guard)
        let guardExpr', stmts = transformAsExpr com ctx guardExpr
        stmts @ [ Statement.assert' guardExpr' ]
    | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
        let asStatement =
            match returnStrategy with
            | None
            | Some ReturnUnit -> true
            | Some(Target _) -> true // Compile as statement so values can be bound
            | Some(Assign _) -> (isPyStatement ctx false thenExpr) || (isPyStatement ctx false elseExpr)
            | Some(ResourceManager _) -> true
            | Some Return ->
                Option.isSome ctx.TailCallOpportunity
                || (isPyStatement ctx false thenExpr)
                || (isPyStatement ctx false elseExpr)

        if asStatement then
            transformIfStatement com ctx r returnStrategy guardExpr thenExpr elseExpr
        else
            // Create refined context for then branch if guard is a type test
            let thenCtx =
                match guardExpr with
                | Fable.Test(Fable.IdentExpr ident, Fable.TypeTest typ, _) ->
                    { ctx with NarrowedTypes = Map.add ident.Name typ ctx.NarrowedTypes }
                | _ -> ctx

            let expr, stmts =
                Expression.withStmts {
                    let! guardExpr' = transformAsExpr com ctx guardExpr
                    let! thenExpr' = transformAsExpr com thenCtx thenExpr // Use refined context
                    let! elseExpr' = transformAsExpr com ctx elseExpr
                    return Expression.ifExp (guardExpr', thenExpr', elseExpr', ?loc = r)
                }

            stmts @ (expr |> resolveExpr ctx thenExpr.Type returnStrategy)

    | Fable.Sequential statements ->
        let lasti = (List.length statements) - 1

        statements
        |> List.mapiToArray (fun i statement ->
            let ret =
                if i < lasti then
                    None
                else
                    returnStrategy

            com.TransformAsStatements(ctx, ret, statement)
        )
        |> List.concat

    | Fable.TryCatch(body, catch, finalizer, r) -> transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

    | Fable.DecisionTree(expr, targets) -> transformDecisionTreeAsStatements com ctx returnStrategy targets expr

    | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
        transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

    | Fable.WhileLoop(TransformExpr com ctx (guard, stmts), body, range) ->
        stmts
        @ [ Statement.while' (guard, transformBlock com ctx None body, ?loc = range) ]

    | Fable.ForLoop(var,
                    TransformExpr com ctx (start, _stmts),
                    TransformExpr com ctx (limit, _stmts'),
                    body,
                    isUp,
                    _range) ->
        let step =
            Expression.intConstant (
                if isUp then
                    1
                else
                    -1
            )

        let iter = libCall com ctx None "util" "range" [ start; limit; step ]

        let body = transformBlock com ctx None body
        let target = com.GetIdentifierAsExpr(ctx, var.Name)

        [ Statement.for' (target = target, iter = iter, body = body) ]

let transformFunction
    com
    ctx
    name
    (args: Fable.Ident list)
    (body: Fable.Expr)
    (repeatedGenerics: Set<string>)
    : Arguments * Statement list
    =
    let tailcallChance =
        Option.map (fun name -> NamedTailCallOpportunity(com, ctx, name, args) :> ITailCallOpportunity) name

    let args = FSharp2Fable.Util.discardUnitArg args

    /// Removes `_mut` or `_mut_1` suffix from the identifier name
    let cleanName (input: string) =
        Regex.Replace(input, @"_mut(_\d+)?$", "")

    // For Python we need to append the TC-arguments to any declared (arrow) function inside the while-loop of the
    // TCO. We will set them as default values to themselves e.g `i=i` to capture the value and not the variable.
    let tcArgs, tcDefaults =
        match ctx.TailCallOpportunity with
        | Some tc ->
            tc.Args
            |> List.choose (fun arg ->
                let (Identifier name) = arg.Arg
                let name = cleanName name

                match name with
                | "tupled_arg_m" -> None // Remove these arguments (not sure why)
                | _ ->
                    let annotation =
                        // Cleanup type annotations to avoid non-repeated generics
                        match arg.Annotation with
                        | Some(Expression.Name { Id = Identifier _name }) -> arg.Annotation
                        | Some(Expression.Subscript {
                                                        Value = value
                                                        Slice = Expression.Name { Id = Identifier name }
                                                    }) when name.StartsWith("_", StringComparison.Ordinal) ->
                            Expression.subscript (value, stdlibModuleAnnotation com ctx "typing" "Any" [])
                            |> Some
                        | _ -> Some(stdlibModuleAnnotation com ctx "typing" "Any" [])

                    (Arg.arg (name, ?annotation = annotation), Expression.name name) |> Some
            )
            |> List.unzip
        | _ -> [], []

    let declaredVars = ResizeArray()
    let mutable isTailCallOptimized = false

    let argTypes = args |> List.map (fun id -> id.Type)
    let genTypeParams = getGenericTypeParams (argTypes @ [ body.Type ])
    let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams

    let ctx =
        { ctx with
            TailCallOpportunity = tailcallChance
            HoistVars =
                fun ids ->
                    declaredVars.AddRange(ids)
                    true
            OptimizeTailCall = fun () -> isTailCallOptimized <- true
            BoundVars = ctx.BoundVars.EnterScope()
            ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams
        }

    let body =
        if body.Type = Fable.Unit then
            transformBlock com ctx (Some ReturnUnit) body
        elif isPyStatement ctx (Option.isSome tailcallChance) body then
            transformBlock com ctx (Some Return) body
        else
            transformAsExpr com ctx body |> wrapExprInBlockWithReturn

    let isUnit =
        List.tryLast args
        |> Option.map (
            function
            | { Type = Fable.GenericParam _ } -> true
            | _ -> false
        )
        |> Option.defaultValue false

    let args, defaults, body =
        match isTailCallOptimized, tailcallChance with
        | true, Some tc ->
            // Replace args, see NamedTailCallOpportunity constructor
            let args' =
                List.zip args tc.Args
                |> List.map (fun (_id, { Arg = Identifier tcArg }) ->
                    let id = com.GetIdentifier(ctx, tcArg)

                    let ta, _ = Annotation.typeAnnotation com ctx (Some repeatedGenerics) _id.Type

                    Arg.arg (id, annotation = ta)
                )

            let varDecls =
                List.zip args tc.Args
                |> List.map (fun (id, { Arg = Identifier tcArg }) ->
                    ident com ctx id, Some(com.GetIdentifierAsExpr(ctx, tcArg))
                )
                |> multiVarDeclaration ctx

            let body = varDecls @ body
            // Make sure we don't get trapped in an infinite loop, see #1624
            let body = body @ [ Statement.break' () ]

            args', [], Statement.while' (Expression.boolConstant true, body) |> List.singleton
        | _ ->
            // Make sure all unit arguments get default values of None
            let defaults =
                args
                |> List.map (fun arg ->
                    match arg.Type with
                    | Fable.Unit -> Some Expression.none
                    | Fable.Any
                    | Fable.Option _ -> Some Expression.none
                    | _ -> None
                )

            let args' =
                args
                |> List.map (fun id ->
                    let ta, _ = Annotation.typeAnnotation com ctx (Some repeatedGenerics) id.Type

                    Arg.arg (ident com ctx id, annotation = ta)
                )

            // Extract defaults for Python function signature
            let finalDefaults =
                defaults
                |> List.rev
                |> List.takeWhile Option.isSome
                |> List.choose id
                |> List.rev

            args', finalDefaults, body

    let arguments =
        match args, isUnit with
        | [], _ ->
            Arguments.arguments (
                args = Arg.arg (Identifier("__unit"), annotation = Expression.name "None") :: tcArgs,
                defaults = Expression.none :: tcDefaults
            )
        // So we can also receive unit
        | [ arg ], true ->
            let optional =
                match arg.Annotation with
                | Some typeArg -> Expression.binOp (typeArg, BitOr, Expression.name "None") |> Some
                | None -> None

            let args = [ { arg with Annotation = optional } ]

            Arguments.arguments (args @ tcArgs, defaults = Expression.none :: tcDefaults)
        | _ -> Arguments.arguments (args @ tcArgs, defaults = defaults @ tcDefaults)

    arguments, body

// Declares a Python entry point, i.e `if __name__ == "__main__"`
let declareEntryPoint (com: IPythonCompiler) (ctx: Context) (funcExpr: Expression) =
    com.GetImportExpr(ctx, "sys") |> ignore

    let args =
        emitExpression None "sys.argv[1:]" []
        |> fun expr -> arrayExpr com ctx expr Fable.ImmutableArray Fable.String

    let test =
        Expression.compare (
            Expression.name "__name__",
            [ ComparisonOperator.Eq ],
            [ Expression.stringConstant "__main__" ]
        )

    let main = Expression.call (funcExpr, [ args ]) |> Statement.expr |> List.singleton

    Statement.if' (test, main)

let declareModuleMember (com: IPythonCompiler) ctx _isPublic (membName: Identifier) typ (expr: Expression) =
    let (Identifier name) = membName

    if com.OutputType = OutputType.Library then
        com.AddExport name |> ignore

    let name = Expression.name membName
    varDeclaration ctx name typ expr

let makeEntityTypeParams (com: IPythonCompiler) ctx (ent: Fable.Entity) : TypeParam list =
    getEntityGenParams ent |> makeTypeParams com ctx

let getUnionFieldsAsIdents (_com: IPythonCompiler) _ctx (_ent: Fable.Entity) =
    let tagId = makeTypedIdent (Fable.Number(Int32, Fable.NumberInfo.Empty)) "tag"

    let fieldsId = makeTypedIdent (Fable.Array(Fable.Any, Fable.MutableArray)) "fields"

    [| tagId; fieldsId |]

let getEntityFieldsAsIdents (com: IPythonCompiler) (ent: Fable.Entity) =
    let entityNamingConvention =
        if shouldUseRecordFieldNaming ent then
            Naming.toRecordFieldSnakeCase
        else
            Naming.toPythonNaming

    ent.FSharpFields
    |> Seq.map (fun field ->
        let name =
            (entityNamingConvention field.Name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun _ -> false)

        let typ = field.FieldType

        { makeTypedIdent typ name with IsMutable = field.IsMutable }
    )
    |> Seq.toArray

let getEntityFieldsAsProps (com: IPythonCompiler) ctx (ent: Fable.Entity) =
    if ent.IsFSharpUnion then
        getUnionFieldsAsIdents com ctx ent |> Array.map (identAsExpr com ctx)
    else
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> Naming.toPythonNaming

            let cleanName =
                (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent Naming.pyBuiltins.Contains

            Expression.name cleanName
        )
        |> Seq.toArray

let declareDataClassType
    (com: IPythonCompiler)
    (ctx: Context)
    (ent: Fable.Entity)
    (entName: string)
    (consArgs: Arguments)
    (_isOptional: bool)
    (_consBody: Statement list)
    (baseExpr: Expression option)
    (classMembers: Statement list)
    _slotMembers
    =
    let name = com.GetIdentifier(ctx, entName)

    let props =
        consArgs.Args
        |> List.map (fun arg ->
            let any _ =
                stdlibModuleAnnotation com ctx "typing" "Any" []

            let annotation = arg.Annotation |> Option.defaultWith any

            Statement.assign (Expression.name arg.Arg, annotation = annotation)
        )


    let typeParams = makeEntityTypeParams com ctx ent
    let bases = baseExpr |> Option.toList

    // Add a __hash__ method to the class
    let hashMethod =
        // Hashing of data classes in python does not support inheritance, so we need to implement
        // this method manually.
        Statement.functionDef (
            Identifier "__hash__",
            Arguments.arguments [ Arg.arg "self" ],
            body =
                [
                    Statement.return' (
                        Expression.call (
                            Expression.name "int",
                            [
                                Expression.call (
                                    Expression.attribute (
                                        value = Expression.name "self",
                                        attr = Identifier "GetHashCode",
                                        ctx = Load
                                    ),
                                    []
                                )
                            ]
                        )
                    )
                ],
            returns = Expression.name "int"
        )

    let classBody =
        let body = [ yield! props; yield! classMembers; yield hashMethod ]

        match body with
        | [] -> [ Statement.ellipsis ]
        | _ -> body

    let dataClass = com.GetImportExpr(ctx, "dataclasses", "dataclass")

    let decorators =
        [
            Expression.call (
                dataClass,
                kw =
                    [
                        Keyword.keyword (Identifier "eq", Expression.boolConstant false)
                        Keyword.keyword (Identifier "repr", Expression.boolConstant false)
                        Keyword.keyword (Identifier "slots", Expression.boolConstant true)
                    ]
            )
        ]

    [
        Statement.classDef (name, body = classBody, decoratorList = decorators, bases = bases, typeParams = typeParams)
    ]

let transformClassAttributes
    (com: IPythonCompiler)
    (ctx: Context)
    (ent: Fable.Entity)
    (classAttributes: ClassAttributes option)
    (extractedInitialValues: (string * Fable.MemberDecl * Expression) list)
    : Statement list
    =
    match classAttributes with
    | Some parms when parms.Style = ClassStyle.Attributes ->
        // Generate class attributes for attached members (deduplicate by property name)
        ent.MembersFunctionsAndValues
        |> Seq.filter (fun memb -> memb.IsInstance && (memb.IsGetter || memb.IsSetter))
        |> Seq.groupBy (fun memb -> memb.DisplayName)
        |> Seq.map (fun (propertyName, members) ->
            // Use the getter's return type for the class attribute type annotation
            let getter = members |> Seq.find (fun memb -> memb.IsGetter)
            let ta, _ = Annotation.typeAnnotation com ctx None getter.ReturnParameter.Type

            if parms.Init then
                // For attributes style with init, class attributes should only have type annotations
                // Default values are handled by the constructor parameters
                Statement.annAssign (Expression.name (propertyName |> Naming.toPropertyNaming), annotation = ta)
            else
                // For attributes style with init=false, avoid emitting initializers that reference
                // constructor parameters (e.g., Age: int = Age) because no __init__ is generated and
                // libraries like Pydantic will synthesize it. Keep literal/call defaults like Field("...").
                let defaultValueOpt =
                    extractedInitialValues
                    |> List.tryFind (fun (propName, _getterMemb, _defaultValue) ->
                        propName = (propertyName |> Naming.toPropertyNaming)
                    )
                    |> Option.map (fun (_propName, _getterMemb, defaultValue) -> defaultValue)

                match defaultValueOpt with
                // Skip defaults that are just bare names (likely ctor params), emit only annotation
                | Some(Name _) ->
                    Statement.annAssign (Expression.name (propertyName |> Naming.toPropertyNaming), annotation = ta)
                // If we have a non-name default (e.g., Field("Name")), keep it
                | Some defaultValue ->
                    Statement.annAssign (
                        Expression.name (propertyName |> Naming.toPropertyNaming),
                        annotation = ta,
                        value = defaultValue
                    )
                // Otherwise, emit only the annotation (no default)
                | None ->
                    Statement.annAssign (Expression.name (propertyName |> Naming.toPropertyNaming), annotation = ta)
        )
        |> List.ofSeq
    | _ -> []

let declareClassType
    (com: IPythonCompiler)
    (ctx: Context)
    (ent: Fable.Entity)
    (entName: string)
    (consArgs: Arguments)
    (isOptional: bool)
    (consBody: Statement list)
    (baseExpr: Expression option)
    (classMembers: Statement list)
    (slotMembers: Statement list)
    (classAttributes: ClassAttributes option)
    (attachedMembers: Fable.MemberDecl list)
    (extractedInitialValues: (string * Fable.MemberDecl * Expression) list)
    =
    // printfn "declareClassType: %A" consBody
    let typeParams = makeEntityTypeParams com ctx ent

    let fieldTypes =
        if ent.IsValueType then
            Some(ent.FSharpFields |> List.map (fun f -> f.FieldType))
        else
            None

    // Generate class constructor if init is enabled
    let classCons =
        match classAttributes with
        | Some parms when not parms.Init -> [] // Skip constructor when init=false
        | _ -> makeClassConstructor consArgs isOptional fieldTypes com ctx consBody

    let classFields = slotMembers // TODO: annotations

    // Generate class attributes if using attributes style
    let classAttributes =
        transformClassAttributes com ctx ent classAttributes extractedInitialValues

    let classMembers = classCons @ classMembers

    let classBody =
        let body = [ yield! classFields; yield! classAttributes; yield! classMembers ]

        match body with
        | [] -> [ Statement.ellipsis ]
        | _ -> body

    let interfaces, stmts =
        // We only use a few interfaces as base classes. The rest is handled as Python protocols (PEP 544) to avoid a massive
        // inheritance tree that will prevent Python of finding a consistent method resolution order.
        let allowedInterfaces = [ "IDisposable"; "IEnumerator_1" ]

        // Check if class implements IEnumerator_1 (which already inherits from IDisposable)
        let hasIEnumerator =
            ent.AllInterfaces
            |> Seq.exists (fun int -> Helpers.removeNamespace (int.Entity.FullName) = "IEnumerator_1")

        ent.AllInterfaces
        |> List.ofSeq
        |> List.filter (fun int ->
            let name = Helpers.removeNamespace (int.Entity.FullName)
            // Filter out IDisposable if IEnumerator_1 is present to avoid diamond inheritance
            allowedInterfaces |> List.contains name
            && not (hasIEnumerator && name = "IDisposable")
        )
        |> List.map (fun int ->
            let genericArgs =
                match int.GenericArgs with
                | [ Fable.DeclaredType({ FullName = fullName }, _genericArgs) ] when
                    Helpers.removeNamespace (fullName) = entName
                    ->
                    [ Fable.Type.Any ]
                | args -> args

            let expr, stmts =
                Annotation.makeEntityTypeAnnotation com ctx int.Entity genericArgs None

            expr, stmts
        )
        |> Helpers.unzipArgs

    let bases = baseExpr |> Option.toList
    let name = com.GetIdentifier(ctx, Naming.toPascalCase entName)

    // Check if the class has static properties using proper detection
    let hasStaticProperties =
        // Don't apply metaclass to classes that inherit from base types with existing metaclasses
        let hasConflictingBase =
            baseExpr
            |> Option.exists (fun expr ->
                match expr with
                | Expression.Attribute { Attr = Identifier name } when
                    name = "Union" || name = "Record" || name = "FSharpException"
                    ->
                    true
                | Expression.Name { Id = Identifier name } when
                    name = "Union" || name = "Record" || name = "FSharpException"
                    ->
                    true
                | _ -> false
            )
            || ent.IsFSharpUnion
            || ent.IsFSharpRecord
            || ent.IsFSharpExceptionDeclaration

        if hasConflictingBase then
            false
        else
            // Get the original member declarations from the entity
            let members = ent.MembersFunctionsAndValues

            members
            |> Seq.exists (fun info -> not info.IsInstance && (info.IsGetter || info.IsSetter))

    let keywords =
        // Add metaclass if the class has static properties but only if the class is
        // decorated with [<AttachMembers>]
        if hasStaticProperties && hasAttribute Atts.attachMembers ent.Attributes then
            let staticPropertyMetaExpr = libValue com ctx "util" "StaticPropertyMeta"
            [ Keyword.keyword (Identifier "metaclass", staticPropertyMetaExpr) ]
        else
            []

    // Generate custom decorators from [<Decorate>] attributes
    let customDecorators =
        let decoratorInfos = Util.getDecoratorInfo com ent.Attributes
        Util.generateDecorators com ctx decoratorInfos

    stmts
    @ [
        Statement.classDef (
            name,
            body = classBody,
            bases = bases @ interfaces,
            typeParams = typeParams,
            keywords = keywords,
            decoratorList = customDecorators
        )
    ]

let createSlotsForRecordType (com: IPythonCompiler) ctx (classEnt: Fable.Entity) =
    let strFromIdent (ident: Identifier) = ident.Name

    if classEnt.IsValueType then
        let elements =
            getEntityFieldsAsProps com ctx classEnt
            |> Array.map (nameFromKey com ctx >> strFromIdent >> Expression.stringConstant)
            |> Array.toList

        let slots = Expression.list (elements, Load)

        [ Statement.assign ([ Expression.name ("__slots__", Store) ], slots) ]
    else
        []

let declareType
    (com: IPythonCompiler)
    (ctx: Context)
    (ent: Fable.Entity)
    (entName: string)
    (consArgs: Arguments)
    (isOptional: bool)
    (consBody: Statement list)
    (baseExpr: Expression option)
    (classMembers: Statement list)
    (pythonClassParams: ClassAttributes option)
    (attachedMembers: Fable.MemberDecl list)
    (extractedInitialValues: (string * Fable.MemberDecl * Expression) list)
    : Statement list
    =
    let slotMembers = createSlotsForRecordType com ctx ent

    let typeDeclaration =
        match ent.IsFSharpRecord with
        | true ->
            declareDataClassType com ctx ent entName consArgs isOptional consBody baseExpr classMembers slotMembers
        | false ->
            declareClassType
                com
                ctx
                ent
                entName
                consArgs
                isOptional
                consBody
                baseExpr
                classMembers
                slotMembers
                pythonClassParams
                attachedMembers
                extractedInitialValues

    let reflectionDeclaration, stmts =
        let ta = fableModuleAnnotation com ctx "Reflection" "TypeInfo" []

        let genArgs =
            Array.init ent.GenericParameters.Length (fun i -> "gen" + string<int> i |> makeIdent)

        let args =
            genArgs
            |> Array.mapToList (fun id -> Arg.arg (ident com ctx id, annotation = ta))

        let args = Arguments.arguments args
        let generics = genArgs |> Array.mapToList (identAsExpr com ctx)

        let body, stmts = transformReflectionInfo com ctx None ent generics
        let expr, stmts' = makeFunctionExpression com ctx None (args, body, [], ta)

        let name =
            com.GetIdentifier(ctx, Naming.toPascalCase entName + Naming.reflectionSuffix)

        expr |> declareModuleMember com ctx ent.IsPublic name None, stmts @ stmts'

    stmts @ typeDeclaration @ reflectionDeclaration

let tryParseEmitMethodMacro (macro: string) =
    // Parse EmitMethod macros like "$0.methodName($1...)" to extract method name
    let pattern = @"\$0\.(\w+)\(\$1\.\.\.\)"
    let regex = Regex pattern
    let m = macro |> regex.Match

    if m.Success then
        Some m.Groups[1].Value
    else
        None

let tryParseEmitConstructorMacro (macro: string) =
    // Parse EmitConstructor macros like "new $0($1...)" to extract constructor call
    let pattern = @"new \$0\(\$1\.\.\.\)"
    let regex = Regex pattern
    let m = macro |> regex.Match

    if m.Success then
        Some() // Return Some unit to indicate it's a constructor pattern
    else
        None

let calculateTypeParams (com: IPythonCompiler) ctx (info: Fable.MemberFunctionOrValue) args returnType bodyType =
    // Calculate type parameters for Python 3.12 syntax
    let explicitGenerics = Annotation.getMemberGenParams info.GenericParameters

    let signatureGenerics =
        extractGenericParamsFromMethodSignature com ctx args returnType

    let bodyGenerics =
        // For getters/setters, also extract generics from the method body/return type
        getGenericTypeParams [ bodyType ] |> Set.difference <| ctx.ScopedTypeParams

    let repeatedGenerics =
        Set.empty
        |> Set.union explicitGenerics
        |> Set.union signatureGenerics
        |> Set.union bodyGenerics
        // Filter out generics that are already defined at class level
        |> Set.difference
        <| ctx.ScopedTypeParams

    makeFunctionTypeParams com ctx repeatedGenerics

let transformModuleFunction
    (com: IPythonCompiler)
    ctx
    (info: Fable.MemberFunctionOrValue)
    (membName: string)
    args
    body
    =
    let args, body', returnType =
        getMemberArgsAndBody com ctx (NonAttached membName) info.HasSpread args body

    let typeParams = calculateTypeParams com ctx info args returnType body.Type
    let name = com.GetIdentifier(ctx, membName |> Naming.toPythonNaming)

    let isAsync = isTaskType body.Type

    let stmt =
        createFunctionWithTypeParams name args body' [] returnType info.XmlDoc typeParams isAsync

    let expr = Expression.name name

    info.Attributes
    |> hasAttribute Atts.entryPoint
    |> function
        | true -> [ stmt; declareEntryPoint com ctx expr ]
        | false ->
            if com.OutputType = OutputType.Library then
                com.AddExport membName |> ignore

            [ stmt ]

let transformAction (com: IPythonCompiler) ctx expr = transformAsStatements com ctx None expr

let nameFromKey (com: IPythonCompiler) (ctx: Context) key =
    match key with
    | Expression.Name { Id = ident } -> ident
    | Expression.Constant(value = StringLiteral name) -> com.GetIdentifier(ctx, name)
    | name -> failwith $"Not a valid name: %A{name}"

let generateStaticPropertySetter
    (com: IPythonCompiler)
    (ctx: Context)
    (className: string)
    (propertyName: string)
    (setterBody: Statement list)
    (args: Arguments)
    : Statement
    =
    let functionName = $"_%s{className}_%s{propertyName}_setter"
    let functionNameIdent = com.GetIdentifier(ctx, functionName)

    // Create setter function with just the value parameter (remove 'self' for static)
    let setterArgs =
        match args.Args with
        | [ _self; valueArg ] -> Arguments.arguments [ valueArg ]
        | [ valueArg ] -> Arguments.arguments [ valueArg ]
        | _ -> Arguments.arguments [ Arg.arg "value" ]

    Statement.functionDef (functionNameIdent, setterArgs, body = setterBody)

// Check if a class needs StaticProperty descriptor (has static properties)
let classHasStaticProperties (members: Fable.MemberDecl list) (com: IPythonCompiler) : bool =
    members
    |> List.exists (fun memb ->
        let info = com.GetMember(memb.MemberRef)
        not info.IsInstance && (info.IsGetter || info.IsSetter)
    )

let transformAttachedProperty
    (com: IPythonCompiler)
    ctx
    (ent: Fable.Entity)
    (info: Fable.MemberFunctionOrValue)
    (memb: Fable.MemberDecl)
    (pythonClassParams: ClassAttributes option)
    =
    let isStatic = not info.IsInstance
    let isGetter = info.IsGetter

    // For static properties, we'll handle them differently using descriptors
    if isStatic then
        // Static properties need special handling - return empty list for now
        // They will be processed at the class level
        []
    else
        // Check if we should use attributes style
        let useAttributesStyle =
            match pythonClassParams with
            | Some parms when parms.Style = ClassStyle.Attributes -> true
            | _ -> false

        if useAttributesStyle then
            // For attributes style, we don't generate property methods
            // The attributes will be handled at the class level
            []
        else
            // Instance properties use the traditional approach (properties style)
            // Get custom decorators from Py.Decorate attributes
            let customDecorators =
                let decoratorInfos = Util.getDecoratorInfo com info.Attributes
                Util.generateDecorators com ctx decoratorInfos

            let decorators =
                customDecorators
                @ [
                    if isGetter then
                        Expression.name "property"
                    else
                        Expression.name $"%s{memb.Name}.setter"
                ]

            let args, body, returnType =
                getMemberArgsAndBody com ctx (Attached isStatic) false memb.Args memb.Body

            // Apply the same naming convention as record fields for record types
            let propertyName =
                //if shouldUseRecordFieldNaming ent then
                //    memb.Name |> Naming.toRecordFieldSnakeCase |> Helpers.clean
                //else
                memb.Name |> Naming.toPropertyNaming

            let key = com.GetIdentifier(ctx, propertyName)

            let self = Arg.arg "self"
            let arguments = { args with Args = self :: args.Args }

            let typeParams =
                calculateTypeParams com ctx info arguments returnType memb.Body.Type

            let isAsync = isTaskType memb.Body.Type

            createFunctionWithTypeParams key arguments body decorators returnType None typeParams isAsync
            |> List.singleton

let transformAttachedMethod (com: IPythonCompiler) ctx (info: Fable.MemberFunctionOrValue) (memb: Fable.MemberDecl) =
    // printfn "transformAttachedMethod: %A" memb

    let isStatic = not info.IsInstance

    // Check if this method has Py.ClassMethod attribute
    let hasClassMethodAttribute =
        info.Attributes
        |> Seq.exists (fun att -> att.Entity.FullName = Atts.pyClassMethod)

    // Get custom decorators from Py.Decorate attributes
    let customDecorators =
        let decoratorInfos = Util.getDecoratorInfo com info.Attributes
        Util.generateDecorators com ctx decoratorInfos

    let decorators =
        // Custom decorators come first (outermost), then static/classmethod decorator
        customDecorators
        @ if isStatic then
              if hasClassMethodAttribute then
                  [ Expression.name "classmethod" ]
              else
                  [ Expression.name "staticmethod" ]
          else
              []

    let makeMethod name args body decorators returnType =
        let key = memberFromName com ctx name |> nameFromKey com ctx

        let typeParams = calculateTypeParams com ctx info args returnType memb.Body.Type
        let isAsync = isTaskType memb.Body.Type

        createFunctionWithTypeParams key args body decorators returnType None typeParams isAsync

    let args, body, returnType =
        getMemberArgsAndBody com ctx (Attached isStatic) info.HasSpread memb.Args memb.Body

    let self = Arg.arg "self"

    let arguments =
        if isStatic then
            args
        else
            { args with Args = self :: args.Args }

    [
        yield makeMethod memb.Name arguments body decorators returnType
        if info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" then
            yield
                makeMethod "__iter__" (Arguments.arguments [ self ]) (enumerator2iterator com ctx) decorators returnType
    ]

let transformUnion (com: IPythonCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
    let fieldIds = getUnionFieldsAsIdents com ctx ent

    let args, isOptional =
        let args =
            fieldIds[0]
            |> ident com ctx
            |> (fun id ->
                let ta, _ = Annotation.typeAnnotation com ctx None fieldIds[0].Type
                Arg.arg (id, annotation = ta)
            )
            |> List.singleton

        let varargs =
            fieldIds[1]
            |> ident com ctx
            |> fun id ->
                let gen = getGenericTypeParams [ fieldIds[1].Type ] |> Set.toList |> List.tryHead

                let ta = Expression.name (gen |> Option.defaultValue "Any")
                Arg.arg (id, annotation = ta)


        let isOptional = Helpers.isOptional fieldIds
        Arguments.arguments (args = args, vararg = varargs), isOptional

    let body =
        [
            yield callSuperAsStatement []
            yield!
                fieldIds
                |> Array.map (fun id ->
                    let left = get com ctx None thisExpr id.Name false

                    let right =
                        match id.Type with
                        | Fable.Number _ -> identAsExpr com ctx id
                        | Fable.Array _ ->
                            // Convert varArg from tuple to array. TODO: we might need to do this other places as well.
                            let array = libValue com ctx "array_" "Array"
                            let type_obj = com.GetImportExpr(ctx, "typing", "Any")
                            let types_array = Expression.subscript (value = array, slice = type_obj, ctx = Load)
                            Expression.call (types_array, [ identAsExpr com ctx id ])
                        | _ -> identAsExpr com ctx id

                    let ta, _ = Annotation.typeAnnotation com ctx None id.Type
                    Statement.assign (left, ta, right)
                )
        ]

    let cases =
        let expr, stmts =
            ent.UnionCases
            |> Seq.map (getUnionCaseName >> makeStrConst)
            |> Seq.toList
            |> makeList com ctx

        let name = Identifier "cases"
        let body = stmts @ [ Statement.return' expr ]
        let decorators = [ Expression.name "staticmethod" ]

        let returnType =
            Expression.subscript (Expression.name "list", Expression.name "str")

        Statement.functionDef (
            name,
            Arguments.arguments (),
            body = body,
            returns = returnType,
            decoratorList = decorators
        )

    let baseExpr = libValue com ctx "types" "Union" |> Some
    let classMembers = List.append [ cases ] classMembers

    declareType com ctx ent entName args isOptional body baseExpr classMembers None [] []

let transformClassWithCompilerGeneratedConstructor
    (com: IPythonCompiler)
    ctx
    (ent: Fable.Entity)
    (entName: string)
    classMembers
    (classAttributes: ClassAttributes)
    =
    // printfn "transformClassWithCompilerGeneratedConstructor"
    let fieldIds = getEntityFieldsAsIdents com ent

    let args =
        fieldIds
        |> Array.map (fun id -> com.GetIdentifier(ctx, id.Name) |> Expression.name)

    let isOptional =
        Helpers.isOptional fieldIds || ent.IsFSharpRecord || ent.IsValueType

    let baseExpr =
        if ent.IsFSharpExceptionDeclaration then
            libValue com ctx "types" "FSharpException" |> Some
        elif ent.IsFSharpRecord || ent.IsValueType then
            libValue com ctx "types" "Record" |> Some
        else
            None

    let body =
        [
            if Option.isSome baseExpr then
                yield callSuperAsStatement []

            // For attributes style, use direct attribute names and proper parameter handling
            if classAttributes.Style = ClassStyle.Attributes then
                yield!
                    ent.FSharpFields
                    |> List.collect (fun field ->
                        let fieldName = field.Name |> Naming.toPropertyNaming
                        let left = get com ctx None thisExpr fieldName false
                        // For attributes style, use the property name as the argument name
                        let argName = field.Name |> Naming.toPropertyNaming
                        let right = Expression.name argName |> wrapIntExpression field.FieldType

                        // Always assign the parameter value to the attribute
                        assign None left right |> exprAsStatement ctx
                    )
            else
                yield!
                    ent.FSharpFields
                    |> List.collecti (fun i field ->
                        let fieldName =
                            if shouldUseRecordFieldNaming ent then
                                field.Name |> Naming.toRecordFieldSnakeCase |> Helpers.clean
                            else
                                match Util.getFieldNamingKind com field.FieldType field.Name with
                                | InstancePropertyBacking -> field.Name |> Naming.toPropertyBackingFieldNaming
                                | StaticProperty -> field.Name |> Naming.toPropertyNaming
                                | RegularField -> field.Name |> Naming.toPythonNaming

                        let left = get com ctx None thisExpr fieldName false
                        let right = args[i] |> wrapIntExpression field.FieldType

                        assign None left right |> exprAsStatement ctx
                    )
        ]

    let args =
        fieldIds
        |> Array.mapToList (fun id ->
            let ta, _ = Annotation.typeAnnotation com ctx None id.Type
            Arg.arg (ident com ctx id, annotation = ta)
        )
        |> (fun args -> Arguments.arguments (args = args))

    declareType com ctx ent entName args isOptional body baseExpr classMembers (Some classAttributes) [] []

let transformClassWithPrimaryConstructor
    (com: IPythonCompiler)
    ctx
    (classDecl: Fable.ClassDecl)
    (classMembers: Statement list)
    (cons: Fable.MemberDecl)
    (classAttributes: ClassAttributes)
    =
    // printfn "transformClassWithPrimaryConstructor: %A" classDecl.Name
    // printfn "PythonClass params: style=%A, init=%b" classAttributes.Style classAttributes.Init
    let classEnt = com.GetEntity(classDecl.Entity)
    let classIdent = Expression.name (com.GetIdentifier(ctx, classDecl.Name))

    // Extract initial values from constructor body for both init=true and init=false cases
    let getterMembers =
        if classAttributes.Style = ClassStyle.Attributes then
            classDecl.AttachedMembers
            |> List.filter (fun memb ->
                let info = com.GetMember(memb.MemberRef)
                info.IsGetter
            )
        else
            []

    let extractedInitialValues, stmts =
        if classAttributes.Style = ClassStyle.Attributes then
            getterMembers
            |> List.map (fun getterMemb ->
                // printfn "Extracting default value for property %s" (getterMemb.Name |> Naming.toPropertyNaming)
                let propertyName = getterMemb.Name |> Naming.toPropertyNaming

                let extracted =
                    Util.tryExtractInitializationValueFromConstructor com ctx cons.Body propertyName

                let result, stmts =
                    extracted
                    |> Option.defaultWith (fun () ->
                        // printfn "Using type default for property %s" propertyName
                        Util.getDefaultValueForType com ctx getterMemb.Body.Type, []
                    )
                // printfn "Final default value for %s: %A" propertyName result
                (propertyName, getterMemb, result), stmts
            )
            |> List.unzip
            // Flatten the list of statements generated from the property extractions
            |> fun (extractedInitialValues, stmts) -> extractedInitialValues, List.concat stmts
        else
            [], []

    let consArgs, consBody, _returnType =
        if classAttributes.Style = ClassStyle.Attributes then
            if classAttributes.Init then
                // For attributes style with init=true, generate new constructor with property names
                // Place required params (no safe default) before optional params (with safe default),
                // so Python's trailing-default rule is respected.
                let requiredProps, optionalProps =
                    extractedInitialValues
                    |> List.partition (fun (propertyName, _getterMemb, defaultValue) ->
                        match defaultValue with
                        // Treat as required only when default is a bare name equal to the parameter itself
                        // e.g., Age: int = Age. Keep None and other expressions as optional defaults.
                        | Name name when name.Id.Name = propertyName -> true
                        | _ -> false
                    )

                let propertyArgs =
                    let toArg (propertyName: string, getterMemb: Fable.MemberDecl, _defaultValue: Expression) =
                        let ta, _ = Annotation.typeAnnotation com ctx None getterMemb.Body.Type
                        Arg.arg (propertyName, annotation = ta)

                    let argsRequired = requiredProps |> List.map toArg
                    let argsOptional = optionalProps |> List.map toArg

                    let defaults = optionalProps |> List.map (fun (_pn, _gm, dv) -> dv)

                    Arguments.arguments (args = (argsRequired @ argsOptional), defaults = defaults)

                let propertyBody =
                    extractedInitialValues
                    |> List.collect (fun (propertyName, _getterMemb, _defaultValue) ->
                        let left = get com ctx None thisExpr propertyName false
                        let right = Expression.name propertyName
                        assign None left right |> exprAsStatement ctx
                    )

                propertyArgs, propertyBody, Expression.none
            else
                // For attributes style with init=false, no constructor arguments or body
                Arguments.empty, [], Expression.none
        else
            let info = com.GetMember(cons.MemberRef)
            getMemberArgsAndBody com ctx ClassConstructor info.HasSpread cons.Args cons.Body

    let isOptional = Helpers.isOptional (cons.Args |> Array.ofList)

    // Change exposed constructor's return type from None to entity type.
    let returnType =
        let availableGenerics =
            cons.Args |> List.map (fun arg -> arg.Type) |> getGenericTypeParams

        let genParams = getEntityGenParams classEnt

        makeGenericTypeAnnotation' com ctx classDecl.Name (genParams |> List.ofSeq) (Some availableGenerics)

    let exposedCons =
        if classAttributes.Style = ClassStyle.Attributes && not classAttributes.Init then
            // For attributes style with init=false, don't generate an exposed constructor
            // since there's no __init__ method to call
            []
        else
            let argExprs = consArgs.Args |> List.map (fun p -> Expression.identifier p.Arg)

            let exposedConsBody = Expression.call (classIdent, argExprs)
            let name = com.GetIdentifier(ctx, cons.Name)
            [ makeFunction name (consArgs, exposedConsBody, [], returnType) ]

    let baseExpr, consBody =
        classDecl.BaseCall
        |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
        |> Option.orElseWith (fun () ->
            if classEnt.IsValueType then
                Some(libValue com ctx "Types" "Record", ([], [], []))
            else
                None
        )
        |> Option.map (fun (baseExpr, (baseArgs, _kw, stmts)) ->
            let consBody = stmts @ [ callSuperAsStatement baseArgs ] @ consBody

            Some baseExpr, consBody
        )
        |> Option.defaultValue (None, consBody)

    [
        yield! stmts
        yield!
            declareType
                com
                ctx
                classEnt
                classDecl.Name
                consArgs
                isOptional
                consBody
                baseExpr
                classMembers
                (Some classAttributes)
                classDecl.AttachedMembers
                extractedInitialValues
        yield! exposedCons
    ]

let transformInterface (com: IPythonCompiler) ctx (classEnt: Fable.Entity) (_classDecl: Fable.ClassDecl) =
    // printfn "transformInterface"
    let classIdent = com.GetIdentifier(ctx, Helpers.removeNamespace classEnt.FullName)

    let members =
        classEnt.MembersFunctionsAndValues
        |> Seq.filter (fun memb ->
            not memb.IsProperty
            // Filter out methods with emit attributes (like EmitMethod, EmitConstructor, etc.)
            // These should be handled as emit expressions, not as interface methods
            && not (hasAnyEmitAttribute memb.Attributes)
        )
        |> List.ofSeq
        |> List.groupBy (fun memb -> memb.DisplayName)
        // Remove duplicate method when we have getters and setters
        |> List.collect (fun (_, gr) ->
            gr
            |> List.filter (fun memb -> gr.Length = 1 || (memb.IsGetter || memb.IsSetter))
        )

    let classMembers =
        [
            for memb in members do
                let name = memb.DisplayName |> Naming.toPythonNaming |> Helpers.clean

                let abstractMethod = com.GetImportExpr(ctx, "abc", "abstractmethod")

                let decorators =
                    [
                        if memb.IsValue || memb.IsGetter then
                            Expression.name "property"
                        if memb.IsSetter then
                            Expression.name $"%s{name}.setter"

                        abstractMethod
                    ] // Must be after @property

                let name = com.GetIdentifier(ctx, name)

                let args =
                    let args =
                        [
                            if memb.IsInstance then
                                Arg.arg "self"
                            for n, parameterGroup in memb.CurriedParameterGroups |> Seq.indexed do
                                for m, pg in parameterGroup |> Seq.indexed do
                                    let ta, _ = Annotation.typeAnnotation com ctx None pg.Type

                                    Arg.arg (pg.Name |> Option.defaultValue $"__arg%d{n + m}", annotation = ta)
                        ]

                    Arguments.arguments args

                let returnType, _ = Annotation.typeAnnotation com ctx None memb.ReturnParameter.Type

                let body = [ Statement.ellipsis ]

                // Calculate type parameters for generic abstract methods
                let typeParams = Annotation.makeMemberTypeParams com ctx memb.GenericParameters

                Statement.functionDef (
                    name,
                    args,
                    body,
                    returns = returnType,
                    decoratorList = decorators,
                    typeParams = typeParams
                )

            if members.IsEmpty then
                Statement.Pass
        ]

    let bases =
        [
            let interfaces =
                classEnt.AllInterfaces
                |> List.ofSeq
                |> List.map (fun int -> int.Entity)
                |> List.filter (fun ent -> ent.FullName <> classEnt.FullName)

            for ref in interfaces do
                let entity = com.TryGetEntity(ref)

                match entity with
                | Some entity ->
                    let expr, _stmts = Annotation.makeEntityTypeAnnotation com ctx entity.Ref [] None

                    expr
                | None -> ()

            // Only add Protocol base if no interfaces (since the included interfaces will be protocols themselves)
            if List.isEmpty interfaces then
                com.GetImportExpr(ctx, "typing", "Protocol")

        // Python 3.12: Generic type parameters are handled in class definition, not as base classes
        ]

    // Generate type parameters for Python 3.12
    let typeParams = makeEntityTypeParams com ctx classEnt

    [
        Statement.classDef (classIdent, body = classMembers, bases = bases, typeParams = typeParams)
    ]

// Helper function to extract initial value from getter body
// Returns (value, isFactory, externalFields, stmts) where isFactory indicates if the value is a lambda function,
// externalFields contains field names that need to be added as class attributes
// and stmts contains statements (e.g., local function declarations) from closurd that need lifted out of the
// to the nearest enclosing scope.
let getInitialValue
    com
    ctx
    (className: string)
    (getterMemb: Fable.MemberDecl)
    (wrapInLambda: bool)
    (fallback: Expression)
    : (Expression * bool * string list * Statement list)
    =
    match getterMemb.Body with
    | Fable.Value(kind, r) ->
        // Use transformValue for literal values - never wrapped
        let value, stmts = transformValue com ctx r kind
        value, false, [], stmts
    | Fable.Get(Fable.IdentExpr _, Fable.FieldGet fieldInfo, _, _) when
        fieldInfo.Name.EndsWith("@", System.StringComparison.Ordinal)
        ->
        // For @ fields (backing fields), use fallback value directly instead of lambda reference
        // The static constructor will set the actual value
        fallback, false, [], []
    | Fable.Get(Fable.IdentExpr identExpr, Fable.FieldGet fieldInfo, _, _) when identExpr.Name = className ->
        // For external field references (like backing_field), create lambda and note the field
        let classNameExpr = Expression.name identExpr.Name
        let originalFieldName = fieldInfo.Name
        // Apply Python naming convention transformation to match setter/static constructor
        let transformedFieldName = originalFieldName |> Naming.toPythonNaming

        let fieldAccess =
            Expression.attribute (classNameExpr, Identifier transformedFieldName)

        if wrapInLambda then
            Expression.lambda (Arguments.arguments [], fieldAccess), true, [ transformedFieldName ], []
        else
            fieldAccess, false, [ transformedFieldName ], []
    | _ ->
        // For other expressions, try to transform
        try
            let expr, stmts = com.TransformAsExpr(ctx, getterMemb.Body)

            if wrapInLambda then
                Expression.lambda (Arguments.arguments [], expr), true, [], stmts
            else
                expr, false, [], stmts
        with _ ->
            fallback, false, [], []


// Process a single static property and return (property assignment, extra statements, external field declarations)
let transformStaticProperty
    (com: IPythonCompiler)
    (ctx: Context)
    (name: string)
    (ent: Fable.Entity)
    (propName: string)
    (members: Fable.MemberDecl list)
    =
    let getter =
        members |> List.tryFind (fun m -> (com.GetMember(m.MemberRef)).IsGetter)

    let setter =
        members |> List.tryFind (fun m -> (com.GetMember(m.MemberRef)).IsSetter)

    // printfn "transformStaticProperty: %A" propName
    let propertyName =
        if shouldUseRecordFieldNaming ent then
            propName |> Naming.toRecordFieldSnakeCase |> Helpers.clean
        else
            propName |> Naming.toPropertyNaming

    // Create generic StaticProperty with type parameter for better type inference
    let makeStaticProperty (propType: Fable.Type) args isFactory =
        let className =
            if isFactory then
                "StaticLazyProperty"
            else
                "StaticProperty"

        let staticPropertyClass = libValue com ctx "util" className

        // Check if the property type references the current class (forward reference needed)
        let typeAnnotation =
            match propType with
            | Fable.DeclaredType(entRef, _) when com.GetEntity(entRef).DisplayName = name ->
                // Use string forward reference for self-referencing types
                Expression.stringConstant name
            | _ ->
                // Use normal type annotation
                let ta, _ = Annotation.typeAnnotation com ctx None propType
                ta

        let genericStaticProperty =
            Expression.subscript (staticPropertyClass, typeAnnotation)

        Expression.call (genericStaticProperty, args)

    // StaticProperty descriptors handle their own storage - no separate backing fields needed

    // Helper function to generate external field declarations
    let generateExternalFieldDeclarations (externalFields: string list) =
        externalFields
        |> List.map (fun fieldName ->
            let fieldType = Expression.name "str" // Default to str type for external fields
            let defaultValue = Expression.stringConstant ""
            Statement.assign (Expression.name fieldName, fieldType, defaultValue)
        )

    match getter, setter with
    | Some getterMemb, None ->
        // Read-only property
        let fallback = Util.getDefaultValueForType com ctx getterMemb.Body.Type

        let initialValue, isFactory, externalFields, initialValueStmts =
            getInitialValue com ctx name getterMemb true fallback

        let propExpr = makeStaticProperty getterMemb.Body.Type [ initialValue ] isFactory

        // Generate class attribute declarations for external fields
        let externalFieldDeclarations = generateExternalFieldDeclarations externalFields

        Some(propertyName, propExpr), initialValueStmts @ externalFieldDeclarations

    | Some getterMemb, Some setterMemb ->
        // Property with both getter and setter
        let setterArgs, setterBody, _setterReturnType =
            getMemberArgsAndBody com ctx (Attached true) false setterMemb.Args setterMemb.Body

        let fallback = Util.getDefaultValueForType com ctx getterMemb.Body.Type

        let initialValue, isFactory, externalFields, initialValueStmts =
            getInitialValue com ctx name getterMemb true fallback

        // Check if setter has custom logic beyond simple assignment to the property itself
        // For simple properties, we don't need setter functions since StaticProperty handles storage
        let hasCustomSetter =
            match setterBody with
            | [ Statement.Global { Names = [] }
                Statement.Assign {
                                     Targets = [ Expression.Attribute {
                                                                          Value = Expression.Name {
                                                                                                      Id = Identifier className_
                                                                                                  }
                                                                          Attr = Identifier propName_
                                                                      } ]
                                 } ] when className_ = name && propName_ = propertyName -> false // Simple assignment to the property itself - no custom setter needed
            | _ -> true // Has custom logic, multiple statements, or assigns to different fields

        if hasCustomSetter then
            // Generate setter function for complex setters
            let setterFunc =
                generateStaticPropertySetter com ctx name propertyName setterBody setterArgs

            let setterFuncName = $"_%s{name}_%s{propertyName}_setter"

            let propExpr =
                makeStaticProperty getterMemb.Body.Type [ initialValue; Expression.name setterFuncName ] isFactory

            // Generate class attribute declarations for external fields
            let externalFieldDeclarations = generateExternalFieldDeclarations externalFields

            Some(propertyName, propExpr), initialValueStmts @ [ setterFunc ] @ externalFieldDeclarations
        else
            // Simple setter, no custom function needed - just use the descriptor
            let propExpr = makeStaticProperty getterMemb.Body.Type [ initialValue ] isFactory

            // Generate class attribute declarations for external fields
            let externalFieldDeclarations = generateExternalFieldDeclarations externalFields

            Some(propertyName, propExpr), initialValueStmts @ externalFieldDeclarations

    | None, Some setterMemb ->
        // Setter-only property (unusual but possible)
        let setterArgs, setterBody, _setterReturnType =
            getMemberArgsAndBody com ctx (Attached true) false setterMemb.Args setterMemb.Body

        let setterFunc =
            generateStaticPropertySetter com ctx name propertyName setterBody setterArgs

        let setterFuncName = $"_%s{name}_%s{propertyName}_setter"

        // For setter-only properties, we need to get the type from somewhere
        // Use the setter's value parameter type
        let propertyType =
            match setterMemb.Args with
            | [ _; valueArg ] -> valueArg.Type
            | [ valueArg ] -> valueArg.Type
            | _ -> Fable.Any

        let propExpr =
            makeStaticProperty propertyType [ Expression.none; Expression.name setterFuncName ] false

        Some(propertyName, propExpr), [ setterFunc ]

    | None, None ->
        // Should not happen
        None, []

// Extract static properties handling into its own function
let transformStaticProperties (com: IPythonCompiler) (ctx: Context) (decl: Fable.ClassDecl) (ent: Fable.Entity) =

    let staticPropMembers =
        decl.AttachedMembers
        |> List.filter (fun memb ->
            let info = com.GetMember(memb.MemberRef)
            not info.IsInstance && (info.IsGetter || info.IsSetter) && not memb.IsMangled
        )

    if List.isEmpty staticPropMembers then
        [], [], []
    else
        // Group static properties by name
        let groupedProps =
            staticPropMembers |> List.groupBy (fun memb -> memb.Name) |> Map.ofList

        let propStatements = ResizeArray<Statement>()
        let propMap = ResizeArray<string * Expression>()

        // Process each static property
        for propName, members in Map.toList groupedProps do
            let propertyAssignment, extraStatements =
                transformStaticProperty com ctx decl.Name ent propName members

            match propertyAssignment with
            | Some(name, expr) -> propMap.Add((name, expr))
            | None -> ()

            extraStatements |> List.iter propStatements.Add

        let allStatements = List.ofSeq propStatements

        // Separate backing field declarations (for class body) from setter functions (for module level)
        let backingFieldDeclarations, setterFunctions =
            allStatements
            |> List.partition (fun stmt ->
                match stmt with
                | Statement.AnnAssign _ -> true // Backing field declarations go in class body
                | Statement.FunctionDef _ -> false // Setter functions go at module level
                | _ -> false
            )

        // Add static property declarations to class members
        let staticPropertyDeclarations =
            propMap
            |> Seq.toList
            |> List.map (fun (propName, propExpr) ->
                // Use the computed expression (factory function or value)
                Statement.assign ([ Expression.name propName ], propExpr)
            )

        // Return: (classMembers, setterFunctions, backingFieldDeclarations)
        staticPropertyDeclarations, setterFunctions, backingFieldDeclarations

let rec transformDeclaration (com: IPythonCompiler) ctx (decl: Fable.Declaration) =
    // printfn "transformDeclaration: %A" decl
    // printfn "ctx.UsedNames: %A" ctx.UsedNames

    let withCurrentScope (ctx: Context) (usedNames: Set<string>) f =
        let ctx =
            { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }

        let result = f ctx

        ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)

        result

    match decl with
    | Fable.ModuleDeclaration decl -> decl.Members |> List.collect (transformDeclaration com ctx)

    | Fable.ActionDeclaration decl ->
        withCurrentScope ctx decl.UsedNames
        <| fun ctx -> transformAction com ctx decl.Body

    | Fable.MemberDeclaration decl ->
        withCurrentScope ctx decl.UsedNames
        <| fun ctx ->
            let info = com.GetMember(decl.MemberRef)

            let decls =
                if info.IsValue then
                    let value, stmts = transformAsExpr com ctx decl.Body
                    let name = com.GetIdentifier(ctx, Naming.toPythonNaming decl.Name)
                    let ta, _ = Annotation.typeAnnotation com ctx None decl.Body.Type

                    stmts @ declareModuleMember com ctx info.IsPublic name (Some ta) value
                else
                    transformModuleFunction com ctx info decl.Name decl.Args decl.Body

            decls

    | Fable.ClassDeclaration decl ->
        // printfn "Class: %A" decl
        let ent = com.GetEntity(decl.Entity)

        // Check for erased unions and generate type aliases
        let hasEraseAttribute =
            ent.Attributes |> Seq.exists (fun att -> att.Entity.FullName = Atts.erase)

        if hasEraseAttribute && ent.IsFSharpUnion then
            // Generate type alias for erased union
            match ent.UnionCases with
            | [ singleCase ] when singleCase.UnionCaseFields.Length = 1 ->
                // Simple case: [<Erase>] type X = X of int becomes X = int
                let field = singleCase.UnionCaseFields.[0]
                let ta, _ = Annotation.typeAnnotation com ctx None field.FieldType
                let name = com.GetIdentifierAsExpr(ctx, decl.Name)
                [ Statement.assign ([ name ], ta) ]
            | _ ->
                // For now, just create an alias to Any for complex cases
                let name = com.GetIdentifierAsExpr(ctx, decl.Name)
                let anyType = com.GetImportExpr(ctx, "typing", "Any")
                [ Statement.assign ([ name ], anyType) ]
        elif hasEraseAttribute && ent.IsInterface then
            // Erased interfaces should not generate any code
            []
        elif hasEraseAttribute then
            // Erased classes (e.g., custom attribute types) should not generate any code
            []
        else
            // Check for PythonClass attribute and extract parameters
            let classAttributes =
                if hasPythonClassAttribute com ent.Attributes then
                    getPythonClassParameters com ent.Attributes
                else
                    ClassAttributes.Default

            // Raise error if classAttributes.Style is ClassStyle.Properties and classAttributes.Init is false
            if classAttributes.Style = ClassStyle.Properties && not classAttributes.Init then
                failwithf "ClassAttributes.Init must be true when ClassAttributes.Style is ClassStyle.Properties"

            // Add class generic parameters to scoped type params so methods don't redeclare them
            let classGenericParams =
                ent.GenericParameters
                |> List.map (fun p -> p.Name |> Helpers.clean)
                |> Set.ofList

            let ctx =
                { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams classGenericParams }

            let classMembers =
                decl.AttachedMembers
                |> List.collect (fun memb ->
                    withCurrentScope ctx memb.UsedNames
                    <| fun ctx ->
                        let info =
                            memb.ImplementedSignatureRef
                            |> Option.map com.GetMember
                            |> Option.defaultWith (fun () -> com.GetMember(memb.MemberRef))

                        if not memb.IsMangled && (info.IsGetter || info.IsSetter) then
                            transformAttachedProperty com ctx ent info memb (Some classAttributes)
                        else
                            transformAttachedMethod com ctx info memb
                )

            // Handle static properties separately using descriptors
            let staticPropertyDeclarations, setterFunctions, backingFieldDeclarations =
                transformStaticProperties com ctx decl ent

            let allClassMembers =
                classMembers @ backingFieldDeclarations @ staticPropertyDeclarations

            let allStatements = setterFunctions

            match ent, decl.Constructor with
            | ent, _ when ent.IsInterface -> transformInterface com ctx ent decl
            | ent, _ when ent.IsFSharpUnion ->
                // Union types need static property statements too!
                let unionStmts = transformUnion com ctx ent decl.Name allClassMembers
                allStatements @ unionStmts
            | _, Some cons ->
                withCurrentScope ctx cons.UsedNames
                <| fun ctx ->
                    let classStmts =
                        transformClassWithPrimaryConstructor com ctx decl allClassMembers cons classAttributes

                    allStatements @ classStmts
            | _, None ->
                let classStmts =
                    transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name allClassMembers classAttributes

                allStatements @ classStmts

let transformTypeVars (com: IPythonCompiler) ctx (typeVars: HashSet<string>) =
    // For Python 3.12, we don't need module-level TypeVar declarations
    // Type parameters are defined directly in function/class signatures
    []

let transformExports (_com: IPythonCompiler) _ctx (exports: HashSet<string>) =
    let exports = exports |> List.ofSeq

    match exports with
    | [] -> []
    | _ ->
        let all = Expression.name "__all__"
        let names = exports |> List.map Expression.stringConstant |> Expression.list

        [ Statement.assign ([ all ], names) ]

let transformImports (_com: IPythonCompiler) (imports: Import list) : Statement list =
    let imports =
        imports
        |> List.map (fun im ->
            let moduleName = im.Module

            match im.Name with
            | Some "*"
            | Some "default" ->
                let (Identifier local) = im.LocalIdent

                if moduleName <> local then
                    Some moduleName, Alias.alias im.LocalIdent
                else
                    None, Alias.alias im.LocalIdent
            | Some name ->
                let name = name |> Naming.toPythonNaming

                Some moduleName, Alias.alias (Identifier(Helpers.clean name), asname = im.LocalIdent)
            | None -> None, Alias.alias (Identifier moduleName, asname = im.LocalIdent)
        )
        |> List.groupBy fst
        |> List.map (fun (a, b) -> a, List.map snd b)
        |> List.sortBy (fun name ->
            let name =
                match name with
                | Some moduleName, _ -> moduleName.ToLower()
                | None, { Name = name } :: _ -> name.Name
                | _ -> ""

            match name with
            | name when name.StartsWith(".", StringComparison.Ordinal) -> "D" + name
            | name when name.StartsWith("__", StringComparison.Ordinal) -> "A" + name
            | name when name.StartsWith("fable", StringComparison.Ordinal) -> "C" + name
            | _ -> "B" + name
        )

    [
        for moduleName, aliases in imports do
            match moduleName with
            | Some name -> Statement.importFrom (Some(Identifier(name)), aliases)
            | None ->
                // Do not put multiple imports on a single line. flake8(E401)
                for alias in aliases do
                    Statement.import [ alias ]
    ]

let getIdentForImport (ctx: Context) (moduleName: string) (name: string option) =
    // printfn "getIdentForImport: %A" (moduleName, name)
    match name with
    | None ->
        // Handle Python-style relative imports like ".native_code", "..native_code", "...module" etc.
        // Path.GetFileNameWithoutExtension(".native_code") returns "" which is wrong
        let moduleName = moduleName.TrimStart('.')

        let lastPart =
            match moduleName.LastIndexOf('.') with
            | -1 -> moduleName
            | idx -> moduleName.Substring(idx + 1)

        if lastPart.Length > 0 then
            lastPart
        else
            Path.GetFileNameWithoutExtension(moduleName)
    | Some name -> name |> Naming.toPythonNaming
    |> getUniqueNameInRootScope ctx
    |> Identifier

let transformFile (com: IPythonCompiler) (file: Fable.File) =
    let declScopes =
        let hs = HashSet()

        for decl in file.Declarations do
            hs.UnionWith(decl.UsedNames)

        hs

    let ctx =
        {
            File = file
            UsedNames =
                {
                    RootScope = HashSet file.UsedNamesInRootScope
                    DeclarationScopes = declScopes
                    CurrentDeclarationScope = Unchecked.defaultof<_>
                }
            BoundVars =
                {
                    EnclosingScope = HashSet()
                    LocalScope = HashSet()
                    Inceptions = 0
                }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty
            TypeParamsScope = 0
            NarrowedTypes = Map.empty
        }

    // printfn "file: %A" file.Declarations
    let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations

    let rootComment =
        com.GetRootModule(com.CurrentFile)
        |> snd
        |> Option.bind FSharp2Fable.TypeHelpers.tryGetXmlDoc

    let typeVars = com.GetAllTypeVars() |> transformTypeVars com ctx
    let importDecls = com.GetAllImports() |> transformImports com
    let exports = com.GetAllExports() |> transformExports com ctx
    let body = importDecls @ typeVars @ rootDecls @ exports
    Module.module' (body, ?comment = rootComment)
