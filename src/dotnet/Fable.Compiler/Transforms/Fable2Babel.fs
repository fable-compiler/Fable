module Fable.Transforms.Fable2Babel

open Fable
open Fable.Core
open Fable.AST
open Fable.AST.Babel
open System.Collections.Generic

type ReturnStrategy =
    | Return
    | Assign of Expression // TODO: Add SourceLocation?

type Import =
  { path: string
    selector: string
    localIdent: string option }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract ReplaceArgs: bool
    abstract IsRecursiveRef: Fable.Expr -> bool

// let private getTailCallArgIds (com: ICompiler) (args: Fable.Ident list) =
//     // If some arguments are functions we need to capture the current values to
//     // prevent delayed references from getting corrupted, for that we use block-scoped
//     // ES2015 variable declarations. See #681
//     let replaceArgs =
//         args |> List.exists (fun arg ->
//             match arg.Type with
//             | Fable.LambdaType _ -> true
//             | _ -> false)
//     replaceArgs, args |> List.map (fun arg -> if replaceArgs then com.GetUniqueVar() else arg.Name)

// type NamedTailCallOpportunity(com: ICompiler, name, args: Fable.Ident list) =
//     let replaceArgs, argIds = getTailCallArgIds com args
//     interface ITailCallOpportunity with
//         member __.Label = name
//         member __.Args = argIds
//         member __.ReplaceArgs = replaceArgs
//         member __.IsRecursiveRef(e) =
//             match e with
//             | Fable.IdentExpr(id, _) -> name = id.Name
//             | _ -> false

type Context =
  { file: Fable.File
    moduleFullName: string
    isFunctionBody: bool
    addDeclaredVar: Fable.Ident -> unit
    tailCallOpportunity: ITailCallOpportunity option
    optimizeTailCall: unit -> unit }

type IBabelCompiler =
    inherit ICompiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetAllDependencies: unit -> seq<string>
    abstract GetImportExpr: Context * selector: string * path: string * Fable.ImportKind -> Expression
    abstract TransformExpr: Context * Fable.Expr -> Expression
    abstract TransformStatement: Context * Fable.Expr -> Statement list
    abstract TransformExprAndResolve: Context * ReturnStrategy * Fable.Expr -> Statement list
    abstract TransformObjectExpr: Context * Fable.ObjectMember list -> Expression
    abstract TransformFunction: Context * ITailCallOpportunity option * Fable.Ident list * Fable.Expr
        -> (Pattern list) * U2<BlockStatement, Expression>

module Util =
    let inline (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
    let inline (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr(ctx, e)
    let inline (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement(ctx, e)

    let (|FunctionArgs|) = function
        | Fable.Lambda arg -> [arg]
        | Fable.Delegate args -> args

    // // TODO: Optimization: Compile sequential as expression if possible
    // | Assignments ctx (declVars, exprs, r) ->
    //     for var in declVars do
    //         ctx.addDeclaredVar(var)
    //     let exprs = List.map (fun e -> com.TransformExpr(ctx, e)) exprs
    //     upcast SequenceExpression(exprs, ?loc=r)

    /// Matches a sequence of assignments and a return value: a.b = 1, a.c = 2, a
    // let (|Assignments|_|) (ctx: Context) e =
    //     match e with
    //     | Fable.Sequential(exprs, r) when ctx.isFunctionBody ->
    //         let length = exprs.Length
    //         ((true, [], [], 1), exprs)
    //         ||> List.fold (fun (areAssignments, declVars, exprs, i) e ->
    //             match areAssignments, e with
    //             | false, _ -> false, [], [], 0
    //             | _, Fable.Set _ when i < length ->
    //                 true, declVars, e::exprs, i + 1
    //             | _, Fable.VarDeclaration(ident, value, _, r) when i < length ->
    //                 let setExpr = Fable.Set(Fable.IdentExpr(ident, None), None, value, r)
    //                 true, ident::declVars, setExpr::exprs, i + 1
    //             | _, e when not e.IsJsStatement -> //Check `i = lenght`?
    //                 true, declVars, e::exprs, i + 1
    //             | _ -> false, [], [], 0)
    //         |> function
    //             | true, declVars, exprs, _ -> Some(List. rev declVars, List.rev exprs, r)
    //             | false, _, _, _ -> None
    //     | _ -> None

    let addErrorAndReturnNull (com: ICompiler) (fileName: string) (range: SourceLocation option) (error: string) =
        com.AddLog(error, Severity.Error, ?range=range, fileName=fileName)
        NullLiteral () :> Expression

    let ident (id: Fable.Ident) =
        Identifier id.Name

    let identFromName name =
        let name = Naming.sanitizeIdent (fun _ -> false) name
        Identifier name

    let ofInt i =
        NumericLiteral(float i) :> Expression

    let propFromName propName: Expression * bool =
        if Naming.hasIdentForbiddenChars propName
        then upcast StringLiteral propName, true
        else upcast Identifier propName, false

    let coreLibCall (com: IBabelCompiler) (ctx: Context) coreModule memb args =
        let callee = com.GetImportExpr(ctx, memb, coreModule, Fable.CoreLib)
        CallExpression(callee, args) :> Expression

    let get r left propName =
        let expr, computed = propFromName propName
        MemberExpression(left, expr, computed, ?loc=r) :> Expression

    let getExpr r (object: Expression) (expr: Expression) =
        let expr, computed =
            match expr with
            | :? StringLiteral as e -> propFromName e.value
            | e -> e, true
        MemberExpression (object, expr, computed, ?loc=r) :> Expression

    let rec accessExpr (members: string list) (baseExpr: Expression) =
        match members with
        | [] -> baseExpr
        | m::ms -> get None baseExpr m |> accessExpr ms

    let buildArray (com: IBabelCompiler) ctx typ (arrayKind: Fable.NewArrayKind) =
        match typ with
        | Fable.Number kind when com.Options.typedArrays ->
            let cons =
                Replacements.getTypedArrayName com kind
                |> Identifier
            let args =
                match arrayKind with
                | Fable.ArrayValues args ->
                    [ List.map (fun e -> com.TransformExpr(ctx, e)) args
                      |> ArrayExpression :> Expression ]
                | Fable.ArrayAlloc size ->
                    [ NumericLiteral(float size) ]
            NewExpression(cons, args) :> Expression
        | _ ->
            match arrayKind with
            | Fable.ArrayValues args ->
                List.map (fun e -> com.TransformExpr(ctx, e)) args
                |> ArrayExpression :> Expression
            | Fable.ArrayAlloc size ->
                upcast NewExpression(Identifier "Array", [ NumericLiteral(float size) ])

    let buildStringArray strings =
        strings
        |> List.map (fun x -> StringLiteral x :> Expression)
        |> ArrayExpression :> Expression

    let assign range left right =
        AssignmentExpression(AssignEqual, left, right, ?loc=range)
        :> Expression

    /// Immediately Invoked LambdaType Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        CallExpression(ArrowFunctionExpression([], body), [])

    let varDeclaration (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration(var, value, kind)

    let macroExpression range (txt: string) args =
        MacroExpression(txt, args, ?loc=range) :> Expression

    let getMemberArgsAndBody (com: IBabelCompiler) ctx tc args (body: Fable.Expr) hasRestParams =
        let args, body = com.TransformFunction(ctx, tc, args, body)
        let args =
            if not hasRestParams
            then args
            else let args = List.rev args
                 (RestElement(args.Head) :> Pattern) :: args.Tail |> List.rev
        match body with
        | U2.Case1 e -> args, e
        | U2.Case2 e -> args, BlockStatement [ReturnStatement e]

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | :? NumericLiteral, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.EnumType _ ->
            BinaryExpression(BinaryOrBitwise, e, NumericLiteral(0.)) :> Expression
        | _ -> e

    let makeAnonymousFunction args body: Expression =
        match body with
        | U2.Case1 body -> body
        | U2.Case2 e -> BlockStatement [ReturnStatement e]
        |> fun body -> upcast FunctionExpression (args, body)

    let transformValue (com: IBabelCompiler) (ctx: Context) value: Expression =
        match value with
        | Fable.This _ -> upcast ThisExpression ()
        | Fable.Null _ -> upcast NullLiteral ()
        | Fable.UnitConstant -> upcast NullLiteral () // TODO: Use `void 0`?
        | Fable.BoolConstant x -> upcast BooleanLiteral (x)
        | Fable.CharConstant x -> upcast StringLiteral (string x)
        | Fable.StringConstant x -> upcast StringLiteral (x)
        | Fable.NumberConstant (x,_) ->
            if x < 0.
            // Negative numeric literals can give issues in Babel AST, see #1186
            then upcast UnaryExpression(UnaryMinus, NumericLiteral(x * -1.))
            else upcast NumericLiteral x
        | Fable.RegexConstant (source, flags) -> upcast RegExpLiteral (source, flags)
        | Fable.NewArray (arrayKind, typ) -> buildArray com ctx typ arrayKind
        | Fable.NewTuple vals -> buildArray com ctx Fable.Any (Fable.ArrayValues vals)
        | Fable.NewList (headAndTail, _) ->
            let vals = match headAndTail with Some(head, tail) -> [head; tail] | None -> []
            buildArray com ctx Fable.Any (Fable.ArrayValues vals)
        | Fable.NewOption (value, t) ->
            match value with
            | Some (TransformExpr com ctx e) ->
                match t with
                // For unit, unresolved generics or nested options, create a runtime wrapper
                // See fable-core/Option.ts for more info
                | Fable.Unit | Fable.GenericParam _ | Fable.Option _ ->
                    coreLibCall com ctx "Option" "some" [e]
                | _ -> e // For other types, erase the option
            | None -> upcast NullLiteral ()
        | Fable.Enum(kind,_) ->
            match kind with
            | Fable.NumberEnum i -> ofInt i
            | Fable.StringEnum s -> upcast StringLiteral s
        | Fable.NewRecord(vals,ent,_) ->
            let members =
                (ent.FSharpFields, vals)
                ||> Seq.map2 (fun fi v -> fi.Name, v, Fable.ObjectValue false)
                |> Seq.toList
            com.TransformObjectExpr(ctx, members)
        | Fable.NewUnion(vals,uci,_,_) ->
            let tag = Fable.Value(Fable.StringConstant uci.Name)
            Fable.ArrayValues (tag::vals) |> buildArray com ctx Fable.Any
        | Fable.NewErasedUnion(e,_) -> com.TransformExpr(ctx, e)
        | Fable.UnionCaseTag(uci,_) -> upcast StringLiteral uci.Name

    let transformObjectExpr (com: IBabelCompiler) ctx members: Expression =
        let makeMethod kind prop computed hasSpread args body =
            let args, body' = getMemberArgsAndBody com ctx None args body hasSpread
            ObjectMethod(kind, prop, args, body', computed=computed) |> U3.Case2
        members |> List.map (fun (name, expr, kind) ->
            let prop, computed = propFromName name
            match expr with
            | Fable.Function(Fable.Delegate args, body) ->
                match kind with
                | Fable.ObjectValue hasSpread ->
                    makeMethod ObjectMeth prop computed hasSpread args body
                | Fable.ObjectGetter ->
                    makeMethod ObjectGetter prop computed false args body
                | Fable.ObjectSetter ->
                    makeMethod ObjectGetter prop computed false args body
            | _ ->
                let value = com.TransformExpr(ctx, expr)
                ObjectProperty(prop, value, computed=computed) |> U3.Case1
        ) |> ObjectExpression :> Expression

    let transformArgs (com: IBabelCompiler) ctx (info: Fable.CallInfo) = function
        | []
        | [Fable.Value Fable.UnitConstant] -> []
        | [_; Fable.Value Fable.UnitConstant] when info.HasThisArg -> []
        | [Fable.Value(Fable.NewTuple args)] when info.HasTupleSpread ->
            List.map (fun e -> com.TransformExpr(ctx, e)) args
        | args when info.HasSeqSpread ->
            match List.rev args with
            | [] -> []
            | Fable.Value(Fable.NewArray(Fable.ArrayValues spreadArgs,_))::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
                rest @ (List.map (fun e -> com.TransformExpr(ctx, e)) spreadArgs)
            | last::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
                rest @ [SpreadElement(com.TransformExpr(ctx, last))]
        | args -> List.map (fun e -> com.TransformExpr(ctx, e)) args

    let transformOperation com ctx range opKind: Expression =
        match opKind with
        | Fable.UnaryOperation(op, TransformExpr com ctx expr) ->
            upcast UnaryExpression (op, expr, ?loc=range)
        | Fable.BinaryOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast BinaryExpression (op, left, right, ?loc=range)
        | Fable.LogicalOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast LogicalExpression (op, left, right, ?loc=range)
        | Fable.Emit(emit, argsAndCallInfo) ->
            match argsAndCallInfo with
            | Some(args, callInfo) ->
                transformArgs com ctx callInfo args
                |> macroExpression range emit
            | None -> macroExpression range emit []
        | Fable.Call(callee, memb, args, callInfo) ->
            let args = transformArgs com ctx callInfo args
            let callee =
                match memb with
                | Some memb -> get None (com.TransformExpr(ctx, callee)) memb
                | None -> com.TransformExpr(ctx, callee)
            if callInfo.IsConstructor
            then upcast NewExpression(callee, args, ?loc=range)
            else upcast CallExpression(callee, args, ?loc=range)
        | Fable.CurriedApply(TransformExpr com ctx applied, args) ->
            match transformArgs com ctx emptyCallInfo args with
            | [] -> upcast CallExpression(applied, [], ?loc=range)
            | head::rest ->
                let baseExpr = CallExpression(applied, [head], ?loc=range) :> Expression
                (baseExpr, rest) ||> List.fold (fun e arg ->
                    CallExpression(e, [arg], ?loc=range) :> Expression)
        | Fable.UnresolvedCall _ ->
            "Unresolved call detected in Babel pass"
            |> addErrorAndReturnNull com ctx.file.SourcePath range

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: BlockStatement =
        match ret with
        | None -> com.TransformStatement(ctx, expr) |> BlockStatement
        | Some ret -> com.TransformExprAndResolve(ctx, ret, expr) |> BlockStatement

    let transformSwitch (com: IBabelCompiler) ctx returnStrategy (matchValue, cases, defaultCase) =
        let transformCase test branch =
            let statements, test =
                let statements =
                    match returnStrategy with
                    | Some ret -> com.TransformExprAndResolve(ctx, ret, branch)
                    | None -> com.TransformStatement(ctx, branch)
                match test with
                | Some(TransformExpr com ctx test) ->
                    match returnStrategy with
                    | Some Return -> statements, Some test
                    | _ -> statements@[BreakStatement()], Some test
                | None -> statements, None // Default branch
            SwitchCase(statements, ?test=test, ?loc=branch.Range)
        let cases =
            cases |> List.collect(fun (tests, branch) ->
                let prev =
                    match List.length tests with
                    | l when l > 1 ->
                        List.take (l - 1) tests
                        |> List.map (fun test ->
                            SwitchCase([], com.TransformExpr(ctx, test)))
                    | _ -> []
                let case = transformCase (List.last tests |> Some) branch
                prev@[case])
        let cases =
            match defaultCase with
            | Some defaultCase -> cases@[transformCase None defaultCase]
            | None -> cases
        SwitchStatement(com.TransformExpr(ctx, matchValue), cases)

    let transformTryCatch com ctx returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with tailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause (ident param,
                    transformBlock com ctx returnStrategy body, ?loc=body.Range))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [TryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer) :> Statement]

    // Even if IfStatement doesn't enforce it, compile both branches as blocks
    // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
    let rec transformIfStatement (com: IBabelCompiler) ctx ret guardExpr thenStmnt elseStmnt =
        let guardExpr = com.TransformExpr(ctx, guardExpr)
        let thenStmnt = transformBlock com ctx ret thenStmnt
        let elseStmnt =
            match elseStmnt: Fable.Expr with
            | Fable.Value(Fable.Null _) when Option.isNone ret -> None
            | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt) ->
                transformIfStatement com ctx ret guardExpr thenStmnt elseStmnt
                :> Statement |> Some
            | e -> transformBlock com ctx ret e :> Statement |> Some
        IfStatement(guardExpr, thenStmnt, ?alternate=elseStmnt)

    let transformGet (com: IBabelCompiler) ctx range expr (getKind: Fable.GetKind) =
        let expr = com.TransformExpr(ctx, expr)
        match getKind with
        | Fable.FieldGet name -> get range expr name
        | Fable.IndexGet index -> getExpr range expr (ofInt index)
        | Fable.DynamicGet(TransformExpr com ctx prop) -> getExpr range expr prop
        | Fable.ListHead -> getExpr range expr (ofInt 0)
        | Fable.ListTail -> getExpr range expr (ofInt 1)
        | Fable.RecordGet(fi,_) -> get range expr fi.Name
        | Fable.TupleGet index -> getExpr range expr (ofInt index)
        | Fable.OptionValue -> coreLibCall com ctx "Option" "value" [expr]
        | Fable.UnionTag _ -> getExpr range expr (ofInt 0)
        | Fable.UnionField(field, uci, _) ->
            let fieldName = field.Name
            let index =
                uci.UnionCaseFields
                |> Seq.findIndex (fun fi -> fi.Name = fieldName)
            getExpr range expr (index + 1 |> ofInt)

    let transformSet (com: IBabelCompiler) ctx range var (value: Fable.Expr) (setKind: Fable.SetKind) =
        let var = com.TransformExpr(ctx, var)
        let value = com.TransformExpr(ctx, value) |> wrapIntExpression value.Type
        let var =
            match setKind with
            | Fable.VarSet -> var
            | Fable.FieldSet name -> get None var name
            | Fable.RecordSet(field, _) -> get None var field.Name
            | Fable.IndexSet i -> getExpr None var (ofInt i)
            | Fable.DynamicSet(TransformExpr com ctx e) -> getExpr None var e
        assign range var value

    let getSetReturnStrategy com ctx (TransformExpr com ctx expr) = function
        | Fable.VarSet -> Assign expr
        | Fable.FieldSet name -> get None expr name |> Assign
        | Fable.IndexSet index -> getExpr None expr (ofInt index) |> Assign
        | Fable.DynamicSet(TransformExpr com ctx prop) -> getExpr None expr prop |> Assign
        | Fable.RecordSet(fi,_) -> get None expr fi.Name |> Assign

    // TODO: Check tail opportunity for inner function declarations
        // let value =
        //     let tc = NamedTailCallOpportunity(com, var.Name, args) :> ITailCallOpportunity |> Some
        //     com.TransformFunction ctx tc args body ||> makeAnonymousFunction body.Range
        // [varDeclaration r (ident var) false value :> Statement]

    let transformImport (com: IBabelCompiler) ctx (selector: string) (path: string) kind =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, kind)
        |> accessExpr parts

    let transformBinding (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if value.IsJsStatement then
            let var = ident var
            let decl = VariableDeclaration var :> Statement
            let body = com.TransformExprAndResolve(ctx, Assign var, value)
            decl::body
        else
            let value =
                match value with
                // Check imports with name placeholder
                | Fable.Import(Naming.placeholder, path, kind, _) ->
                    transformImport com ctx var.Name path kind
                | _ -> com.TransformExpr(ctx, value) |> wrapIntExpression value.Type
            [varDeclaration (ident var) var.IsMutable value :> Statement]

    let rec transformStatement com ctx (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                WhileStatement(guard, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.ForOf (var, TransformExpr com ctx enumerable, body) ->
                // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right
                let var = VariableDeclaration(ident var, kind=Let)
                ForOfStatement(U2.Case1 var, enumerable, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.For (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp) ->
                let op1, op2 =
                    if isUp
                    then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                    else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
                ForStatement(
                    transformBlock com ctx None body,
                    start |> varDeclaration (ident var) true |> U2.Case1,
                    BinaryExpression (op1, ident var, limit),
                    UpdateExpression (op2, false, ident var), ?loc=range) :> Statement
            |> List.singleton

        | Fable.Set(expr, setKind, value, _range) ->
            let ret = getSetReturnStrategy com ctx expr setKind
            com.TransformExprAndResolve(ctx, ret, value)

        | Fable.Let (bindings, body) ->
            let bindings = bindings |> List.collect (fun (i, v) -> transformBinding com ctx i v)
            bindings @ (transformStatement com ctx body)

        | Fable.TryCatch (body, catch, finalizer) ->
            transformTryCatch com ctx None (body, catch, finalizer)

        | Fable.Throw (TransformExpr com ctx ex, _, range) ->
            [ThrowStatement(ex, ?loc=range) :> Statement]

        | Fable.Debugger -> [DebuggerStatement() :> Statement]

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt) ->
            [transformIfStatement com ctx None guardExpr thenStmnt elseStmnt :> Statement ]

        | Fable.Switch(matchValue, cases, defaultCase, _) ->
            [transformSwitch com ctx None (matchValue, cases, defaultCase) :> Statement]

        | Fable.Sequential statements ->
            List.collect (transformStatement com ctx) statements

        // Expressions become ExpressionStatements
        | Fable.Value _ | Fable.IdentExpr _ | Fable.Cast _ | Fable.Import _
        | Fable.Function _ | Fable.ObjectExpr _ | Fable.Operation _ | Fable.Get _  ->
            [ExpressionStatement(com.TransformExpr(ctx, expr), ?loc=expr.Range) :> Statement]

    let rec transformExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        // TODO: Warn an unhandlend cast has reached Babel pass
        | Fable.Cast(expr, _) -> transformExpr com ctx expr

        | Fable.Value kind -> transformValue com ctx kind

        | Fable.IdentExpr ident -> upcast Identifier ident.Name

        | Fable.Import(selector, path, kind, _) ->
            transformImport com ctx selector path kind

        | Fable.Function(FunctionArgs args, body) ->
            com.TransformFunction(ctx, None, args, body) ||> makeAnonymousFunction

        | Fable.ObjectExpr (members, _) ->
            transformObjectExpr com ctx members

        | Fable.Operation(opKind, _, range) ->
            transformOperation com ctx range opKind

        | Fable.Get(expr, getKind, _, range) ->
            transformGet com ctx range expr getKind

        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            TransformExpr com ctx thenExpr,
                            TransformExpr com ctx elseExpr) ->
            upcast ConditionalExpression(guardExpr, thenExpr, elseExpr)

        | Fable.Set(var, setKind, value, range) ->
            transformSet com ctx range var value setKind

        // TODO: Hoist the var declaration to avoid the iife
        // Check also Assignments optimization
        | Fable.Let _ -> iife com ctx expr :> Expression

        // These cannot appear in expression position in JS
        // They must be wrapped in a lambda
        | Fable.Debugger _ | Fable.Throw _
        | Fable.Sequential _ | Fable.Loop _
        | Fable.TryCatch _ | Fable.Switch _ ->
            iife com ctx expr :> Expression

    // let optimizeTailCall (com: IBabelCompiler) (ctx: Context) (tc: ITailCallOpportunity) args =
    //     ctx.optimizeTailCall()
    //     let zippedArgs = List.zip tc.Args args
    //     let tempVars =
    //         let rec checkCrossRefs acc = function
    //             | [] | [_] -> acc
    //             | (argId, _arg)::rest ->
    //                 rest |> List.exists (snd >> deepExists
    //                     (function Fable.IdentExpr(i,_) -> argId = i.Name | _ -> false))
    //                 |> function true -> Map.add argId (com.GetUniqueVar()) acc | false -> acc
    //                 |> checkCrossRefs <| rest
    //         checkCrossRefs Map.empty zippedArgs
    //     [ for (argId, arg) in zippedArgs do
    //         let arg = transformExpr com ctx arg
    //         match Map.tryFind argId tempVars with
    //         | Some tempVar ->
    //             yield varDeclaration (Identifier tempVar) false arg :> Statement
    //         | None ->
    //             yield assign None (Identifier argId) arg |> ExpressionStatement :> Statement
    //       for KeyValue(argId,tempVar) in tempVars do
    //         yield assign None (Identifier argId) (Identifier tempVar) |> ExpressionStatement :> Statement
    //       yield upcast ContinueStatement(Identifier tc.Label) ]

    let rec transformExprAndResolve (com: IBabelCompiler) ctx ret
                                    (expr: Fable.Expr): Statement list =
        let resolve strategy babelExpr: Statement =
            match strategy with
            // TODO: Where to put these int wrappings? Add them also for function arguments?
            | Return -> upcast ReturnStatement(wrapIntExpression expr.Type babelExpr)
            | Assign left -> upcast ExpressionStatement(assign None left babelExpr)

        match expr with
        // TODO: Warn an unhandlend cast has reached Babel pass
        | Fable.Cast(expr, _) ->
            transformExprAndResolve com ctx ret expr

        | Fable.Value kind ->
            transformValue com ctx kind
            |> resolve ret |> List.singleton

        | Fable.IdentExpr ident ->
            Identifier ident.Name :> Expression
            |> resolve ret |> List.singleton

        | Fable.Import(selector, path, kind, _) ->
            transformImport com ctx selector path kind
            |> resolve ret |> List.singleton

        | Fable.Let (bindings, body) ->
            let bindings = bindings |> List.collect (fun (i, v) -> transformBinding com ctx i v)
            bindings @ (transformExprAndResolve com ctx ret body)

        | Fable.ObjectExpr (members, _) ->
            transformObjectExpr com ctx members
            |> resolve ret |> List.singleton

        | Fable.Function(FunctionArgs args, body) ->
            com.TransformFunction(ctx, None, args, body) ||> makeAnonymousFunction
            |> resolve ret |> List.singleton

        | Fable.Operation(callKind, _, range) ->
            // TODO TODO TODO
            // match ctx.tailCallOpportunity, callee, memb, isCons, ret with
            // | Some tc, Fable.Callee callee, None, false, Return
            //         when List.sameLength tc.Args args && tc.IsRecursiveRef callee ->
            //     optimizeTailCall com ctx tc args
            transformOperation com ctx range callKind
            |> resolve ret |> List.singleton

        | Fable.Get(expr, getKind, _, range) ->
            transformGet com ctx range expr getKind
            |> resolve ret |> List.singleton

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt) ->
            [transformIfStatement com ctx (Some ret) guardExpr thenStmnt elseStmnt :> Statement ]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                if i < lasti
                then com.TransformStatement(ctx, statement)
                else com.TransformExprAndResolve(ctx, ret, statement))
            |> List.concat

        | Fable.TryCatch (body, catch, finalizer) ->
            transformTryCatch com ctx (Some ret) (body, catch, finalizer)

        | Fable.Switch(matchValue, cases, defaultCase, _) ->
            [transformSwitch com ctx (Some ret) (matchValue, cases, defaultCase) :> Statement]

        // These cannot be resolved (don't return anything)
        // Just compile as a statement
        | Fable.Debugger _ | Fable.Throw _
        | Fable.Sequential _ | Fable.Loop _ | Fable.Set _
        | Fable.TryCatch _ | Fable.Switch _ ->
            com.TransformStatement(ctx, expr)

    let transformFunction com ctx tailcallChance (args: Fable.Ident list) (body: Fable.Expr) =
        let tailcallChance, args =
            match args with
            | [] -> None, []
            | [unitArg] when unitArg.Type = Fable.Unit -> None, []
            | args -> tailcallChance, List.map ident args
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with isFunctionBody = true
                       addDeclaredVar = declaredVars.Add
                       tailCallOpportunity = tailcallChance
                       optimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body: U2<BlockStatement, Expression> =
            match body with
            | ExprType Fable.Unit
            | Fable.Throw _ | Fable.Debugger _ | Fable.Loop _ | Fable.Set _ ->
                transformBlock com ctx None body |> U2.Case1
            | Fable.Sequential _ | Fable.Let _ | Fable.TryCatch _ | Fable.Switch _ ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | Fable.IfThenElse _ when body.IsJsStatement ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | _ ->
                if Option.isSome tailcallChance
                then transformBlock com ctx (Some Return) body |> U2.Case1
                else transformExpr com ctx body |> U2.Case2
        let args, body =
            match isTailCallOptimized, tailcallChance, body with
            | true, Some tc, U2.Case1 body ->
                let args, body =
                    if tc.ReplaceArgs
                    then
                        let statements =
                            (List.zip args tc.Args, []) ||> List.foldBack (fun (arg, tempVar) acc ->
                                (varDeclaration arg false (Identifier tempVar) :> Statement)::acc)
                        tc.Args |> List.map Identifier, BlockStatement (statements@body.body)
                    else args, body
                args, LabeledStatement(Identifier tc.Label, WhileStatement(BooleanLiteral true, body))
                :> Statement |> List.singleton |> BlockStatement |> U2.Case1
            | _ -> args, body
        let body =
            if declaredVars.Count = 0
            then body
            else
                let varDeclStatements =
                    List.ofSeq declaredVars
                    |> List.distinctBy (fun var -> var.Name)
                    |> List.map (fun var -> VariableDeclaration(ident var, kind=Var) :> Statement)
                let loc, bodyStatements =
                    match body with
                    | U2.Case1 bodyBlock ->
                        bodyBlock.loc, bodyBlock.body
                    | U2.Case2 bodyExpr ->
                        let returnStatement = ReturnStatement(bodyExpr, ?loc=bodyExpr.loc)
                        bodyExpr.loc, [returnStatement :> Statement]
                BlockStatement (varDeclStatements@bodyStatements) |> U2.Case1
        args |> List.map (fun x -> x :> Pattern), body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = CallExpression (funcExpr, [argv]) :> Expression
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        ExpressionStatement(main) :> Statement

    let declareModuleMember publicName privateName isMutable (expr: Expression) =
        let privateIdent = identFromName privateName
        let decl: Declaration =
            match expr with
            | :? ClassExpression as e ->
                upcast ClassDeclaration(e.body, privateIdent,
                    ?super=e.superClass, ?typeParams=e.typeParameters)
            | :? FunctionExpression as e ->
                upcast FunctionDeclaration(privateIdent, e.``params``, e.body)
            | _ -> upcast varDeclaration privateIdent isMutable expr
        match publicName with
        | None ->
            U2.Case1 (decl :> Statement) |> List.singleton
        | Some publicName when publicName = privateName ->
            ExportNamedDeclaration(decl)
            :> ModuleDeclaration |> U2.Case2 |> List.singleton
        | Some publicName ->
            // Replace ident forbidden chars of root members, see #207
            let publicName = Naming.replaceIdentForbiddenChars publicName
            let expSpec = ExportSpecifier(privateIdent, Identifier publicName)
            let expDecl = ExportNamedDeclaration(specifiers=[expSpec])
            [expDecl :> ModuleDeclaration |> U2.Case2; decl :> Statement |> U2.Case1]

    let transformModuleFunction (com: IBabelCompiler) ctx args body (info: Fable.DeclarationInfo) =
        let args, body =
            // let tc = NamedTailCallOpportunity(com, privName, args) :> ITailCallOpportunity |> Some
            let tc = None
            getMemberArgsAndBody com ctx tc args body info.HasSpread
        // Don't lexically bind `this` (with arrow function) or it will fail with extension members
        let expr: Expression = upcast FunctionExpression(args, body)
        declareModuleMember info.PublicName info.PrivateName false expr

    let transformDeclarations (com: IBabelCompiler) ctx decls =
        ([], decls) ||> List.fold (fun acc decl ->
            match decl with
            | Fable.ActionDeclaration e ->
                let statements = transformStatement com ctx e
                let hasVarDeclarations =
                    statements |> List.exists (function
                        | :? VariableDeclaration -> true
                        | _ -> false)
                let statements =
                    if hasVarDeclarations then
                        CallExpression(FunctionExpression([], BlockStatement(statements)), [])
                        |> ExpressionStatement :> Statement
                        |> List.singleton
                    else statements
                statements
                |> List.map U2.Case1
                |> List.append acc
            | Fable.ValueDeclaration(value, info) ->
                match info.PublicName, value with
                // Mutable public values must be compiled as functions (see #986)
                | Some publicName, _ when info.IsMutable ->
                    failwith "TODO: Mutable public values"
            //     // Mutable module values are compiled as functions, because values
            //     // imported from ES2015 modules cannot be modified (see #986)
            //     let expr = transformExpr com ctx body
            //     let import = getCoreLibImport com ctx "Util" "createAtom"
            //     upcast CallExpression(import, [U2.Case1 expr])
                | _, Fable.Function(Fable.Delegate args, body) ->
                    transformModuleFunction com ctx args body info
                | _ ->
                    let value = transformExpr com ctx value
                    declareModuleMember info.PublicName info.PrivateName info.IsMutable value
                |> List.append acc)

    let transformImports (imports: Import seq): U2<Statement, ModuleDeclaration> list =
        imports |> Seq.map (fun import ->
            let specifier =
                import.localIdent
                |> Option.map (fun localId ->
                    let localId = Identifier(localId)
                    match import.selector with
                    | "*" -> ImportNamespaceSpecifier(localId) |> U3.Case3
                    | "default" | "" -> ImportDefaultSpecifier(localId) |> U3.Case2
                    | memb -> ImportSpecifier(localId, Identifier memb) |> U3.Case1)
            import.path, specifier)
        |> Seq.groupBy (fun (path, _) -> path)
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    let t =
                        match x with
                        | U3.Case1 x -> x.``type``
                        | U3.Case2 x -> x.``type``
                        | U3.Case3 x -> x.``type``
                    match t with
                    | "ImportNamespaceSpecifier" -> mems, defs, x::alls
                    | "ImportDefaultSpecifier" -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // There seem to be errors if we mix member, default and namespace imports
            // so we must issue an import statement for each kind
            match [mems; defs; alls] with
            | [[];[];[]] ->
                // No specifiers, so this is just a import for side effects
                [ImportDeclaration([], StringLiteral path) :> ModuleDeclaration |> U2.Case2]
            | specifiers ->
                specifiers |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(specifiers, StringLiteral path)
                    :> ModuleDeclaration |> U2.Case2 |> Some))
        |> Seq.toList

    let makeCompiler (com: ICompiler) =
        let sanitizeSelector com selector =
            if selector = "*"
            then selector
            elif selector = Naming.placeholder
            then "`importMember` must be assigned to a variable"
                 |> addError com None; selector
            // Replace ident forbidden chars of root members, see #207
            else Naming.replaceIdentForbiddenChars selector

        let getLocalIdent (ctx: Context) (imports: Dictionary<string,Import>) (path: string) (selector: string) =
            match selector with
            | "" -> None
            | "*" | "default" ->
                let x = path.TrimEnd('/')
                x.Substring(x.LastIndexOf '/' + 1) |> Some
            | selector -> Some selector
            |> Option.map (Naming.sanitizeIdent (fun s ->
                ctx.file.UsedVarNames.Contains s
                    || (imports.Values |> Seq.exists (fun i -> i.localIdent = Some s))))

        let dependencies = HashSet<string>()
        let imports = Dictionary<string,Import>()

        { new IBabelCompiler with
            member bcom.GetImportExpr(ctx, selector, path, kind) =
                match imports.TryGetValue(path + "::" + selector) with
                | true, i ->
                    match i.localIdent with
                    | Some localIdent -> upcast Identifier(localIdent)
                    | None -> upcast NullLiteral ()
                | false, _ ->
                    let localId = getLocalIdent ctx imports path selector
                    let sanitizedPath =
                        match kind with
                        | Fable.CustomImport | Fable.Internal _ -> path
                        | Fable.CoreLib -> com.FableCore + "/" + path + Naming.targetFileExtension
                    let i =
                      { selector = sanitizeSelector bcom selector
                        localIdent = localId
                        path = sanitizedPath }
                    imports.Add(path + "::" + selector, i)
                    match localId with
                    | Some localId -> upcast Identifier(localId)
                    | None -> upcast NullLiteral ()
            member __.GetAllImports() = upcast imports.Values
            member __.GetAllDependencies() = upcast dependencies
            member bcom.TransformExpr(ctx, e) = transformExpr bcom ctx e
            member bcom.TransformStatement(ctx, e) = transformStatement bcom ctx e
            member bcom.TransformExprAndResolve(ctx, ret, e) = transformExprAndResolve bcom ctx ret e
            member bcom.TransformFunction(ctx, tc, args, body) = transformFunction bcom ctx tc args body
            member bcom.TransformObjectExpr(ctx, members) = transformObjectExpr bcom ctx members
        interface ICompiler with
            member __.Options = com.Options
            member __.FableCore = com.FableCore
            member __.CurrentFile = com.CurrentFile
            member __.GetUniqueVar() = com.GetUniqueVar()
            member __.GetRootModule(fileName) = com.GetRootModule(fileName)
            member __.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)
        }

module Compiler =
    open Util

    let createFacade (sourceFiles: string[]) (facadeFile: string) =
        let decls =
            let importFile = Array.last sourceFiles
            StringLiteral(Path.getRelativeFileOrDirPath false facadeFile false importFile)
            |> ExportAllDeclaration :> ModuleDeclaration |> U2.Case2 |> List.singleton
        Program(facadeFile, decls, sourceFiles=sourceFiles)

    let transformFile (com: ICompiler) (file: Fable.File) =
        try
            // let t = PerfTimer("Fable > Babel")
            let com = makeCompiler com
            let ctx =
              { file = file
                moduleFullName = com.GetRootModule(file.SourcePath)
                isFunctionBody = false
                addDeclaredVar = fun _ -> ()
                tailCallOpportunity = None
                optimizeTailCall = fun () -> () }
            let rootDecls = transformDeclarations com ctx file.Declarations
            let importDecls = transformImports <| com.GetAllImports()
            let dependencies =
                com.GetAllDependencies()
                |> Seq.append file.Dependencies
                |> Seq.toArray
            Program(file.SourcePath, importDecls@rootDecls, dependencies=dependencies)
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.SourcePath, ex) |> raise
