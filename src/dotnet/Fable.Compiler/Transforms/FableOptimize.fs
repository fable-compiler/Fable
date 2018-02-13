module Fable.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open System.Collections.Generic

// TODO: Use trampoline here?
let rec visit f e =
    match e with
    | This | Null _ | IdentExpr _ | Import _ | EntityRef _ | Debugger _ -> e
    | Value kind ->
        match kind with
        | TupleCons exprs -> TupleCons(List.map (visit f) exprs) |> Value
        | ArrayCons(exprs, t) -> ArrayCons(List.map (visit f) exprs, t) |> Value
        | ListCons(head, tail, t) -> ListCons(visit f head, visit f tail, t) |> Value
        | SomeConst(expr, t) -> SomeConst(visit f expr, t) |> Value
        | ArrayAlloc _ | NoneConst _ | ListEmpty _ | NumberCons _ | StringCons _ | BoolCons _ | RegexCons _ -> e
    | Uncurry(e, a) -> Uncurry(visit f e, a)
    | Spread e -> Spread(visit f e)
    | Lambda(args, body, range) -> Lambda(args, visit f body, range)
    | Get (expr, field, typ, range) ->
        Get (visit f expr, visit f field, typ, range)
    | Apply(callee, args, range) ->
        Apply(callee, List.map (visit f) args, range)
    | Call(callee, field, args, isCons, typ, range) ->
        let callee =
            match callee with
            | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ -> callee
            | Callee callee -> Callee(visit f callee)
        Call(callee, Option.map (visit f) field, List.map (visit f) args, isCons, typ, range)
    | VarDeclaration (var, expr, isMut, range) ->
        VarDeclaration(var, visit f expr, isMut, range)
    | Sequential (exprs, range) ->
        Sequential(List.map (visit f) exprs, range)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(visit f cond, visit f thenExpr, visit f elseExpr, r)
    | Set(callee, prop, value, r) ->
        Set(visit f callee, Option.map (visit f) prop, visit f value, r)
    | ObjectExpr(decls, r) ->
        let decls = decls |> List.map (fun (m, idents, e) -> m, idents, visit f e)
        ObjectExpr(decls, r)
    | Loop (kind, r) ->
        match kind with
        | While(e1, e2) -> Loop(While(visit f e1, visit f e2), r)
        | For(i, e1, e2, e3, up) -> Loop(For(i, visit f e1, visit f e2, visit f e3, up), r)
        | ForOf(i, e1, e2) -> Loop(ForOf(i, visit f e1, visit f e2), r)
    | TryCatch(body, catch, finalizer, r) ->
        TryCatch(visit f body,
                 Option.map (fun (i, e) -> i, visit f e) catch,
                 Option.map (visit f) finalizer, r)
    | Switch(matchValue, cases, defaultCase, t, r) ->
        Switch(visit f matchValue,
               List.map (fun (cases, body) -> List.map (visit f) cases, visit f body) cases,
               Option.map (visit f) defaultCase, t, r)
    | Throw(e, t, r) -> Throw(visit f e, t, r)
    |> f

let checkArgsForDoubleEvalRisk bodyExpr (vars: Ident seq) =
    let varsDic = Dictionary()
    let mutable doubleEvalRisk = false
    for var in vars do varsDic.Add(var.Name, 0)
    let rec countRefs = function
        | IdentExpr(v, _) ->
            match varsDic.TryGetValue(v.Name) with
            | true, count ->
                varsDic.[v.Name] <- count + 1
                if count > 0 then
                    doubleEvalRisk <- true
            | false, _ -> ()
        | expr ->
            if not doubleEvalRisk then
                expr.ImmediateSubExpressions |> Seq.iter countRefs
    countRefs bodyExpr
    doubleEvalRisk

let replaceVars (vars: Ident seq) (exprs: Expr seq) bodyExpr =
    let varsDic = Dictionary()
    for var, expr in Seq.zip vars exprs do
        varsDic.Add(var.Name, expr)
    let replaceVars' = function
        | IdentExpr(v, _) as e ->
            match varsDic.TryGetValue(v.Name) with
            | true, replacement -> replacement
            | false, _ -> e
        | e -> e
    visit replaceVars' bodyExpr

let (|RestAndTail|_|) (xs: _ list) =
    match List.rev xs with
    | [] -> None
    | [x] -> Some([], x)
    | tail::rest -> Some(List.rev rest, tail)

let (|LambdaExpr|_|) (expr: Expr) =
    match expr with
    | Lambda(args, body, _) -> Some([], args, body)
    | Sequential(RestAndTail(exprs, Lambda(args, body, _)),_) -> Some(exprs, args, body)
    | _ -> None

let optimizeExpr (com: ICompiler) (expr: Expr) =
    let optimizeExpr' = function
        // TODO: Optimize also binary operations with numerical or string literals
        // TODO: Don't inline if one of the arguments is `this`?
        | Apply(LambdaExpr(prevExprs, args, body), argExprs, range) when List.sameLength args argExprs ->
            let doubleEvalRisk =
                List.zip args argExprs
                // Check which argExprs have doubleEvalRisk, i.e. they're not IdentValue
                |> List.choose (fun (arg, argExpr) ->
                    if hasDoubleEvalRisk argExpr then Some arg else None)
                |> checkArgsForDoubleEvalRisk body
            let lambdaBody =
                if doubleEvalRisk then
                    let tempVars =
                        argExprs |> List.map (fun argExpr ->
                            let tmpVar = com.GetUniqueVar() |> makeIdent
                            let tmpVarExp = IdentExpr(tmpVar, None)
                            tmpVarExp, VarDeclaration(tmpVar, argExpr, false, None))
                    let body = replaceVars args (List.map fst tempVars) body
                    makeSequential body.Range ((List.map snd tempVars) @ [body])
                else
                    replaceVars args argExprs body
            match prevExprs with
            | [] -> lambdaBody
            | prevExprs -> Sequential(prevExprs@[lambdaBody], range)
        | e -> e
    visit optimizeExpr' expr

let rec optimizeDeclaration (com: ICompiler) = function
    | EntityDeclaration(e, isPublic, privName, decls, r) ->
        EntityDeclaration(e, isPublic, privName, List.map (optimizeDeclaration com) decls, r)
    | ActionDeclaration(expr, range) ->
        ActionDeclaration(optimizeExpr com expr, range)
    | FunctionDeclaration(isPublic, privName, args, body, range) ->
        FunctionDeclaration(isPublic, privName, args, optimizeExpr com body, range)

let optimizeFile (com: ICompiler) (file: File) =
    let newDecls = List.map (optimizeDeclaration com) file.Declarations
    File(file.SourcePath, file.Root, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
