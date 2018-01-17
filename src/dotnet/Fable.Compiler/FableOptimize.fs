module Fable.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open System.Collections.Generic

// TODO: Use trampoline here?
// TODO: Just use ImmediateSubexpressions?
let rec visit f e =
    match e with
    | Value kind ->
        match kind with
        | Spread e -> Value(Spread(visit f e))
        | TypeRef(e, gen) -> Value(TypeRef(e, List.map (fun (n,e) -> n, visit f e) gen))
        | TupleConst exprs -> Value(TupleConst(List.map (visit f) exprs))
        | ArrayConst(kind, t) ->
            match kind with
            | ArrayValues exprs -> ArrayConst(ArrayValues(List.map (visit f) exprs), t) |> Value
            | ArrayAlloc e -> ArrayConst(ArrayAlloc(visit f e), t) |> Value
        | Lambda(args, body, info) -> Value(Lambda(args, visit f body, info))
        | Null | This | Super | IdentValue _ | ImportRef _
        | NumberConst _ | StringConst _ | BoolConst _ | RegexConst _
        | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ -> e
    | Apply(callee, args, kind, typ, range) ->
        Apply(visit f callee, List.map (visit f) args, kind, typ, range)
    | Wrapped(expr, t) ->
        Wrapped(visit f expr, t)
    | VarDeclaration (var, expr, isMut, range) ->
        VarDeclaration(var, visit f expr, isMut, range)
    | Sequential (exprs, range) ->
        Sequential(List.map (visit f) exprs, range)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(visit f cond, visit f thenExpr, visit f elseExpr, r)
    | Set(callee, prop, value, r) ->
        Set(visit f callee, Option.map f prop, visit f value, r)
    | ObjExpr(decls, r) ->
        let decls = decls |> List.map (fun (m, idents, e) -> m, idents, visit f e)
        ObjExpr(decls, r)
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
    | Quote e -> Quote(visit f e)
    | Throw(e, t, r) -> Throw(visit f e, t, r)
    | DebugBreak _ -> e
    |> f

let checkArgsForDoubleEvalRisk bodyExpr (vars: Ident seq) =
    let varsDic = Dictionary()
    let mutable doubleEvalRisk = false
    for var in vars do varsDic.Add(var.Name, 0)
    let rec countRefs = function
        | Value(IdentValue v) ->
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
        | Value(IdentValue v) as e ->
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
    | Value(Lambda(args, body, _)) -> Some([], args, body)
    | Sequential(RestAndTail(exprs, Value(Lambda(args, body, _))),_) -> Some(exprs, args, body)
    | _ -> None

let optimizeExpr (com: ICompiler) (expr: Expr) =
    let optimizeExpr' = function
        // TODO: Optimize also binary operations with numerical or string literals
        | Apply(LambdaExpr(prevExprs, args, body), argExprs, ApplyMeth, _typ, range)
                                                when List.sameLength args argExprs ->
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
                            let tmpVarExp = Value(IdentValue tmpVar)
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
    | MemberDeclaration(memb, isPublic, privName, args, body, range) ->
        MemberDeclaration(memb, isPublic, privName, args, optimizeExpr com body, range)

let optimizeFile (com: ICompiler) (file: File) =
    let newDecls = List.map (optimizeDeclaration com) file.Declarations
    File(file.SourcePath, file.Root, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
