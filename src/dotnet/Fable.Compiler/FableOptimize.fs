module Fable.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open System.Collections.Generic

let rec transformExprTree f e =
    match e with
    | Value kind ->
        match kind with
        | Spread e -> Value(Spread(f e))
        | TypeRef(e, gen) -> Value(TypeRef(e, List.map (fun (n,e) -> n, f e) gen))
        | TupleConst exprs -> Value(TupleConst(List.map f exprs))
        | ArrayConst(kind, t) ->
            match kind with
            | ArrayValues exprs -> ArrayConst(ArrayValues(List.map f exprs), t) |> Value
            | ArrayAlloc e -> ArrayConst(ArrayAlloc(f e), t) |> Value
        | Lambda(args, body, info) -> Value(Lambda(args, f body, info))
        | Null | This | Super | IdentValue _ | ImportRef _
        | NumberConst _ | StringConst _ | BoolConst _ | RegexConst _
        | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ -> e
    | Apply(callee, args, kind, typ, range) ->
        Apply(f callee, List.map f args, kind, typ, range)
    | Wrapped(expr, t) ->
        Wrapped(f expr, t)
    | VarDeclaration (var, expr, isMut, range) ->
        VarDeclaration(var, f expr, isMut, range)
    | Sequential (exprs, range) ->
        Sequential(List.map f exprs, range)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(f cond, f thenExpr, f elseExpr, r)
    | Set(callee, prop, value, r) ->
        Set(f callee, Option.map f prop, f value, r)
    | ObjExpr(decls, ifcs, baseClass, r) ->
        let decls = decls |> List.map (fun (m, idents, e) ->
                m, idents, f e)
        ObjExpr(decls, ifcs, Option.map f baseClass, r)
    | Loop (kind, r) ->
        match kind with
        | While(e1, e2) -> Loop(While(f e1, f e2), r)
        | For(i, e1, e2, e3, up) -> Loop(For(i, f e1, f e2, f e3, up), r)
        | ForOf(i, e1, e2) -> Loop(ForOf(i, f e1, f e2), r)
    | TryCatch(body, catch, finalizer, r) ->
        TryCatch(f body,
                 Option.map (fun (i, e) -> i, f e) catch,
                 Option.map f finalizer, r)
    | Switch(matchValue, cases, defaultCase, t, r) ->
        Switch(f matchValue,
               List.map (fun (cases, body) -> List.map f cases, f body) cases,
               Option.map f defaultCase, t, r)
    | Quote e -> Quote(f e)
    | Throw(e, t, r) -> Throw(f e, t, r)
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
    transformExprTree replaceVars' bodyExpr

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
    transformExprTree optimizeExpr' expr

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
