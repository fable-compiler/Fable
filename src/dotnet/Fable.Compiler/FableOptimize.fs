module Fable.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open System.Collections.Generic

let hasDoubleEvalRisk (vars: Ident seq) bodyExpr =
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
    let rec replaceVars' e =
        match e with
        | Value kind ->
            match kind with
            | IdentValue v ->
                match varsDic.TryGetValue(v.Name) with
                | true, replacement -> replacement
                | false, _ -> e
            | Lambda(args, body, info) -> Value(Lambda(args, replaceVars' body, info))
            | _ -> e
        | Apply(callee, args, kind, typ, range) ->
            Apply(replaceVars' callee, List.map replaceVars' args, kind, typ, range)
        | Wrapped(expr, t) ->
            Wrapped(replaceVars' expr, t)
        | VarDeclaration (var, expr, isMut, range) ->
            VarDeclaration(var, replaceVars' expr, isMut, range)
        | Sequential (exprs, range) ->
            Sequential(List.map replaceVars' exprs, range)
        // TODO
        | IfThenElse _ -> e
        | Set _ -> e
        | ObjExpr _ -> e
        | Loop (kind,_) ->
            match kind with
            | While(_e1,_e2) -> e
            | For(_,_e1,_e2,_e3,_) -> e
            | ForOf(_,_e1,_e2) -> e
        | TryCatch _ -> e
        | Switch _ -> e
        // Don't need optimization
        | Quote _ -> e
        | Throw _ -> e
        | DebugBreak _ -> e
    replaceVars' bodyExpr

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

let rec optimizeExpr (expr: Expr) =
    match expr with
    // TODO: Optimize also binary operations with numerical or string literals
    | Apply(LambdaExpr(prevExprs, args, body), argExprs, ApplyMeth, _typ, range) when List.sameLength args argExprs ->
        let argExprs = List.map optimizeExpr argExprs
        // TODO: Check which of argExprs have doubleEvalRisk, i.e. they're not IdentValue
        let lambdaBody =
            if hasDoubleEvalRisk args body
            then replaceVars args argExprs body // TODO: Use temp vars
            else replaceVars args argExprs body
        match prevExprs with
        | [] -> lambdaBody
        | prevExprs ->
            let prevExprs = List.map optimizeExpr prevExprs
            Sequential(prevExprs@[lambdaBody], range)
    | Apply(callee, args, kind, typ, range) ->
        Apply(optimizeExpr callee, List.map optimizeExpr args, kind, typ, range)
    | Wrapped(expr, t) ->
        Wrapped(optimizeExpr expr, t)
    | VarDeclaration (var, expr, isMut, range) ->
        VarDeclaration(var, optimizeExpr expr, isMut, range)
    | Sequential (exprs, range) ->
        Sequential(List.map optimizeExpr exprs, range)
    | Value kind ->
        match kind with
        | Lambda(args, body, info) -> Value(Lambda(args, optimizeExpr body, info))
        | _ -> expr
    // TODO
    | IfThenElse _ -> expr
    | Set _ -> expr
    | ObjExpr _ -> expr
    | Loop (kind,_) ->
        match kind with
        | While(_e1,_e2) -> expr
        | For(_,_e1,_e2,_e3,_) -> expr
        | ForOf(_,_e1,_e2) -> expr
    | TryCatch _ -> expr
    | Switch _ -> expr
    // Don't need optimization
    | Quote _ -> expr
    | Throw _ -> expr
    | DebugBreak _ -> expr

let optimizeFile (file: File) =
    let newDecls =
        file.Declarations
        |> List.map (fun d ->
            match d with
            | EntityDeclaration _ -> d // TODO
            | ActionDeclaration(expr, range) ->
                ActionDeclaration(optimizeExpr expr, range)
            | MemberDeclaration(memb, isPublic, privName, args, body, range) ->
                MemberDeclaration(memb, isPublic, privName, args, optimizeExpr body, range))
    File(file.SourcePath, file.Root, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
