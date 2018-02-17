module Fable.Transforms.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open System.Collections.Generic
open Newtonsoft.Json.Bson
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: Use trampoline here?
let rec visit f e =
    match e with
    | IdentExpr _ | Import _ | Debugger _ -> e
    | Value kind ->
        match kind with
        | This _ | Null _ | UnitConstant
        | BoolConstant _
        | CharConstant _
        | StringConstant _
        | NumberConstant _
        | RegexConstant _
        | Enum _
        | UnionCaseTag _ -> e
        | NewOption(e, t) -> NewOption(Option.map (visit f) e, t) |> Value
        | NewTuple exprs -> NewTuple(List.map (visit f) exprs) |> Value
        | NewArray(kind, t) ->
            match kind with
            | ArrayValues exprs -> NewArray(ArrayValues(List.map (visit f) exprs), t) |> Value
            | ArrayAlloc _ -> e
        | NewList(ht, t) ->
            let ht = ht |> Option.map (fun (h,t) -> visit f h, visit f t)
            NewList(ht, t) |> Value
        | NewRecord(exprs, ent, genArgs) ->
            NewRecord(List.map (visit f) exprs, ent, genArgs) |> Value
        | NewErasedUnion(e, genArgs) ->
            NewErasedUnion(visit f e, genArgs) |> Value
        | NewUnion(exprs, uci, ent, genArgs) ->
            NewUnion(List.map (visit f) exprs, uci, ent, genArgs) |> Value
    | Cast(e, t) -> Cast(visit f e, t)
    | Function(kind, body) -> Function(kind, visit f body)
    | ObjectExpr _ -> e // TODO
    | Operation(kind, t, r) ->
        match kind with
        | Call(callee, memb, args, info) ->
            Operation(Call(visit f callee, memb, List.map (visit f) args, info), t, r)
        | _ -> e // TODO
        // | Apply of applied: Expr * args: Expr list * argTypes: Type list
        // | DynamicApply of applied: Expr * args: Expr list
        // | UnresolvedCall of callee: Expr option * args: Expr list * info: CallInfo
        // | Emit of macro: string * argsAndCallInfo: (Expr list * CallInfo) option
        // | UnaryOperation of UnaryOperator * Expr
        // | BinaryOperation of BinaryOperator * left:Expr * right:Expr
        // | LogicalOperation of LogicalOperator * left:Expr * right:Expr
    | Get _ -> e // TODO
    | Throw(e, typ, r) -> Throw(visit f e, typ, r)
    | Sequential exprs -> Sequential(List.map (visit f) exprs)
    | Let(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, visit f e)
        Let(bs, visit f body)
    | IfThenElse(cond, thenExpr, elseExpr) ->
        IfThenElse(visit f cond, visit f thenExpr, visit f elseExpr)
    | Set _ -> e // TODO
    | Loop (kind, r) ->
        match kind with
        | While(e1, e2) -> Loop(While(visit f e1, visit f e2), r)
        | For(i, e1, e2, e3, up) -> Loop(For(i, visit f e1, visit f e2, visit f e3, up), r)
        | ForOf(i, e1, e2) -> Loop(ForOf(i, visit f e1, visit f e2), r)
    | TryCatch(body, catch, finalizer) ->
        TryCatch(visit f body,
                 Option.map (fun (i, e) -> i, visit f e) catch,
                 Option.map (visit f) finalizer)
    | Switch(matchValue, cases, defaultCase, t) ->
        Switch(visit f matchValue,
               List.map (fun (cases, body) -> List.map (visit f) cases, visit f body) cases,
               Option.map (visit f) defaultCase, t)
    |> f

(*
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
*)

let (|EntFullName|_|) fullName (ent: FSharpEntity) =
    match ent.TryFullName with
    | Some fullName2 when fullName = fullName2 -> Some EntFullName
    | _ -> None

let (|ListLiteral|_|) e =
    let rec untail t acc = function
        | Value(NewList(None, _)) -> Some(List.rev acc, t)
        | Value(NewList(Some(head, tail), _)) -> untail t (head::acc) tail
        | _ -> None
    match e with
    | Value(NewList(None, t)) -> Some([], t)
    | Value(NewList(Some(head, tail), t)) -> untail t [head] tail
    | _ -> None

// TODO: Some cases of coertion shouldn't be erased
// string :> seq #1279
// list (and others) :> seq in Fable 2.0
// concrete type :> interface in Fable 2.0
let cast (com: ICompiler) = function
    | Cast(e, t) ->
        match t with
        | DeclaredType(EntFullName Types.enumerable, _) ->
            match e with
            | ListLiteral(exprs, t) -> NewArray(ArrayValues exprs, t) |> Value
            | _ -> e
        | _ -> e
    | e -> e

let rec optimizeExpr (com: ICompiler) e =
    let optimizeExpr' com e =
        (e, [cast]) ||> List.fold (fun e f -> f com e)
    visit (optimizeExpr' com) e

let rec optimizeDeclaration (com: ICompiler) = function
    | ActionDeclaration expr ->
        ActionDeclaration(optimizeExpr com expr)
    | FunctionDeclaration(publicName, privName, args, body) ->
        FunctionDeclaration(publicName, privName, args, optimizeExpr com body)
    | ValueDeclaration(publicName, privName, value, isMutable) ->
        ValueDeclaration(publicName, privName, optimizeExpr com value, isMutable)

let optimizeFile (com: ICompiler) (file: File) =
    let newDecls = List.map (optimizeDeclaration com) file.Declarations
    File(file.SourcePath, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
