module Fable.Transforms.Beam.Util

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Beam

/// Convert an F# ident to an Erlang variable name
let toErlangVar (ident: Ident) =
    Naming.capitalizeFirst ident.Name |> Naming.sanitizeErlangVar

let isIntegerType (typ: Fable.AST.Fable.Type) =
    match typ with
    | Fable.AST.Fable.Type.Number(kind, _) ->
        match kind with
        | Float16
        | Float32
        | Float64 -> false
        | _ -> true // Decimal is now a fixed-scale integer
    | _ -> true // default to integer division

/// Check if a Fable expression contains a reference to the given identifier name
let rec containsIdentRef (name: string) (expr: Expr) : bool =
    match expr with
    | IdentExpr ident -> ident.Name = name
    | Call(callee, info, _, _) -> containsIdentRef name callee || info.Args |> List.exists (containsIdentRef name)
    | CurriedApply(applied, args, _, _) -> containsIdentRef name applied || args |> List.exists (containsIdentRef name)
    | Let(_, value, body) -> containsIdentRef name value || containsIdentRef name body
    | LetRec(bindings, body) ->
        bindings |> List.exists (fun (_, v) -> containsIdentRef name v)
        || containsIdentRef name body
    | Lambda(_, body, _) -> containsIdentRef name body
    | Delegate(_, body, _, _) -> containsIdentRef name body
    | IfThenElse(g, t, e, _) -> containsIdentRef name g || containsIdentRef name t || containsIdentRef name e
    | Sequential exprs -> exprs |> List.exists (containsIdentRef name)
    | Value(value, _) ->
        match value with
        | NewAnonymousRecord(values, _, _, _)
        | NewRecord(values, _, _)
        | NewUnion(values, _, _, _)
        | NewTuple(values, _) -> values |> List.exists (containsIdentRef name)
        | NewList(Some(h, t), _) -> containsIdentRef name h || containsIdentRef name t
        | NewOption(Some e, _, _) -> containsIdentRef name e
        | NewArray(NewArrayKind.ArrayValues values, _, _) -> values |> List.exists (containsIdentRef name)
        | StringTemplate(_, _, values) -> values |> List.exists (containsIdentRef name)
        | _ -> false
    | TypeCast(e, _) -> containsIdentRef name e
    | Operation(kind, _, _, _) ->
        match kind with
        | Binary(_, l, r) -> containsIdentRef name l || containsIdentRef name r
        | Unary(_, e) -> containsIdentRef name e
        | Logical(_, l, r) -> containsIdentRef name l || containsIdentRef name r
    | DecisionTree(e, targets) ->
        containsIdentRef name e
        || targets |> List.exists (fun (_, t) -> containsIdentRef name t)
    | DecisionTreeSuccess(_, values, _) -> values |> List.exists (containsIdentRef name)
    | Test(e, _, _) -> containsIdentRef name e
    | Get(e, _, _, _) -> containsIdentRef name e
    | Set(e, _, _, v, _) -> containsIdentRef name e || containsIdentRef name v
    | WhileLoop(guard, body, _) -> containsIdentRef name guard || containsIdentRef name body
    | ForLoop(_, start, limit, body, _, _) ->
        containsIdentRef name start
        || containsIdentRef name limit
        || containsIdentRef name body
    | TryCatch(body, catch, finalizer, _) ->
        containsIdentRef name body
        || (catch
            |> Option.map (fun (_, e) -> containsIdentRef name e)
            |> Option.defaultValue false)
        || (finalizer |> Option.map (containsIdentRef name) |> Option.defaultValue false)
    | Emit(emitInfo, _, _) -> emitInfo.CallInfo.Args |> List.exists (containsIdentRef name)
    | ObjectExpr(members, _, baseCall) ->
        members |> List.exists (fun m -> containsIdentRef name m.Body)
        || match baseCall with
           | Some e -> containsIdentRef name e
           | None -> false
    | _ -> false

/// Check if an identifier is captured inside a closure (Lambda/Delegate) within the expression.
/// Used to determine if a mutable variable's process-dict ref should NOT be erased at scope end.
let isCapturedInClosure (name: string) (expr: Expr) : bool =
    let rec check (inClosure: bool) (expr: Expr) : bool =
        match expr with
        | IdentExpr ident -> inClosure && ident.Name = name
        | Lambda(_, body, _)
        | Delegate(_, body, _, _) -> check true body
        | Call(callee, info, _, _) -> check inClosure callee || info.Args |> List.exists (check inClosure)
        | CurriedApply(applied, args, _, _) -> check inClosure applied || args |> List.exists (check inClosure)
        | Let(_, value, body) -> check inClosure value || check inClosure body
        | LetRec(bindings, body) ->
            bindings |> List.exists (fun (_, v) -> check inClosure v)
            || check inClosure body
        | IfThenElse(g, t, e, _) -> check inClosure g || check inClosure t || check inClosure e
        | Sequential exprs -> exprs |> List.exists (check inClosure)
        | Value(value, _) ->
            match value with
            | NewAnonymousRecord(values, _, _, _)
            | NewRecord(values, _, _)
            | NewUnion(values, _, _, _)
            | NewTuple(values, _) -> values |> List.exists (check inClosure)
            | NewList(Some(h, t), _) -> check inClosure h || check inClosure t
            | NewOption(Some e, _, _) -> check inClosure e
            | NewArray(NewArrayKind.ArrayValues values, _, _) -> values |> List.exists (check inClosure)
            | StringTemplate(_, _, values) -> values |> List.exists (check inClosure)
            | _ -> false
        | TypeCast(e, _) -> check inClosure e
        | Operation(kind, _, _, _) ->
            match kind with
            | Binary(_, l, r) -> check inClosure l || check inClosure r
            | Unary(_, e) -> check inClosure e
            | Logical(_, l, r) -> check inClosure l || check inClosure r
        | DecisionTree(e, targets) -> check inClosure e || targets |> List.exists (fun (_, t) -> check inClosure t)
        | DecisionTreeSuccess(_, values, _) -> values |> List.exists (check inClosure)
        | Test(e, _, _) -> check inClosure e
        | Get(e, _, _, _) -> check inClosure e
        | Set(e, _, _, v, _) -> check inClosure e || check inClosure v
        | WhileLoop(guard, body, _) -> check inClosure guard || check inClosure body
        | ForLoop(_, start, limit, body, _, _) -> check inClosure start || check inClosure limit || check inClosure body
        | TryCatch(body, catch, finalizer, _) ->
            check inClosure body
            || (catch
                |> Option.map (fun (_, e) -> check inClosure e)
                |> Option.defaultValue false)
            || (finalizer |> Option.map (check inClosure) |> Option.defaultValue false)
        | Emit(emitInfo, _, _) -> emitInfo.CallInfo.Args |> List.exists (check inClosure)
        | ObjectExpr(members, _, baseCall) ->
            members |> List.exists (fun m -> check true m.Body)
            || match baseCall with
               | Some e -> check inClosure e
               | None -> false
        | _ -> false

    check false expr

/// Check if a Fable type is a non-byte array (i.e., needs ref-wrapping)
let isRefArray (typ: Fable.AST.Fable.Type) =
    match typ with
    | Fable.AST.Fable.Type.Array(Fable.AST.Fable.Type.Number(UInt8, _), _) -> false
    | Fable.AST.Fable.Type.Array _ -> true
    | _ -> false

/// Flatten nested Block expressions into a flat list
let rec flattenBlock (expr: Beam.ErlExpr) : Beam.ErlExpr list =
    match expr with
    | Beam.ErlExpr.Block exprs -> exprs |> List.collect flattenBlock
    | _ -> [ expr ]

/// Extract a Block into (hoisted_statements, final_expression).
/// Used to ensure multi-expression Blocks don't appear in argument positions.
let extractBlock (expr: Beam.ErlExpr) : Beam.ErlExpr list * Beam.ErlExpr =
    let flat = flattenBlock expr

    match flat with
    | [] -> ([], Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")))
    | [ single ] -> ([], single)
    | many ->
        let stmts = many.[.. many.Length - 2]
        let final = many.[many.Length - 1]
        (stmts, final)

/// Hoist Block expressions from a list of transformed arguments
let hoistBlocksFromArgs (args: Beam.ErlExpr list) : Beam.ErlExpr list * Beam.ErlExpr list =
    let results = args |> List.map extractBlock
    (results |> List.collect fst, results |> List.map snd)

/// Wrap an expression with hoisted statements, if any
let wrapWithHoisted (hoisted: Beam.ErlExpr list) (expr: Beam.ErlExpr) : Beam.ErlExpr =
    if List.isEmpty hoisted then
        expr
    else
        Beam.ErlExpr.Block(hoisted @ [ expr ])

let atomLit name =
    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom name))
