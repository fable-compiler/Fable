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
    | Emit(emitInfo, _, _) -> emitInfo.CallInfo.Args |> List.exists (containsIdentRef name)
    | ObjectExpr(members, _, baseCall) ->
        members |> List.exists (fun m -> containsIdentRef name m.Body)
        || (
            match baseCall with
            | Some e -> containsIdentRef name e
            | None -> false
        )
    | _ -> false

/// Check if an expression contains any array mutation (indexed set or value set) on a given identifier.
/// Used to detect non-mutable array bindings that need ref-wrapping for mutation.
let rec containsArrayMutation (name: string) (expr: Expr) : bool =
    match expr with
    | Set(IdentExpr ident, _, _, _, _) when ident.Name = name -> true
    | Set(e, _, _, v, _) -> containsArrayMutation name e || containsArrayMutation name v
    | IdentExpr _ -> false
    | Call(callee, info, _, _) ->
        containsArrayMutation name callee
        || info.Args |> List.exists (containsArrayMutation name)
    | CurriedApply(applied, args, _, _) ->
        containsArrayMutation name applied
        || args |> List.exists (containsArrayMutation name)
    | Let(_, value, body) -> containsArrayMutation name value || containsArrayMutation name body
    | LetRec(bindings, body) ->
        bindings |> List.exists (fun (_, v) -> containsArrayMutation name v)
        || containsArrayMutation name body
    | Lambda(_, body, _) -> containsArrayMutation name body
    | Delegate(_, body, _, _) -> containsArrayMutation name body
    | IfThenElse(g, t, e, _) ->
        containsArrayMutation name g
        || containsArrayMutation name t
        || containsArrayMutation name e
    | Sequential exprs -> exprs |> List.exists (containsArrayMutation name)
    | Value(value, _) ->
        match value with
        | NewAnonymousRecord(values, _, _, _)
        | NewRecord(values, _, _)
        | NewUnion(values, _, _, _)
        | NewTuple(values, _) -> values |> List.exists (containsArrayMutation name)
        | NewList(Some(h, t), _) -> containsArrayMutation name h || containsArrayMutation name t
        | NewOption(Some e, _, _) -> containsArrayMutation name e
        | NewArray(NewArrayKind.ArrayValues values, _, _) -> values |> List.exists (containsArrayMutation name)
        | StringTemplate(_, _, values) -> values |> List.exists (containsArrayMutation name)
        | _ -> false
    | TypeCast(e, _) -> containsArrayMutation name e
    | Operation(kind, _, _, _) ->
        match kind with
        | Binary(_, l, r) -> containsArrayMutation name l || containsArrayMutation name r
        | Unary(_, e) -> containsArrayMutation name e
        | Logical(_, l, r) -> containsArrayMutation name l || containsArrayMutation name r
    | DecisionTree(e, targets) ->
        containsArrayMutation name e
        || targets |> List.exists (fun (_, t) -> containsArrayMutation name t)
    | DecisionTreeSuccess(_, values, _) -> values |> List.exists (containsArrayMutation name)
    | Test(e, _, _) -> containsArrayMutation name e
    | Get(e, _, _, _) -> containsArrayMutation name e
    | Emit(emitInfo, _, _) -> emitInfo.CallInfo.Args |> List.exists (containsArrayMutation name)
    | ObjectExpr(members, _, baseCall) ->
        members |> List.exists (fun m -> containsArrayMutation name m.Body)
        || (
            match baseCall with
            | Some e -> containsArrayMutation name e
            | None -> false
        )
    | _ -> false

/// Detect function parameters that get array-mutated in the body,
/// and return an updated context (with those params in MutableVars)
/// plus ref-init expressions to prepend to the body.
let wrapMutatedParams (args: Ident list) (body: Expr) (mutableVars: Map<string, string>) =
    let mutatedArgs =
        args
        |> List.filter (fun arg ->
            arg.Name <> "_"
            && not (arg.Name.StartsWith("_"))
            && containsArrayMutation arg.Name body
        )

    let mutableVars' =
        mutatedArgs
        |> List.fold
            (fun m arg ->
                let refVarName =
                    (Naming.capitalizeFirst arg.Name |> Naming.sanitizeErlangVar) + "_ref"

                m |> Map.add arg.Name refVarName
            )
            mutableVars

    let refInits =
        mutatedArgs
        |> List.map (fun arg ->
            let refVarName =
                (Naming.capitalizeFirst arg.Name |> Naming.sanitizeErlangVar) + "_ref"

            let erlVarName = Naming.capitalizeFirst arg.Name |> Naming.sanitizeErlangVar

            Beam.ErlExpr.Match(
                Beam.PVar(refVarName),
                Beam.ErlExpr.Call(Some "fable_utils", "new_ref", [ Beam.ErlExpr.Variable(erlVarName) ])
            )
        )

    mutableVars', refInits

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
