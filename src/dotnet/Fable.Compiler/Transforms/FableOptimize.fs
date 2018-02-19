module Fable.Transforms.FableOptimize

open Fable
open Fable.AST.Fable
open Fable.AST.Fable.Util
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: Use trampoline here?
let rec private visit f e =
    match e with
    | IdentExpr _ | Import _ | Debugger _ -> e
    | Value kind ->
        match kind with
        | This _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ | Enum _ | UnionCaseTag _ -> e
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
    | ObjectExpr(values, t) ->
        let values = values |> List.map (fun (k,v) -> k, visit f v)
        ObjectExpr(values, t)
    | Operation(kind, t, r) ->
        match kind with
        | Call(callee, memb, args, info) ->
            Operation(Call(visit f callee, memb, List.map (visit f) args, info), t, r)
        | CurriedApply(callee, args) ->
            Operation(CurriedApply(visit f callee, List.map (visit f) args), t, r)
        | Emit(macro, info) ->
            let info = info |> Option.map (fun (args, info) -> List.map (visit f) args, info)
            Operation(Emit(macro, info), t, r)
        | UnresolvedCall(callee, args, info, info2) ->
            Operation(UnresolvedCall(Option.map (visit f) callee, List.map (visit f) args, info, info2), t, r)
        | UnaryOperation(operator, operand) ->
            Operation(UnaryOperation(operator, visit f operand), t, r)
        | BinaryOperation(op, left, right) ->
            Operation(BinaryOperation(op, visit f left, visit f right), t, r)
        | LogicalOperation(op, left, right) ->
            Operation(LogicalOperation(op, visit f left, visit f right), t, r)
    | Get(e, kind, t, r) ->
        match kind with
        | FieldGet _ | IndexGet _ | ListHead | ListTail
        | OptionValue | TupleGet _ | UnionTag _
        | UnionField _ | RecordGet _ -> Get(visit f e, kind, t, r)
        | DynamicGet e2 -> Get(visit f e, DynamicGet (visit f e2), t, r)
    | Throw(e, typ, r) -> Throw(visit f e, typ, r)
    | Sequential exprs -> Sequential(List.map (visit f) exprs)
    | Let(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, visit f e)
        Let(bs, visit f body)
    | IfThenElse(cond, thenExpr, elseExpr) ->
        IfThenElse(visit f cond, visit f thenExpr, visit f elseExpr)
    | Set(e, kind, v, r) ->
        match kind with
        | VarSet | FieldSet _ | IndexSet _ | RecordSet _ ->
            Set(visit f e, kind, visit f v, r)
        | DynamicSet e2 -> Set(visit f e, DynamicSet (visit f e2), visit f v, r)
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

module private Transforms =
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

    let (|LambdaOrDelegate|_|) = function
        | Function(Lambda arg, body) -> Some([arg], body)
        | Function(Delegate args, body) -> Some(args, body)
        | _ -> None

    let (|Apply|_|) = function
        | Operation(Call(expr, None, args, _), t, r)
        | Operation(CurriedApply(expr, args), t, r) ->
            Some(expr, args, t, r)
        | _ -> None

    // TODO: Some cases of coertion shouldn't be erased
    // string :> seq #1279
    // list (and others) :> seq in Fable 2.0
    // concrete type :> interface in Fable 2.0
    let cast (_: ICompiler) = function
        | Cast(e, t) ->
            match t with
            | DeclaredType(EntFullName Types.enumerable, _) ->
                match e with
                | ListLiteral(exprs, t) -> NewArray(ArrayValues exprs, t) |> Value
                | _ -> e
            | _ -> e
        | e -> e

    let replaceValues replacements expr =
        if Map.isEmpty replacements
        then expr
        else expr |> visit (function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some e -> e
                | None -> e
            | e -> e)

    let lambdaBetaReduction (_: ICompiler) = function
        // TODO: Optimize also binary operations with numerical or string literals
        // TODO: Don't inline if one of the arguments is `this`?
        | Apply(LambdaOrDelegate(args, body), argExprs, _, _)
                            when List.sameLength args argExprs ->
            let bindings, replacements =
                (([], Map.empty), args, argExprs)
                |||> List.fold2 (fun (bindings, replacements) ident expr ->
                    if hasDoubleEvalRisk expr
                    then (ident, expr)::bindings, replacements
                    else bindings, Map.add ident.Name expr replacements
                )
            match bindings with
            | [] -> replaceValues replacements body
            | bindings -> Let(List.rev bindings, replaceValues replacements body)
        | e -> e

    let bindingBetaReduction (_: ICompiler) e =
        let isReferencedMoreThan limit identName e =
            match e with
            // Don't try to erase bindings to these expressions
            | Import _ | Throw _ | Sequential _ | IfThenElse _
            | Switch _ | TryCatch _ -> true
            | _ ->
                let mutable count = 0
                // TODO: Can we optimize this to shortcircuit when count > 1?
                e |> visit (function
                    | _ when count > limit -> e
                    | IdentExpr id2 as e when id2.Name = identName ->
                        count <- count + 1; e
                    | e -> e) |> ignore
                count > limit
        match e with
        | Let(bindings, body) ->
            let values = bindings |> List.map snd
            let remaining, replacements =
                (([], Map.empty), bindings)
                ||> List.fold (fun (remaining, replacements) (ident, value) ->
                    let identName = ident.Name
                    if hasDoubleEvalRisk value |> not then
                        remaining, Map.add identName value replacements
                    else
                        let isReferencedMoreThanOnce =
                            (false, values) ||> List.fold (fun positive value ->
                                positive || isReferencedMoreThan 0 identName value)
                            |> function true -> true
                                      | false -> isReferencedMoreThan 1 identName body
                        if isReferencedMoreThanOnce
                        then (ident, value)::remaining, replacements
                        else remaining, Map.add ident.Name value replacements)
            match remaining with
            | [] -> replaceValues replacements body
            | bindings -> Let(List.rev bindings, replaceValues replacements body)
        | e -> e

open Transforms

let rec optimizeExpr (com: ICompiler) e =
    // ATTENTION: Order of transforms matters for optimizations
    (e, [lambdaBetaReduction; bindingBetaReduction; cast])
    ||> List.fold (fun e f -> visit (f com) e)

let rec optimizeDeclaration (com: ICompiler) = function
    | ActionDeclaration expr ->
        ActionDeclaration(optimizeExpr com expr)
    | ValueDeclaration(publicName, privName, value, isMutable) ->
        ValueDeclaration(publicName, privName, optimizeExpr com value, isMutable)

let optimizeFile (com: ICompiler) (file: File) =
    let newDecls = List.map (optimizeDeclaration com) file.Declarations
    File(file.SourcePath, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
