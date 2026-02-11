module Fable.Transforms.Beam.Compiler

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Fable.Beam.Naming
open Fable.Transforms.Beam.Util

// Use fully qualified names for Beam AST types to avoid conflicts with Fable AST
// e.g., Beam.ErlExpr, Beam.ErlLiteral, Beam.Atom, etc.

/// Beam-specific version of mustWrapOption. Unlike JS/Python where unit is null/undefined,
/// Erlang's unit (ok atom) is distinct from None (undefined atom), so Unit doesn't need wrapping.
let rec mustWrapOption =
    function
    | Any
    | GenericParam _
    | Option _ -> true
    | _ -> false

type IBeamCompiler =
    inherit Fable.Compiler
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

type Context =
    {
        File: string
        UsedNames: Set<string>
        DecisionTargets: (Ident list * Expr) list
        RecursiveBindings: Set<string>
        MutualRecBindings: Map<string, string * string> // name -> (bundleVarName, atomTag)
        MutableVars: Set<string> // names of mutable variables (use process dict put/get)
        LocalVars: Set<string> // names of locally-bound variables (params, let bindings, lambda args)
        ThisArgVar: string option // Erlang variable name for `this` in class constructors/methods
        ThisIdentNames: Set<string> // original Fable ident names that refer to `this` (e.g., "_", "__")
        CtorFieldExprs: Map<string, Beam.ErlExpr> // field name -> Erlang expr during constructor
    }

/// Check if an entity ref refers to an interface type
let private isInterfaceType (com: IBeamCompiler) (entityRef: EntityRef) =
    match com.TryGetEntity(entityRef) with
    | Some entity -> entity.IsInterface
    | None -> false

/// Check if an entity ref refers to a user-defined class (not record, union, module, interface, or BCL type)
let private isClassType (com: IBeamCompiler) (entityRef: EntityRef) =
    // Only consider user-defined types (SourcePath), not BCL/core types
    match entityRef.Path with
    | SourcePath _
    | PrecompiledLib _ ->
        match com.TryGetEntity(entityRef) with
        | Some entity ->
            not entity.IsFSharpRecord
            && not entity.IsFSharpUnion
            && not entity.IsFSharpModule
            && not entity.IsInterface
            && not entity.IsFSharpExceptionDeclaration
        | None -> false
    | _ -> false


let private atomLit name =
    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom name))

let private getDecisionTarget (ctx: Context) targetIndex =
    match List.tryItem targetIndex ctx.DecisionTargets with
    | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
    | Some(idents, target) -> idents, target

let private matchTargetIdentAndValues idents values =
    if List.isEmpty idents then
        []
    elif List.length idents = List.length values then
        List.zip idents values
    else
        failwith "Target idents/values lengths differ"

let rec transformExpr (com: IBeamCompiler) (ctx: Context) (expr: Expr) : Beam.ErlExpr =
    match expr with
    | Value(kind, _range) -> transformValue com ctx kind

    | Call(callee, info, _typ, _range) -> transformCall com ctx callee info

    | IdentExpr ident ->
        if ctx.ThisIdentNames.Contains(ident.Name) then
            // This is a reference to the `this` parameter — use the renamed variable
            match ctx.ThisArgVar with
            | Some varName -> Beam.ErlExpr.Variable varName
            | None -> Beam.ErlExpr.Variable "This"
        elif ctx.MutableVars.Contains(ident.Name) then
            // Mutable variable: read from process dictionary
            Beam.ErlExpr.Call(
                None,
                "get",
                [
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName ident.Name)))
                ]
            )
        else
            match ctx.MutualRecBindings.TryFind(ident.Name) with
            | Some(bundleName, atomTag) ->
                // Return the dispatched closure: Mk_rec_N(atom_tag)
                Beam.ErlExpr.Apply(
                    Beam.ErlExpr.Variable(bundleName),
                    [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
                )
            | None ->
                if ctx.LocalVars.Contains(ident.Name) || ctx.RecursiveBindings.Contains(ident.Name) then
                    Beam.ErlExpr.Variable(capitalizeFirst ident.Name |> sanitizeErlangVar)
                else
                    // Module-level function reference: call as 0-arity function
                    Beam.ErlExpr.Call(None, sanitizeErlangName ident.Name, [])

    | Sequential exprs ->
        let erlExprs = exprs |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Block erlExprs

    | Let(ident, value, body) when
        ident.IsMutable
        && not (ident.Name.StartsWith("copyOfStruct", System.StringComparison.Ordinal))
        ->
        // Mutable variable: use process dictionary put/get
        // (but not for compiler-generated struct copies which aren't truly mutated)
        let atomKey =
            Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName ident.Name)))

        let erlValue = transformExpr com ctx value
        let ctx' = { ctx with MutableVars = ctx.MutableVars.Add(ident.Name) }
        let erlBody = transformExpr com ctx' body
        Beam.ErlExpr.Block [ Beam.ErlExpr.Call(None, "put", [ atomKey; erlValue ]); erlBody ]

    | Let(ident, value, body) ->
        let varName = capitalizeFirst ident.Name |> sanitizeErlangVar

        match value with
        | Lambda(arg, lambdaBody, _) when containsIdentRef ident.Name lambdaBody ->
            // Self-recursive lambda: use Erlang named fun
            let ctx' =
                { ctx with
                    RecursiveBindings = ctx.RecursiveBindings.Add(ident.Name)
                    LocalVars = ctx.LocalVars.Add(arg.Name)
                }

            let erlBody = transformExpr com ctx' lambdaBody

            let bodyExprs =
                match erlBody with
                | Beam.ErlExpr.Block es -> es
                | e -> [ e ]

            let namedFun =
                Beam.ErlExpr.NamedFun(
                    varName,
                    [
                        {
                            Patterns = [ Beam.PVar(capitalizeFirst arg.Name |> sanitizeErlangVar) ]
                            Guard = []
                            Body = bodyExprs
                        }
                    ]
                )

            let erlOuterBody = transformExpr com ctx' body
            Beam.ErlExpr.Block [ Beam.ErlExpr.Match(Beam.PVar varName, namedFun); erlOuterBody ]
        | Delegate(args, lambdaBody, _, _) when containsIdentRef ident.Name lambdaBody ->
            // Self-recursive delegate: use Erlang named fun
            let ctx' =
                { ctx with
                    RecursiveBindings = ctx.RecursiveBindings.Add(ident.Name)
                    LocalVars = args |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars
                }

            let argPats =
                args
                |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar))

            let erlBody = transformExpr com ctx' lambdaBody

            let bodyExprs =
                match erlBody with
                | Beam.ErlExpr.Block es -> es
                | e -> [ e ]

            let namedFun =
                Beam.ErlExpr.NamedFun(
                    varName,
                    [
                        {
                            Patterns = argPats
                            Guard = []
                            Body = bodyExprs
                        }
                    ]
                )

            let erlOuterBody = transformExpr com ctx' body
            Beam.ErlExpr.Block [ Beam.ErlExpr.Match(Beam.PVar varName, namedFun); erlOuterBody ]
        | _ ->
            let erlValue = transformExpr com ctx value
            let hoisted, cleanValue = extractBlock erlValue
            let ctx' = { ctx with LocalVars = ctx.LocalVars.Add(ident.Name) }
            let erlBody = transformExpr com ctx' body
            Beam.ErlExpr.Block(hoisted @ [ Beam.ErlExpr.Match(Beam.PVar varName, cleanValue); erlBody ])

    | TypeCast(expr, _typ) -> transformExpr com ctx expr

    | CurriedApply(applied, args, _typ, _range) ->
        let appliedExpr = transformExpr com ctx applied
        let argExprs = args |> List.map (transformExpr com ctx)
        let appliedHoisted, cleanApplied = extractBlock appliedExpr
        let argsHoisted, cleanArgs = hoistBlocksFromArgs argExprs
        let allHoisted = appliedHoisted @ argsHoisted

        let result =
            match cleanApplied with
            | Beam.ErlExpr.Call(None, _f, []) when cleanArgs.Length = 1 ->
                // 0-arity module value retrieval + single arg: apply to result
                // e.g., webApp(arg) where webApp/0 returns a function
                Beam.ErlExpr.Apply(cleanApplied, cleanArgs)
            | Beam.ErlExpr.Call(None, _f, []) ->
                // 0-arity module value retrieval + multiple args: apply curried
                // e.g., webApp(B0, B1) where webApp/0 returns a curried function
                Beam.ErlExpr.Call(Some "fable_utils", "apply_curried", [ cleanApplied; Beam.ErlExpr.List cleanArgs ])
            | Beam.ErlExpr.Call(None, f, existingArgs) ->
                // Unqualified (same-module) call with args: safe to merge
                Beam.ErlExpr.Call(None, f, existingArgs @ cleanArgs)
            | Beam.ErlExpr.Call(Some m, f, existingArgs) when m.StartsWith("fable_") ->
                // Fable library call: designed to handle merged args via multi-arity overloads
                Beam.ErlExpr.Call(Some m, f, existingArgs @ cleanArgs)
            | Beam.ErlExpr.Call(Some _, _, _) when cleanArgs.Length = 1 ->
                // Qualified call to non-fable module (e.g., maps:get): returns a value
                // If it's a function, apply the single arg to it
                Beam.ErlExpr.Apply(cleanApplied, cleanArgs)
            | Beam.ErlExpr.Call(Some _, _, _) ->
                // Qualified call with multiple curried args: apply one at a time
                Beam.ErlExpr.Call(Some "fable_utils", "apply_curried", [ cleanApplied; Beam.ErlExpr.List cleanArgs ])
            | _ -> Beam.ErlExpr.Apply(cleanApplied, cleanArgs)

        result |> wrapWithHoisted allHoisted

    | Operation(kind, _tags, typ, _range) -> transformOperation com ctx kind typ

    | IfThenElse(guardExpr, thenExpr, elseExpr, _range) ->
        let erlGuard = transformExpr com ctx guardExpr
        let erlThen = transformExpr com ctx thenExpr
        let erlElse = transformExpr com ctx elseExpr

        Beam.ErlExpr.Case(
            erlGuard,
            [
                {
                    Pattern = Beam.PLiteral(Beam.BoolLit true)
                    Guard = []
                    Body = [ erlThen ]
                }
                {
                    Pattern = Beam.PLiteral(Beam.BoolLit false)
                    Guard = []
                    Body = [ erlElse ]
                }
            ]
        )

    | Lambda(arg, body, _name) ->
        let ctx' = { ctx with LocalVars = ctx.LocalVars.Add(arg.Name) }
        let erlBody = transformExpr com ctx' body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        Beam.ErlExpr.Fun
            [
                {
                    Patterns = [ Beam.PVar(capitalizeFirst arg.Name |> sanitizeErlangVar) ]
                    Guard = []
                    Body = bodyExprs
                }
            ]

    | Delegate(args, body, _name, _tags) ->
        let argPats =
            args
            |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar))

        let ctx' =
            { ctx with LocalVars = args |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars }

        let erlBody = transformExpr com ctx' body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        Beam.ErlExpr.Fun
            [
                {
                    Patterns = argPats
                    Guard = []
                    Body = bodyExprs
                }
            ]

    | Test(expr, kind, _range) -> transformTest com ctx kind expr

    | Get(expr, kind, typ, _range) -> transformGet com ctx kind typ expr

    | DecisionTree(expr, targets) ->
        let ctx = { ctx with DecisionTargets = targets }
        transformExpr com ctx expr

    | DecisionTreeSuccess(targetIndex, boundValues, _typ) ->
        let idents, target = getDecisionTarget ctx targetIndex
        let bindings = matchTargetIdentAndValues idents boundValues

        match bindings with
        | [] -> transformExpr com ctx target
        | bindings ->
            let target =
                List.rev bindings
                |> List.fold (fun e (i, v) -> Fable.AST.Fable.Expr.Let(i, v, e)) target

            transformExpr com ctx target

    | Set(expr, ValueSet, _typ, value, _range) ->
        match expr with
        | IdentExpr ident when ctx.MutableVars.Contains(ident.Name) ->
            // Mutable variable: update via process dictionary
            let atomKey =
                Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName ident.Name)))

            Beam.ErlExpr.Call(None, "put", [ atomKey; transformExpr com ctx value ])
        | IdentExpr ident -> Beam.ErlExpr.Match(Beam.PVar(capitalizeFirst ident.Name), transformExpr com ctx value)
        | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_set"))

    | Set(expr, FieldSet fieldName, _, value, _) ->
        let erlExpr = transformExpr com ctx expr
        let erlValue = transformExpr com ctx value

        let atomField =
            Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName fieldName)))

        match expr.Type with
        | Fable.AST.Fable.Type.DeclaredType(entityRef, _) when isClassType com entityRef ->
            // Class instance: update state in process dict
            // put(Ref, maps:put(field, Value, get(Ref)))
            Beam.ErlExpr.Call(
                None,
                "put",
                [
                    erlExpr
                    Beam.ErlExpr.Call(
                        Some "maps",
                        "put",
                        [ atomField; erlValue; Beam.ErlExpr.Call(None, "get", [ erlExpr ]) ]
                    )
                ]
            )
        | _ ->
            // Record: existing behavior (maps:put returning new map)
            Beam.ErlExpr.Call(Some "maps", "put", [ atomField; erlValue; erlExpr ])

    | Set _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_set"))

    | LetRec(bindings, body) ->
        let allAreLambdas =
            bindings
            |> List.forall (fun (_, value) ->
                match value with
                | Lambda _
                | Delegate _ -> true
                | _ -> false
            )

        if allAreLambdas && bindings.Length > 1 then
            // Mutual recursion: bundle all functions into a single named fun
            // that dispatches on an atom tag
            let bundleName = $"Mk_rec_%d{com.IncrementCounter()}"

            // Build atom tags and the MutualRecBindings map
            let bindingInfos =
                bindings
                |> List.map (fun (ident, value) ->
                    let atomTag = sanitizeErlangName ident.Name
                    (ident, value, atomTag)
                )

            let mutualRecMap =
                bindingInfos
                |> List.fold (fun acc (ident, _, atomTag) -> Map.add ident.Name (bundleName, atomTag) acc) Map.empty

            let allNames = bindings |> List.map (fun (ident, _) -> ident.Name) |> Set.ofList

            let ctx' =
                { ctx with
                    RecursiveBindings = ctx.RecursiveBindings |> Set.union allNames
                    MutualRecBindings = mutualRecMap
                }

            // Build clauses for the named fun: each clause matches an atom tag
            // and returns a closure for that function
            let clauses =
                bindingInfos
                |> List.map (fun (_ident, value, atomTag) ->
                    let argPats, lambdaBody, lambdaCtx =
                        match value with
                        | Lambda(arg, lambdaBody, _) ->
                            [ Beam.PVar(capitalizeFirst arg.Name |> sanitizeErlangVar) ],
                            lambdaBody,
                            { ctx' with LocalVars = ctx'.LocalVars.Add(arg.Name) }
                        | Delegate(args, lambdaBody, _, _) ->
                            args
                            |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar)),
                            lambdaBody,
                            { ctx' with LocalVars = args |> List.fold (fun s a -> s.Add(a.Name)) ctx'.LocalVars }
                        | _ -> failwith "unreachable: already checked allAreLambdas"

                    let erlBody = transformExpr com lambdaCtx lambdaBody

                    let bodyExprs =
                        match erlBody with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    // The clause pattern is the atom tag; the body returns a fun
                    let innerFun =
                        Beam.ErlExpr.Fun
                            [
                                {
                                    Beam.ErlFunClause.Patterns = argPats
                                    Guard = []
                                    Body = bodyExprs
                                }
                            ]

                    ({
                        Patterns = [ Beam.PLiteral(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
                        Guard = []
                        Body = [ innerFun ]
                    }
                    : Beam.ErlFunClause)
                )

            let namedFun = Beam.ErlExpr.NamedFun(bundleName, clauses)

            // Bind the bundle: Mk_rec_N = fun Mk_rec_N(...) -> ... end
            let bundleMatch = Beam.ErlExpr.Match(Beam.PVar bundleName, namedFun)

            // Bind each individual name: IsEven = Mk_rec_N(is_even)
            let individualBindings =
                bindingInfos
                |> List.map (fun (ident, _, atomTag) ->
                    Beam.ErlExpr.Match(
                        Beam.PVar(capitalizeFirst ident.Name),
                        Beam.ErlExpr.Apply(
                            Beam.ErlExpr.Variable bundleName,
                            [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
                        )
                    )
                )

            let erlBody = transformExpr com ctx' body
            Beam.ErlExpr.Block([ bundleMatch ] @ individualBindings @ [ erlBody ])
        elif allAreLambdas && bindings.Length = 1 then
            // Single self-recursive function: use named fun
            let (ident, value) = bindings.Head
            let varName = capitalizeFirst ident.Name |> sanitizeErlangVar
            let ctx' = { ctx with RecursiveBindings = ctx.RecursiveBindings.Add(ident.Name) }

            let argPats, lambdaBody, lambdaCtx =
                match value with
                | Lambda(arg, lambdaBody, _) ->
                    [ Beam.PVar(capitalizeFirst arg.Name |> sanitizeErlangVar) ],
                    lambdaBody,
                    { ctx' with LocalVars = ctx'.LocalVars.Add(arg.Name) }
                | Delegate(args, lambdaBody, _, _) ->
                    args
                    |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar)),
                    lambdaBody,
                    { ctx' with LocalVars = args |> List.fold (fun s a -> s.Add(a.Name)) ctx'.LocalVars }
                | _ -> failwith "unreachable: already checked allAreLambdas"

            let erlLambdaBody = transformExpr com lambdaCtx lambdaBody

            let bodyExprs =
                match erlLambdaBody with
                | Beam.ErlExpr.Block es -> es
                | e -> [ e ]

            let namedFun =
                Beam.ErlExpr.NamedFun(
                    varName,
                    [
                        {
                            Patterns = argPats
                            Guard = []
                            Body = bodyExprs
                        }
                    ]
                )

            let erlOuterBody = transformExpr com ctx' body
            Beam.ErlExpr.Block [ Beam.ErlExpr.Match(Beam.PVar varName, namedFun); erlOuterBody ]
        else
            // Simple case: non-lambda bindings
            let assignments =
                bindings
                |> List.map (fun (ident, value) ->
                    Beam.ErlExpr.Match(Beam.PVar(capitalizeFirst ident.Name), transformExpr com ctx value)
                )

            let erlBody = transformExpr com ctx body
            Beam.ErlExpr.Block(assignments @ [ erlBody ])

    | Emit(emitInfo, _typ, _range) ->
        let args = emitInfo.CallInfo.Args |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Emit(emitInfo.Macro, args)

    | TryCatch(body, catch_, _finalizer, _range) ->
        let erlBody = transformExpr com ctx body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        match catch_ with
        | Some(ident, catchExpr) ->
            // Create a temp var for the raw reason, then bind the catch ident.
            // Custom exceptions (maps with __type) are preserved as-is.
            // Plain errors (binaries from failwith, atoms, etc.) are wrapped in #{message => ...}.
            let ctr = com.IncrementCounter()
            let reasonVar = $"Exn_reason_%d{ctr}"
            let identVar = capitalizeFirst ident.Name

            let ctx' = { ctx with LocalVars = ctx.LocalVars.Add(ident.Name) }
            let erlCatchBody = transformExpr com ctx' catchExpr

            let catchBodyExprs =
                match erlCatchBody with
                | Beam.ErlExpr.Block es -> es
                | e -> [ e ]

            let reasonRef = Beam.ErlExpr.Variable reasonVar

            let formatExpr =
                Beam.ErlExpr.Call(
                    None,
                    "iolist_to_binary",
                    [
                        Beam.ErlExpr.Call(
                            Some "io_lib",
                            "format",
                            [
                                Beam.ErlExpr.Call(
                                    None,
                                    "binary_to_list",
                                    [ Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit "~p") ]
                                )
                                Beam.ErlExpr.List [ reasonRef ]
                            ]
                        )
                    ]
                )

            let messageExpr =
                Beam.ErlExpr.Case(
                    reasonRef,
                    [
                        {
                            Pattern = Beam.PWildcard
                            Guard = [ Beam.ErlExpr.Call(None, "is_binary", [ reasonRef ]) ]
                            Body = [ reasonRef ]
                        }
                        {
                            Pattern = Beam.PWildcard
                            Guard = []
                            Body = [ formatExpr ]
                        }
                    ]
                )

            // If reason is already a map (custom exception with __type), use it directly.
            // Otherwise wrap in #{message => ...} for .Message access.
            let bindIdent =
                Beam.ErlExpr.Match(
                    Beam.PVar identVar,
                    Beam.ErlExpr.Case(
                        reasonRef,
                        [
                            {
                                Pattern = Beam.PWildcard
                                Guard = [ Beam.ErlExpr.Call(None, "is_map", [ reasonRef ]) ]
                                Body = [ reasonRef ]
                            }
                            {
                                Pattern = Beam.PWildcard
                                Guard = []
                                Body =
                                    [
                                        Beam.ErlExpr.Map
                                            [
                                                Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "message")),
                                                messageExpr
                                            ]
                                    ]
                            }
                        ]
                    )
                )

            Beam.ErlExpr.TryCatch(bodyExprs, reasonVar, [ bindIdent ] @ catchBodyExprs)
        | None ->
            // No catch handler, just execute the body (finalizer not yet supported)
            erlBody

    | WhileLoop(guard, body, _range) ->
        // Transform while loop to a tail-recursive named fun:
        // WhileLoop_ = fun WhileLoop_() ->
        //     case Guard of
        //         true -> Body, WhileLoop_();
        //         false -> ok
        //     end
        // end,
        // WhileLoop_()
        let ctr = com.IncrementCounter()
        let loopName = $"While_loop_%d{ctr}"
        let erlGuard = transformExpr com ctx guard
        let erlBody = transformExpr com ctx body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        let loopFun =
            Beam.ErlExpr.NamedFun(
                loopName,
                [
                    {
                        Patterns = []
                        Guard = []
                        Body =
                            [
                                Beam.ErlExpr.Case(
                                    erlGuard,
                                    [
                                        {
                                            Pattern = Beam.PLiteral(Beam.BoolLit true)
                                            Guard = []
                                            Body =
                                                bodyExprs @ [ Beam.ErlExpr.Apply(Beam.ErlExpr.Variable loopName, []) ]
                                        }
                                        {
                                            Pattern = Beam.PLiteral(Beam.BoolLit false)
                                            Guard = []
                                            Body = [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")) ]
                                        }
                                    ]
                                )
                            ]
                    }
                ]
            )

        Beam.ErlExpr.Block
            [
                Beam.ErlExpr.Match(Beam.PVar loopName, loopFun)
                Beam.ErlExpr.Apply(Beam.ErlExpr.Variable loopName, [])
            ]

    | ForLoop(ident, start, limit, body, isUp, _range) ->
        // Transform for loop to a tail-recursive named fun:
        // For_loop_ = fun For_loop_(I) ->
        //     case (isUp andalso I =< Limit) orelse (not isUp andalso I >= Limit) of
        //         true -> Body, For_loop_(I +/- 1);
        //         false -> ok
        //     end
        // end,
        // For_loop_(Start)
        let ctr = com.IncrementCounter()
        let loopName = $"For_loop_%d{ctr}"
        let varName = capitalizeFirst ident.Name |> sanitizeErlangVar
        let erlStart = transformExpr com ctx start
        let erlLimit = transformExpr com ctx limit
        let loopCtx = { ctx with LocalVars = ctx.LocalVars.Add(ident.Name) }
        let erlBody = transformExpr com loopCtx body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        let compareOp =
            if isUp then
                "=<"
            else
                ">="

        let stepOp =
            if isUp then
                "+"
            else
                "-"

        let guardExpr =
            Beam.ErlExpr.BinOp(compareOp, Beam.ErlExpr.Variable varName, erlLimit)

        let nextStep =
            Beam.ErlExpr.BinOp(stepOp, Beam.ErlExpr.Variable varName, Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L))

        let loopFun =
            Beam.ErlExpr.NamedFun(
                loopName,
                [
                    {
                        Patterns = [ Beam.PVar varName ]
                        Guard = []
                        Body =
                            [
                                Beam.ErlExpr.Case(
                                    guardExpr,
                                    [
                                        {
                                            Pattern = Beam.PLiteral(Beam.BoolLit true)
                                            Guard = []
                                            Body =
                                                bodyExprs
                                                @ [ Beam.ErlExpr.Apply(Beam.ErlExpr.Variable loopName, [ nextStep ]) ]
                                        }
                                        {
                                            Pattern = Beam.PLiteral(Beam.BoolLit false)
                                            Guard = []
                                            Body = [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")) ]
                                        }
                                    ]
                                )
                            ]
                    }
                ]
            )

        Beam.ErlExpr.Block
            [
                Beam.ErlExpr.Match(Beam.PVar loopName, loopFun)
                Beam.ErlExpr.Apply(Beam.ErlExpr.Variable loopName, [ erlStart ])
            ]

    | ObjectExpr(members, _typ, _baseCall) ->
        // Build a map: #{method_name => fun(Args) -> Body end, ...}
        // Getters are stored as evaluated values (since call site uses FieldGet),
        // methods are stored as closures.
        // Members include `this` as first arg (from bindMemberArgs). For simple cases
        // (no self-reference), we skip `this` and create a plain map.
        let mapEntries =
            members
            |> List.map (fun memb ->
                let memberInfo = com.GetMember(memb.MemberRef)
                let methodName = sanitizeErlangName memb.Name
                // Skip the `this` arg (first arg for instance members)
                let nonThisArgs =
                    match memb.Args with
                    | first :: rest when first.IsThisArgument -> rest
                    | args -> args

                let membCtx =
                    { ctx with LocalVars = nonThisArgs |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars }

                let erlBody = transformExpr com membCtx memb.Body

                if memberInfo.IsGetter then
                    // Property getter: store the evaluated value directly
                    // Call site uses Get(obj, FieldGet(name)) → maps:get(name, obj)
                    atomLit methodName, erlBody
                else
                    let argPats = nonThisArgs |> List.map (fun a -> Beam.PVar(toErlangVar a))

                    let bodyExprs =
                        match erlBody with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    let funExpr =
                        Beam.ErlExpr.Fun
                            [
                                {
                                    Patterns = argPats
                                    Guard = []
                                    Body = bodyExprs
                                }
                            ]

                    atomLit methodName, funExpr
            )

        Beam.ErlExpr.Map(mapEntries)

    | Extended(kind, _range) ->
        match kind with
        | Throw(Some exprArg, _typ) ->
            let erlExpr = transformExpr com ctx exprArg
            Beam.ErlExpr.Call(Some "erlang", "error", [ erlExpr ])
        | Throw(None, _typ) ->
            // Re-raise (should not normally happen outside catch context)
            Beam.ErlExpr.Call(
                Some "erlang",
                "error",
                [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "rethrow")) ]
            )
        | Curry(e, arity) -> transformExpr com ctx (Replacements.Api.curryExprAtRuntime com arity e)
        | Debugger -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_debugger"))

    | Import(importInfo, typ, _range) ->
        // Standalone import (function reference from another module, not a direct call).
        // Generate a lambda wrapper: fun(A1, ..., AN) -> module:function(A1, ..., AN) end
        let moduleName =
            let name = moduleNameFromFile importInfo.Path
            let currentModule = moduleNameFromFile com.CurrentFile

            if name = currentModule then
                None
            else
                Some name

        let funcName = sanitizeErlangName importInfo.Selector

        // Count arity from the function type (follows all nested LambdaType levels)
        let rec functionArity (t: Fable.AST.Fable.Type) =
            match t with
            | Fable.AST.Fable.Type.LambdaType(_, returnType) -> 1 + functionArity returnType
            | Fable.AST.Fable.Type.DelegateType(argTypes, _) -> argTypes.Length
            | _ -> 0

        // Determine the actual Erlang arity of the imported function.
        // For MemberImport, use NonCurriedArgTypes which gives us the actual parameter count.
        // This is critical because functionArity(typ) follows the full curried type chain
        // (including return type nesting), which over-counts when the return type is itself
        // a function type (e.g., HttpHandler -> HttpHandler -> HttpHandler where HttpHandler
        // is a function type — functionArity returns 4 but actual Erlang arity is 2).
        let arity =
            match importInfo.Kind with
            | Fable.AST.Fable.MemberImport(Fable.AST.Fable.MemberRef(_, info)) ->
                match info.NonCurriedArgTypes with
                | Some argTypes -> argTypes.Length
                | None -> 0 // Value binding (no explicit params) → 0-arity in Erlang
            | _ -> functionArity typ

        if arity = 0 then
            // Not a function or value binding — call with no args to get the value
            Beam.ErlExpr.Call(moduleName, funcName, [])
        else
            // Generate lambda wrapper for the function reference
            let counter = com.IncrementCounter()

            let argNames = List.init arity (fun i -> $"Import_arg_%d{i}_%d{counter}")
            let argPats = argNames |> List.map Beam.PVar
            let argExprs = argNames |> List.map Beam.ErlExpr.Variable

            Beam.ErlExpr.Fun
                [
                    {
                        Patterns = argPats
                        Guard = []
                        Body = [ Beam.ErlExpr.Call(moduleName, funcName, argExprs) ]
                    }
                ]

    | _ ->
        let exprName = expr.GetType().Name
        Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"todo_%s{exprName.ToLowerInvariant()}"))

and transformValue (com: IBeamCompiler) (ctx: Context) (value: ValueKind) : Beam.ErlExpr =
    match value with
    | StringConstant s -> Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit s)

    | BoolConstant b -> Beam.ErlExpr.Literal(Beam.ErlLiteral.BoolLit b)

    | NumberConstant(NumberValue.Int32 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))

    | NumberConstant(NumberValue.Float64 f, _) ->
        if System.Double.IsPositiveInfinity(f) then
            Beam.ErlExpr.Call(Some "fable_utils", "pos_infinity", [])
        elif System.Double.IsNegativeInfinity(f) then
            Beam.ErlExpr.Call(Some "fable_utils", "neg_infinity", [])
        elif System.Double.IsNaN(f) then
            Beam.ErlExpr.Call(Some "fable_utils", "nan", [])
        else
            Beam.ErlExpr.Literal(Beam.ErlLiteral.Float f)

    | UnitConstant -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok"))

    | Null _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined"))

    | NewTuple(values, _isStruct) ->
        let erlValues = values |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Tuple erlValues

    | NewList(None, _typ) -> Beam.ErlExpr.List []

    | NewList(Some(head, tail), _typ) ->
        let headExpr = transformExpr com ctx head
        let tailExpr = transformExpr com ctx tail
        Beam.ErlExpr.ListCons(headExpr, tailExpr)

    | NewArray(ArrayValues values, _typ, _kind) ->
        let erlValues = values |> List.map (transformExpr com ctx)
        let hoisted, cleanValues = hoistBlocksFromArgs erlValues
        Beam.ErlExpr.List cleanValues |> wrapWithHoisted hoisted

    | NewArray(ArrayFrom expr, _typ, _kind) -> transformExpr com ctx expr

    | NewArray(ArrayAlloc size, _typ, _kind) ->
        // Create an array of N zeros: lists:duplicate(N, 0)
        let erlSize = transformExpr com ctx size
        Beam.ErlExpr.Call(Some "lists", "duplicate", [ erlSize; Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 0L) ])

    | NewUnion(values, tag, _ref, _genArgs) ->
        let erlValues = values |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Tuple(Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 tag)) :: erlValues)

    | NewOption(Some value, typ, _isStruct) ->
        let erlValue = transformExpr com ctx value

        match typ with
        | GenericParam _
        | Any ->
            // Runtime decision: use smart constructor that wraps only ambiguous values
            Beam.ErlExpr.Call(Some "fable_option", "some", [ erlValue ])
        | _ when mustWrapOption typ ->
            // Compile-time decision: we know wrapping is needed (e.g., Option<Option<T>>)
            Beam.ErlExpr.Tuple [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "some")); erlValue ]
        | _ -> erlValue

    | NewOption(None, _typ, _isStruct) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined"))

    | ThisValue _ ->
        match ctx.ThisArgVar with
        | Some varName -> Beam.ErlExpr.Variable varName
        | None -> Beam.ErlExpr.Variable "This" // fallback

    | CharConstant c -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 c))

    | NumberConstant(NumberValue.Int64 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer i)
    | NumberConstant(NumberValue.Int8 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.UInt8 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.Int16 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.UInt16 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.UInt32 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.UInt64 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.Float32 f, _) ->
        let d = float f

        if System.Double.IsPositiveInfinity(d) then
            Beam.ErlExpr.Call(Some "fable_utils", "pos_infinity", [])
        elif System.Double.IsNegativeInfinity(d) then
            Beam.ErlExpr.Call(Some "fable_utils", "neg_infinity", [])
        elif System.Double.IsNaN(d) then
            Beam.ErlExpr.Call(Some "fable_utils", "nan", [])
        else
            Beam.ErlExpr.Literal(Beam.ErlLiteral.Float d)
    | NumberConstant(NumberValue.NativeInt i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.UNativeInt i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))
    | NumberConstant(NumberValue.Decimal d, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Float(float d))

    | NewRecord(values, ref, _genArgs) ->
        match com.TryGetEntity(ref) with
        | Some entity ->
            let fieldEntries =
                List.zip (entity.FSharpFields |> List.map (fun f -> f.Name)) values
                |> List.map (fun (name, value) ->
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(toSnakeCase name))),
                    transformExpr com ctx value
                )

            if entity.IsFSharpExceptionDeclaration then
                // Exception types: add __type tag for type discrimination and message field
                let typeName = sanitizeErlangName entity.DisplayName

                let hasMessageField =
                    entity.FSharpFields |> List.exists (fun f -> toSnakeCase f.Name = "message")

                let messageEntry =
                    if hasMessageField then
                        // A field named "message" already exists, don't add a duplicate
                        []
                    else
                        match values with
                        | [ singleValue ] ->
                            // Single-field exception: message = the field value
                            [ atomLit "message", transformExpr com ctx singleValue ]
                        | _ ->
                            // Multi-field exception: message = type name string
                            [ atomLit "message", Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit typeName) ]

                Beam.ErlExpr.Map([ atomLit "exn_type", atomLit typeName ] @ messageEntry @ fieldEntries)
            else
                Beam.ErlExpr.Map fieldEntries
        | None ->
            let entries =
                values
                |> List.mapi (fun i v ->
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"field_%d{i}")), transformExpr com ctx v
                )

            Beam.ErlExpr.Map entries

    | NewAnonymousRecord(values, fieldNames, _genArgs, _isStruct) ->
        let entries =
            Array.zip fieldNames (values |> List.toArray)
            |> Array.toList
            |> List.map (fun (name, value) ->
                Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(toSnakeCase name))), transformExpr com ctx value
            )

        Beam.ErlExpr.Map entries

    | StringTemplate(_tag, parts, values) ->
        let toStringExpr erlValue (typ: Fable.AST.Fable.Type) =
            match typ with
            | Fable.AST.Fable.Type.String -> erlValue
            | Fable.AST.Fable.Type.Number(kind, _) ->
                match kind with
                | Float16
                | Float32
                | Float64
                | Decimal ->
                    // io_lib:format needs charlist format string, use binary_to_list to convert
                    let fmtStr =
                        Beam.ErlExpr.Call(
                            None,
                            "binary_to_list",
                            [ Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit "~p") ]
                        )

                    Beam.ErlExpr.Call(
                        None,
                        "iolist_to_binary",
                        [
                            Beam.ErlExpr.Call(Some "io_lib", "format", [ fmtStr; Beam.ErlExpr.List [ erlValue ] ])
                        ]
                    )
                | _ -> Beam.ErlExpr.Call(None, "integer_to_binary", [ erlValue ])
            | _ -> Beam.ErlExpr.Call(Some "fable_string", "to_string", [ erlValue ])

        match parts with
        | [] -> Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit "")
        | [ part ] -> Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit part)
        | firstPart :: restParts ->
            let elements =
                ([ Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit firstPart) ], List.zip values restParts)
                ||> List.fold (fun acc (value, part) ->
                    let erlValue = transformExpr com ctx value
                    let stringified = toStringExpr erlValue value.Type
                    acc @ [ stringified; Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit part) ]
                )

            Beam.ErlExpr.Call(None, "iolist_to_binary", [ Beam.ErlExpr.List elements ])

    | TypeInfo(typ, _tags) -> Reflection.transformTypeInfo com None Map.empty typ

    | _ ->
        let kindName = value.GetType().Name
        Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"todo_%s{kindName.ToLowerInvariant()}"))

and transformOperation
    (com: IBeamCompiler)
    (ctx: Context)
    (kind: OperationKind)
    (typ: Fable.AST.Fable.Type)
    : Beam.ErlExpr
    =
    match kind with
    | Binary(op, left, right) ->
        let erlLeft = transformExpr com ctx left
        let erlRight = transformExpr com ctx right
        let leftHoisted, cleanLeft = extractBlock erlLeft
        let rightHoisted, cleanRight = extractBlock erlRight
        let allHoisted = leftHoisted @ rightHoisted

        match op with
        | BinaryPlus when
            left.Type = Fable.AST.Fable.Type.String
            || right.Type = Fable.AST.Fable.Type.String
            ->
            // String concatenation: use iolist_to_binary([Left, Right])
            Beam.ErlExpr.Call(None, "iolist_to_binary", [ Beam.ErlExpr.List [ cleanLeft; cleanRight ] ])
            |> wrapWithHoisted allHoisted
        | BinaryExponent ->
            Beam.ErlExpr.Call(Some "math", "pow", [ cleanLeft; cleanRight ])
            |> wrapWithHoisted allHoisted
        | _ ->
            let erlOp =
                match op with
                | BinaryPlus -> "+"
                | BinaryMinus -> "-"
                | BinaryMultiply -> "*"
                | BinaryDivide ->
                    if isIntegerType typ then
                        "div"
                    else
                        "/"
                | BinaryModulus -> "rem"
                | BinaryEqual -> "=:="
                | BinaryUnequal -> "=/="
                | BinaryLess -> "<"
                | BinaryLessOrEqual -> "=<"
                | BinaryGreater -> ">"
                | BinaryGreaterOrEqual -> ">="
                | BinaryShiftLeft -> "bsl"
                | BinaryShiftRightSignPropagating -> "bsr"
                | BinaryShiftRightZeroFill -> "bsr"
                | BinaryOrBitwise -> "bor"
                | BinaryXorBitwise -> "bxor"
                | BinaryAndBitwise -> "band"
                | BinaryExponent -> "+" // unreachable, handled above

            Beam.ErlExpr.BinOp(erlOp, cleanLeft, cleanRight) |> wrapWithHoisted allHoisted

    | Unary(op, operand) ->
        let erlOperand = transformExpr com ctx operand
        let hoisted, cleanOperand = extractBlock erlOperand

        let result =
            match op with
            | UnaryMinus -> Beam.ErlExpr.UnaryOp("-", cleanOperand)
            | UnaryPlus -> cleanOperand
            | UnaryNot -> Beam.ErlExpr.UnaryOp("not", cleanOperand)
            | UnaryNotBitwise -> Beam.ErlExpr.UnaryOp("bnot", cleanOperand)
            | UnaryAddressOf ->
                // For mutable variables, pass the atom key (process dict key)
                // instead of the dereferenced value. This enables out-parameter support
                // (e.g., Dictionary.TryGetValue) where the callee needs to put() a new value.
                match operand with
                | IdentExpr ident when ctx.MutableVars.Contains(ident.Name) ->
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName ident.Name)))
                | _ -> cleanOperand

        result |> wrapWithHoisted hoisted

    | Logical(op, left, right) ->
        let erlLeft = transformExpr com ctx left
        let erlRight = transformExpr com ctx right
        let leftHoisted, cleanLeft = extractBlock erlLeft
        let rightHoisted, cleanRight = extractBlock erlRight
        let allHoisted = leftHoisted @ rightHoisted

        let erlOp =
            match op with
            | LogicalAnd -> "andalso"
            | LogicalOr -> "orelse"

        Beam.ErlExpr.BinOp(erlOp, cleanLeft, cleanRight) |> wrapWithHoisted allHoisted

and transformTest (com: IBeamCompiler) (ctx: Context) (kind: TestKind) (expr: Expr) : Beam.ErlExpr =
    let erlExpr = transformExpr com ctx expr

    match kind with
    | UnionCaseTest tag ->
        Beam.ErlExpr.BinOp(
            "=:=",
            Beam.ErlExpr.Call(Some "erlang", "element", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L); erlExpr ]),
            Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 tag))
        )
    | ListTest isCons ->
        if isCons then
            Beam.ErlExpr.BinOp("=/=", erlExpr, Beam.ErlExpr.Literal(Beam.ErlLiteral.NilLit))
        else
            Beam.ErlExpr.BinOp("=:=", erlExpr, Beam.ErlExpr.Literal(Beam.ErlLiteral.NilLit))
    | OptionTest isSome ->
        if isSome then
            Beam.ErlExpr.BinOp("=/=", erlExpr, Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined")))
        else
            Beam.ErlExpr.BinOp("=:=", erlExpr, Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined")))
    | TypeTest typ ->
        match typ with
        | Fable.AST.Fable.Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Beam.ErlExpr.Call(None, "is_float", [ erlExpr ])
            | _ -> Beam.ErlExpr.Call(None, "is_integer", [ erlExpr ])
        | Fable.AST.Fable.Type.String -> Beam.ErlExpr.Call(None, "is_binary", [ erlExpr ])
        | Fable.AST.Fable.Type.Boolean -> Beam.ErlExpr.Call(None, "is_boolean", [ erlExpr ])
        | Fable.AST.Fable.Type.List _ -> Beam.ErlExpr.Call(None, "is_list", [ erlExpr ])
        | Fable.AST.Fable.Type.Array _ -> Beam.ErlExpr.Call(None, "is_list", [ erlExpr ])
        | Fable.AST.Fable.Type.Tuple _ -> Beam.ErlExpr.Call(None, "is_tuple", [ erlExpr ])
        | Fable.AST.Fable.Type.DeclaredType(ref, _) ->
            // For exception types, check __type tag: (is_map(X) andalso maps:get(__type, X, undefined) =:= type_name)
            // For other types, just check is_map
            let isMap = Beam.ErlExpr.Call(None, "is_map", [ erlExpr ])

            match com.TryGetEntity(ref) with
            | Some entity when entity.IsFSharpExceptionDeclaration ->
                let typeName = sanitizeErlangName entity.DisplayName

                let typeCheck =
                    Beam.ErlExpr.BinOp(
                        "=:=",
                        Beam.ErlExpr.Call(Some "maps", "get", [ atomLit "exn_type"; erlExpr; atomLit "undefined" ]),
                        atomLit typeName
                    )

                Beam.ErlExpr.BinOp("andalso", isMap, typeCheck)
            | _ -> isMap
        | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.BoolLit true)

and transformGet (com: IBeamCompiler) (ctx: Context) (kind: GetKind) (typ: Type) (expr: Expr) : Beam.ErlExpr =
    let erlExpr = transformExpr com ctx expr

    match kind with
    | TupleIndex i ->
        Beam.ErlExpr.Call(
            Some "erlang",
            "element",
            [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 (i + 1))); erlExpr ]
        )
    | UnionTag ->
        Beam.ErlExpr.Call(Some "erlang", "element", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L); erlExpr ])
    | UnionField info ->
        Beam.ErlExpr.Call(
            None,
            "element",
            [
                Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 (info.FieldIndex + 2)))
                erlExpr
            ]
        )
    | ListHead -> Beam.ErlExpr.Call(Some "erlang", "hd", [ erlExpr ])
    | ListTail -> Beam.ErlExpr.Call(Some "erlang", "tl", [ erlExpr ])
    | OptionValue ->
        if mustWrapOption typ then
            Beam.ErlExpr.Call(Some "fable_option", "value", [ erlExpr ])
        else
            erlExpr
    | FieldGet info ->
        let fieldName = sanitizeErlangName info.Name
        let fieldAtom = Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom fieldName))

        match expr.Type with
        | Fable.AST.Fable.Type.DeclaredType(entityRef, _) when isClassType com entityRef ->
            // During constructor, field values may reference other fields via this.FieldName.
            // Since put(Ref, #{...}) hasn't happened yet, we use the precomputed Erlang expressions.
            let isThisRef =
                match ctx.ThisArgVar, erlExpr with
                | Some thisVar, Beam.ErlExpr.Variable v -> v = thisVar
                | _ -> false

            match isThisRef, ctx.CtorFieldExprs.TryFind(info.Name) with
            | true, Some cachedExpr -> cachedExpr
            | _ ->
                // Class instance: state is stored in process dict, ref is the key
                Beam.ErlExpr.Call(Some "maps", "get", [ fieldAtom; Beam.ErlExpr.Call(None, "get", [ erlExpr ]) ])
        | Fable.AST.Fable.Type.DeclaredType(entityRef, _) when isInterfaceType com entityRef ->
            // Interface dispatch: works for both object expressions (maps) and class instances (refs)
            Beam.ErlExpr.Call(Some "fable_utils", "iface_get", [ fieldAtom; erlExpr ])
        | _ ->
            // Record/union: direct map access
            Beam.ErlExpr.Call(Some "maps", "get", [ fieldAtom; erlExpr ])
    | ExprGet indexExpr ->
        let erlIndex = transformExpr com ctx indexExpr

        match expr.Type with
        | Fable.AST.Fable.Type.Array _ ->
            // Array (Erlang list): lists:nth is 1-based, F# is 0-based
            Beam.ErlExpr.Call(
                Some "lists",
                "nth",
                [
                    Beam.ErlExpr.BinOp("+", erlIndex, Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L))
                    erlExpr
                ]
            )
        | Fable.AST.Fable.Type.String ->
            // String indexing: binary:at(Str, Idx)
            Beam.ErlExpr.Call(Some "binary", "at", [ erlExpr; erlIndex ])
        | _ ->
            // Check if the index is a string constant (from JS Replacements field access like getFieldWith)
            match indexExpr with
            | Value(StringConstant "length", _) ->
                // .Length property fallthrough from JS Replacements → byte_size for binaries
                Beam.ErlExpr.Call(None, "byte_size", [ erlExpr ])
            | Value(StringConstant key, _) ->
                // String-keyed field access → maps:get(atom, obj)
                let fieldAtom =
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(sanitizeErlangName key)))

                Beam.ErlExpr.Call(Some "maps", "get", [ fieldAtom; erlExpr ])
            | _ ->
                // Integer index → tuple element (1-based)
                Beam.ErlExpr.Call(Some "erlang", "element", [ erlIndex; erlExpr ])

and transformCall (com: IBeamCompiler) (ctx: Context) (callee: Expr) (info: CallInfo) : Beam.ErlExpr =
    match callee with
    | Import(importInfo, _typ, _range) ->
        let importModuleName =
            let name = moduleNameFromFile importInfo.Path
            let currentModule = moduleNameFromFile com.CurrentFile

            if name = currentModule then
                None
            else
                Some name

        match importInfo.Selector with
        | "assertEqual"
        | "Testing_equal" ->
            match info.Args with
            | actual :: expected :: _ ->
                let erlActual = transformExpr com ctx actual
                let erlExpected = transformExpr com ctx expected
                let actualHoisted, cleanActual = extractBlock erlActual
                let expectedHoisted, cleanExpected = extractBlock erlExpected
                let allHoisted = actualHoisted @ expectedHoisted
                // Store complex expressions in temp variables to avoid duplicate evaluation
                // and Erlang "unsafe variable" errors
                let counter = com.IncrementCounter()

                let storeIfComplex name expr =
                    match expr with
                    | Beam.ErlExpr.Literal _
                    | Beam.ErlExpr.Variable _ -> ([], expr)
                    | _ ->
                        let varName = $"%s{name}_%d{counter}"
                        ([ Beam.ErlExpr.Match(Beam.PVar varName, expr) ], Beam.ErlExpr.Variable varName)

                let tempActualH, useActual = storeIfComplex "Assert_actual" cleanActual
                let tempExpectedH, useExpected = storeIfComplex "Assert_expected" cleanExpected

                Beam.ErlExpr.Case(
                    Beam.ErlExpr.BinOp("=:=", useActual, useExpected),
                    [
                        {
                            Pattern = Beam.PLiteral(Beam.BoolLit true)
                            Guard = []
                            Body = [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")) ]
                        }
                        {
                            Pattern = Beam.PLiteral(Beam.BoolLit false)
                            Guard = []
                            Body =
                                [
                                    Beam.ErlExpr.Call(
                                        Some "erlang",
                                        "error",
                                        [
                                            Beam.ErlExpr.Tuple
                                                [
                                                    Beam.ErlExpr.Literal(
                                                        Beam.ErlLiteral.AtomLit(Beam.Atom "assert_equal")
                                                    )
                                                    useExpected
                                                    useActual
                                                ]
                                        ]
                                    )
                                ]
                        }
                    ]
                )
                |> wrapWithHoisted (allHoisted @ tempActualH @ tempExpectedH)
            | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_assert_equal"))
        | "assertNotEqual"
        | "Testing_notEqual" ->
            match info.Args with
            | actual :: expected :: _ ->
                let erlActual = transformExpr com ctx actual
                let erlExpected = transformExpr com ctx expected
                let actualHoisted, cleanActual = extractBlock erlActual
                let expectedHoisted, cleanExpected = extractBlock erlExpected
                let allHoisted = actualHoisted @ expectedHoisted
                let counter = com.IncrementCounter()

                let storeIfComplex name expr =
                    match expr with
                    | Beam.ErlExpr.Literal _
                    | Beam.ErlExpr.Variable _ -> ([], expr)
                    | _ ->
                        let varName = $"%s{name}_%d{counter}"
                        ([ Beam.ErlExpr.Match(Beam.PVar varName, expr) ], Beam.ErlExpr.Variable varName)

                let tempActualH, useActual = storeIfComplex "Assert_actual" cleanActual
                let tempExpectedH, useExpected = storeIfComplex "Assert_expected" cleanExpected

                Beam.ErlExpr.Case(
                    Beam.ErlExpr.BinOp("=/=", useActual, useExpected),
                    [
                        {
                            Pattern = Beam.PLiteral(Beam.BoolLit true)
                            Guard = []
                            Body = [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")) ]
                        }
                        {
                            Pattern = Beam.PLiteral(Beam.BoolLit false)
                            Guard = []
                            Body =
                                [
                                    Beam.ErlExpr.Call(
                                        Some "erlang",
                                        "error",
                                        [
                                            Beam.ErlExpr.Tuple
                                                [
                                                    Beam.ErlExpr.Literal(
                                                        Beam.ErlLiteral.AtomLit(Beam.Atom "assert_not_equal")
                                                    )
                                                    useExpected
                                                    useActual
                                                ]
                                        ]
                                    )
                                ]
                        }
                    ]
                )
                |> wrapWithHoisted (allHoisted @ tempActualH @ tempExpectedH)
            | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_assert_not_equal"))
        | "concat" when importModuleName = Some "string" ->
            // String.Concat from JS Replacements → use binary concatenation
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            Beam.ErlExpr.Call(None, "iolist_to_binary", [ Beam.ErlExpr.List cleanArgs ])
            |> wrapWithHoisted hoisted
        | "isNullOrEmpty" ->
            // String.IsNullOrEmpty(s) → (S =:= undefined) orelse (S =:= <<"">>)
            match info.Args with
            | [ arg ] ->
                let erlArg = transformExpr com ctx arg
                let hoisted, cleanArg = extractBlock erlArg

                Beam.ErlExpr.BinOp(
                    "orelse",
                    Beam.ErlExpr.BinOp(
                        "=:=",
                        cleanArg,
                        Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined"))
                    ),
                    Beam.ErlExpr.BinOp("=:=", cleanArg, Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit ""))
                )
                |> wrapWithHoisted hoisted
            | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_is_null_or_empty"))
        | selector when importModuleName = Some "list" ->
            // Map F# List operations to Erlang stdlib
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            match selector with
            | "head" -> Beam.ErlExpr.Call(Some "erlang", "hd", cleanArgs)
            | "tail" -> Beam.ErlExpr.Call(Some "erlang", "tl", cleanArgs)
            | "length" -> Beam.ErlExpr.Call(Some "erlang", "length", cleanArgs)
            | "map" -> Beam.ErlExpr.Call(Some "lists", "map", cleanArgs)
            | "filter" -> Beam.ErlExpr.Call(Some "lists", "filter", cleanArgs)
            | "rev"
            | "reverse" -> Beam.ErlExpr.Call(Some "lists", "reverse", cleanArgs)
            | "append" -> Beam.ErlExpr.Call(Some "lists", "append", cleanArgs)
            | "sum" ->
                // ReplacementsInject adds IGenericAdder arg — Erlang's lists:sum/1 doesn't need it
                match cleanArgs with
                | list :: _ -> Beam.ErlExpr.Call(Some "lists", "sum", [ list ])
                | _ -> Beam.ErlExpr.Call(Some "lists", "sum", cleanArgs)
            | "fold" ->
                // F# fold: folder(acc, item), Erlang foldl: fun(item, acc) — must swap
                match cleanArgs with
                | [ f; state; list ] ->
                    let ctr = com.IncrementCounter()
                    let xVar, accVar = $"Fold_x_%d{ctr}", $"Fold_acc_%d{ctr}"

                    let wrapper =
                        Beam.ErlExpr.Fun
                            [
                                {
                                    Patterns = [ Beam.PVar xVar; Beam.PVar accVar ]
                                    Guard = []
                                    Body =
                                        [
                                            Beam.ErlExpr.Apply(
                                                f,
                                                [ Beam.ErlExpr.Variable accVar; Beam.ErlExpr.Variable xVar ]
                                            )
                                        ]
                                }
                            ]

                    Beam.ErlExpr.Call(Some "lists", "foldl", [ wrapper; state; list ])
                | _ -> Beam.ErlExpr.Call(Some "lists", "foldl", cleanArgs)
            | _ ->
                // Fallback: try lists:selector(args...)
                Beam.ErlExpr.Call(Some "lists", sanitizeErlangName selector, cleanArgs)
            |> wrapWithHoisted hoisted
        | selector when importModuleName = Some "array" ->
            // Array module calls from JS Replacements — arrays are lists in Erlang
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            match selector with
            | "item" ->
                // array:item(idx, arr) → lists:nth(idx+1, arr) (0-based to 1-based)
                match cleanArgs with
                | [ idx; arr ] ->
                    Beam.ErlExpr.Call(
                        Some "lists",
                        "nth",
                        [
                            Beam.ErlExpr.BinOp("+", idx, Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L))
                            arr
                        ]
                    )
                | _ -> Beam.ErlExpr.Call(Some "lists", "nth", cleanArgs)
            | _ ->
                // Fallback: try lists:selector(args...)
                Beam.ErlExpr.Call(Some "lists", sanitizeErlangName selector, cleanArgs)
            |> wrapWithHoisted hoisted
        | selector when importModuleName = Some "map" ->
            // Map module calls from JS Replacements fallthrough
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            match selector with
            | "ofList"
            | "ofArray"
            | "ofSeq" ->
                // May have injected comparer as first arg: [comparer; pairs]
                let pairs =
                    match cleanArgs with
                    | [ _; p ] -> p
                    | [ p ] -> p
                    | _ -> Beam.ErlExpr.List []

                Beam.ErlExpr.Call(Some "maps", "from_list", [ pairs ])
            | "empty" -> Beam.ErlExpr.Map []
            | "add" ->
                match cleanArgs with
                | [ key; value; map ] -> Beam.ErlExpr.Call(Some "maps", "put", [ key; value; map ])
                | _ -> Beam.ErlExpr.Call(Some "maps", "put", cleanArgs)
            | "find" ->
                match cleanArgs with
                | [ key; map ] -> Beam.ErlExpr.Call(Some "maps", "get", [ key; map ])
                | _ -> Beam.ErlExpr.Call(Some "maps", "get", cleanArgs)
            | "containsKey" ->
                match cleanArgs with
                | [ key; map ] -> Beam.ErlExpr.Call(Some "maps", "is_key", [ key; map ])
                | _ -> Beam.ErlExpr.Call(Some "maps", "is_key", cleanArgs)
            | "remove" ->
                match cleanArgs with
                | [ key; map ] -> Beam.ErlExpr.Call(Some "maps", "remove", [ key; map ])
                | _ -> Beam.ErlExpr.Call(Some "maps", "remove", cleanArgs)
            | "count" -> Beam.ErlExpr.Call(Some "maps", "size", cleanArgs)
            | "isEmpty" ->
                match cleanArgs with
                | [ map ] ->
                    Beam.ErlExpr.BinOp(
                        "=:=",
                        Beam.ErlExpr.Call(Some "maps", "size", [ map ]),
                        Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 0L)
                    )
                | _ -> Beam.ErlExpr.Call(Some "maps", "size", cleanArgs)
            | "toList"
            | "toArray"
            | "toSeq" -> Beam.ErlExpr.Call(Some "maps", "to_list", cleanArgs)
            | _ -> Beam.ErlExpr.Call(Some "maps", sanitizeErlangName selector, cleanArgs)
            |> wrapWithHoisted hoisted
        | selector ->
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            // For instance method/property calls, prepend ThisArg to args
            let allHoisted, allArgs =
                match info.ThisArg with
                | Some thisExpr ->
                    let erlThis = transformExpr com ctx thisExpr
                    let thisHoisted, cleanThis = extractBlock erlThis
                    hoisted @ thisHoisted, cleanThis :: cleanArgs
                | None -> hoisted, cleanArgs

            Beam.ErlExpr.Call(importModuleName, sanitizeErlangName selector, allArgs)
            |> wrapWithHoisted allHoisted

    | IdentExpr ident ->
        let args = info.Args |> List.map (transformExpr com ctx)
        let hoisted, cleanArgs = hoistBlocksFromArgs args

        // For instance method/property calls, prepend ThisArg to args
        let allHoisted, allArgs =
            match info.ThisArg with
            | Some thisExpr ->
                let erlThis = transformExpr com ctx thisExpr
                let thisHoisted, cleanThis = extractBlock erlThis
                hoisted @ thisHoisted, cleanThis :: cleanArgs
            | None -> hoisted, cleanArgs

        match ctx.MutualRecBindings.TryFind(ident.Name) with
        | Some(bundleName, atomTag) ->
            // Mutual recursion: (Mk_rec_N(atom_tag))(args)
            let bundleCall =
                Beam.ErlExpr.Apply(
                    Beam.ErlExpr.Variable(bundleName),
                    [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
                )

            Beam.ErlExpr.Apply(bundleCall, allArgs) |> wrapWithHoisted allHoisted
        | None ->
            if ctx.RecursiveBindings.Contains(ident.Name) || ctx.LocalVars.Contains(ident.Name) then
                Beam.ErlExpr.Apply(Beam.ErlExpr.Variable(capitalizeFirst ident.Name |> sanitizeErlangVar), allArgs)
                |> wrapWithHoisted allHoisted
            else
                Beam.ErlExpr.Call(None, sanitizeErlangName ident.Name, allArgs)
                |> wrapWithHoisted allHoisted

    | Get(calleeExpr, FieldGet fieldInfo, _, _) ->
        // Check if this is an interface method call (callee is an interface type)
        let isInterfaceExpr =
            match calleeExpr.Type with
            | Fable.AST.Fable.Type.DeclaredType(entityRef, _) ->
                match com.TryGetEntity(entityRef) with
                | Some entity -> entity.IsInterface
                | None -> false
            | _ -> false

        let erlCallee = transformExpr com ctx calleeExpr
        let args = info.Args |> List.map (transformExpr com ctx)
        let calleeHoisted, cleanCallee = extractBlock erlCallee
        let argsHoisted, cleanArgs = hoistBlocksFromArgs args
        let allHoisted = calleeHoisted @ argsHoisted

        if isInterfaceExpr then
            // Interface method dispatch: (fable_utils:iface_get(method_name, Obj))(Args)
            // Works for both object expressions (maps) and class instances (refs)
            let methodAtom = atomLit (sanitizeErlangName fieldInfo.Name)

            let lookup =
                Beam.ErlExpr.Call(Some "fable_utils", "iface_get", [ methodAtom; cleanCallee ])

            Beam.ErlExpr.Apply(lookup, cleanArgs) |> wrapWithHoisted allHoisted
        else
            match fieldInfo.Name with
            | "indexOf" ->
                // str.indexOf(sub) → case binary:match(Str, Sub) of {Pos,_} -> Pos; nomatch -> -1 end
                match cleanArgs with
                | [ sub ] ->
                    let ctr = com.IncrementCounter()
                    let posVar = $"Idx_pos_%d{ctr}"

                    Beam.ErlExpr.Case(
                        Beam.ErlExpr.Call(Some "binary", "match", [ cleanCallee; sub ]),
                        [
                            {
                                Pattern = Beam.PTuple [ Beam.PVar posVar; Beam.PWildcard ]
                                Guard = []
                                Body = [ Beam.ErlExpr.Variable posVar ]
                            }
                            {
                                Pattern = Beam.PLiteral(Beam.ErlLiteral.AtomLit(Beam.Atom "nomatch"))
                                Guard = []
                                Body = [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer -1L) ]
                            }
                        ]
                    )
                    |> wrapWithHoisted allHoisted
                | _ ->
                    Beam.ErlExpr.Call(None, "unknown_call", cleanCallee :: cleanArgs)
                    |> wrapWithHoisted allHoisted
            | methodName ->
                // Check if callee is a record/anonymous record (function-valued field)
                let isRecordLike =
                    match calleeExpr.Type with
                    | Fable.AST.Fable.Type.AnonymousRecordType _ -> true
                    | Fable.AST.Fable.Type.DeclaredType(entityRef, _) ->
                        match com.TryGetEntity(entityRef) with
                        | Some entity -> entity.IsFSharpRecord
                        | None -> false
                    | _ -> false

                if isRecordLike then
                    // Function-valued record field: (maps:get(field, Record))(Args)
                    let fieldAtom = atomLit (sanitizeErlangName methodName)
                    let lookup = Beam.ErlExpr.Call(Some "maps", "get", [ fieldAtom; cleanCallee ])
                    Beam.ErlExpr.Apply(lookup, cleanArgs) |> wrapWithHoisted allHoisted
                else
                    Beam.ErlExpr.Call(None, sanitizeErlangName methodName, cleanCallee :: cleanArgs)
                    |> wrapWithHoisted allHoisted

    | _ ->
        // Generic callee expression (e.g., operator value like (+) passed as function arg).
        // Transform the callee and apply it as a function value.
        let erlCallee = transformExpr com ctx callee
        let args = info.Args |> List.map (transformExpr com ctx)
        let calleeHoisted, cleanCallee = extractBlock erlCallee
        let argsHoisted, cleanArgs = hoistBlocksFromArgs args
        let allHoisted = calleeHoisted @ argsHoisted

        Beam.ErlExpr.Apply(cleanCallee, cleanArgs) |> wrapWithHoisted allHoisted

and transformClassDeclaration
    (com: IBeamCompiler)
    (ctx: Context)
    (className: string)
    (ent: Entity)
    (decl: ClassDecl)
    : Beam.ErlForm list
    =
    let constructorForms =
        match decl.Constructor with
        | Some cons ->
            // Extract constructor field assignments from the body.
            // The constructor body pattern is:
            //   Sequential [ObjectExpr; Sequential[Set(FieldSet(name), ThisValue, value); ...]]
            // We collect all Set(FieldSet(name), _, value) into initial map entries.
            let rec extractFieldSets (expr: Expr) : (string * Expr) list =
                match expr with
                | Set(_, FieldSet fieldName, _, value, _) -> [ (fieldName, value) ]
                | Sequential exprs -> exprs |> List.collect extractFieldSets
                | _ -> []

            let fields = extractFieldSets cons.Body

            // Constructor args: skip `this` (first arg in the Fable IR for instance members)
            let ctorArgs = cons.Args |> List.filter (fun a -> a.Name <> "this")

            let argPatterns =
                ctorArgs
                |> List.map (fun a ->
                    let name = capitalizeFirst a.Name |> sanitizeErlangVar

                    if a.Name.StartsWith("unitVar", System.StringComparison.Ordinal) then
                        Beam.PVar("_" + name)
                    else
                        Beam.PVar(name)
                )

            if ent.IsFSharpExceptionDeclaration then
                // F# exception construction goes through NewRecord, not ClassDecl constructor.
                // No need to generate a constructor function here.
                []
            else
                // Regular class: object = make_ref(), state in process dict
                let ctorCtx =
                    { ctx with
                        ThisArgVar = Some "Ref"
                        LocalVars = ctorArgs |> List.fold (fun (s: Set<string>) a -> s.Add(a.Name)) ctx.LocalVars
                    }

                // Process fields in order, progressively building a map of field name → Erlang expr.
                // This allows later fields to reference earlier fields via this.FieldName
                // (which would otherwise fail because put(Ref, #{...}) hasn't happened yet).
                let mapEntries, _ =
                    fields
                    |> List.fold
                        (fun (entries, fieldCtx: Context) (name, value) ->
                            let erlValue = transformExpr com fieldCtx value
                            let entries' = entries @ [ atomLit (sanitizeErlangName name), erlValue ]

                            let fieldCtx' =
                                { fieldCtx with CtorFieldExprs = fieldCtx.CtorFieldExprs.Add(name, erlValue) }

                            entries', fieldCtx'
                        )
                        ([], ctorCtx)

                // Build interface entries from AttachedMembers that implement interfaces.
                // These are stored in the process dict state map alongside field state,
                // so interface dispatch (fable_utils:iface_get) can find them.
                let interfaceEntries =
                    decl.AttachedMembers
                    |> List.choose (fun memb ->
                        match memb.ImplementedSignatureRef with
                        | Some sigRef ->
                            let sigInfo = com.GetMember(sigRef)
                            let memberName = sanitizeErlangName memb.Name

                            if sigInfo.IsGetter then
                                // Property getter: evaluate the body in constructor context
                                // to get the value. The body can reference `this` fields via get(Ref).
                                let ifaceCtorCtx =
                                    match memb.Args with
                                    | first :: _ when first.IsThisArgument || first.Name = "this" ->
                                        { ctorCtx with ThisIdentNames = Set.singleton first.Name }
                                    | _ -> ctorCtx

                                let erlBody = transformExpr com ifaceCtorCtx memb.Body
                                Some(atomLit memberName, erlBody)
                            else
                                // Method: create a closure with the body inlined directly.
                                let nonThisArgs =
                                    memb.Args |> List.filter (fun a -> not a.IsThisArgument && a.Name <> "this")

                                let argPats =
                                    nonThisArgs
                                    |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar))

                                // Set up context so `this` references resolve to `Ref`
                                // and non-this args are tracked as local vars
                                let ifaceCtorCtx =
                                    let argNames =
                                        nonThisArgs
                                        |> List.fold (fun (s: Set<string>) a -> s.Add(a.Name)) ctorCtx.LocalVars

                                    match memb.Args with
                                    | first :: _ when first.IsThisArgument || first.Name = "this" ->
                                        { ctorCtx with
                                            ThisIdentNames = Set.singleton first.Name
                                            LocalVars = argNames
                                        }
                                    | _ -> { ctorCtx with LocalVars = argNames }

                                let erlBody = transformExpr com ifaceCtorCtx memb.Body

                                let bodyExprs =
                                    match erlBody with
                                    | Beam.ErlExpr.Block es -> es
                                    | e -> [ e ]

                                let closure =
                                    Beam.ErlExpr.Fun
                                        [
                                            {
                                                Patterns = argPats
                                                Guard = []
                                                Body = bodyExprs
                                            }
                                        ]

                                Some(atomLit memberName, closure)
                        | None -> None
                    )

                let body =
                    [
                        // Ref = make_ref()
                        Beam.ErlExpr.Match(Beam.PVar "Ref", Beam.ErlExpr.Call(None, "make_ref", []))
                        // put(Ref, #{field1 => Val1, field2 => Val2, ...})
                        Beam.ErlExpr.Call(None, "put", [ Beam.ErlExpr.Variable "Ref"; Beam.ErlExpr.Map mapEntries ])
                    ]
                    @ (if interfaceEntries.IsEmpty then
                           []
                       else
                           // Step 2: merge interface entries into state (separate put so getter bodies
                           // that reference this.SomeField via get(Ref) work correctly)
                           [
                               Beam.ErlExpr.Call(
                                   None,
                                   "put",
                                   [
                                       Beam.ErlExpr.Variable "Ref"
                                       Beam.ErlExpr.Call(
                                           Some "maps",
                                           "merge",
                                           [
                                               Beam.ErlExpr.Call(None, "get", [ Beam.ErlExpr.Variable "Ref" ])
                                               Beam.ErlExpr.Map interfaceEntries
                                           ]
                                       )
                                   ]
                               )
                           ])
                    @ [
                        // Return Ref
                        Beam.ErlExpr.Variable "Ref"
                    ]

                // Use the constructor's compiled name so call sites can find it
                let ctorFuncName = sanitizeErlangName cons.Name

                let funcDef: Beam.ErlFunctionDef =
                    {
                        Name = Beam.Atom ctorFuncName
                        Arity = argPatterns.Length
                        Clauses =
                            [
                                {
                                    Patterns = argPatterns
                                    Guard = []
                                    Body = body
                                }
                            ]
                    }

                [ Beam.ErlForm.Function funcDef ]
        | None -> []

    // Attached members: getters, setters, methods, secondary constructors
    let memberForms =
        decl.AttachedMembers
        |> List.collect (fun memb ->
            // Skip interface implementations — they are inlined as closures
            // in interfaceEntries and don't need separate module-level functions.
            if memb.ImplementedSignatureRef.IsSome then
                []
            else

                let info =
                    memb.ImplementedSignatureRef
                    |> Option.map com.GetMember
                    |> Option.defaultWith (fun () -> com.GetMember(memb.MemberRef))

                // Helper: separate this arg from other args, set up proper context
                let getThisAndArgs () =
                    let argNames =
                        memb.Args
                        |> List.fold (fun (s: Set<string>) (a: Ident) -> s.Add(a.Name)) ctx.LocalVars

                    match memb.Args with
                    | first :: rest when first.IsThisArgument || first.Name = "this" ->
                        let thisIdentNames = Set.singleton first.Name

                        let memberCtx =
                            { ctx with
                                ThisArgVar = Some "This"
                                ThisIdentNames = thisIdentNames
                                LocalVars = argNames
                            }

                        Some first, rest, memberCtx
                    | _ -> None, memb.Args, { ctx with LocalVars = argNames }

                if info.IsConstructor then
                    // Secondary constructor: generates an additional function with different arity
                    // The body typically calls the primary constructor
                    let ctorArgs =
                        memb.Args
                        |> List.filter (fun a ->
                            not a.IsThisArgument
                            && a.Name <> "this"
                            && not (a.Name.StartsWith("unitVar", System.StringComparison.Ordinal))
                        )

                    let argPatterns =
                        ctorArgs
                        |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar))

                    let ctorCtx =
                        { ctx with LocalVars = ctorArgs |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars }

                    let bodyExpr = transformExpr com ctorCtx memb.Body

                    let body =
                        match bodyExpr with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    let funcDef: Beam.ErlFunctionDef =
                        {
                            Name = Beam.Atom(sanitizeErlangName memb.Name)
                            Arity = argPatterns.Length
                            Clauses =
                                [
                                    {
                                        Patterns = argPatterns
                                        Guard = []
                                        Body = body
                                    }
                                ]
                        }

                    [ Beam.ErlForm.Function funcDef ]
                elif not memb.IsMangled && info.IsGetter then
                    // Property getter: class_name_get_prop(This) -> maps:get(prop, get(This)).
                    let propName = sanitizeErlangName memb.Name
                    let funcName = $"%s{className}_%s{propName}"
                    let _thisArg, _nonThisArgs, memberCtx = getThisAndArgs ()

                    let bodyExpr = transformExpr com memberCtx memb.Body

                    let body =
                        match bodyExpr with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    let funcDef: Beam.ErlFunctionDef =
                        {
                            Name = Beam.Atom funcName
                            Arity = 1
                            Clauses =
                                [
                                    {
                                        Patterns = [ Beam.PVar "This" ]
                                        Guard = []
                                        Body = body
                                    }
                                ]
                        }

                    [ Beam.ErlForm.Function funcDef ]
                elif not memb.IsMangled && info.IsSetter then
                    // Property setter: class_name_set_prop(This, Value) -> put(This, maps:put(prop, Value, get(This))).
                    let propName = sanitizeErlangName memb.Name
                    let funcName = $"%s{className}_set_%s{propName}"
                    let _thisArg, nonThisArgs, memberCtx = getThisAndArgs ()

                    let argPatterns =
                        [ Beam.PVar "This" ]
                        @ (nonThisArgs
                           |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar)))

                    let bodyExpr = transformExpr com memberCtx memb.Body

                    let body =
                        match bodyExpr with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    let funcDef: Beam.ErlFunctionDef =
                        {
                            Name = Beam.Atom funcName
                            Arity = argPatterns.Length
                            Clauses =
                                [
                                    {
                                        Patterns = argPatterns
                                        Guard = []
                                        Body = body
                                    }
                                ]
                        }

                    [ Beam.ErlForm.Function funcDef ]
                else
                    // Regular method: class_name_method(This, Args...) -> Body.
                    let methodName = sanitizeErlangName memb.Name
                    let funcName = $"%s{className}_%s{methodName}"
                    let _thisArg, nonThisArgs, memberCtx = getThisAndArgs ()

                    let argPatterns =
                        [ Beam.PVar "This" ]
                        @ (nonThisArgs
                           |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name |> sanitizeErlangVar)))

                    let bodyExpr = transformExpr com memberCtx memb.Body

                    let body =
                        match bodyExpr with
                        | Beam.ErlExpr.Block es -> es
                        | e -> [ e ]

                    let funcDef: Beam.ErlFunctionDef =
                        {
                            Name = Beam.Atom funcName
                            Arity = argPatterns.Length
                            Clauses =
                                [
                                    {
                                        Patterns = argPatterns
                                        Guard = []
                                        Body = body
                                    }
                                ]
                        }

                    [ Beam.ErlForm.Function funcDef ]
        )

    // Deduplicate functions by name+arity. This can happen when a property setter
    // (e.g., `set StatusCode`) and a method (e.g., `SetStatusCode`) both mangle to
    // the same Erlang function name. Unlike JS/Python which have native getter/setter
    // syntax, Erlang uses plain functions so name collisions produce duplicate definitions.
    let allForms = constructorForms @ memberForms

    let dedup =
        allForms
        |> List.fold
            (fun (seen, acc) form ->
                match form with
                | Beam.ErlForm.Function def ->
                    let key = (def.Name, def.Arity)

                    if Set.contains key seen then
                        (seen, acc)
                    else
                        (Set.add key seen, form :: acc)
                | _ -> (seen, form :: acc)
            )
            (Set.empty, [])
        |> snd
        |> List.rev

    dedup

and transformDeclaration (com: IBeamCompiler) (ctx: Context) (decl: Declaration) : Beam.ErlForm list =
    match decl with
    | MemberDeclaration memDecl ->
        let info =
            memDecl.ImplementedSignatureRef
            |> Option.map com.GetMember
            |> Option.defaultWith (fun () -> com.GetMember(memDecl.MemberRef))

        let name = sanitizeErlangName memDecl.Name
        let arity = memDecl.Args.Length

        // For instance members, the first arg is `this` (named `_` or `__` or `this$` etc.)
        // We need to rename it to a proper Erlang variable and set up the context
        let args, memberCtx =
            if info.IsInstance && memDecl.Args.Length > 0 then
                let thisArg = memDecl.Args.[0]

                let thisVarName =
                    let n = capitalizeFirst thisArg.Name
                    // Erlang's `_` is anonymous (can't be referenced), need a real var
                    if n = "_" || n = "__" then
                        "This"
                    elif n.EndsWith("$", System.StringComparison.Ordinal) then
                        n.TrimEnd('$') // e.g., This$ -> This
                    else
                        n

                let restArgs =
                    memDecl.Args.[1..] |> List.map (fun arg -> Beam.PVar(toErlangVar arg))

                Beam.PVar(thisVarName) :: restArgs,
                { ctx with
                    ThisArgVar = Some thisVarName
                    ThisIdentNames = Set.singleton thisArg.Name
                    LocalVars = memDecl.Args |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars
                }
            else
                memDecl.Args |> List.map (fun arg -> Beam.PVar(toErlangVar arg)),
                { ctx with LocalVars = memDecl.Args |> List.fold (fun s a -> s.Add(a.Name)) ctx.LocalVars }

        let bodyExpr = transformExpr com memberCtx memDecl.Body

        let body =
            match bodyExpr with
            | Beam.ErlExpr.Block exprs -> exprs
            | expr -> [ expr ]

        let funcDef: Beam.ErlFunctionDef =
            {
                Name = Beam.Atom name
                Arity = arity
                Clauses =
                    [
                        {
                            Patterns = args
                            Guard = []
                            Body = body
                        }
                    ]
            }

        [ Beam.ErlForm.Function funcDef ]

    | ActionDeclaration actionDecl ->
        let bodyExpr = transformExpr com ctx actionDecl.Body

        let body =
            match bodyExpr with
            | Beam.ErlExpr.Block exprs -> exprs
            | expr -> [ expr ]

        let funcDef: Beam.ErlFunctionDef =
            {
                Name = Beam.Atom "main"
                Arity = 0
                Clauses =
                    [
                        {
                            Patterns = []
                            Guard = []
                            Body = body
                        }
                    ]
            }

        [ Beam.ErlForm.Function funcDef ]

    | ModuleDeclaration modDecl -> modDecl.Members |> List.collect (transformDeclaration com ctx)

    | ClassDeclaration decl ->
        let ent = com.GetEntity(decl.Entity)
        let className = sanitizeErlangName decl.Name
        transformClassDeclaration com ctx className ent decl

let transformFile (com: Fable.Compiler) (file: File) : Beam.ErlModule =
    let moduleName = moduleNameFromFile com.CurrentFile

    let ctx =
        {
            File = com.CurrentFile
            UsedNames = file.UsedNamesInRootScope
            DecisionTargets = []
            RecursiveBindings = Set.empty
            MutualRecBindings = Map.empty
            MutableVars = Set.empty
            LocalVars = Set.empty
            ThisArgVar = None
            ThisIdentNames = Set.empty
            CtorFieldExprs = Map.empty
        }

    let beamCom =
        { new IBeamCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                com.AddLog(msg, Severity.Warning, ?range = range, fileName = com.CurrentFile, tag = "FABLE")

            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.ProjectOptions = com.ProjectOptions
            member _.SourceFiles = com.SourceFiles
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.IncrementCounter() = com.IncrementCounter()
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(ref) = com.TryGetEntity(ref)
            member _.GetInlineExpr(key) = com.GetInlineExpr(key)
            member _.AddWatchDependency(file) = com.AddWatchDependency(file)

            member _.AddLog(msg, severity, ?range, ?fileName, ?tag) =
                com.AddLog(msg, severity, ?range = range, ?fileName = fileName, ?tag = tag)
        }

    let forms =
        file.Declarations
        |> List.collect (transformDeclaration beamCom ctx)
        // Deduplicate functions by name+arity at module level. This handles cases where
        // a property setter and a method mangle to the same Erlang function name, even
        // when they come from different declaration paths (ClassDeclaration vs MemberDeclaration).
        |> List.fold
            (fun (seen, acc) form ->
                match form with
                | Beam.ErlForm.Function def ->
                    let key = (def.Name, def.Arity)

                    if Set.contains key seen then
                        (seen, acc)
                    else
                        (Set.add key seen, form :: acc)
                | _ -> (seen, form :: acc)
            )
            (Set.empty, [])
        |> snd
        |> List.rev

    let exports =
        forms
        |> List.choose (fun form ->
            match form with
            | Beam.ErlForm.Function def -> Some(def.Name, def.Arity)
            | _ -> None
        )

    let allForms =
        [
            Beam.ErlForm.Attribute(Beam.ModuleAttr(Beam.Atom moduleName))
            Beam.ErlForm.Attribute(Beam.ExportAttr exports)
            yield! forms
        ]

    {
        Beam.ErlModule.Name = Beam.Atom moduleName
        Beam.ErlModule.Forms = allForms
    }
