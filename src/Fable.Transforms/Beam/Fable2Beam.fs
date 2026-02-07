module Fable.Transforms.Beam.Compiler

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms

// Use fully qualified names for Beam AST types to avoid conflicts with Fable AST
// e.g., Beam.ErlExpr, Beam.ErlLiteral, Beam.Atom, etc.

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
    }

let private toSnakeCase (name: string) =
    let sb = System.Text.StringBuilder()

    for i = 0 to name.Length - 1 do
        let c = name.[i]

        if System.Char.IsUpper(c) then
            if i > 0 then
                sb.Append('_') |> ignore

            sb.Append(System.Char.ToLowerInvariant(c)) |> ignore
        else
            sb.Append(c) |> ignore

    sb.ToString()

let private sanitizeErlangName (name: string) =
    // Decode $XXXX hex sequences from F# compiled names (e.g. $0020 -> space -> _)
    System.Text.RegularExpressions.Regex.Replace(
        name,
        @"\$([0-9A-Fa-f]{4})",
        fun m ->
            let c = char (System.Convert.ToInt32(m.Groups.[1].Value, 16))

            if System.Char.IsLetterOrDigit(c) then
                string c
            else
                "_"
    )
    |> fun s -> s.Replace("'", "")
    |> toSnakeCase
    |> fun s -> System.Text.RegularExpressions.Regex.Replace(s, "_+", "_")
    |> fun s -> s.Trim('_')

let private moduleNameFromFile (filePath: string) =
    System.IO.Path.GetFileNameWithoutExtension(filePath)
    |> toSnakeCase
    |> fun s -> s.Replace(".", "_").Replace("-", "_")

let private capitalizeFirst (s: string) =
    if s.Length = 0 then
        s
    else
        s.[0..0].ToUpperInvariant() + s.[1..]

/// Convert an F# ident to an Erlang variable name, prefixing unused unit args with _
let private toErlangVar (ident: Ident) =
    let name = capitalizeFirst ident.Name
    // Prefix unit parameters with _ to suppress Erlang unused variable warnings
    if ident.Name.StartsWith("unitVar") then
        "_" + name
    else
        name

let private isIntegerType (typ: Fable.AST.Fable.Type) =
    match typ with
    | Fable.AST.Fable.Type.Number(kind, _) ->
        match kind with
        | Float16
        | Float32
        | Float64
        | Decimal -> false
        | _ -> true
    | _ -> true // default to integer division

/// Check if a Fable expression contains a reference to the given identifier name
let rec private containsIdentRef (name: string) (expr: Expr) : bool =
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
    | Value _ -> false
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
    | _ -> false

/// Flatten nested Block expressions into a flat list
let rec private flattenBlock (expr: Beam.ErlExpr) : Beam.ErlExpr list =
    match expr with
    | Beam.ErlExpr.Block exprs -> exprs |> List.collect flattenBlock
    | _ -> [ expr ]

/// Extract a Block into (hoisted_statements, final_expression).
/// Used to ensure multi-expression Blocks don't appear in argument positions.
let private extractBlock (expr: Beam.ErlExpr) : Beam.ErlExpr list * Beam.ErlExpr =
    let flat = flattenBlock expr

    match flat with
    | [] -> ([], Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "ok")))
    | [ single ] -> ([], single)
    | many ->
        let stmts = many.[.. many.Length - 2]
        let final = many.[many.Length - 1]
        (stmts, final)

/// Hoist Block expressions from a list of transformed arguments
let private hoistBlocksFromArgs (args: Beam.ErlExpr list) : Beam.ErlExpr list * Beam.ErlExpr list =
    let results = args |> List.map extractBlock
    (results |> List.collect fst, results |> List.map snd)

/// Wrap an expression with hoisted statements, if any
let private wrapWithHoisted (hoisted: Beam.ErlExpr list) (expr: Beam.ErlExpr) : Beam.ErlExpr =
    if List.isEmpty hoisted then
        expr
    else
        Beam.ErlExpr.Block(hoisted @ [ expr ])

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
        match ctx.MutualRecBindings.TryFind(ident.Name) with
        | Some(bundleName, atomTag) ->
            // Return the dispatched closure: Mk_rec_N(atom_tag)
            Beam.ErlExpr.Apply(
                Beam.ErlExpr.Variable(bundleName),
                [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
            )
        | None -> Beam.ErlExpr.Variable(capitalizeFirst ident.Name)

    | Sequential exprs ->
        let erlExprs = exprs |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Block erlExprs

    | Let(ident, value, body) ->
        let varName = capitalizeFirst ident.Name

        match value with
        | Lambda(arg, lambdaBody, _) when containsIdentRef ident.Name lambdaBody ->
            // Self-recursive lambda: use Erlang named fun
            let ctx' = { ctx with RecursiveBindings = ctx.RecursiveBindings.Add(ident.Name) }
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
                            Patterns = [ Beam.PVar(capitalizeFirst arg.Name) ]
                            Guard = []
                            Body = bodyExprs
                        }
                    ]
                )

            let erlOuterBody = transformExpr com ctx' body
            Beam.ErlExpr.Block [ Beam.ErlExpr.Match(Beam.PVar varName, namedFun); erlOuterBody ]
        | Delegate(args, lambdaBody, _, _) when containsIdentRef ident.Name lambdaBody ->
            // Self-recursive delegate: use Erlang named fun
            let ctx' = { ctx with RecursiveBindings = ctx.RecursiveBindings.Add(ident.Name) }
            let argPats = args |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name))
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
            let erlBody = transformExpr com ctx body
            Beam.ErlExpr.Block [ Beam.ErlExpr.Match(Beam.PVar varName, erlValue); erlBody ]

    | TypeCast(expr, _typ) -> transformExpr com ctx expr

    | CurriedApply(applied, args, _typ, _range) ->
        let appliedExpr = transformExpr com ctx applied
        let argExprs = args |> List.map (transformExpr com ctx)
        let appliedHoisted, cleanApplied = extractBlock appliedExpr
        let argsHoisted, cleanArgs = hoistBlocksFromArgs argExprs
        let allHoisted = appliedHoisted @ argsHoisted

        let result =
            match cleanApplied with
            | Beam.ErlExpr.Call(m, f, existingArgs) -> Beam.ErlExpr.Call(m, f, existingArgs @ cleanArgs)
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
        let erlBody = transformExpr com ctx body

        let bodyExprs =
            match erlBody with
            | Beam.ErlExpr.Block es -> es
            | e -> [ e ]

        Beam.ErlExpr.Fun
            [
                {
                    Patterns = [ Beam.PVar(capitalizeFirst arg.Name) ]
                    Guard = []
                    Body = bodyExprs
                }
            ]

    | Delegate(args, body, _name, _tags) ->
        let argPats = args |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name))
        let erlBody = transformExpr com ctx body

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

    | Get(expr, kind, _typ, _range) -> transformGet com ctx kind expr

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
        | IdentExpr ident -> Beam.ErlExpr.Match(Beam.PVar(capitalizeFirst ident.Name), transformExpr com ctx value)
        | _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_set"))

    | Set(expr, FieldSet fieldName, _, value, _) ->
        Beam.ErlExpr.Call(
            Some "maps",
            "put",
            [
                Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(toSnakeCase fieldName)))
                transformExpr com ctx value
                transformExpr com ctx expr
            ]
        )

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
            let bundleName = $"Mk_rec_{com.IncrementCounter()}"

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
                    let argPats, lambdaBody =
                        match value with
                        | Lambda(arg, lambdaBody, _) -> [ Beam.PVar(capitalizeFirst arg.Name) ], lambdaBody
                        | Delegate(args, lambdaBody, _, _) ->
                            args |> List.map (fun a -> Beam.PVar(capitalizeFirst a.Name)), lambdaBody
                        | _ -> failwith "unreachable: already checked allAreLambdas"

                    let erlBody = transformExpr com ctx' lambdaBody

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
        else
            // Simple case: single binding or non-lambda bindings
            let assignments =
                bindings
                |> List.map (fun (ident, value) ->
                    Beam.ErlExpr.Match(Beam.PVar(capitalizeFirst ident.Name), transformExpr com ctx value)
                )

            let erlBody = transformExpr com ctx body
            Beam.ErlExpr.Block(assignments @ [ erlBody ])

    | Emit(emitInfo, _typ, _range) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit $"%%emit: {emitInfo.Macro}")

    | _ ->
        let exprName = expr.GetType().Name
        Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"todo_{exprName.ToLowerInvariant()}"))

and transformValue (com: IBeamCompiler) (ctx: Context) (value: ValueKind) : Beam.ErlExpr =
    match value with
    | StringConstant s -> Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit s)

    | BoolConstant b -> Beam.ErlExpr.Literal(Beam.ErlLiteral.BoolLit b)

    | NumberConstant(NumberValue.Int32 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))

    | NumberConstant(NumberValue.Float64 f, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Float f)

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

    | NewUnion(values, tag, _ref, _genArgs) ->
        let erlValues = values |> List.map (transformExpr com ctx)
        Beam.ErlExpr.Tuple(Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 tag)) :: erlValues)

    | NewOption(Some value, _typ, _isStruct) -> transformExpr com ctx value

    | NewOption(None, _typ, _isStruct) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "undefined"))

    | CharConstant c -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 c))

    | NumberConstant(NumberValue.Int64 i, _) -> Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer i)

    | NewRecord(values, ref, _genArgs) ->
        match com.TryGetEntity(ref) with
        | Some entity ->
            let entries =
                List.zip (entity.FSharpFields |> List.map (fun f -> f.Name)) values
                |> List.map (fun (name, value) ->
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(toSnakeCase name))),
                    transformExpr com ctx value
                )

            Beam.ErlExpr.Map entries
        | None ->
            let entries =
                values
                |> List.mapi (fun i v ->
                    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"field_{i}")), transformExpr com ctx v
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
            | _ ->
                let fmtStr =
                    Beam.ErlExpr.Call(None, "binary_to_list", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit "~p") ])

                Beam.ErlExpr.Call(
                    None,
                    "iolist_to_binary",
                    [
                        Beam.ErlExpr.Call(Some "io_lib", "format", [ fmtStr; Beam.ErlExpr.List [ erlValue ] ])
                    ]
                )

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

    | _ ->
        let kindName = value.GetType().Name
        Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom $"todo_{kindName.ToLowerInvariant()}"))

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
            | UnaryAddressOf -> cleanOperand

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
            Beam.ErlExpr.Call(None, "element", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L); erlExpr ]),
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
    | TypeTest _ -> Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom "todo_type_test"))

and transformGet (com: IBeamCompiler) (ctx: Context) (kind: GetKind) (expr: Expr) : Beam.ErlExpr =
    let erlExpr = transformExpr com ctx expr

    match kind with
    | TupleIndex i ->
        Beam.ErlExpr.Call(None, "element", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 (i + 1))); erlExpr ])
    | UnionTag -> Beam.ErlExpr.Call(None, "element", [ Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer 1L); erlExpr ])
    | UnionField info ->
        Beam.ErlExpr.Call(
            None,
            "element",
            [
                Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 (info.FieldIndex + 2)))
                erlExpr
            ]
        )
    | ListHead -> Beam.ErlExpr.Call(None, "hd", [ erlExpr ])
    | ListTail -> Beam.ErlExpr.Call(None, "tl", [ erlExpr ])
    | OptionValue -> erlExpr
    | FieldGet info ->
        Beam.ErlExpr.Call(
            Some "maps",
            "get",
            [
                Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom(toSnakeCase info.Name)))
                erlExpr
            ]
        )
    | ExprGet indexExpr -> Beam.ErlExpr.Call(None, "element", [ transformExpr com ctx indexExpr; erlExpr ])

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
        | "toConsole" ->
            match info.Args with
            | [ arg ] ->
                let erlArg = transformExpr com ctx arg
                let hoisted, cleanArg = extractBlock erlArg

                Beam.ErlExpr.Call(
                    Some "io",
                    "format",
                    [
                        Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit "~s~n")
                        Beam.ErlExpr.List [ cleanArg ]
                    ]
                )
                |> wrapWithHoisted hoisted
            | _ ->
                let args = info.Args |> List.map (transformExpr com ctx)
                let hoisted, cleanArgs = hoistBlocksFromArgs args
                Beam.ErlExpr.Call(Some "io", "format", cleanArgs) |> wrapWithHoisted hoisted
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
                        let varName = $"{name}_{counter}"
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
                        let varName = $"{name}_{counter}"
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
            | "head" -> Beam.ErlExpr.Call(None, "hd", cleanArgs)
            | "tail" -> Beam.ErlExpr.Call(None, "tl", cleanArgs)
            | "length" -> Beam.ErlExpr.Call(None, "length", cleanArgs)
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
                    let xVar, accVar = $"Fold_x_{ctr}", $"Fold_acc_{ctr}"

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
        | selector ->
            let args = info.Args |> List.map (transformExpr com ctx)
            let hoisted, cleanArgs = hoistBlocksFromArgs args

            Beam.ErlExpr.Call(importModuleName, sanitizeErlangName selector, cleanArgs)
            |> wrapWithHoisted hoisted

    | IdentExpr ident ->
        let args = info.Args |> List.map (transformExpr com ctx)
        let hoisted, cleanArgs = hoistBlocksFromArgs args

        match ctx.MutualRecBindings.TryFind(ident.Name) with
        | Some(bundleName, atomTag) ->
            // Mutual recursion: (Mk_rec_N(atom_tag))(args)
            let bundleCall =
                Beam.ErlExpr.Apply(
                    Beam.ErlExpr.Variable(bundleName),
                    [ Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom atomTag)) ]
                )

            Beam.ErlExpr.Apply(bundleCall, cleanArgs) |> wrapWithHoisted hoisted
        | None ->
            if ctx.RecursiveBindings.Contains(ident.Name) then
                Beam.ErlExpr.Apply(Beam.ErlExpr.Variable(capitalizeFirst ident.Name), cleanArgs)
                |> wrapWithHoisted hoisted
            else
                Beam.ErlExpr.Call(None, sanitizeErlangName ident.Name, cleanArgs)
                |> wrapWithHoisted hoisted

    | Get(calleeExpr, FieldGet fieldInfo, _, _) ->
        // Instance method calls (e.g., str.indexOf(sub) from String.Contains replacement)
        let erlCallee = transformExpr com ctx calleeExpr
        let args = info.Args |> List.map (transformExpr com ctx)
        let calleeHoisted, cleanCallee = extractBlock erlCallee
        let argsHoisted, cleanArgs = hoistBlocksFromArgs args
        let allHoisted = calleeHoisted @ argsHoisted

        match fieldInfo.Name with
        | "indexOf" ->
            // str.indexOf(sub) → case binary:match(Str, Sub) of {Pos,_} -> Pos; nomatch -> -1 end
            match cleanArgs with
            | [ sub ] ->
                let ctr = com.IncrementCounter()
                let posVar = $"Idx_pos_{ctr}"

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
            Beam.ErlExpr.Call(None, sanitizeErlangName methodName, cleanCallee :: cleanArgs)
            |> wrapWithHoisted allHoisted

    | _ ->
        let args = info.Args |> List.map (transformExpr com ctx)
        let hoisted, cleanArgs = hoistBlocksFromArgs args

        Beam.ErlExpr.Call(None, "unknown_call", cleanArgs) |> wrapWithHoisted hoisted

and transformDeclaration (com: IBeamCompiler) (ctx: Context) (decl: Declaration) : Beam.ErlForm list =
    match decl with
    | MemberDeclaration memDecl ->
        let name = sanitizeErlangName memDecl.Name
        let arity = memDecl.Args.Length

        let args = memDecl.Args |> List.map (fun arg -> Beam.PVar(toErlangVar arg))

        let bodyExpr = transformExpr com ctx memDecl.Body

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

    | ClassDeclaration _ -> [ Beam.ErlForm.Comment "TODO: class declaration" ]

let transformFile (com: Fable.Compiler) (file: File) : Beam.ErlModule =
    let moduleName = moduleNameFromFile com.CurrentFile

    let ctx =
        {
            File = com.CurrentFile
            UsedNames = file.UsedNamesInRootScope
            DecisionTargets = []
            RecursiveBindings = Set.empty
            MutualRecBindings = Map.empty
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

    let forms = file.Declarations |> List.collect (transformDeclaration beamCom ctx)

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
