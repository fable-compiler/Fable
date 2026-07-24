// F# quotation runtime for the Fable Rust target (functions).
//
// Authored in F# and transpiled by Fable so the FSharpExpr/FSharpVar types land at the
// right namespace and every mk*/is* signature matches what generated user code expects.
// The module is named `quotation_` to match the `libModule = "quotation"` import path.
module quotation_

open Microsoft.FSharp.Quotations

// --- Var constructor + accessors ---

let mkQuotVar (name: string) (typ: string) (isMutable: bool) : FSharpVar =
    {
        Name = name
        Type = typ
        IsMutable = isMutable
    }

let varGetName (v: FSharpVar) : string = v.Name
let varGetType (v: FSharpVar) : string = v.Type
let varGetIsMutable (v: FSharpVar) : bool = v.IsMutable

// --- Expr constructors ---

// Generic so callers can pass an unboxed literal (i32/bool/string/...); `box` erases
// it to obj. A non-generic obj parameter would require the caller to box, which the
// emitter does not do on statically typed targets.
let mkValue<'T> (value: 'T) (typ: string) : FSharpExpr = ExprValue(box value, typ)

// A null-value node (no instance / null literal / empty option or list).
// The payload is a boxed sentinel (not a null literal, which the Rust target can't
// represent for an untyped obj); consumers distinguish null nodes by the type tag.
let mkNull (typ: string) : FSharpExpr = ExprValue(box 0, typ)

let mkVar (v: FSharpVar) : FSharpExpr = ExprVarExpr v
let mkLambda (v: FSharpVar) (body: FSharpExpr) : FSharpExpr = ExprLambda(v, body)
let mkApplication (func: FSharpExpr) (arg: FSharpExpr) : FSharpExpr = ExprApplication(func, arg)
let mkLet (v: FSharpVar) (value: FSharpExpr) (body: FSharpExpr) : FSharpExpr = ExprLet(v, value, body)

let mkIfThenElse (guard: FSharpExpr) (thenExpr: FSharpExpr) (elseExpr: FSharpExpr) : FSharpExpr =
    ExprIfThenElse(guard, thenExpr, elseExpr)

let mkCall (instance: FSharpExpr) (method: string) (args: FSharpExpr[]) (declaringType: string) : FSharpExpr =
    ExprCall(instance, method, args, declaringType)

let mkSequential (first: FSharpExpr) (second: FSharpExpr) : FSharpExpr = ExprSequential(first, second)

// A correctly-typed empty argument array. Rust is statically typed, so an empty
// obj[] built by the emitter won't unify with the FSharpExpr[] parameters of
// mkCall/mkNewUnion/mkNewTuple (zero-arg calls like property getters, nullary
// union cases). The emitter routes empty expr-arrays here for the Rust target.
let emptyExprArray () : FSharpExpr[] = [||]

let mkNewTuple (elements: FSharpExpr[]) : FSharpExpr = ExprNewTuple elements

let mkNewUnion (typeName: string) (tag: int) (caseName: string) (fields: FSharpExpr[]) : FSharpExpr =
    ExprNewUnion(typeName, tag, caseName, fields)

let mkNewRecord (typeName: string) (fieldNames: string[]) (values: FSharpExpr[]) : FSharpExpr =
    ExprNewRecord(typeName, fieldNames, values)

// --- UnionCaseInfo accessors (backing uci.Name / uci.Tag on a NewUnionCase binding) ---

let unionCaseName (u: FSharpUnionCaseInfo) : string = u.Name
let unionCaseTag (u: FSharpUnionCaseInfo) : int = u.Tag

// --- MethodInfo accessors (backing mi.Name / mi.DeclaringType on a Call binding) ---

// mi.Name -> string (dedicated accessor; distinct from the generic reflection name<T>()).
let methodName (m: FSharpMethodInfo) : string = m.Name

// mi.DeclaringType -> System.Type. Rust has no faithful System.Type runtime, so the
// declaring type is represented by its fullname string boxed as obj (System.Type erases
// to Any on Rust). mi.DeclaringType.FullName then routes to Reflection.fullName, which
// unboxes the string.
let methodDeclaringType (m: FSharpMethodInfo) : obj = box m.DeclaringType
let mkNewList (head: FSharpExpr) (tail: FSharpExpr) : FSharpExpr = ExprNewList(head, tail)
let mkTupleGet (e: FSharpExpr) (index: int) : FSharpExpr = ExprTupleGet(e, index)
let mkUnionTag (e: FSharpExpr) : FSharpExpr = ExprUnionTag e
let mkUnionField (e: FSharpExpr) (fieldIndex: int) : FSharpExpr = ExprUnionField(e, fieldIndex)
let mkFieldGet (e: FSharpExpr) (fieldName: string) : FSharpExpr = ExprFieldGet(e, fieldName)
let mkFieldSet (e: FSharpExpr) (fieldName: string) (value: FSharpExpr) : FSharpExpr = ExprFieldSet(e, fieldName, value)
let mkVarSet (target: FSharpExpr) (value: FSharpExpr) : FSharpExpr = ExprVarSet(target, value)

// --- Type accessor ---

let getType (e: FSharpExpr) : string =
    match e with
    | ExprValue(_, t) -> t
    | ExprLambda(v, _) -> v.Type
    | _ -> "obj"

// --- Pattern helpers (return option, following the active-pattern convention) ---

let isValue (e: FSharpExpr) =
    match e with
    | ExprValue(v, t) -> Some(v, t)
    | _ -> None

let isVar (e: FSharpExpr) =
    match e with
    | ExprVarExpr v -> Some v
    | _ -> None

let isLambda (e: FSharpExpr) =
    match e with
    | ExprLambda(v, b) -> Some(v, b)
    | _ -> None

let isApplication (e: FSharpExpr) =
    match e with
    | ExprApplication(f, a) -> Some(f, a)
    | _ -> None

let isLet (e: FSharpExpr) =
    match e with
    | ExprLet(v, value, body) -> Some(v, value, body)
    | _ -> None

let isIfThenElse (e: FSharpExpr) =
    match e with
    | ExprIfThenElse(g, t, el) -> Some(g, t, el)
    | _ -> None

let isCall (e: FSharpExpr) =
    match e with
    | ExprCall(instance, m, args, dt) ->
        // A static/operator call carries the "novalue" node as its instance;
        // expose it as None so Patterns.Call matches F#. The tag is distinct from
        // "null", which is a genuine quoted null value.
        let inst =
            match instance with
            | ExprValue(_, "novalue") -> None
            | _ -> Some instance

        // Build the MethodInfo carrier from the stored compiled-name + declaring-type
        // fullname, so the Call binding exposes the real F# shape (mi.Name /
        // mi.DeclaringType.FullName). This is the SQLProvider linchpin: the declaring
        // type distinguishes List.map from Array.map.
        let mi: FSharpMethodInfo =
            {
                Name = m
                DeclaringType = dt
            }

        Some(inst, mi, List.ofArray args)
    | _ -> None

let isSequential (e: FSharpExpr) =
    match e with
    | ExprSequential(f, s) -> Some(f, s)
    | _ -> None

let isNewTuple (e: FSharpExpr) =
    match e with
    | ExprNewTuple els -> Some(List.ofArray els)
    | _ -> None

let isNewUnionCase (e: FSharpExpr) =
    match e with
    | ExprNewUnion(typeName, tag, caseName, fields) ->
        // Return F#'s (UnionCaseInfo * Expr list) shape. The UnionCaseInfo carrier
        // backs uci.Name/uci.Tag via unionCaseName/unionCaseTag.
        let uci: FSharpUnionCaseInfo =
            {
                Name = caseName
                Tag = tag
                DeclaringType = typeName
            }

        Some(uci, List.ofArray fields)
    | _ -> None

let isNewRecord (e: FSharpExpr) =
    match e with
    | ExprNewRecord(typeName, _names, values) ->
        // Return F#'s (Type * Expr list) shape. Rust has no faithful System.Type runtime,
        // so the type slot is erased to a boxed value (the record type name). The Rust
        // compiler erases the pattern's System.Type slot to Any to match (see Replacements).
        Some(box typeName, List.ofArray values)
    | _ -> None

let isTupleGet (e: FSharpExpr) =
    match e with
    | ExprTupleGet(inner, i) -> Some(inner, i)
    | _ -> None

let isFieldGet (e: FSharpExpr) =
    match e with
    | ExprFieldGet(inner, n) ->
        // Return F#'s (Expr option * PropertyInfo * Expr list) shape, as JS/TS/Python do.
        // A static property get carries the "novalue" node as its target (distinct from
        // "null", a genuine quoted null); expose it as None so Patterns.PropertyGet matches
        // F#. The third slot mirrors PropertyGet's indexer args, always empty here.
        let inst =
            match inner with
            | ExprValue(_, "novalue") -> None
            | _ -> Some inner

        let pi: FSharpPropertyInfo = { Name = n }
        Some(inst, pi, ([]: FSharpExpr list))
    | _ -> None

// --- Free variables ---

let getFreeVars (e: FSharpExpr) : FSharpVar list =
    let seen = System.Collections.Generic.HashSet<string>()
    let acc = ResizeArray<FSharpVar>()

    let rec walk (bound: Set<string>) (e: FSharpExpr) =
        match e with
        | ExprVarExpr v ->
            if not (bound.Contains v.Name) && seen.Add v.Name then
                acc.Add v
        | ExprLambda(v, body) -> walk (bound.Add v.Name) body
        | ExprLet(v, value, body) ->
            walk bound value
            walk (bound.Add v.Name) body
        | ExprApplication(f, a) ->
            walk bound f
            walk bound a
        | ExprIfThenElse(g, t, el) ->
            walk bound g
            walk bound t
            walk bound el
        | ExprCall(_, _, args, _) ->
            for a in args do
                walk bound a
        | ExprSequential(f, s) ->
            walk bound f
            walk bound s
        | ExprNewTuple els ->
            for el in els do
                walk bound el
        | ExprTupleGet(inner, _) -> walk bound inner
        | ExprFieldGet(inner, _) -> walk bound inner
        | _ -> ()

    walk Set.empty e
    List.ofSeq acc

// --- Evaluation (structural cases + common operators; SQL translation deconstructs
// rather than evaluates, so this covers the tested subset). ---

let private applyOperator (method: string) (args: obj list) : obj =
    // Extract args positionally rather than binding them in the match: pattern-binding
    // obj (Rc<dyn Any>) variables makes Fable zero-initialize a fat pointer, which is
    // invalid at runtime. Matching only on the method string avoids that.
    // if/elif (not match): Fable-Rust would emit string literals as match patterns,
    // which isn't valid Rust; an if-chain compiles to string == comparisons.
    let i (n: int) : int = unbox<int> (List.item n args)
    let b (n: int) : bool = unbox<bool> (List.item n args)

    if method = "op_Addition" then
        box (i 0 + i 1)
    elif method = "op_Subtraction" then
        box (i 0 - i 1)
    elif method = "op_Multiply" then
        box (i 0 * i 1)
    elif method = "op_Division" then
        box (i 0 / i 1)
    elif method = "op_Modulus" then
        box (i 0 % i 1)
    elif method = "op_UnaryNegation" then
        box (-(i 0))
    elif method = "op_Equality" then
        box (i 0 = i 1)
    elif method = "op_Inequality" then
        box (i 0 <> i 1)
    elif method = "op_LessThan" then
        box (i 0 < i 1)
    elif method = "op_LessThanOrEqual" then
        box (i 0 <= i 1)
    elif method = "op_GreaterThan" then
        box (i 0 > i 1)
    elif method = "op_GreaterThanOrEqual" then
        box (i 0 >= i 1)
    elif method = "op_BooleanAnd" then
        box (b 0 && b 1)
    elif method = "op_BooleanOr" then
        box (b 0 || b 1)
    elif method = "op_LogicalNot" then
        box (not (b 0))
    else
        failwithf "Cannot evaluate method: %s" method

let evaluate (e: FSharpExpr) : obj =
    let rec eval (env: Map<string, obj>) (e: FSharpExpr) : obj =
        match e with
        | ExprValue(v, _) -> v
        | ExprVarExpr v -> Map.find v.Name env
        | ExprLambda(v, body) -> box (fun (arg: obj) -> eval (Map.add v.Name arg env) body)
        | ExprApplication(f, a) ->
            let func = unbox<obj -> obj> (eval env f)
            func (eval env a)
        | ExprLet(v, value, body) -> eval (Map.add v.Name (eval env value) env) body
        | ExprIfThenElse(g, t, el) ->
            if unbox<bool> (eval env g) then
                eval env t
            else
                eval env el
        | ExprSequential(a, b) ->
            eval env a |> ignore
            eval env b
        | ExprCall(_, method, args, _) -> applyOperator method [ for a in args -> eval env a ]
        // A tuple literal evaluates to a boxed obj[] of its evaluated elements, mirroring
        // PHP (which produces a plain array). TupleGet then indexes into that array; without
        // this arm `eval inner` on a tuple literal would fall through to failwith and there
        // would be nothing indexable, so both arms are needed together.
        | ExprNewTuple els -> box [| for el in els -> eval env el |]
        | ExprTupleGet(inner, index) ->
            // The match-bound `index` is a borrowed &i32 on Rust while the array Index
            // operator wants an owned i32; `index + 0` forces an owned i32 rvalue (the
            // arithmetic dereferences the borrow) so the indexing type-checks.
            let i = index + 0
            (unbox<obj[]> (eval env inner)).[i]
        | _ -> failwith "Cannot evaluate expression"

    eval Map.empty e
