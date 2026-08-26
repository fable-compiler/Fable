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

let private joinStrings (separator: string) (parts: string list) =
    match parts with
    | [] -> ""
    | first :: rest -> List.fold (fun result part -> result + separator + part) first rest

let private operatorSymbol (methodName: string) =
    match methodName with
    | "op_Addition" -> Some "+"
    | "op_Subtraction" -> Some "-"
    | "op_Multiply" -> Some "*"
    | "op_Division" -> Some "/"
    | "op_Modulus" -> Some "%"
    | "op_Exponentiation" -> Some "**"
    | "op_Equality" -> Some "="
    | "op_Inequality" -> Some "<>"
    | "op_LessThan" -> Some "<"
    | "op_LessThanOrEqual" -> Some "<="
    | "op_GreaterThan" -> Some ">"
    | "op_GreaterThanOrEqual" -> Some ">="
    | "op_BooleanAnd" -> Some "&&"
    | "op_BooleanOr" -> Some "||"
    | "op_UnaryNegation" -> Some "-"
    | "op_LogicalNot" -> Some "not"
    | _ -> None

let private valueToString (value: obj) (typ: string) =
    match typ with
    | "string" -> "\"" + unbox<string> value + "\""
    | "unit" -> "()"
    | "bool" ->
        if unbox<bool> value then
            "true"
        else
            "false"
    | "char" -> sprintf "%O" (unbox<char> value)
    | "int8" -> sprintf "%O" (unbox<int8> value)
    | "uint8" -> sprintf "%O" (unbox<uint8> value)
    | "int16" -> sprintf "%O" (unbox<int16> value)
    | "uint16" -> sprintf "%O" (unbox<uint16> value)
    | "int32" -> sprintf "%O" (unbox<int32> value)
    | "uint32" -> sprintf "%O" (unbox<uint32> value)
    | "int64" -> sprintf "%O" (unbox<int64> value)
    | "uint64" -> sprintf "%O" (unbox<uint64> value)
    | "nativeint" -> sprintf "%O" (unbox<nativeint> value)
    | "unativeint" -> sprintf "%O" (unbox<unativeint> value)
    | "float32" -> sprintf "%O" (unbox<float32> value)
    | "float64" -> sprintf "%O" (unbox<float> value)
    | _ -> "<value>"

let rec exprToString (e: FSharpExpr) : string =
    match e with
    | ExprValue(value, typ) -> valueToString value typ
    | ExprVarExpr v -> v.Name
    | ExprLambda(v, body) -> "fun " + v.Name + " -> " + exprToString body
    | ExprApplication(func, arg) -> exprToString func + " " + exprToString arg
    | ExprLet(v, value, body) -> "let " + v.Name + " = " + exprToString value + " in " + exprToString body
    | ExprIfThenElse(guard, thenExpr, elseExpr) ->
        "if "
        + exprToString guard
        + " then "
        + exprToString thenExpr
        + " else "
        + exprToString elseExpr
    | ExprCall(_, methodName, args, _) ->
        let renderedArgs = args |> List.ofArray |> List.map exprToString

        match operatorSymbol methodName, renderedArgs with
        | Some symbol, [ left; right ] -> "(" + left + " " + symbol + " " + right + ")"
        | Some symbol, [ operand ] -> symbol + operand
        | _ -> methodName + "(" + joinStrings ", " renderedArgs + ")"
    | ExprSequential(first, second) -> exprToString first + "; " + exprToString second
    | ExprNewTuple elements ->
        "("
        + (elements |> List.ofArray |> List.map exprToString |> joinStrings ", ")
        + ")"
    | ExprTupleGet(inner, index) -> "Item" + string (index + 1) + "(" + exprToString inner + ")"
    | ExprFieldGet(inner, fieldName) -> exprToString inner + "." + fieldName
    | _ -> "<expr>"

let substitute (e: FSharpExpr) (substitution: FSharpVar -> FSharpExpr option) : FSharpExpr =
    let rec substituteExpr expression =
        match expression with
        | ExprValue _ -> expression
        | ExprVarExpr variable ->
            match substitution variable with
            | Some replacement -> replacement
            | None -> expression
        | ExprLambda(variable, body) -> ExprLambda(variable, substituteExpr body)
        | ExprApplication(func, arg) -> ExprApplication(substituteExpr func, substituteExpr arg)
        | ExprLet(variable, value, body) -> ExprLet(variable, substituteExpr value, substituteExpr body)
        | ExprIfThenElse(guard, thenExpr, elseExpr) ->
            ExprIfThenElse(substituteExpr guard, substituteExpr thenExpr, substituteExpr elseExpr)
        | ExprCall(instance, methodName, args, declaringType) ->
            let substitutedArgs = ResizeArray<FSharpExpr>()

            for argument in args do
                substitutedArgs.Add(substituteExpr argument)

            ExprCall(substituteExpr instance, methodName, substitutedArgs.ToArray(), declaringType)
        | ExprSequential(first, second) -> ExprSequential(substituteExpr first, substituteExpr second)
        | ExprNewTuple elements ->
            let substitutedElements = ResizeArray<FSharpExpr>()

            for element in elements do
                substitutedElements.Add(substituteExpr element)

            ExprNewTuple(substitutedElements.ToArray())
        | ExprNewUnion(typeName, tag, caseName, fields) ->
            let substitutedFields = ResizeArray<FSharpExpr>()

            for field in fields do
                substitutedFields.Add(substituteExpr field)

            ExprNewUnion(typeName, tag, caseName, substitutedFields.ToArray())
        | ExprNewRecord(typeName, fieldNames, values) ->
            let substitutedValues = ResizeArray<FSharpExpr>()

            for value in values do
                substitutedValues.Add(substituteExpr value)

            ExprNewRecord(typeName, fieldNames, substitutedValues.ToArray())
        | ExprNewList(head, tail) -> ExprNewList(substituteExpr head, substituteExpr tail)
        | ExprTupleGet(inner, index) -> ExprTupleGet(substituteExpr inner, index)
        | ExprUnionTag inner -> ExprUnionTag(substituteExpr inner)
        | ExprUnionField(inner, fieldIndex) -> ExprUnionField(substituteExpr inner, fieldIndex)
        | ExprFieldGet(inner, fieldName) -> ExprFieldGet(substituteExpr inner, fieldName)
        | ExprFieldSet(inner, fieldName, value) -> ExprFieldSet(substituteExpr inner, fieldName, substituteExpr value)
        | ExprVarSet(target, value) -> ExprVarSet(substituteExpr target, substituteExpr value)

    substituteExpr e

// --- Free variables ---

let getFreeVars (e: FSharpExpr) : FSharpVar seq =
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
        | ExprCall(instance, _, args, _) ->
            walk bound instance

            for a in args do
                walk bound a
        | ExprSequential(f, s) ->
            walk bound f
            walk bound s
        | ExprNewTuple els ->
            for el in els do
                walk bound el
        | ExprNewUnion(_, _, _, fields) ->
            for field in fields do
                walk bound field
        | ExprNewRecord(_, _, values) ->
            for value in values do
                walk bound value
        | ExprNewList(head, tail) ->
            walk bound head
            walk bound tail
        | ExprTupleGet(inner, _) -> walk bound inner
        | ExprUnionTag inner -> walk bound inner
        | ExprUnionField(inner, _) -> walk bound inner
        | ExprFieldGet(inner, _) -> walk bound inner
        | ExprFieldSet(inner, _, value) ->
            walk bound inner
            walk bound value
        | ExprVarSet(target, value) ->
            walk bound target
            walk bound value
        | _ -> ()

    walk Set.empty e
    acc.ToArray() |> Seq.ofArray

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
    let rec eval (env: Map<string, obj> ref) (e: FSharpExpr) : obj =
        let evalArrayField (values: obj[]) (index: int) =
            if index < 0 || index >= values.Length then
                failwith "Quotation field index is out of range"

            values.[index]

        match e with
        | ExprValue(v, typ) ->
            match typ with
            | "unit" -> v
            | "list" -> box ([||]: obj[])
            | _ -> v
        | ExprVarExpr v -> Map.find v.Name env.Value
        | ExprLambda(v, body) ->
            let capturedEnv = env.Value
            box (fun (arg: obj) -> eval (ref (Map.add v.Name arg capturedEnv)) body)
        | ExprApplication(f, a) ->
            let func = unbox<obj -> obj> (eval env f)
            func (eval env a)
        | ExprLet(v, value, body) ->
            let value = eval env value
            eval (ref (Map.add v.Name value env.Value)) body
        | ExprIfThenElse(g, t, el) ->
            if unbox<bool> (eval env g) then
                eval env t
            else
                eval env el
        | ExprSequential(a, b) ->
            eval env a |> ignore
            eval env b
        | ExprCall(instance, method, args, _) ->
            let evaluatedArgs = [ for argument in args -> eval env argument ]

            match method with
            | "get_IsSome"
            | "get_IsNone" ->
                let values = unbox<obj[]> (eval env instance)
                let isSome = values.Length > 0 && unbox<int> values.[0] = 1

                box (
                    if method = "get_IsSome" then
                        isSome
                    else
                        not isSome
                )
            | "get_IsCons"
            | "get_IsEmpty" ->
                let values = unbox<obj[]> (eval env instance)
                let isCons = values.Length > 0

                box (
                    if method = "get_IsCons" then
                        isCons
                    else
                        not isCons
                )
            | _ -> applyOperator method evaluatedArgs
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
            evalArrayField (unbox<obj[]> (eval env inner)) i
        | ExprNewUnion(_, tag, _, fields) ->
            let values = ResizeArray<obj>()
            values.Add(box tag)

            for field in fields do
                values.Add(eval env field)

            box (values.ToArray())
        | ExprNewRecord(_, fieldNames, values) ->
            let result = ResizeArray<obj>()

            for i = 0 to fieldNames.Length - 1 do
                result.Add(box fieldNames.[i])
                result.Add(eval env values.[i])

            box (result.ToArray())
        | ExprNewList(head, tail) ->
            let result = ResizeArray<obj>()
            result.Add(eval env head)

            for value in unbox<obj[]> (eval env tail) do
                result.Add(value)

            box (result.ToArray())
        | ExprUnionTag inner ->
            let values = unbox<obj[]> (eval env inner)
            evalArrayField values 0
        | ExprUnionField(inner, fieldIndex) ->
            let values = unbox<obj[]> (eval env inner)
            evalArrayField values (fieldIndex + 1)
        | ExprFieldGet(inner, fieldName) ->
            let values = unbox<obj[]> (eval env inner)

            if fieldName = "Head" then
                evalArrayField values 0
            elif fieldName = "Tail" then
                box values.[1..]
            elif fieldName = "Value" && values.Length > 1 && unbox<int> values.[0] = 1 then
                values.[1]
            else
                let mutable result = None
                let mutable index = 0

                while index + 1 < values.Length && result.IsNone do
                    if unbox<string> values.[index] = fieldName then
                        result <- Some values.[index + 1]

                    index <- index + 2

                match result with
                | Some value -> value
                | None -> failwithf "Quotation field not found: %s" fieldName
        | ExprFieldSet(target, fieldName, value) ->
            let target = unbox<obj[]> (eval env target)
            let value = eval env value

            let rec findFieldIndex index =
                if index + 1 >= target.Length then
                    None
                elif unbox<string> target.[index] = fieldName then
                    Some index
                else
                    findFieldIndex (index + 2)

            match findFieldIndex 0 with
            | Some index -> target.[index + 1] <- value
            | None -> failwithf "Quotation field not found: %s" fieldName

            box 0
        | ExprVarSet(target, value) ->
            let value = eval env value

            match target with
            | ExprVarExpr variable -> env.Value <- Map.add variable.Name value env.Value
            | _ -> failwith "Quotation VarSet target must be a variable"

            box 0

    eval (ref Map.empty) e
