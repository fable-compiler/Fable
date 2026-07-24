module Fable.Tests.QuotationTests

open Util.Testing
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

// NOTE: quotations are deconstructed through an untyped `Expr` parameter (helpers below
// take `(e: Expr)`); matching a typed `Expr<'T>` binding directly is not yet supported on
// Rust (see quotation notes). Binding the boxed `obj` payload out of a Value node now works
// (the getZeroObj fix); the type element is bound as a wildcard since the runtime models it
// as a string rather than System.Type. Array-valued nodes now deconstruct: NewTuple exposes
// its elements, NewUnionCase binds a real UnionCaseInfo (uci.Name / uci.Tag) plus its args, and
// NewRecord binds its (erased) type slot plus its args. The NewRecord type slot and
// UnionCaseInfo.GetFields() are not modelled (System.Type / PropertyInfo have no Rust runtime).

let private lambdaVarName (e: Expr) =
    match e with
    | Lambda(v, _body) -> v.Name
    | _ -> "?"

let private letVarName (e: Expr) =
    match e with
    | Let(v, _value, _body) -> v.Name
    | _ -> "?"

let private isIfThenElseNode (e: Expr) =
    match e with
    | IfThenElse(_g, _t, _e) -> true
    | _ -> false

[<Fact>]
let ``Evaluate a value`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
    unbox<int> r |> equal 42

// Typed Expr<'T> matched directly (not routed through an (e: Expr) helper): the type
// argument is erased so the quotation shares the untyped FSharpExpr representation.

[<Fact>]
let ``Typed quotation matches Value directly`` () =
    match <@ 42 @> with
    | Value(v, _) -> unbox<int> v |> equal 42
    | _ -> failwith "Expected Value"

[<Fact>]
let ``Typed quotation matches Lambda directly`` () =
    match <@ fun (x: int) -> x + 1 @> with
    | Lambda(v, _) -> v.Name |> equal "x"
    | _ -> failwith "Expected Lambda"

[<Fact>]
let ``Lambda quotation exposes the parameter name`` () =
    lambdaVarName <@ fun (x: int) -> x + 1 @> |> equal "x"

[<Fact>]
let ``Let quotation exposes the bound name`` () =
    letVarName <@ let y = 5 in y + 1 @> |> equal "y"

[<Fact>]
let ``IfThenElse quotation deconstructs`` () =
    isIfThenElseNode <@ if true then 1 else 2 @> |> equal true

[<Fact>]
let ``Evaluate addition`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ 2 + 3 @>
    unbox<int> r |> equal 5

[<Fact>]
let ``Evaluate subtraction`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ 10 - 4 @>
    unbox<int> r |> equal 6

[<Fact>]
let ``Evaluate comparison`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ 5 > 3 @>
    unbox<bool> r |> equal true

[<Fact>]
let ``Evaluate let binding`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ let x = 10 in x + 5 @>
    unbox<int> r |> equal 15

[<Fact>]
let ``Evaluate lambda application`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ (fun a -> a * 2) 21 @>
    unbox<int> r |> equal 42

[<Fact>]
let ``Evaluate let-bound lambda`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ let f = (fun x -> x + 1) in f 41 @>
    unbox<int> r |> equal 42

// --- Parity expansion (toward the Python 27) ---
// Binding the boxed obj payload out of a Value node previously tripped getZero on Rust;
// the getZeroObj / active-pattern fixes should now allow these. Type element bound as
// wildcard (`Value(v, _)`) to sidestep the System.Type-vs-string model difference.

let private valueInt (e: Expr) =
    match e with
    | Value(v, _) -> unbox<int> v
    | _ -> -1

let private valueBool (e: Expr) =
    match e with
    | Value(v, _) -> unbox<bool> v
    | _ -> false

let private valueStr (e: Expr) =
    match e with
    | Value(v, _) -> unbox<string> v
    | _ -> "?"

let private letBoundInt (e: Expr) =
    match e with
    | Let(_, Value(v, _), _) -> unbox<int> v
    | _ -> -1

let private seqSecondInt (e: Expr) =
    match e with
    | Sequential(_, Value(v, _)) -> unbox<int> v
    | _ -> -1

let private newTupleLen (e: Expr) =
    match e with
    | NewTuple(exprs) -> List.length exprs
    | _ -> -1

let private isAppLambdaValue (e: Expr) =
    match e with
    | Application(Lambda _, Value _) -> true
    | _ -> false

let private lambdaVarBodyName (e: Expr) =
    match e with
    | Lambda(v, Var v2) -> v.Name + "/" + v2.Name
    | _ -> "?"

let private isLambdaIfThenElse (e: Expr) =
    match e with
    | Lambda(_, IfThenElse(_, _, _)) -> true
    | _ -> false

[<Fact>]
let ``Value node binds int payload`` () =
    valueInt <@ 42 @> |> equal 42

[<Fact>]
let ``Value node binds bool payload`` () =
    valueBool <@ true @> |> equal true

[<Fact>]
let ``Value node binds string payload`` () =
    valueStr <@ "hello" @> |> equal "hello"

[<Fact>]
let ``Let binds value node payload`` () =
    letBoundInt <@ let x = 5 in x @> |> equal 5

[<Fact>]
let ``Sequential exposes second value payload`` () =
    seqSecondInt <@ (); 42 @> |> equal 42

[<Fact>]
let ``NewTuple exposes element count`` () =
    newTupleLen <@ (1, 2, 3) @> |> equal 3

[<Fact>]
let ``Application of lambda to value deconstructs`` () =
    isAppLambdaValue <@ (fun x -> x) 42 @> |> equal true

[<Fact>]
let ``Lambda body Var refers to the parameter`` () =
    lambdaVarBodyName <@ fun x -> x @> |> equal "x/x"

[<Fact>]
let ``Option match in quotation lowers to IfThenElse`` () =
    isLambdaIfThenElse <@ fun (o: int option) -> match o with Some v -> v | None -> 0 @> |> equal true

[<Fact>]
let ``Literal match in quotation lowers to IfThenElse`` () =
    isLambdaIfThenElse <@ fun (x: int) -> match x with 0 -> "zero" | _ -> "other" @> |> equal true

[<Fact>]
let ``Evaluate multiplication`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ 6 * 7 @>
    unbox<int> r |> equal 42

[<Fact>]
let ``Evaluate nested let bindings`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ let x = 3 in let y = 4 in x * y @>
    unbox<int> r |> equal 12

[<Fact>]
let ``Evaluate nested lambda`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> (fun y -> x + y)) 3 4 @>
    unbox<int> r |> equal 7

// --- NewUnionCase / NewRecord deconstruction ---

type private MyRec = { X: int; Y: int }

let private newUnionCaseName (e: Expr) =
    match e with
    | NewUnionCase(uci, _args) -> uci.Name
    | _ -> "?"

let private newUnionCaseArgCount (e: Expr) =
    match e with
    | NewUnionCase(_uci, args) -> List.length args
    | _ -> -1

let private newUnionCaseTag (e: Expr) =
    match e with
    | NewUnionCase(uci, _args) -> uci.Tag
    | _ -> -1

let private newRecordArgCount (e: Expr) =
    match e with
    | NewRecord(_t, args) -> List.length args
    | _ -> -1

[<Fact>]
let ``NewUnionCase exposes the case name`` () =
    newUnionCaseName <@ Some 42 @> |> equal "Some"

[<Fact>]
let ``NewUnionCase exposes the argument count`` () =
    newUnionCaseArgCount <@ Some 42 @> |> equal 1

[<Fact>]
let ``NewUnionCase exposes the tag`` () =
    newUnionCaseTag <@ Some 42 @> |> equal 1

[<Fact>]
let ``NewRecord exposes the argument count`` () =
    newRecordArgCount <@ ({ X = 1; Y = 2 }: MyRec) @> |> equal 2

// --- TupleGet evaluation (B3) ---
// A tuple-deconstructing let lowers to a NewTuple bound to a var, then TupleGet nodes
// pulling out each element. Evaluating it exercises both the ExprNewTuple arm (builds a
// boxed obj[]) and the ExprTupleGet arm (indexes into it). Before the fix, TupleGet
// returned the whole tuple, so `a`/`b` were the obj[] and `a + b` would not evaluate.

[<Fact>]
let ``Evaluate tuple-get picks the element at the index`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ let (a, b) = (10, 20) in a + b @>
    unbox<int> r |> equal 30

[<Fact>]
let ``Evaluate tuple-get picks the second element`` () =
    let r = LeafExpressionConverter.EvaluateQuotation <@ let (a, b, c) = (1, 2, 3) in b @>
    unbox<int> r |> equal 2

// --- Call deconstruction with MethodInfo (B1) ---
// Deconstructing a Call now binds a real MethodInfo: mi.Name is the compiled method name and
// mi.DeclaringType.FullName is the declaring module fullname. The declaring type is the
// SQLProvider linchpin: it distinguishes List.map from Array.map. MethodInfo erases to the
// Rust-native FSharpMethodInfo carrier; DeclaringType is the fullname string boxed as
// System.Type (System.Type erases to Any), read back via Reflection.fullName.

let private callMethodName (e: Expr) =
    match e with
    | Call(_, mi, _) -> mi.Name
    | _ -> "?"

let private callDeclaringTypeName (e: Expr) =
    match e with
    | Call(_, mi, _) -> mi.DeclaringType.FullName
    | _ -> "?"

[<Fact>]
let ``Call exposes the method name`` () =
    callMethodName <@ List.map (fun x -> x + 1) [ 1 ] @> |> equal "Map"

[<Fact>]
let ``Call exposes the declaring type full name`` () =
    callDeclaringTypeName <@ List.map (fun x -> x + 1) [ 1 ] @>
    |> equal "Microsoft.FSharp.Collections.ListModule"

[<Fact>]
let ``Call distinguishes List.map from Array.map by declaring type`` () =
    callDeclaringTypeName <@ Array.map (fun x -> x + 1) [| 1 |] @>
    |> equal "Microsoft.FSharp.Collections.ArrayModule"

[<Fact>]
let ``Call binds MethodInfo directly`` () =
    match <@ List.map (fun x -> x + 1) [ 1 ] @> with
    | Call(_, mi, _) ->
        mi.Name |> equal "Map"
        mi.DeclaringType.FullName |> equal "Microsoft.FSharp.Collections.ListModule"
    | _ -> failwith "Expected Call"

// --- PropertyGet (isFieldGet) ---
// A property getter deconstructs as PropertyGet(instance, propInfo, indexerArgs), matching
// real F# quotations and the JS/TS/Python runtimes. System.Reflection.PropertyInfo erases to
// the shared FSharpPropertyInfo carrier, so pi.Name works here exactly as it does for the
// PropertyInfo values returned by FSharpType.GetRecordFields.

type QuotPropHolder =
    static member Version = 42

[<Fact>]
let ``Option.Value access inside a quotation is a PropertyGet`` () =
    let q = <@ fun (o: int option) -> o.Value @>

    match q with
    | Lambda(_, PropertyGet(Some _, _, [])) -> ()
    | _ -> failwith "Expected Lambda with PropertyGet body"

[<Fact>]
let ``List.Head access inside a quotation is a PropertyGet`` () =
    let q = <@ fun (xs: int list) -> xs.Head @>

    match q with
    | Lambda(_, PropertyGet(Some _, _, [])) -> ()
    | _ -> failwith "Expected Lambda with PropertyGet body"

// A static property carries the "novalue" no-instance sentinel as its target, which must
// surface as None rather than Some sentinelNode.
[<Fact>]
let ``Static property access inside a quotation is a PropertyGet with no instance`` () =
    let q = <@ QuotPropHolder.Version @>

    match q with
    | PropertyGet(None, _, []) -> ()
    | _ -> failwith "Expected PropertyGet with no instance"

[<Fact>]
let ``PropertyGet exposes the property name`` () =
    let q = <@ fun (o: int option) -> o.Value @>

    match q with
    | Lambda(_, PropertyGet(_, pi, _)) -> pi.Name |> equal "Value"
    | _ -> failwith "Expected Lambda with PropertyGet body"

// --- Call instance slot (isCall) ---
// A static call carries the "novalue" no-instance sentinel as its instance, which must
// surface as None rather than Some sentinelNode — the isCall counterpart of the static
// PropertyGet test above. The instance case proves the sentinel is not over-applied.

[<Fact>]
let ``Static call inside a quotation has no instance`` () =
    let q = <@ List.length [ 1; 2; 3 ] @>

    match q with
    | Call(None, _mi, args) -> equal 1 (List.length args)
    | _ -> failwith "Expected a static Call with no instance"

[<Fact>]
let ``Instance call inside a quotation has an instance`` () =
    let q = <@ fun (s: string) -> s.Trim() @>

    match q with
    | Lambda(_, Call(Some _, _mi, _)) -> ()
    | _ -> failwith "Expected Lambda with an instance Call body"
