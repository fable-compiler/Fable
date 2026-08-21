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

// --- Typed Expr.Call builder (B2) ---
// The B2 arity fix (quotationExprs now passes a 4th declaringType arg to mkCall) is verified
// by inspecting the generated Rust: Expr.Call(instance, mi, args) emits the full 4-arg
// `mkCall(instance, mi, args, string(""))` instead of a 3-arg partial application. An
// end-to-end Rust test cannot compile, though: deconstructing a Call binds `mi: MethodInfo`,
// and MethodInfo has no Rust runtime type (Fable emits an unresolved
// `System::Reflection::MethodInfo` import). Constructing/round-tripping a MethodInfo is the
// separate MethodInfo/declaringType task, so no runnable Rust test is added here.
