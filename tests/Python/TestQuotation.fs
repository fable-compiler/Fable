module Fable.Tests.Quotation

open Fable.Tests.Util
open Util.Testing
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

[<Fact>]
let ``test Simple integer value quotation`` () =
    let q = <@ 42 @>
    match q with
    | Value(v, _t) -> equal 42 (v :?> int)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``test Boolean value quotation`` () =
    let q = <@ true @>
    match q with
    | Value(v, _t) -> equal true (v :?> bool)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``test String value quotation`` () =
    let q = <@ "hello" @>
    match q with
    | Value(v, _t) -> equal "hello" (v :?> string)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``test Lambda quotation`` () =
    let q = <@ fun x -> x + 1 @>
    match q with
    | Lambda(v, _body) -> equal "x" v.Name
    | _ -> failwith "Expected Lambda"

[<Fact>]
let ``test Let binding quotation`` () =
    let q = <@ let x = 5 in x @>
    match q with
    | Let(v, Value(value, _), _) ->
        equal "x" v.Name
        equal 5 (value :?> int)
    | _ -> failwith "Expected Let"

[<Fact>]
let ``test IfThenElse quotation`` () =
    let q = <@ if true then 1 else 2 @>
    match q with
    | IfThenElse(_, _, _) -> ()
    | _ -> failwith "Expected IfThenElse"

[<Fact>]
let ``test Application quotation`` () =
    let q = <@ (fun x -> x) 42 @>
    match q with
    | Application(Lambda _, Value _) -> ()
    | _ -> failwith "Expected Application of Lambda"

[<Fact>]
let ``test Evaluate simple value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
    equal 42 (result :?> int)

[<Fact>]
let ``test Evaluate boolean value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ true @>
    equal true (result :?> bool)

[<Fact>]
let ``test Evaluate string value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ "hello" @>
    equal "hello" (result :?> string)

[<Fact>]
let ``test Evaluate addition`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 1 + 2 @>
    equal 3 (result :?> int)

[<Fact>]
let ``test Evaluate let binding`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 10 in x + 5 @>
    equal 15 (result :?> int)

[<Fact>]
let ``test Evaluate if then else true branch`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ if true then 1 else 2 @>
    equal 1 (result :?> int)

[<Fact>]
let ``test Evaluate if then else false branch`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ if false then 1 else 2 @>
    equal 2 (result :?> int)

[<Fact>]
let ``test Evaluate lambda application`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> x + 1) 5 @>
    equal 6 (result :?> int)

// --- Pattern matching: new node types ---

[<Fact>]
let ``test NewTuple quotation`` () =
    let q = <@ (1, 2, 3) @>
    match q with
    | NewTuple(exprs) -> equal 3 exprs.Length
    | _ -> failwith "Expected NewTuple"

[<Fact>]
let ``test Sequential quotation`` () =
    let q = <@ (); 42 @>
    match q with
    | Sequential(_, Value(v, _)) -> equal 42 (v :?> int)
    | _ -> failwith "Expected Sequential"

[<Fact>]
let ``test Var quotation`` () =
    let q = <@ fun x -> x @>
    match q with
    | Lambda(v, Var(v2)) ->
        equal v.Name v2.Name
    | _ -> failwith "Expected Lambda with Var body"

// --- Evaluation: more operators ---

[<Fact>]
let ``test Evaluate subtraction`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 10 - 3 @>
    equal 7 (result :?> int)

[<Fact>]
let ``test Evaluate multiplication`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 6 * 7 @>
    equal 42 (result :?> int)

[<Fact>]
let ``test Evaluate comparison`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 5 > 3 @>
    equal true (result :?> bool)

[<Fact>]
let ``test Evaluate nested let bindings`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 3 in let y = 4 in x * y @>
    equal 12 (result :?> int)

[<Fact>]
let ``test Evaluate nested lambda`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> (fun y -> x + y)) 3 4 @>
    equal 7 (result :?> int)

[<Fact>]
let ``test Evaluate tuple`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (1, 2) @>
    let t = result :?> (int * int)
    equal (1, 2) t

// --- FSharpExpr instance methods ---

[<Fact>]
let ``test Expr.GetFreeVars returns empty for closed expr`` () =
    let q = <@ fun x -> x + 1 @>
    let freeVars = q.GetFreeVars() |> Seq.length
    equal 0 freeVars
