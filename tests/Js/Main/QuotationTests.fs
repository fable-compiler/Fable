module Fable.Tests.Quotation

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Util.Testing

#if FABLE_COMPILER
open Microsoft.FSharp.Linq.RuntimeHelpers
open Fable.Core.JsInterop
#endif

let tests =
  testList "Quotations" [
#if FABLE_COMPILER
    testCase "Simple integer value quotation" <| fun () ->
        let q = <@ 42 @>
        match q with
        | Value(v, _t) -> equal 42 (v :?> int)
        | _ -> failwith "Expected Value"

    testCase "Boolean value quotation" <| fun () ->
        let q = <@ true @>
        match q with
        | Value(v, _t) -> equal true (v :?> bool)
        | _ -> failwith "Expected Value"

    testCase "String value quotation" <| fun () ->
        let q = <@ "hello" @>
        match q with
        | Value(v, _t) -> equal "hello" (v :?> string)
        | _ -> failwith "Expected Value"

    testCase "Lambda quotation" <| fun () ->
        let q = <@ fun x -> x + 1 @>
        match q with
        | Lambda(v, _body) -> equal "x" v.Name
        | _ -> failwith "Expected Lambda"

    testCase "Let binding quotation" <| fun () ->
        let q = <@ let x = 5 in x @>
        match q with
        | Let(v, Value(value, _), _) ->
            v.Name.StartsWith("x") |> equal true
            equal 5 (value :?> int)
        | _ -> failwith "Expected Let"

    testCase "IfThenElse quotation" <| fun () ->
        let q = <@ if true then 1 else 2 @>
        match q with
        | IfThenElse(_, _, _) -> ()
        | _ -> failwith "Expected IfThenElse"

    testCase "Application quotation" <| fun () ->
        let q = <@ (fun x -> x) 42 @>
        match q with
        | Application(Lambda _, Value _) -> ()
        | _ -> failwith "Expected Application of Lambda"

    testCase "NewTuple quotation" <| fun () ->
        let q = <@ (1, 2, 3) @>
        match q with
        | NewTuple(exprs) -> equal 3 (Seq.length exprs)
        | _ -> failwith "Expected NewTuple"

    testCase "Sequential quotation" <| fun () ->
        let q = <@ (); 42 @>
        match q with
        | Sequential(_, Value(v, _)) -> equal 42 (v :?> int)
        | _ -> failwith "Expected Sequential"

    testCase "Var quotation" <| fun () ->
        let q = <@ fun x -> x @>
        match q with
        | Lambda(v, Var(v2)) ->
            equal v.Name v2.Name
        | _ -> failwith "Expected Lambda with Var body"

    testCase "Untyped quotation works" <| fun () ->
        let q = <@@ 42 @@>
        match q with
        | Value(v, _t) -> equal 42 (v :?> int)
        | _ -> failwith "Expected Value"

    testCase "Evaluate simple value" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
        equal 42 (result :?> int)

    testCase "Evaluate addition" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 1 + 2 @>
        equal 3 (result :?> int)

    testCase "Evaluate let binding" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 10 in x + 5 @>
        equal 15 (result :?> int)

    testCase "Evaluate if then else true branch" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ if true then 1 else 2 @>
        equal 1 (result :?> int)

    testCase "Evaluate if then else false branch" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ if false then 1 else 2 @>
        equal 2 (result :?> int)

    testCase "Evaluate lambda application" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> x + 1) 5 @>
        equal 6 (result :?> int)

    testCase "Evaluate subtraction" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 10 - 3 @>
        equal 7 (result :?> int)

    testCase "Evaluate multiplication" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 6 * 7 @>
        equal 42 (result :?> int)

    testCase "Evaluate comparison" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 5 > 3 @>
        equal true (result :?> bool)

    testCase "Evaluate nested let bindings" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 3 in let y = 4 in x * y @>
        equal 12 (result :?> int)

    testCase "Evaluate nested lambda" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> (fun y -> x + y)) 3 4 @>
        equal 7 (result :?> int)

    testCase "Evaluate tuple" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ (1, 2) @>
        let t = result :?> (int * int)
        equal (1, 2) t

    testCase "Expr.GetFreeVars returns empty for closed expr" <| fun () ->
        let q = <@ fun x -> x + 1 @>
        let freeVars = q.GetFreeVars() |> Seq.length
        equal 0 freeVars

    testCase "JSON serialization produces Thoth-compatible format" <| fun () ->
        let q = <@ 42 @>
        let json = Fable.Core.JS.JSON.stringify(q)
        // Thoth.Json format: {"Case":"Value","Fields":[42,"int32"]}
        json.Contains("Case") |> equal true
        json.Contains("Value") |> equal true
        json.Contains("Fields") |> equal true
        json.Contains("42") |> equal true

    testCase "JSON round-trip preserves structure" <| fun () ->
        let q = <@ 1 + 2 @>
        let json = Fable.Core.JS.JSON.stringify(q)
        let parsed = Fable.Core.JS.JSON.parse(json)
        // Thoth format: {"Case":"Call","Fields":[...]}
        parsed?Case |> unbox<string> |> equal "Call"
#endif
  ]
