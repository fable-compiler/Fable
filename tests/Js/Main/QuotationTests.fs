module Fable.Tests.Quotation

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Util.Testing

#if FABLE_COMPILER
open Microsoft.FSharp.Linq.RuntimeHelpers
open Fable.Core.JsInterop
#endif

type QuotationTestUnion =
    | QuotCircle of float
    | QuotSquare of float

let inline quotTestDouble x = x * 2

type QuotationIndexerTest(values: int[]) =
    member _.Item
        with get (i: int) = values.[i]

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

    // Note: these only check the outer IfThenElse shape. The guard itself is a synthetic Call
    // here (e.g. "get_IsNone", "op_TypeTest"), not the real UnionCaseTest/TypeTest node .NET uses.
    testCase "Option match deconstructs to IfThenElse" <| fun () ->
        let q = <@ fun (o: int option) -> match o with Some v -> v | None -> 0 @>
        match q with
        | Lambda(_, IfThenElse(_, _, _)) -> ()
        | _ -> failwith "Expected Lambda with IfThenElse body"

    testCase "Literal match deconstructs to IfThenElse" <| fun () ->
        let q = <@ fun (x: int) -> match x with 0 -> "zero" | _ -> "other" @>
        match q with
        | Lambda(_, IfThenElse(_, _, _)) -> ()
        | _ -> failwith "Expected Lambda with IfThenElse body"

    testCase "Union case match deconstructs to IfThenElse" <| fun () ->
        let q =
            <@ fun (s: QuotationTestUnion) ->
                match s with
                | QuotCircle _ -> true
                | QuotSquare _ -> false @>

        match q with
        | Lambda(_, IfThenElse(_, _, _)) -> ()
        | _ -> failwith "Expected Lambda with IfThenElse body"

    testCase "List match deconstructs to IfThenElse" <| fun () ->
        let q =
            <@ fun (xs: int list) ->
                match xs with
                | [] -> true
                | _ :: _ -> false @>

        match q with
        | Lambda(_, IfThenElse(_, _, _)) -> ()
        | _ -> failwith "Expected Lambda with IfThenElse body"

    testCase "Type test match deconstructs to IfThenElse" <| fun () ->
        let q =
            <@ fun (o: obj) ->
                match o with
                | :? int -> true
                | _ -> false @>

        match q with
        | Lambda(_, IfThenElse(_, _, _)) -> ()
        | _ -> failwith "Expected Lambda with IfThenElse body"

    // --- Member calls inside quotations keep original .NET metadata (not inlined) ---

    testCase "Inline function call inside quotation is preserved, not inlined" <| fun () ->
        let q = <@ quotTestDouble 5 @>

        match q with
        | Call(None, _mi, args) -> equal 1 (Seq.length args)
        | _ -> failwith "Expected a preserved Call node with a single argument"

    // --- Option/List property-getter accessors (ListHead/ListTail/OptionValue) deconstruct
    // as PropertyGet, matching real F# quotations ---

    testCase "Option.Value access inside a quotation is a PropertyGet" <| fun () ->
        let q = <@ fun (o: int option) -> o.Value @>

        match q with
        | Lambda(_, PropertyGet(Some _, _, [])) -> ()
        | _ -> failwith "Expected Lambda with PropertyGet body"

    testCase "List.Head access inside a quotation is a PropertyGet" <| fun () ->
        let q = <@ fun (xs: int list) -> xs.Head @>

        match q with
        | Lambda(_, PropertyGet(Some _, _, [])) -> ()
        | _ -> failwith "Expected Lambda with PropertyGet body"

    testCase "List.Tail access inside a quotation is a PropertyGet" <| fun () ->
        let q = <@ fun (xs: int list) -> xs.Tail @>

        match q with
        | Lambda(_, PropertyGet(Some _, _, [])) -> ()
        | _ -> failwith "Expected Lambda with PropertyGet body"

    // --- Runtime-built null nodes (mkNullExpr) ---

    testCase "Unit quotation still matches Value node" <| fun () ->
        let q = <@ () @>

        match q with
        | Value(_, _) -> ()
        | _ -> failwith "Expected Value"

    testCase "Call on an instance whose value is null keeps a Some instance" <| fun () ->
        // Guards against conflating "no instance" (static/operator call) with "instance value
        // is null" — both used to serialize to the same node.
        let q = <@ (Unchecked.defaultof<System.Nullable<int>>).HasValue @>

        let rec hasSomeInstancePropertyGet expr =
            match expr with
            | PropertyGet(Some _, _, []) -> true
            | Let(_, _, body) -> hasSomeInstancePropertyGet body
            | _ -> false

        if not (hasSomeInstancePropertyGet q) then
            failwith "Expected a PropertyGet with a Some instance, even though its value is null"

    // Known limitation: only zero-arg getters are tagged "property", so an indexer still
    // surfaces as a plain Call instead of PropertyGet(instance, propInfo, args) like real F#
    // quotations. FABLE_COMPILER-only: real .NET already represents indexers as PropertyGet.
    testCase "Indexer property access inside a quotation is a Call, not yet PropertyGet" <| fun () ->
        let q = <@ fun (t: QuotationIndexerTest) -> t.[0] @>

        match q with
        | Lambda(_, Call(Some _, _, args)) -> equal 1 (Seq.length args)
        | _ -> failwith "Expected Lambda with a Call(Some instance, _, [index]) body"

    testCase "JSON serialization produces Thoth Auto-compatible format" <| fun () ->
        let q = <@ 42 @>
        let json = Fable.Core.JS.JSON.stringify(q)
        // Thoth.Json Auto format: ["Value",42,"int32"]
        json.Contains("Value") |> equal true
        json.Contains("42") |> equal true

    testCase "JSON round-trip preserves structure" <| fun () ->
        let q = <@ 1 + 2 @>
        let json = Fable.Core.JS.JSON.stringify(q)
        let parsed = Fable.Core.JS.JSON.parse(json) :?> obj[]
        // Thoth Auto format: ["Call", ...]
        parsed[0] |> unbox<string> |> equal "Call"
#endif
  ]
