module Fable.Tests.Dart.Quotation

open Util
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

type C =
    | A of int
    | B

type Cfg =
    static member Version = 42

type MutableRec = { mutable Value: int }

let tests() =
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
        // NOTE: uses a unique binder name (`letv`) instead of the canonical `x`.
        // All Dart tests share one `tests()` function, so reusing `x` across test
        // bodies makes Fable rename later occurrences (e.g. `x` -> `x_1`), which the
        // quotation faithfully reports. A unique name avoids the collision rename.
        let q = <@ let letv = 5 in letv @>
        match q with
        | Let(v, Value(value, _), _) ->
            equal "letv" v.Name
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

    testCase "Evaluate simple value" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
        equal 42 (result :?> int)

    testCase "Evaluate boolean value" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ true @>
        equal true (result :?> bool)

    testCase "Evaluate string value" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ "hello" @>
        equal "hello" (result :?> string)

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

    testCase "NewTuple quotation" <| fun () ->
        let q = <@ (1, 2, 3) @>
        match q with
        | NewTuple(exprs) -> equal 3 exprs.Length
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
        // NOTE: the canonical suite casts the result to `(int * int)`, but Dart
        // reifies generic type arguments, so `obj :?> (int * int)` fails for a
        // tuple built from dynamic values. The runtime evaluates a tuple to an
        // obj[] (mirroring the TS/JS runtime), so we inspect the elements.
        let result = LeafExpressionConverter.EvaluateQuotation <@ (1, 2) @>
        let arr = result :?> obj[]
        equal 1 (arr.[0] :?> int)
        equal 2 (arr.[1] :?> int)

    testCase "Expr.GetFreeVars returns empty for closed expr" <| fun () ->
        let q = <@ fun x -> x + 1 @>
        let freeVars = q.GetFreeVars() |> Seq.length
        equal 0 freeVars

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

    testCase "NewUnionCase quotation (single-field DU) exposes fields" <| fun () ->
        let q = <@ A 7 @>
        match q with
        | NewUnionCase(_uci, args) -> equal 1 (List.length args)
        | _ -> failwith "Expected NewUnionCase"

    testCase "NewUnionCase quotation (no-field DU) exposes empty fields" <| fun () ->
        let q = <@ B @>
        match q with
        | NewUnionCase(_uci, args) -> equal 0 (List.length args)
        | _ -> failwith "Expected NewUnionCase"

    // --- Option/List property-getter accessors (ListHead/ListTail/OptionValue) deconstruct
    // as PropertyGet, matching real F# quotations ---

    testCase "Option.Value access inside a quotation is a PropertyGet" <| fun () ->
        let q = <@ fun (o: int option) -> o.Value @>
        match q with
        | Lambda(_, PropertyGet(Some _, _, [])) -> ()
        | _ -> failwith "Expected Lambda with PropertyGet body"

    testCase "Static property access inside a quotation is a PropertyGet with no instance" <| fun () ->
        let q = <@ Cfg.Version @>
        match q with
        | PropertyGet(None, _, []) -> ()
        | _ -> failwith "Expected PropertyGet with no instance"

    testCase "Evaluate exponentiation" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 2.0 ** 3.0 @>
        equal 8.0 (result :?> float)

    testCase "Evaluate exponentiation with negative exponent" <| fun () ->
        // Regression test: `_pow` used to loop `b` times, wrongly returning `1.0` here.
        let result = LeafExpressionConverter.EvaluateQuotation <@ 2.0 ** -1.0 @>
        equal 0.5 (result :?> float)

    testCase "Evaluate exponentiation with fractional exponent" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ 4.0 ** 0.5 @>
        equal 2.0 (result :?> float)

    testCase "NewRecord quotation exposes field values" <| fun () ->
        let q = <@ { Value = 5 } : MutableRec @>
        match q with
        | NewRecord(_ty, values) -> equal 1 (List.length values)
        | _ -> failwith "Expected NewRecord"

    testCase "Evaluate mutable variable set (VarSet)" <| fun () ->
        let result = LeafExpressionConverter.EvaluateQuotation <@ let mutable v = 1 in (v <- 2); v @>
        equal 2 (result :?> int)

    testCase "Evaluate mutable record field set (FieldSet)" <| fun () ->
        let result =
            LeafExpressionConverter.EvaluateQuotation
                <@ let r = { Value = 1 }: MutableRec in r.Value <- 5; r.Value @>

        equal 5 (result :?> int)
