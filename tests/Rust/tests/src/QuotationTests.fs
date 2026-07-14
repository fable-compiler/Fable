module Fable.Tests.QuotationTests

open Util.Testing
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

// NOTE: quotations are deconstructed through an untyped `Expr` parameter. Matching a
// typed `Expr<'T>` binding directly is not yet supported on Rust (see quotation notes),
// and `obj` values are not bound out of patterns (only structure / Var names / arithmetic
// evaluation are exercised).

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
    // Value nodes are exercised via evaluate: deconstructing them directly on Rust is
    // limited (the obj payload trips getZero; the type element is System.Type, not string).
    let r = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
    unbox<int> r |> equal 42

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
