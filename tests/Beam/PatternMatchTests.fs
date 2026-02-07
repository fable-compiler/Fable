module Fable.Tests.PatternMatch

open Fable.Tests.Util
open Util.Testing

// Simple integer matching
let intMatch2Cases x =
    match x with
    | 1 -> "one"
    | _ -> "other"

let intMatch3Cases x =
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "other"

let intMatch4Cases x =
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "other"

[<Fact>]
let ``test integer match with 2 cases`` () =
    intMatch2Cases 1 |> equal "one"
    intMatch2Cases 5 |> equal "other"

[<Fact>]
let ``test integer match with 3 cases`` () =
    intMatch3Cases 1 |> equal "one"
    intMatch3Cases 2 |> equal "two"
    intMatch3Cases 5 |> equal "other"

[<Fact>]
let ``test integer match with 4 cases`` () =
    intMatch4Cases 1 |> equal "one"
    intMatch4Cases 2 |> equal "two"
    intMatch4Cases 3 |> equal "three"
    intMatch4Cases 5 |> equal "other"

// String matching
let stringMatch s =
    match s with
    | "hello" -> "greeting"
    | "bye" -> "farewell"
    | _ -> "unknown"

[<Fact>]
let ``test string match works`` () =
    stringMatch "hello" |> equal "greeting"
    stringMatch "bye" |> equal "farewell"
    stringMatch "foo" |> equal "unknown"

// Boolean matching
[<Fact>]
let ``test boolean match works`` () =
    let describe b =
        match b with
        | true -> "yes"
        | false -> "no"
    describe true |> equal "yes"
    describe false |> equal "no"

// Tuple matching
[<Fact>]
let ``test tuple match works`` () =
    let classify pair =
        match pair with
        | (0, 0) -> "origin"
        | (x, 0) -> "x-axis"
        | (0, y) -> "y-axis"
        | _ -> "other"
    classify (0, 0) |> equal "origin"
    classify (3, 0) |> equal "x-axis"
    classify (0, 5) |> equal "y-axis"
    classify (1, 2) |> equal "other"

// Option matching
[<Fact>]
let ``test option match works`` () =
    let describe opt =
        match opt with
        | Some x -> $"value: {x}"
        | None -> "none"
    describe (Some 42) |> equal "value: 42"
    describe None |> equal "none"

// Guards (when clauses)
[<Fact>]
let ``test match with when guard works`` () =
    let classify x =
        match x with
        | n when n < 0 -> "negative"
        | 0 -> "zero"
        | n when n > 0 -> "positive"
        | _ -> "impossible"
    classify -5 |> equal "negative"
    classify 0 |> equal "zero"
    classify 10 |> equal "positive"

// If/then/else
[<Fact>]
let ``test if/then/else works`` () =
    let max a b = if a > b then a else b
    max 3 5 |> equal 5
    max 7 2 |> equal 7

[<Fact>]
let ``test nested if/then/else works`` () =
    let classify x =
        if x > 0 then "positive"
        elif x < 0 then "negative"
        else "zero"
    classify 5 |> equal "positive"
    classify -3 |> equal "negative"
    classify 0 |> equal "zero"
