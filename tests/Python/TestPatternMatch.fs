module Fable.Tests.PatternMatch

open Util.Testing

// ============================================================================
// Pattern Matching Tests for Python Match Statement Generation
// ============================================================================

// ----------------------------------------------------------------------------
// 1. Simple Integer Matching
// ----------------------------------------------------------------------------

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

// ----------------------------------------------------------------------------
// 2. String Matching
// ----------------------------------------------------------------------------

let stringMatch2Cases s =
    match s with
    | "hello" -> "greeting"
    | _ -> "unknown"

let stringMatch3Cases s =
    match s with
    | "hello" -> "greeting"
    | "bye" -> "farewell"
    | _ -> "unknown"

[<Fact>]
let ``test string match with 2 cases`` () =
    stringMatch2Cases "hello" |> equal "greeting"
    stringMatch2Cases "foo" |> equal "unknown"

[<Fact>]
let ``test string match with 3 cases`` () =
    stringMatch3Cases "hello" |> equal "greeting"
    stringMatch3Cases "bye" |> equal "farewell"
    stringMatch3Cases "foo" |> equal "unknown"

// ----------------------------------------------------------------------------
// 3. Or-Patterns
// ----------------------------------------------------------------------------

let orPattern2 x =
    match x with
    | 1 | 2 -> "one or two"
    | _ -> "other"

let orPattern3 x =
    match x with
    | 1 | 2 -> "one or two"
    | 3 | 4 | 5 -> "three to five"
    | _ -> "other"

[<Fact>]
let ``test or-pattern with 2 alternatives`` () =
    orPattern2 1 |> equal "one or two"
    orPattern2 2 |> equal "one or two"
    orPattern2 3 |> equal "other"

[<Fact>]
let ``test or-pattern with multiple groups`` () =
    orPattern3 1 |> equal "one or two"
    orPattern3 2 |> equal "one or two"
    orPattern3 3 |> equal "three to five"
    orPattern3 4 |> equal "three to five"
    orPattern3 5 |> equal "three to five"
    orPattern3 6 |> equal "other"

// ----------------------------------------------------------------------------
// 4. Union Types
// ----------------------------------------------------------------------------

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Triangle of base': float * height: float

let unionMatch shape =
    match shape with
    | Circle r -> $"Circle {r}"
    | Rectangle (w, h) -> $"Rectangle {w}x{h}"
    | Triangle (b, h) -> $"Triangle {b}x{h}"

let unionWildcard shape =
    match shape with
    | Circle _ -> "circle"
    | _ -> "not circle"

type Color = Red | Blue | Green

let colorMatch2 c =
    match c with
    | Red -> "red"
    | _ -> "not red"

let colorMatch3 c =
    match c with
    | Red -> "red"
    | Blue -> "blue"
    | _ -> "other"

[<Fact>]
let ``test union match with field extraction`` () =
    unionMatch (Circle 5.0) |> equal "Circle 5"
    unionMatch (Rectangle (3.0, 4.0)) |> equal "Rectangle 3x4"
    unionMatch (Triangle (6.0, 2.0)) |> equal "Triangle 6x2"

[<Fact>]
let ``test union match with wildcard`` () =
    unionWildcard (Circle 1.0) |> equal "circle"
    unionWildcard (Rectangle (1.0, 1.0)) |> equal "not circle"
    unionWildcard (Triangle (1.0, 1.0)) |> equal "not circle"

[<Fact>]
let ``test simple union match with 2 cases`` () =
    colorMatch2 Red |> equal "red"
    colorMatch2 Blue |> equal "not red"
    colorMatch2 Green |> equal "not red"

[<Fact>]
let ``test simple union match with 3 cases`` () =
    colorMatch3 Red |> equal "red"
    colorMatch3 Blue |> equal "blue"
    colorMatch3 Green |> equal "other"

// ----------------------------------------------------------------------------
// 5. Guard Expressions (when clauses)
// ----------------------------------------------------------------------------

let guardSimple x =
    match x with
    | n when n > 10 -> "big"
    | n when n > 0 -> "small"
    | _ -> "zero or negative"

let guardCapture x =
    match x with
    | v when v % 2 = 0 -> $"even: {v}"
    | v when v % 2 = 1 -> $"odd: {v}"
    | _ -> "unexpected"

let guardMixed x =
    match x with
    | 0 -> "zero"
    | 1 -> "one"
    | n when n > 100 -> "big"
    | _ -> "other"

// More guard cases to test match statement generation with 4+ guards
let guardMultiple x =
    match x with
    | n when n > 100 -> "huge"
    | n when n > 50 -> "big"
    | n when n > 10 -> "medium"
    | n when n > 0 -> "small"
    | _ -> "zero or negative"

// Guard with capture used in body (5 cases)
let guardCaptureMultiple x =
    match x with
    | n when n > 100 -> $"huge: {n}"
    | n when n > 50 -> $"big: {n}"
    | n when n > 10 -> $"medium: {n}"
    | n when n > 0 -> $"small: {n}"
    | _ -> "zero or negative"

[<Fact>]
let ``test guard expression simple`` () =
    guardSimple 15 |> equal "big"
    guardSimple 5 |> equal "small"
    guardSimple 0 |> equal "zero or negative"
    guardSimple -5 |> equal "zero or negative"

[<Fact>]
let ``test guard expression with capture`` () =
    guardCapture 4 |> equal "even: 4"
    guardCapture 7 |> equal "odd: 7"
    guardCapture 0 |> equal "even: 0"

[<Fact>]
let ``test guard expression mixed with constants`` () =
    guardMixed 0 |> equal "zero"
    guardMixed 1 |> equal "one"
    guardMixed 150 |> equal "big"
    guardMixed 50 |> equal "other"

[<Fact>]
let ``test guard expression with 5 cases`` () =
    guardMultiple 150 |> equal "huge"
    guardMultiple 75 |> equal "big"
    guardMultiple 25 |> equal "medium"
    guardMultiple 5 |> equal "small"
    guardMultiple 0 |> equal "zero or negative"
    guardMultiple -10 |> equal "zero or negative"

[<Fact>]
let ``test guard expression capture with 5 cases`` () =
    guardCaptureMultiple 150 |> equal "huge: 150"
    guardCaptureMultiple 75 |> equal "big: 75"
    guardCaptureMultiple 25 |> equal "medium: 25"
    guardCaptureMultiple 5 |> equal "small: 5"
    guardCaptureMultiple 0 |> equal "zero or negative"

// ----------------------------------------------------------------------------
// 6. Nested Matching
// ----------------------------------------------------------------------------

let nestedSimple x y =
    match x with
    | 1 ->
        match y with
        | 1 -> "both one"
        | _ -> "x is one"
    | _ -> "x is not one"

let nestedComplex x y =
    match x with
    | 1 ->
        match y with
        | 1 -> "x=1, y=1"
        | 2 -> "x=1, y=2"
        | _ -> "x=1, y=other"
    | 2 ->
        match y with
        | 1 -> "x=2, y=1"
        | _ -> "x=2, y=other"
    | _ -> "x=other"

[<Fact>]
let ``test nested match simple`` () =
    nestedSimple 1 1 |> equal "both one"
    nestedSimple 1 2 |> equal "x is one"
    nestedSimple 2 1 |> equal "x is not one"

[<Fact>]
let ``test nested match complex`` () =
    nestedComplex 1 1 |> equal "x=1, y=1"
    nestedComplex 1 2 |> equal "x=1, y=2"
    nestedComplex 1 3 |> equal "x=1, y=other"
    nestedComplex 2 1 |> equal "x=2, y=1"
    nestedComplex 2 2 |> equal "x=2, y=other"
    nestedComplex 3 1 |> equal "x=other"

// ----------------------------------------------------------------------------
// 7. Option Matching
// ----------------------------------------------------------------------------

let optionMatch opt =
    match opt with
    | Some x -> $"value: {x}"
    | None -> "none"

[<Fact>]
let ``test option match`` () =
    optionMatch (Some 42) |> equal "value: 42"
    optionMatch None |> equal "none"

// ----------------------------------------------------------------------------
// 8. Result Matching
// ----------------------------------------------------------------------------

let resultMatch res =
    match res with
    | Ok v -> $"ok: {v}"
    | Error e -> $"error: {e}"

[<Fact>]
let ``test result match`` () =
    resultMatch (Ok 100) |> equal "ok: 100"
    resultMatch (Error "fail") |> equal "error: fail"

// ----------------------------------------------------------------------------
// 9. List Matching
// ----------------------------------------------------------------------------

let listMatch lst =
    match lst with
    | [] -> "empty"
    | [x] -> $"single: {x}"
    | [x; y] -> $"pair: {x}, {y}"
    | _ -> "many"

[<Fact>]
let ``test list match`` () =
    listMatch [] |> equal "empty"
    listMatch [1] |> equal "single: 1"
    listMatch [1; 2] |> equal "pair: 1, 2"
    listMatch [1; 2; 3] |> equal "many"

// ----------------------------------------------------------------------------
// 10. Char Matching
// ----------------------------------------------------------------------------

let charMatch c =
    match c with
    | 'a' -> "letter a"
    | 'b' -> "letter b"
    | _ -> "other"

[<Fact>]
let ``test char match`` () =
    charMatch 'a' |> equal "letter a"
    charMatch 'b' |> equal "letter b"
    charMatch 'c' |> equal "other"
