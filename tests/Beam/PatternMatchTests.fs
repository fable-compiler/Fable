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
let ``test if then else works`` () =
    let max a b = if a > b then a else b
    max 3 5 |> equal 5
    max 7 2 |> equal 7

[<Fact>]
let ``test nested if then else works`` () =
    let classify x =
        if x > 0 then "positive"
        elif x < 0 then "negative"
        else "zero"
    classify 5 |> equal "positive"
    classify -3 |> equal "negative"
    classify 0 |> equal "zero"

// Or-patterns
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

// Union types
type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Triangle of base': float * height: float

let unionMatch shape =
    match shape with
    | Circle _ -> "circle"
    | Rectangle _ -> "rectangle"
    | Triangle _ -> "triangle"

let unionExtract shape =
    match shape with
    | Circle r -> r
    | Rectangle (w, _) -> w
    | Triangle (b, _) -> b

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
    unionMatch (Circle 5.0) |> equal "circle"
    unionMatch (Rectangle (3.0, 4.0)) |> equal "rectangle"
    unionMatch (Triangle (6.0, 2.0)) |> equal "triangle"
    unionExtract (Circle 5.0) |> equal 5.0
    unionExtract (Rectangle (3.0, 4.0)) |> equal 3.0
    unionExtract (Triangle (6.0, 2.0)) |> equal 6.0

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

// More guard expressions
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

let guardMultiple x =
    match x with
    | n when n > 100 -> "huge"
    | n when n > 50 -> "big"
    | n when n > 10 -> "medium"
    | n when n > 0 -> "small"
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

// Nested matching
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

// Result matching
let resultMatch res =
    match res with
    | Ok v -> v
    | Error _ -> -1

[<Fact>]
let ``test result match`` () =
    resultMatch (Ok 100) |> equal 100
    resultMatch (Error "fail") |> equal -1

// List matching
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

// Char matching
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

// Tuple bool guard patterns
let tupleBoolGuardSimple (result: bool * int * int) =
    match result with
    | true, _, i when i > -1 -> "found"
    | _, _, _ -> "not found"

let tupleBoolGuardWithValue (result: bool * int * int) =
    match result with
    | true, _, i when i >= 0 -> $"found at {i}"
    | _, _, _ -> "not found"

let tupleBoolGuardMultiple (result: bool * int * int) =
    match result with
    | true, _, i when i > 10 -> "found high"
    | true, _, i when i > 0 -> "found low"
    | true, _, 0 -> "found at zero"
    | _, _, _ -> "not found"

[<Fact>]
let ``test tuple bool guard simple`` () =
    tupleBoolGuardSimple (true, 123, 5) |> equal "found"
    tupleBoolGuardSimple (true, 123, -1) |> equal "not found"
    tupleBoolGuardSimple (false, 0, -1) |> equal "not found"

[<Fact>]
let ``test tuple bool guard with value`` () =
    tupleBoolGuardWithValue (true, 0, 42) |> equal "found at 42"
    tupleBoolGuardWithValue (true, 0, 0) |> equal "found at 0"
    tupleBoolGuardWithValue (true, 0, -1) |> equal "not found"
    tupleBoolGuardWithValue (false, 0, -1) |> equal "not found"

[<Fact>]
let ``test tuple bool guard multiple conditions`` () =
    tupleBoolGuardMultiple (true, 0, 15) |> equal "found high"
    tupleBoolGuardMultiple (true, 0, 5) |> equal "found low"
    tupleBoolGuardMultiple (true, 0, 0) |> equal "found at zero"
    tupleBoolGuardMultiple (true, 0, -1) |> equal "not found"
    tupleBoolGuardMultiple (false, 0, 10) |> equal "not found"
