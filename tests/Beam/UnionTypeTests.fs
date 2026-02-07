module Fable.Tests.UnionTypes

open Fable.Tests.Util
open Util.Testing

type Gender = Male | Female

type Either<'TL, 'TR> =
    | Left of 'TL
    | Right of 'TR

type Shape =
    | Circle of radius: float
    | Square of side: float
    | Rectangle of width: float * height: float

type MyUnion =
    | Case0
    | Case1 of string
    | Case2 of string * string

[<Fact>]
let ``test Union cases matches with no arguments can be generated`` () =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

[<Fact>]
let ``test Union cases matches with one argument can be generated`` () =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Union case construction works`` () =
    let x = Case1 "hello"
    match x with
    | Case0 -> "zero"
    | Case1 s -> s
    | Case2 _ -> "two"
    |> equal "hello"

[<Fact>]
let ``test Union case with no data works`` () =
    let x = Case0
    match x with
    | Case0 -> "zero"
    | Case1 _ -> "one"
    | Case2 _ -> "two"
    |> equal "zero"

[<Fact>]
let ``test Shape union works`` () =
    let area shape =
        match shape with
        | Circle r -> 3.14159 * r * r
        | Square s -> s * s
        | Rectangle(w, h) -> w * h
    area (Square 5.0) |> equal 25.0
    area (Rectangle(3.0, 4.0)) |> equal 12.0

[<Fact>]
let ``test Either union works`` () =
    let describe (x: Either<int, string>) =
        match x with
        | Left n -> $"Left: {n}"
        | Right s -> $"Right: {s}"
    describe (Left 42) |> equal "Left: 42"
    describe (Right "hello") |> equal "Right: hello"

[<Fact>]
let ``test Nested match on union works`` () =
    let classify x =
        match x with
        | Left (Left _) -> "left-left"
        | Left (Right _) -> "left-right"
        | Right _ -> "right"
    classify (Left (Left 1)) |> equal "left-left"
    classify (Left (Right "x")) |> equal "left-right"
    classify (Right 0) |> equal "right"

[<Fact>]
let ``test DU structural equality works`` () =
    let x = Left 42
    let y = Left 42
    equal true (x = y)

[<Fact>]
let ``test DU structural inequality works`` () =
    let x = Left 1
    let y = Left 2
    equal true (x <> y)

[<Fact>]
let ``test DU different cases are not equal`` () =
    let x: Either<int, int> = Left 1
    let y: Either<int, int> = Right 1
    equal true (x <> y)
