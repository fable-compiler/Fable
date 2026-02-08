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
    | Case3 of string * string * string

type MyUnion2 =
    | Tag of string
    | NewTag of string

let (|Functional|NonFunctional|) (s: string) =
    match s with
    | "fsharp" | "haskell" | "ocaml" -> Functional
    | _ -> NonFunctional

let (|Small|Medium|Large|) i =
    if i < 3 then Small 5
    elif i >= 3 && i < 6 then Medium "foo"
    else Large

let (|FSharp|_|) (document : string) =
    if document = "fsharp" then Some FSharp else None

let (|A|) n = n

[<RequireQualifiedAccess>]
type MyUnion3 =
| Case1
| Case2
| Case3

type R = {
    Name: string
    UnionCase: MyUnion3
}

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
    | Case3 _ -> "three"
    |> equal "hello"

[<Fact>]
let ``test Union case with no data works`` () =
    let x = Case0
    match x with
    | Case0 -> "zero"
    | Case1 _ -> "one"
    | Case2 _ -> "two"
    | Case3 _ -> "three"
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

[<Fact>]
let ``test Union cases matches with many arguments can be generated`` () =
    let x = Case3("a", "b", "c")
    match x with
    | Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Pattern matching with common targets works`` () =
    let x = MyUnion.Case2("a", "b")
    match x with
    | MyUnion.Case0 -> failwith "unexpected"
    | MyUnion.Case1 _
    | MyUnion.Case2 _ -> "a"
    | MyUnion.Case3(a, b, c) -> a + b + c
    |> equal "a"

[<Fact>]
let ``test Union cases called Tag still work`` () =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Comprehensive active patterns work`` () =
    let isFunctional = function
        | Functional -> true
        | NonFunctional -> false
    isFunctional "fsharp" |> equal true
    isFunctional "csharp" |> equal false
    isFunctional "haskell" |> equal true

[<Fact>]
let ``test Comprehensive active patterns can return values`` () =
    let measure = function
        | Small i -> string i
        | Medium s -> s
        | Large -> "bar"
    measure 0 |> equal "5"
    measure 10 |> equal "bar"
    measure 5 |> equal "foo"

[<Fact>]
let ``test Partial active patterns which do not return values work`` () =
    let isFunctional = function
        | FSharp -> "yes"
        | "scala" -> "fifty-fifty"
        | _ -> "dunno"
    isFunctional "scala" |> equal "fifty-fifty"
    isFunctional "smalltalk" |> equal "dunno"
    isFunctional "fsharp" |> equal "yes"

[<Fact>]
let ``test Active patterns can be combined with union case matching`` () =
    let test = function
        | Some(A n, Some(A m)) -> n + m
        | _ -> 0
    Some(5, Some 2) |> test |> equal 7
    Some(5, None) |> test |> equal 0
    None |> test |> equal 0

[<Fact>]
let ``test Equality works in filter`` () =
    let original = [| { Name = "1"; UnionCase = MyUnion3.Case1 } ; { Name = "2"; UnionCase = MyUnion3.Case1 }; { Name = "3"; UnionCase = MyUnion3.Case2 }; { Name = "4"; UnionCase = MyUnion3.Case3 } |]
    original
    |> Array.filter (fun r -> r.UnionCase = MyUnion3.Case1)
    |> Array.length
    |> equal 2
