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

type IntUnion =
    | IntCase1 of int
    | IntCase2 of int
    | IntCase3 of int

[<Struct>]
type StructPoint2D =
    | Planar2D of float * float
    | Infinity

type WrappedUnion =
    | AString of string

type DeepRecord = { Value: string }

type DeepWrappedUnion =
    | DeepWrappedA of string * DeepRecord
    | DeepWrappedB of string
    | DeepWrappedC of int
    | DeepWrappedD of DeepRecord
    | DeepWrappedE of int * int
    | DeepWrappedF of WrappedUnion
    | DeepWrappedG of {| X: DeepRecord; Y: int |}

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

// --- Tests ported from Rust UnionTests ---

[<Fact>]
let ``test Struct unions work`` () =
    let distance p =
        match p with
        | Planar2D (x, y) -> sqrt (x * x + y * y)
        | Infinity -> infinity
    let p = Planar2D (3, 4)
    let res = distance p
    res |> equal 5.

[<Fact>]
let ``test Union case matching works`` () =
    let x = IntCase1 5
    let res =
        match x with
        | IntCase1 a -> a
        | IntCase2 b -> b
        | IntCase3 c -> c
    res |> equal 5

[<Fact>]
let ``test Union case equality works`` () =
    IntCase1 5 = IntCase1 5 |> equal true
    IntCase1 5 = IntCase2 5 |> equal false
    IntCase3 2 = IntCase3 3 |> equal false
    IntCase2 1 = IntCase2 1 |> equal true
    IntCase3 1 = IntCase3 1 |> equal true

let unionFnAlways1 = function
    | IntCase1 x -> x
    | _ -> -1

let unionFnRetNum = function
    | IntCase1 a -> a
    | IntCase2 b -> b
    | IntCase3 c -> c

[<Fact>]
let ``test Union fn call works`` () =
    let x = IntCase1 3
    let res = unionFnAlways1 x
    let res2 = unionFnAlways1 x
    let res3 = unionFnRetNum x
    let res4 = unionFnRetNum (IntCase2 24)
    res |> equal 3
    res2 |> equal 3
    res3 |> equal 3
    res4 |> equal 24

[<Fact>]
let ``test Union with wrapped type works`` () =
    let a = AString "hello"
    let b = match a with AString s -> s + " world"
    b |> equal "hello world"

let matchStrings = function
    | DeepWrappedA (s, d) -> d.Value + s
    | DeepWrappedB s -> s
    | DeepWrappedC _ -> "nothing"
    | DeepWrappedD d -> d.Value
    | DeepWrappedE _ -> "nothing2"
    | DeepWrappedF (AString s) -> s
    | DeepWrappedG x -> x.X.Value

let matchNumbers = function
    | DeepWrappedA _ -> 0
    | DeepWrappedB _ -> 0
    | DeepWrappedC c -> c
    | DeepWrappedD _ -> 0
    | DeepWrappedE(a, b) -> a + b
    | DeepWrappedF _ -> 0
    | DeepWrappedG x -> x.Y

[<Fact>]
let ``test Deep union with wrapped type works`` () =
    let a = DeepWrappedA (" world", { Value = "hello" })
    let b = DeepWrappedB "world"
    let c = DeepWrappedC 42
    let d = DeepWrappedD { Value = "hello" }
    let f = DeepWrappedF (AString "doublewrapped")
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    a |> matchStrings |> equal "hello world"
    b |> matchStrings |> equal "world"
    c |> matchStrings |> equal "nothing"
    d |> matchStrings |> equal "hello"
    f |> matchStrings |> equal "doublewrapped"
    g |> matchStrings |> equal "G"

[<Fact>]
let ``test Deep union with tuped prim type works`` () =
    let e = DeepWrappedE (3, 2)
    let c = DeepWrappedC 42
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    e |> matchNumbers |> equal 5
    c |> matchNumbers |> equal 42
    g |> matchNumbers |> equal 365

let matchStringWhenNotHello = function
    | DeepWrappedB s when s <> "hello" -> "not hello"
    | _ -> "hello"

[<Fact>]
let ``test Match with condition works`` () =
    let b1 = DeepWrappedB "hello"
    let b2 = DeepWrappedB "not"
    b1 |> matchStringWhenNotHello |> equal "hello"
    b2 |> matchStringWhenNotHello |> equal "not hello"
