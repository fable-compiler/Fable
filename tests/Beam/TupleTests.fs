module Fable.Tests.TupleTest

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Tuple creation works`` () =
    let t = (1, 2)
    fst t |> equal 1
    snd t |> equal 2

[<Fact>]
let ``test Tuple3 creation works`` () =
    let (a, b, c) = (1, "hello", true)
    a |> equal 1
    b |> equal "hello"
    c |> equal true

[<Fact>]
let ``test Tuple destructuring works`` () =
    let swap (a, b) = (b, a)
    let result = swap (1, 2)
    fst result |> equal 2
    snd result |> equal 1

[<Fact>]
let ``test fst works`` () =
    fst (42, "hello") |> equal 42

[<Fact>]
let ``test snd works`` () =
    snd (42, "hello") |> equal "hello"

[<Fact>]
let ``test Tuple equality works`` () =
    (1, 2) = (1, 2) |> equal true
    (1, 2) = (1, 3) |> equal false

[<Fact>]
let ``test Tuple in list works`` () =
    let pairs = [ (1, "a"); (2, "b"); (3, "c") ]
    List.length pairs |> equal 3
    fst (List.head pairs) |> equal 1

[<Fact>]
let ``test Tuple pattern match works`` () =
    let describe t =
        match t with
        | (0, _) -> "zero"
        | (_, 0) -> "second zero"
        | _ -> "other"
    describe (0, 5) |> equal "zero"
    describe (5, 0) |> equal "second zero"
    describe (1, 2) |> equal "other"

[<Fact>]
let ``test Nested tuple works`` () =
    let t = ((1, 2), (3, 4))
    fst (fst t) |> equal 1
    snd (snd t) |> equal 4

[<Fact>]
let ``test Tuple as function return works`` () =
    let divRem a b = (a / b, a % b)
    let (q, r) = divRem 10 3
    q |> equal 3
    r |> equal 1

[<Fact>]
let ``test Tuple dereferencing can be generated`` () =
    let x = 10, true
    let y, z = x
    equal 10 y
    equal true z

// TODO: System.Tuple.Item1/Item2 not supported in Beam
// [<Fact>]
// let ``test Tuple Item1 and Item2 work`` () =
//     let t = (1, 2)
//     t.Item1 |> equal 1
//     t.Item2 |> equal 2

[<Fact>]
let ``test struct tuples work`` () =
    let t = struct (1, 2)
    let (struct (a, b)) = t
    a |> equal 1
    b |> equal 2

[<Fact>]
let ``test Tuple map works`` () =
    let f (a, b) = (a * 2, b + 1)
    let (x, y) = f (3, 4)
    x |> equal 6
    y |> equal 5

[<Fact>]
let ``test Tuple comparison works`` () =
    (1, 2) < (1, 3) |> equal true
    (1, 2) < (2, 1) |> equal true
    (1, 2) > (1, 1) |> equal true
    (1, 2) >= (1, 2) |> equal true
