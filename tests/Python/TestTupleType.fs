module Fable.Tests.TupleTypes

open System
open Util.Testing

[<Fact>]
let ``test Tuple dereferencing can be generated`` () =
    let x = 10, true
    let y, z = x
    equal 10 y
    equal true z

[<Fact>]
let ``test fst function can be generated`` () =
    let xy = 10., true
    fst xy |> equal 10.

[<Fact>]
let ``test snd function can be generated`` () =
    let xy = 10., true
    snd xy |> equal true

[<Fact>]
let ``test Tuple constructor works`` () =
    let t2 = Tuple<_,_>(2, 3)
    let t3 = Tuple.Create("a", 14, "c")
    equal (2, 3) t2
    equal ("a", 14, "c") t3

[<Fact>]
let ``test ValueTuple constructor works`` () =
    let t2 = ValueTuple<_,_>(2, 3)
    let t3 = ValueTuple.Create("a", 14, "c")
    equal (struct (2, 3)) t2
    equal (struct ("a", 14, "c")) t3

[<Fact>]
let ``test Can transform ref tuple to value tuple`` () =
    let t2 = (2, 3)
    t2.ToValueTuple() |> equal (struct (2, 3))

[<Fact>]
let ``test Can transform value tuple to ref tuple`` () =
    let t3 = (struct (2, 3, "c"))
    t3.ToTuple() |> equal (2, 3, "c")

[<Fact>]
let ``test Can create tuples of single element`` () =
    let t1 = Tuple<_> 4
    let t1b = Tuple.Create "b"
    t1.Item1 |> equal 4
    t1b.Item1 |> equal "b"

// A struct tuple pattern inside an array pattern reads the element through its address
[<Fact>]
let ``test Struct tuple patterns inside array patterns work`` () =
    let firstOfOnly (pairs: struct (int * string) array) =
        match pairs with
        | [| struct (n, _) |] -> n
        | [| struct (_, s); struct (n, _) |] -> n + s.Length
        | _ -> -1

    firstOfOnly [| struct (4, "a") |] |> equal 4
    firstOfOnly [| struct (4, "abc"); struct (5, "d") |] |> equal 8
    firstOfOnly [||] |> equal -1
