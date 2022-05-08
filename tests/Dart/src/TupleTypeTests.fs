module Fable.Tests.Dart.TupleType

open Util
open System

let tests() =
    testCase "Tuple dereferencing can be generated" <| fun () ->
        let x = 10, true
        let y, z = x
        equal 10 y
        equal true z

    testCase "fst function can be generated" <| fun () ->
        let xy = 10., true
        fst xy |> equal 10.

    testCase "snd function can be generated" <| fun () ->
        let xy = 10., true
        snd xy |> equal true

    testCase "Tuple constructor works" <| fun () ->
        let t2 = Tuple<_,_>(2, 3)
        let t3 = Tuple.Create("a", 14, "c")
        equal (2, 3) t2
        equal ("a", 14, "c") t3

    testCase "ValueTuple constructor works" <| fun () ->
        let t2 = ValueTuple<_,_>(2, 3)
        let t3 = ValueTuple.Create("a", 14, "c")
        equal (struct (2, 3)) t2
        equal (struct ("a", 14, "c")) t3

    testCase "Can transform ref tuple to value tuple" <| fun () ->
        let t2 = (2, 3)
        t2.ToValueTuple() |> equal (struct (2, 3))

    testCase "Can transform value tuple to ref tuple" <| fun () ->
        let t3 = (struct (2, 3, "c"))
        t3.ToTuple() |> equal (2, 3, "c")

    testCase "Can create tuples of single element" <| fun () ->
        let t1 = Tuple<_> 4
        let t1b = Tuple.Create "b"
        t1.Item1 |> equal 4
        t1b.Item1 |> equal "b"
