module Fable.Tests.TupleTypes

open Util.Testing

let tests =
  testList "Tuples" [
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
  ]