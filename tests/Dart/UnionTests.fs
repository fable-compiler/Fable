module Fable.Tests.Dart.Union

open Util

type MyUnion =
    | Case1 of int
    | Case2 of int
    | Case3 of int

let tests() =
    testCase "Union case matching works" <| fun () ->
        let x = Case1 5
        let res =
            match x with
            | Case1 a -> a
            | Case2 b -> b
            | Case3 c -> c
        res |> equal 5
