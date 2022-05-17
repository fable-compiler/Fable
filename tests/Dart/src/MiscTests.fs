module Fable.Tests.Dart.Misc

open System
open Util

let increase (x: int ref) =
    x.Value <- x.Value + 1

let tests() =
    testCase "ref works" <| fun () ->
        let x = ref 5
        increase x
        x.Value |> equal 6
