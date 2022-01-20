module Fable.Tests.Dart.Arithmetic

open System
open Util.Testing
open Fable.Tests.Util

let tests =
  testList "Arithmetic" [
    testCase "Infix add can be generated" <| fun () ->
        4 + 2 |> equal 6
]
