module Fable.Tests.SystemIO

open System.IO
open Util.Testing

let tests =
    testList "Path" [
        testCase "Path.Combine works with two parts" <| fun () ->
            Path.Combine("foo", "bar")
            |> equal "foo/bar"

        testCase "Path.Combine works with three parts" <| fun () ->
            Path.Combine("foo", "bar", "baz.txt")
            |> equal "foo/bar/baz.txt"
    ]
