module Fable.Tests.Nullable

open Util.Testing
open System

module Conversions =

    open FSharp.Linq

    let tests =
        testList "Conversions" [
            testCase "Nullable.float" <| fun () ->
                equal (Nullable.float (Nullable 1uy)) (Nullable 1.0)
                equal (Nullable.float (Nullable 2y)) (Nullable 2.0)
                equal (Nullable.float (Nullable 3s)) (Nullable 3.0)
                equal (Nullable.float (Nullable 4us)) (Nullable 4.0)
                equal (Nullable.float (Nullable 5)) (Nullable 5.0)
                equal (Nullable.float (Nullable 6u)) (Nullable 6.0)
                equal (Nullable.float (Nullable 7L)) (Nullable 7.0)
                equal (Nullable.float (Nullable 8UL)) (Nullable 8.0)
                equal (Nullable.float (Nullable 9m)) (Nullable 9.0)
                equal (Nullable.float (Nullable 10.0)) (Nullable 10.0)
                equal (Nullable.float (Nullable 11.0f)) (Nullable 11.0)
                equal (Nullable.float (Nullable 'c')) (Nullable 99.0)
        ]

let tests =
    testList "Nullable" [
        Conversions.tests
    ]
