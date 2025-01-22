module Fable.Tests.Nullable

open Util.Testing
open System

module Conversions =

    open FSharp.Linq

    let tests =
        testList "Conversions" [
            testCase "Nullable.char" <| fun () ->
                equal (Nullable.char (Nullable 49uy)) (Nullable '1')
                equal (Nullable.char (Nullable 50y)) (Nullable '2')
                equal (Nullable.char (Nullable 51s)) (Nullable '3')
                equal (Nullable.char (Nullable 52us)) (Nullable '4')
                equal (Nullable.char (Nullable 53)) (Nullable '5')
                equal (Nullable.char (Nullable 54u)) (Nullable '6')
                equal (Nullable.char (Nullable 55L)) (Nullable '7')
                equal (Nullable.char (Nullable 56UL)) (Nullable '8')
                equal (Nullable.char (Nullable 57m)) (Nullable '9')
                equal (Nullable.char (Nullable 58.0)) (Nullable ':')
                equal (Nullable.char (Nullable 59.0f)) (Nullable ';')
                equal (Nullable.char (Nullable 'a')) (Nullable 'a')
                // equal (Nullable.float (Nullable.enum(Nullable 2 ): Nullable<DayOfWeek>)) (Nullable 2.0)

            testCase "Nullable.decimal" <| fun () ->
                ()

            testCase "Nullable.double" <| fun () ->
                ()

            testCase "Nullable.enum" <| fun () ->
                ()

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
                // equal (Nullable.float (Nullable.enum(Nullable 2 ): Nullable<DayOfWeek>)) (Nullable 2.0)

            testCase "Nullable.float32" <| fun () ->
                equal (Nullable.float32 (Nullable 1uy)) (Nullable 1.0f)
                equal (Nullable.float32 (Nullable 2y)) (Nullable 2.0f)
                equal (Nullable.float32 (Nullable 3s)) (Nullable 3.0f)
                equal (Nullable.float32 (Nullable 4us)) (Nullable 4.0f)
                equal (Nullable.float32 (Nullable 5)) (Nullable 5.0f)
                equal (Nullable.float32 (Nullable 6u)) (Nullable 6.0f)
                equal (Nullable.float32 (Nullable 7L)) (Nullable 7.0f)
                equal (Nullable.float32 (Nullable 8UL)) (Nullable 8.0f)
                equal (Nullable.float32 (Nullable 9m)) (Nullable 9.0f)
                equal (Nullable.float32 (Nullable 10.0)) (Nullable 10.0f)
                equal (Nullable.float32 (Nullable 11.0f)) (Nullable 11.0f)
                equal (Nullable.float32 (Nullable 'c')) (Nullable 99.0f)
                // equal (Nullable.float32 (Nullable.enum(Nullable 2 ): Nullable<DayOfWeek>)) (Nullable 2.0f)

            testCase "Nullable.int" <| fun () ->
                ()

            testCase "Nullable.int16" <| fun () ->
                ()

            testCase "Nullable.int32" <| fun () ->
                ()

            testCase "Nullable.int64" <| fun () ->
                ()

            testCase "Nullable.int8" <| fun () ->
                ()

            testCase "Nullable.sbyte" <| fun () ->
                ()

            testCase "Nullable.single" <| fun () ->
                ()

            testCase "Nullable.uint" <| fun () ->
                ()

            testCase "Nullable.uint16" <| fun () ->
                ()

            testCase "Nullable.uint32" <| fun () ->
                ()

            testCase "Nullable.uint64" <| fun () ->
                ()

            testCase "Nullable.uint8" <| fun () ->
                ()
        ]

let tests =
    testList "Nullable" [
        Conversions.tests
    ]
