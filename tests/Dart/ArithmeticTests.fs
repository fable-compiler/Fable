module Fable.Tests.Dart.Arithmetic

open Util

let tests () =
    testCase "Infix add can be generated" <| fun () ->
        4 + 2 |> equal 6

    // testCase "Int32 literal addition is optimized" <| fun () ->
    //     aLiteral + 7 |> equal 12
    //     notALiteral + 7 |> equal 12

    // testCase "Unary negation with negative literal values works" <| fun () ->
    //     -literalNegativeValue |> equal 345

    // testCase "Unary negation with integer MinValue works" <| fun () ->
    //     -(-128y) |> equal System.SByte.MinValue
    //     -(-32768s) |> equal System.Int16.MinValue
    //     -(-2147483648) |> equal System.Int32.MinValue
    //     -(-9223372036854775808L) |> equal System.Int64.MinValue

    testCase "Infix subtract can be generated" <| fun () ->
        4 - 2 |> equal 2

    testCase "Infix multiply can be generated" <| fun () ->
        4 * 2 |> equal 8

    testCase "Infix divide can be generated" <| fun () ->
        4 / 2 |> equal 2

    testCase "Integer division doesn't produce floats" <| fun () ->
        5. / 2. |> equal 2.5
        5 / 2 |> equal 2
        5 / 3 |> equal 1
        // float 5 / 2. |> equal 2.5 // TODO: Number conversion

    testCase "Infix modulo can be generated" <| fun () ->
        4 % 3 |> equal 1
