module Fable.Tests.Char

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.JsInterop
open System.Collections.Generic

// System.Char

let tests =
    testList "Char" [
        testCase "Char.ToUpper works" <| fun () ->
              Char.ToUpper('b') |> equal 'B'

        testCase "Char.ToLower works" <| fun () ->
              Char.ToLower('B') |> equal 'b'

        testCase "Char.ToUpperInvariant works" <| fun () ->
              Char.ToUpperInvariant('b') |> equal 'B'

        testCase "Char.ToLowerInvariant works" <| fun () ->
              Char.ToLowerInvariant('B') |> equal 'b'

        testCase "Char.IsLetter works" <| fun () ->
            equal true (Char.IsLetter('a'))
            equal false (Char.IsLetter('1'))
            equal true (Char.IsLetter('β'))
        //     equal true (Char.IsLetter('家')) Non-European alphabets don't work

        testCase "Char.IsLetter with two args works" <| fun () ->
            let str = "al1"
            equal true  (Char.IsLetter(str,0))
            equal false (Char.IsLetter(str,2))

        testCase "Char.IsDigit works" <| fun () ->
            equal false (Char.IsDigit('a'))
            equal true  (Char.IsDigit('1'))

        testCase "Char.IsDigit with two args works" <| fun () ->
            let str = "ba6"
            equal false (Char.IsDigit(str,0))
            equal true  (Char.IsDigit(str,2))

        testCase "Char.IsLetterOrDigit works" <| fun () ->
            equal true  (Char.IsLetterOrDigit('a'))
            equal true  (Char.IsLetterOrDigit('1'))
            equal true  (Char.IsLetterOrDigit('β'))
            equal false (Char.IsLetterOrDigit('-'))

        testCase "Char.IsLetterOrDigit with two args works" <| fun () ->
            let str = "x-6"
            equal true  (Char.IsLetterOrDigit(str,0))
            equal false (Char.IsLetterOrDigit(str,1))

        testCase "Char.IsUpper works" <| fun () ->
            equal true  (Char.IsUpper('A'))
            equal false (Char.IsUpper('b'))
            equal false (Char.IsUpper('2'))

        testCase "Char.IsUpper with two args works" <| fun () ->
            let str = "Ab2"
            equal true  (Char.IsUpper(str,0))
            equal false (Char.IsUpper(str,1))

        testCase "Char.IsLower works" <| fun () ->
            equal false (Char.IsLower('A'))
            equal true  (Char.IsLower('b'))
            equal false (Char.IsLower('2'))

        testCase "Char.IsLower with two args works" <| fun () ->
            let str = "Ab2"
            equal false (Char.IsLower(str,0))
            equal true  (Char.IsLower(str,1))

        testCase "Char.IsWhitespace works" <| fun () ->
            equal true (Char.IsWhiteSpace(' '))
            equal true (Char.IsWhiteSpace('\n'))
            equal true (Char.IsWhiteSpace('\t'))

        testCase "Char.IsWhitespace works with two args" <| fun () ->
            let input = " \r"
            equal true (Char.IsWhiteSpace(input, 0))
            equal true (Char.IsWhiteSpace(input, 1))

        testCase "Char.Parse works" <| fun () ->
            equal 'A' (Char.Parse "A")

        testCase "Char.Parse fails if an empty string is given" <| fun () ->
            try
                Char.Parse ""
                |> failwithf "Unexpected result '%c'"
            with
            | _ -> ()

        testCase "Char.Parse fails if a string with length > 1 is given" <| fun () ->
            try
                Char.Parse "Fable"
                |> failwithf "Unexpected result '%c'"
            with
            | _ -> ()
    ]