module Fable.Tests.Chars

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.JsInterop
open System.Collections.Generic
open System.Globalization

// System.Char

let tests =
    testList "Chars" [
        testCase "Char.ToUpper works" <| fun () ->
            Char.ToUpper('b') |> equal 'B'

        testCase "Char.ToLower works" <| fun () ->
            Char.ToLower('B') |> equal 'b'

        testCase "Char.ToUpperInvariant works" <| fun () ->
            Char.ToUpperInvariant('b') |> equal 'B'

        testCase "Char.ToLowerInvariant works" <| fun () ->
            Char.ToLowerInvariant('B') |> equal 'b'

        testCase "Char.ToString works" <| fun () ->
            Char.ToString('b') |> equal "b"

        testCase "Char.GetUnicodeCategory works" <| fun () ->
            Char.GetUnicodeCategory('a') |> equal UnicodeCategory.LowercaseLetter
            Char.GetUnicodeCategory('1') |> equal UnicodeCategory.DecimalDigitNumber

        testCase "Char.GetUnicodeCategory with two args works" <| fun () ->
            let str = "Ba6"
            Char.GetUnicodeCategory(str,0) |> int |> equal 0 //UnicodeCategory.UppercaseLetter
            Char.GetUnicodeCategory(str,1) |> int |> equal 1 //UnicodeCategory.LowercaseLetter
            Char.GetUnicodeCategory(str,2) |> int |> equal 8 //UnicodeCategory.DecimalDigitNumber

        testCase "Char.IsControl works" <| fun () ->
            Char.IsControl('a') |> equal false
            Char.IsControl('\u0000') |> equal true
            Char.IsControl('\u001F') |> equal true
            Char.IsControl('\u007F') |> equal true
            Char.IsControl('\u009F') |> equal true

        testCase "Char.IsControl with two args works" <| fun () ->
            let str = "a\u0001\u0002\u0003"
            Char.IsControl(str,0) |> equal false
            Char.IsControl(str,1) |> equal true
            Char.IsControl(str,2) |> equal true
            Char.IsControl(str,3) |> equal true

        testCase "Char.IsLetter works" <| fun () ->
            Char.IsLetter('a') |> equal true
            Char.IsLetter('1') |> equal false
            Char.IsLetter('β') |> equal true
            Char.IsLetter('家') |> equal true
            Char.IsLetter('“') |> equal false
            Char.IsLetter('”') |> equal false

        testCase "Char.IsLetter with two args works" <| fun () ->
            let str = "al1"
            Char.IsLetter(str,0) |> equal true
            Char.IsLetter(str,2) |> equal false

        testCase "Char.IsDigit works" <| fun () ->
            Char.IsDigit('a') |> equal false
            Char.IsDigit('1') |> equal true

        testCase "Char.IsDigit with two args works" <| fun () ->
            let str = "ba6"
            Char.IsDigit(str,0) |> equal false
            Char.IsDigit(str,2) |> equal true

        testCase "Char.IsLetterOrDigit works" <| fun () ->
            Char.IsLetterOrDigit('a') |> equal true
            Char.IsLetterOrDigit('1') |> equal true
            Char.IsLetterOrDigit('β') |> equal true
            Char.IsLetterOrDigit('-') |> equal false

        testCase "Char.IsLetterOrDigit with two args works" <| fun () ->
            let str = "x-6"
            Char.IsLetterOrDigit(str,0) |> equal true
            Char.IsLetterOrDigit(str,1) |> equal false
            Char.IsLetterOrDigit(str,2) |> equal true

        testCase "Char.IsUpper works" <| fun () ->
            Char.IsUpper('A') |> equal true
            Char.IsUpper('b') |> equal false
            Char.IsUpper('2') |> equal false

        testCase "Char.IsUpper with two args works" <| fun () ->
            let str = "Ab2"
            Char.IsUpper(str,0) |> equal true
            Char.IsUpper(str,1) |> equal false

        testCase "Char.IsLower works" <| fun () ->
            Char.IsLower('A') |> equal false
            Char.IsLower('b') |> equal true
            Char.IsLower('2') |> equal false

        testCase "Char.IsLower with two args works" <| fun () ->
            let str = "Ab2"
            Char.IsLower(str,0) |> equal false
            Char.IsLower(str,1) |> equal true

        testCase "Char.IsNumber works" <| fun () ->
            Char.IsNumber('a') |> equal false
            Char.IsNumber('1') |> equal true

        testCase "Char.IsNumber with two args works" <| fun () ->
            let str = "ba6"
            Char.IsNumber(str,0) |> equal false
            Char.IsNumber(str,2) |> equal true

        testCase "Char.IsPunctuation works" <| fun () ->
            Char.IsPunctuation('a') |> equal false
            Char.IsPunctuation('.') |> equal true

        testCase "Char.IsPunctuation with two args works" <| fun () ->
            let str = "ba,"
            Char.IsPunctuation(str,0) |> equal false
            Char.IsPunctuation(str,2) |> equal true

        testCase "Char.IsSeparator works" <| fun () ->
            Char.IsSeparator('a') |> equal false
            Char.IsSeparator(' ') |> equal true

        testCase "Char.IsSeparator with two args works" <| fun () ->
            let str = "ba "
            Char.IsSeparator(str,0) |> equal false
            Char.IsSeparator(str,2) |> equal true

        testCase "Char.IsSymbol works" <| fun () ->
            Char.IsSymbol('a') |> equal false
            Char.IsSymbol('+') |> equal true

        testCase "Char.IsSymbol with two args works" <| fun () ->
            let str = "ba+"
            Char.IsSymbol(str,0) |> equal false
            Char.IsSymbol(str,2) |> equal true

        testCase "Char.IsWhitespace works" <| fun () ->
            Char.IsWhiteSpace(' ') |> equal true
            Char.IsWhiteSpace('\n') |> equal true
            Char.IsWhiteSpace('\t') |> equal true
            Char.IsWhiteSpace('\009') |> equal true
            Char.IsWhiteSpace('\013') |> equal true
            Char.IsWhiteSpace('\133') |> equal true
            Char.IsWhiteSpace('\160') |> equal true
            Char.IsWhiteSpace('-') |> equal false

        testCase "Char.IsWhitespace works with two args" <| fun () ->
            let input = " \r"
            Char.IsWhiteSpace(input, 0) |> equal true
            Char.IsWhiteSpace(input, 1) |> equal true

        testCase "Char.IsHighSurrogate works" <| fun () ->
            Char.IsHighSurrogate('a') |> equal false
            Char.IsHighSurrogate("\U00010F00".[0]) |> equal true

        testCase "Char.IsHighSurrogate with two args works" <| fun () ->
            let str = "a\U00010F00z"
            Char.IsHighSurrogate(str,0) |> equal false
            Char.IsHighSurrogate(str,1) |> equal true
            Char.IsHighSurrogate(str,2) |> equal false
            Char.IsHighSurrogate(str,3) |> equal false

        testCase "Char.IsLowSurrogate works" <| fun () ->
            Char.IsLowSurrogate('a') |> equal false
            Char.IsLowSurrogate("\U00010F00".[1]) |> equal true

        testCase "Char.IsLowSurrogate with two args works" <| fun () ->
            let str = "a\U00010F00z"
            Char.IsLowSurrogate(str,0) |> equal false
            Char.IsLowSurrogate(str,1) |> equal false
            Char.IsLowSurrogate(str,2) |> equal true
            Char.IsLowSurrogate(str,3) |> equal false

        testCase "Char.IsSurrogate works" <| fun () ->
            Char.IsSurrogate('a') |> equal false
            Char.IsSurrogate("\U00010F00".[1]) |> equal true

        testCase "Char.IsSurrogate with two args works" <| fun () ->
            let str = "a\U00010F00z"
            Char.IsSurrogate(str,0) |> equal false
            Char.IsSurrogate(str,1) |> equal true
            Char.IsSurrogate(str,2) |> equal true
            Char.IsSurrogate(str,3) |> equal false

        testCase "Char.IsSurrogatePair works" <| fun () ->
            Char.IsSurrogatePair('a', 'b') |> equal false
            Char.IsSurrogatePair("\U00010F00".[0], "\U00010F00".[1]) |> equal true

        testCase "Char.IsSurrogatePair with two args works" <| fun () ->
            let str = "a\U00010F00z"
            Char.IsSurrogatePair(str,0) |> equal false
            Char.IsSurrogatePair(str,1) |> equal true
            Char.IsSurrogatePair(str,2) |> equal false

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