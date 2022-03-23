module Fable.Tests.Dart.Regex

open Util
open System.Text.RegularExpressions

let tests() =
    testCase "Regex.Options works" <| fun _ ->
        let option1 = RegexOptions.IgnoreCase
        let option2 = RegexOptions.ECMAScript
        let options = option1 ||| option2
        let r = Regex("[a-z]", options)
        int r.Options |> equal 257

    testCase "Regex.IsMatch with IgnoreCase and Multiline works" <| fun _ ->
        let str = "ab\ncd"
        let option1 = RegexOptions.IgnoreCase
        let option2 = RegexOptions.Multiline
        let options = option1 ||| option2
        let test pattern expected =
            Regex.IsMatch(str, pattern, options)
            |> equal expected
        test "^ab" true
        test "^cd" true
        test "^AB" true
        test "^bc" false