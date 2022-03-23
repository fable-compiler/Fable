module Fable.Tests.Dart.Regex

open Util
open System.Text.RegularExpressions

let tests() =
    testCase "Regex literals work with forward slashes" <| fun _ ->
        let str = "<a>foo</a><b>baz</b>"
        let reg = Regex(@"(<\w>)\w+(</\w>)")
        reg.Replace(str, "$1bar$2")
        |> equal "<a>bar</a><b>bar</b>"

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

    testCase ".Net named group gets replaced" <| fun _ ->
        let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
        let text = "Numbers: +1 12; +49 456; +44 7890;"
        let replace = "00${country}-${num}"

        r.Replace(text, replace)
        |> equal "Numbers: 001-12; 0049-456; 0044-7890;"