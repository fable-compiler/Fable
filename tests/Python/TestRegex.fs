module Fable.Tests.Regex

open Util.Testing
open System.Text.RegularExpressions


[<Fact>]
let ``test Regex literals work with forward slashes`` () =
    let str = "<a>foo</a><b>bar</b>"
    let reg = Regex(@"(<a>)\w+(</a>)")

    reg.Replace(str, "$1bar$2")
    |> equal "<a>bar</a><b>bar</b>"

[<Fact>]
let ``test Regex literals work with new lines`` () =
    let str =
        """<a>`   foo `</a>
<b>bar</b>"""

    let reg =
        Regex(
            """(<a>)`   \w+ `(</a>)
(<b>)\w+(</b>)"""
        )

    reg.Replace(str, "$1bar$2$3ham$4")
    |> equal "<a>bar</a><b>ham</b>"

[<Fact>]
let ``test Literal regex works with whitespace characters`` () = // See #2635
    Regex("\r\n|\n").IsMatch("foo\r\nbar")
    |> equal true

    Regex(@"\r\n|\n").IsMatch("foo\r\nbar")
    |> equal true

    Regex("\\r\\n|\\n").IsMatch("foo\r\nbar")
    |> equal true

    Regex(@"\\r\\n|\\n").IsMatch("foo\r\nbar")
    |> equal false

    Regex("\t").IsMatch("foo\tbar") |> equal true
    Regex(@"\t").IsMatch("foo\tbar") |> equal true
    Regex("\\t").IsMatch("foo\tbar") |> equal true
    Regex(@"\\t").IsMatch("foo\tbar") |> equal false

[<Fact>]
let ``test Regex.Options works`` () =
    let option1 = int RegexOptions.IgnoreCase
    let option2 = int RegexOptions.ECMAScript
    let options = option1 ||| option2
    let r = Regex("[a-z]", unbox options)
    int r.Options |> equal 257

[<Fact>]
let ``test Regex.IsMatch with IgnoreCase and Multiline works`` () =
    let str = "ab\ncd"
    let option1 = int RegexOptions.IgnoreCase
    let option2 = int RegexOptions.Multiline
    let options = option1 ||| option2

    let test pattern expected =
        Regex.IsMatch(str, pattern, unbox options)
        |> equal expected

    test "^ab" true
    test "^cd" true
    test "^AB" true
    test "^bc" false

[<Fact>]
let ``test Regex.Escape works`` () =
    // TODO: a few chars are not escaped (e.g. # and white space)
    Regex.Escape(@"\*+?|{[()^$")
    |> equal @"\\\*\+\?\|\{\[\(\)\^\$"

    Regex.Escape(@"C:\Temp") |> equal @"C:\\Temp"

[<Fact>]
let ``test Regex.Unescape works`` () =
    Regex.Unescape(@"\\\*\+\?\|\{\[\(\)\^\$")
    |> equal @"\*+?|{[()^$"

    Regex.Unescape(@"C:\\Temp") |> equal @"C:\Temp"

[<Fact>]
let ``test Regex instance IsMatch works`` () =
    let str = "For more information, see Chapter 3.4.5.1"

    Regex("Chapter \d+(\.\d)*").IsMatch(str)
    |> equal true

    Regex("chapter \d+(\.\d)*").IsMatch(str)
    |> equal false

[<Fact>]
let ``test Regex instance IsMatch with offset works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let re = Regex("Chapter \d+(\.\d)*")
    re.IsMatch(str, 10) |> equal true
    re.IsMatch(str, 40) |> equal false

[<Fact>]
let ``test Regex instance Match and Matches work`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    let test pattern expected =
        let re = Regex(pattern, RegexOptions.IgnoreCase)
        let index = let m = re.Match(str) in if m.Success then m.Index else -1
        let ms = re.Matches(str)
        index + ms.Count |> equal expected

    test "[A-E]" 10
    test "(ZZ)+" -1

[<Fact>]
let ``test Regex instance Match and Matches with offset work`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let test offset expected =
        let re = Regex("[A-E]", RegexOptions.IgnoreCase)
        let index = let m = re.Match(str, offset) in if m.Success then m.Index else -1
        let ms = re.Matches(str, offset)
        (index + ms.Count) |> equal expected
    test 10 31
    test 40 -1