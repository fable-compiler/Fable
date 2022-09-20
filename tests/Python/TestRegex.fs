﻿module Fable.Tests.Regex

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

[<Fact>]
let ``test Regex.IsMatch works`` () =
    let str = "For more information, see Chapter 3.4.5.1"

    Regex.IsMatch(str, "Chapter \d+(\.\d)*")
    |> equal true

    Regex.IsMatch(str, "chapter \d+(\.\d)*")
    |> equal false

[<Fact>]
let ``test Regex.IsMatch with IgnoreCase works`` () =
    let str = "For more information, see Chapter 3.4.5.1"

    Regex.IsMatch(str, "Chapter \d+(\.\d)*", RegexOptions.IgnoreCase)
    |> equal true

    Regex.IsMatch(str, "chapter \d+(\.\d)*", RegexOptions.IgnoreCase)
    |> equal true

[<Fact>]
let ``test Regex.IsMatch with Multiline works`` () =
    let str = "ab\ncd"

    Regex.IsMatch(str, "^ab", RegexOptions.Multiline)
    |> equal true

    Regex.IsMatch(str, "^cd", RegexOptions.Multiline)
    |> equal true

    Regex.IsMatch(str, "^AB", RegexOptions.Multiline)
    |> equal false

[<Fact>]
let ``test RegexOptions.Singleline works`` () =
    let str = "ab\ncd"
    let m1 = Regex.Match(str, ".+")
    let m2 = Regex.Match(str, ".+", RegexOptions.Singleline)
    m1.Length |> equal 2
    m2.Length |> equal 5

[<Fact>]
let ``test Regex.Match works`` () =
    let str = "For more information, see Chapter 3.4.5.1"

    Regex.Match(str, "Chapter \d+(\.\d)*").Success
    |> equal true

    Regex.Match(str, "chapter \d+(\.\d)*").Success
    |> equal false

[<Fact>]
let ``Match.Groups indexer getter works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    let g = m.Groups.[1]
    g.Value |> equal ".1"

[<Fact>]
let ``Match.Groups iteration works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    let mutable count = 0

    for g in m.Groups do
        count <- count + g.Value.Length

    equal 17 count

[<Fact>]
let ``Match.Groups iteration works II`` () = // See #2167
    let m = Regex.Match("foo bar baz", @"(\w+) \w+ (\w+)")
    let li = [ for g in m.Groups -> g.Value ]
    let x = li |> List.skip 1 |> List.reduce (+)
    equal "foobaz" x

[<Fact>]
let ``Match.Groups.Count works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    m.Groups.Count |> equal 2

[<Fact>]
let ``test Match.Index works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    m.Index |> equal 26

[<Fact>]
let ``test Match.Length work`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    m.Length |> equal 15

[<Fact>]
let ``test Match.Value works`` () =
    let str = "For more information, see Chapter 3.4.5.1"
    let m = Regex.Match(str, "Chapter \d+(\.\d)*")
    m.Value |> equal "Chapter 3.4.5.1"

[<Fact>]
let ``test Regex.Matches indexer getter works`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
    ms.[8].Index |> equal 29

[<Fact>]
let ``test Regex.Matches iteration works`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
    let count = ref 0
    for m in ms do count.Value <- count.Value + m.Value.Length
    equal 10 count.Value

[<Fact>]
let ``test Regex.Matches iteration with casting works`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
    let count =
        ms |> Seq.cast<Match> |> Seq.fold(fun acc m -> acc + m.Value.Length) 0
    equal 10 count

[<Fact>]
let ``test MatchCollection.Count works`` () =
    let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let test pattern expected =
        let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
        ms.Count |> equal expected
    test "[A-E]" 10
    test "(ZZ)+" 0

// [<Fact>]
// let ``test Regex.Matches doesn't fall in infinite loop`` () = // See #2845
//     let test input pattern expected =
//         Regex.Matches(input, pattern)
//         |> Seq.cast<Match>
//         |> Seq.map (fun m -> m.Value)
//         |> Seq.toList
//         |> equal expected

//     test "aaa" @".*" ["aaa"; ""]
//     test "ab" @"(?<=a)(?=b)" [""]

//     // unescaped pattern: (?<=")(?:\\\\|\\"|.)*(?=")|[A-Za-z0-9_-]+
//     let pattern = @"(?<="")(?:\\\\|\\""|.)*(?="")|[A-Za-z0-9_-]+"
//     test "\"\"" pattern [""]
//     test "a \"\" b" pattern ["a"; ""; "b"]
//     test "a \"b \\\"c\" de\"f\"" pattern ["a"; "b \\\"c\" de\"f"]
//     test "a\" \\\\\" \\\"\"\" \"b \\\"c\" de\"f\"" pattern ["a"; " \\\\\" \\\"\"\" \"b \\\"c\" de\"f"]
//     test "a\" \\\\\" \\\"\"\"a\\sdf\\ \" \"\" \\\\ \\ \\\\\"\" \\\"\"\"b \\\"c\" de\"f\"" pattern ["a"; " \\\\\" \\\"\"\"a\\sdf\\ \" \"\" \\\\ \\ \\\\\"\" \\\"\"\"b \\\"c\" de\"f"]
//     test "a\" \\\\\" \\\"\"\"a\\sdf\\  '\"' \\' A\"Sd \\af\\aef '\\ a ' ''\\\\\\\\\"\"\" \"\"\" \" \" |\"\" |\" \"\"\"\" \"\" \\\\ \\ \\\\\"\" \\\"\"\"b \\\"c\" de\"f\"" pattern ["a"; " \\\\\" \\\"\"\"a\\sdf\\  '\"' \\' A\"Sd \\af\\aef '\\ a ' ''\\\\\\\\\"\"\" \"\"\" \" \" |\"\" |\" \"\"\"\" \"\" \\\\ \\ \\\\\"\" \\\"\"\"b \\\"c\" de\"f"]

[<Fact>]
let ``test Regex.Split works`` () =
    let test str expected =
        let splits = Regex.Split(str, "[;,]")
        splits.Length |> equal expected
    test "plum;pear,orange" 3
    test "" 1

[<Fact>]
let ``test Regex.Split with limit works`` () =
    let s = "blah blah blah, blah blah blah"
    let r = Regex(" ")
    r.Split(s, 1).Length |> equal 1
    r.Split(s, 3).Length |> equal 3

[<Fact>]
let ``test Regex.Split with limit and offset works`` () =
    let s = "blah blah blah, blah blah blah"
    let r = Regex(" ")
    r.Split(s, 10, 0).Length |> equal 6
    r.Split(s, 10, 20).Length |> equal 3

[<Fact>]
let ``test Regex.Replace works`` () =
    let str = "Too   much   space"

    Regex.Replace(str, "\\s+", " ")
    |> equal "Too much space"

    Regex.Replace(str, "", " ")
    |> equal " T o o       m u c h       s p a c e "

[<Fact>]
let ``test Regex.Replace with macros works`` () =
    let str = "Alfonso Garcia-Caro"

    Regex.Replace(str, "([A-Za-z]+) ([A-Za-z\-]+)", "$2 $1")
    |> equal "Garcia-Caro Alfonso"

    Regex.Replace(str, "(fon)(so)", "$2 $1")
    |> equal "Also fon Garcia-Caro"

[<Fact>]
let ``test Regex.Replace with limit works`` () =
    let str = "Too   much   space"
    let r = Regex("\\s+")

    r.Replace(str, " ", count = 1)
    |> equal "Too much   space"

    r.Replace(str, " ", count = 2)
    |> equal "Too much space"

[<Fact>]
let ``test Regex.Replace with limit and offset works`` () =
    let str = "Too   much   space"
    let r = Regex("\\s+")

    r.Replace(str, " ", count = 20, startat = 0)
    |> equal "Too much space"

    r.Replace(str, " ", count = 20, startat = 5)
    |> equal "Too   much space"

[<Fact>]
let ``test Regex.Replace with limit, offset and macros works`` () =
    let str = "Alfonso Garcia-Caro"
    let re = Regex("([A-Za-z]+) ([A-Za-z\-]+)")

    re.Replace(str, "$2 $1", 1)
    |> equal "Garcia-Caro Alfonso"

    re.Replace(str, "$2 $1", 10, 5)
    |> equal "AlfonGarcia-Caro so"

[<Fact>]
let ``test Regex.Replace with evaluator works`` () =
    let str = "Alfonso García-Caro"
    let test pattern expected =
        Regex.Replace(str, pattern, fun (m: Match) ->
            m.Groups.[2].Value + " " + m.Groups.[1].Value)
        |> equal expected
    test "([A-Za-z]+) ([A-Za-z\-]+)" "Garc Alfonsoía-Caro"
    test "(fon)(so)" "Also fon García-Caro"

[<Fact>]
let ``test Regex.Replace with evaluator and limit works`` () =
    let str = "abcabcabcabcabcabcabcabc"
    let r = Regex("c")
    let test count expected =
        r.Replace(str, (fun (m: Match) -> string m.Index), count=count)
        |> equal expected
    test 1 "ab2abcabcabcabcabcabcabc"
    test 3 "ab2ab5ab8abcabcabcabcabc"

[<Fact>]
let ``test Regex.Replace with evaluator, limit and offset works`` () =
    let str = "abcCcabCCabcccabcabcabCCCcabcabc"
    let r = Regex("c+", RegexOptions.IgnoreCase)
    let test startat expected =
        r.Replace(str, (fun (m: Match) -> string m.Length), count=3, startat=startat)
        |> equal expected
    test 0 "ab3ab2ab3abcabcabCCCcabcabc"
    test 10 "abcCcabCCab3ab1ab1abCCCcabcabc"

[<Fact>]
let ``test Replacing with $0 works`` () = // See #1155
    let s = Regex.Replace("1234567890", ".{2}", "$0-")
    equal "12-34-56-78-90-" s

// See #838
[<Fact>]
let ``test Group values are correct and empty when not being matched`` () =
    Regex.Matches("\n\n\n", @"(?:([^\n\r]+)|\r\n|\n\r|\n|\r)")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.forall (fun value -> value = "")
    |> equal true

[<Fact>]
let ``test Group values can be converted to int`` () = // See #1753
    let m = Regex.Match("ABC123", @"([A-Z]+)(\d+)")
    let group = m.Groups.[2]
    int (group.Value) |> equal 123

// see #2359
[<Fact>]
let ``test Regex.Replace with elevator works when regex has named capture group`` () =
    let r = Regex "0(?<number>\\d+)"
    let text = "Number 012345!"

    let replace (m: Match) =
        m.Success |> equal true
        m.Length |> equal 6
        m.Groups.Count |> equal 2
        m.Groups |> Seq.toList |> List.map (fun m -> m.Value) |> equal [ "012345"; "12345" ]

        sprintf "%s" m.Groups.[1].Value

    let actual = r.Replace(text, replace)

    actual |> equal "Number 12345!"

[<Fact>]
let ``test Unicode patterns work`` () = // See #2925
    let regex = Regex(@"^[\S ,.'-]+$", RegexOptions.ECMAScript)
    let res = regex.Match("John Smith")
    res.Value |> equal "John Smith"

[<Fact>]
let ``test succeeds when match`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"
    m.Success |> equal true

[<Fact>]
let ``test doesn't succeed when unmatched`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Hello World"
    m.Success |> equal false

[<Fact>]
let ``test can get value of existing group`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["number"].Value |> equal "12345"

[<Fact>]
let ``test can get values of multiple existing groups`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let m = r.Match "Number: +49 1234!"

    m.Groups.["country"].Value |> equal "49"
    m.Groups.["num"].Value |> equal "1234"

[<Fact>]
let ``test doesn't succeed for not existing named group`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["nothing"].Success |> equal false

[<Fact>]
let ``test doesn't succeed for not existing named group in regex without named groups`` () =
    let r = Regex "\\d+"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["nothing"].Success |> equal false

[<Fact>]
let ``test doesn't succeed for existing unmatched group`` () =
    let r = Regex "(?<exact>42)|(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["exact"].Success |> equal false

[<Fact>]
let ``test named capture group group name from string`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["number"].Value |> equal "12345"

[<Fact>]
let ``test named capture group group name from variable`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    let g = "number"
    m.Groups.[g].Value |> equal "12345"

[<Fact>]
let ``test group name from addition`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.["num" + "ber"].Value |> equal "12345"

[<Fact>]
let ``test group name from string in function`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    let namedGroup (name: string) (m: Match): string =
        m.Groups.[name].Value

    m |> namedGroup "number" |> equal "12345"

[<Fact>]
let ``test group name from variable in function`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    let namedGroup (name: string) (m: Match): string =
        m.Groups.[name].Value

    let g = "number"
    m |> namedGroup g |> equal "12345"

[<Fact>]
let ``test group name from function call`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    let getName () = "number"
    m.Groups.[getName ()].Value |> equal "12345"

[<Fact>]
let ``test group name from if expression`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"

    m.Groups.[if m.Success then "number" else failwith "Regex didn't match!"].Value |> equal "12345"


[<Fact>]
let ``test gets all matches with all named groups`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let expected = [
        ("1", "12")
        ("49", "456")
        ("44", "7890")
    ]

    let actual =
        r.Matches text
        |> Seq.map (fun m -> m.Groups.["country"].Value, m.Groups.["num"].Value)
        |> Seq.toList

    actual |> equal expected

[<Fact>]
let ``test group name from string`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let expected = [
        ("1", "12")
        ("49", "456")
        ("44", "7890")
    ]

    let actual =
        r.Matches text
        |> Seq.map (fun m -> m.Groups.["country"].Value, m.Groups.["num"].Value)
        |> Seq.toList

    actual |> equal expected

[<Fact>]
let ``test group name from variable`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let expected = [
        ("1", "12")
        ("49", "456")
        ("44", "7890")
    ]

    let g1, g2 = (1, "num")

    let actual =
        r.Matches text
        |> Seq.map (fun m -> m.Groups.[g1].Value, m.Groups.[g2].Value)
        |> Seq.toList

    actual |> equal expected

[<Fact>]
let ``test can access named groups`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let replace (m: Match) =
        sprintf "00%s-%s" m.Groups.["country"].Value m.Groups.["num"].Value

    let expected = "Numbers: 001-12; 0049-456; 0044-7890;"
    let actual = r.Replace(text, replace)

    actual |> equal expected

[<Fact>]
let ``test doesn't succeed when not existing group`` () =
    let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let replace (m: Match) =
        if m.Groups.["nothing"].Success then
            failwith "Shouldn't get group 'nothing'"
        else
            sprintf "00%s-%s" m.Groups.["country"].Value m.Groups.["num"].Value

    let expected = "Numbers: 001-12; 0049-456; 0044-7890;"
    let actual = r.Replace(text, replace)

    actual |> equal expected

[<Fact>]
let ``test doesn't succeed when not existing group without any named groups`` () =
    let r = Regex "\\+(\\d{1,3}) (\\d+)"
    let text = "Numbers: +1 12; +49 456; +44 7890;"

    let replace (m: Match) =
        if m.Groups.["nothing"].Success then
            failwith "Shouldn't get group 'nothing'"
        else
            sprintf "00%s-%s" m.Groups.[1].Value m.Groups.[2].Value

    let expected = "Numbers: 001-12; 0049-456; 0044-7890;"
    let actual = r.Replace(text, replace)

    actual |> equal expected
