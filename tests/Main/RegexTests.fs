module Fable.Tests.Regex

open Util.Testing
open System.Text.RegularExpressions

let tests =
  testList "Regex" [
    testCase "Regex.Options works" <| fun _ ->
        let option1 = int RegexOptions.IgnoreCase
        let option2 = int RegexOptions.ECMAScript
        let options = option1 ||| option2
        let r = Regex("[a-z]", unbox options)
        int r.Options |> equal 257

    testCase "Regex.IsMatch with IgnoreCase and Multiline works" <| fun _ ->
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

    testCase "Regex.Escape works" <| fun _ ->
        // TODO: a few chars are not escaped (e.g. # and white space)
        Regex.Escape(@"\*+?|{[()^$") |> equal @"\\\*\+\?\|\{\[\(\)\^\$"
        Regex.Escape(@"C:\Temp") |> equal @"C:\\Temp"

    testCase "Regex.Unescape works" <| fun _ ->
        Regex.Unescape(@"\\\*\+\?\|\{\[\(\)\^\$") |> equal @"\*+?|{[()^$"
        Regex.Unescape(@"C:\\Temp") |> equal @"C:\Temp"

    testCase "Regex instance IsMatch works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        Regex("Chapter \d+(\.\d)*").IsMatch(str) |> equal true
        Regex("chapter \d+(\.\d)*").IsMatch(str) |> equal false

    testCase "Regex instance IsMatch with offset works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let re = Regex("Chapter \d+(\.\d)*")
        re.IsMatch(str, 10) |> equal true
        re.IsMatch(str, 40) |> equal false

    testCase "Regex instance Match and Matches work" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let test pattern expected =
            let re = Regex(pattern, RegexOptions.IgnoreCase)
            let index = let m = re.Match(str) in if m.Success then m.Index else -1
            let ms = re.Matches(str)
            index + ms.Count |> equal expected
        test "[A-E]" 10
        test "(ZZ)+" -1

    testCase "Regex instance Match and Matches with offset work" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let test offset expected =
            let re = Regex("[A-E]", RegexOptions.IgnoreCase)
            let index = let m = re.Match(str, offset) in if m.Success then m.Index else -1
            let ms = re.Matches(str, offset)
            (index + ms.Count) |> equal expected
        test 10 31
        test 40 -1

    testCase "Regex.IsMatch works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        Regex.IsMatch(str, "Chapter \d+(\.\d)*") |> equal true
        Regex.IsMatch(str, "chapter \d+(\.\d)*") |> equal false

    testCase "Regex.IsMatch with IgnoreCase works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        Regex.IsMatch(str, "Chapter \d+(\.\d)*", RegexOptions.IgnoreCase) |> equal true
        Regex.IsMatch(str, "chapter \d+(\.\d)*", RegexOptions.IgnoreCase) |> equal true

    testCase "Regex.IsMatch with Multiline works" <| fun _ ->
        let str = "ab\ncd"
        Regex.IsMatch(str, "^ab", RegexOptions.Multiline) |> equal true
        Regex.IsMatch(str, "^cd", RegexOptions.Multiline) |> equal true
        Regex.IsMatch(str, "^AB", RegexOptions.Multiline) |> equal false

    testCase "RegexOptions.Singleline works" <| fun _ ->
        let str = "ab\ncd"
        let m1 = Regex.Match(str, ".+")
        let m2 = Regex.Match(str, ".+", RegexOptions.Singleline)
        m1.Length |> equal 2
        m2.Length |> equal 5

    testCase "Regex.Match works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        Regex.Match(str, "Chapter \d+(\.\d)*").Success |> equal true
        Regex.Match(str, "chapter \d+(\.\d)*").Success |> equal false

    testCase "Match.Groups indexer getter works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        let g = m.Groups.[1]
        g.Value |> equal ".1"

    testCase "Match.Groups iteration works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        let count = ref 0
        for g in m.Groups do count := !count + g.Value.Length
        equal 17 !count

    testCase "Match.Groups.Count works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        m.Groups.Count |> equal 2

    testCase "Match.Index works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        m.Index |> equal 26

    testCase "Match.Length works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        m.Length |> equal 15

    testCase "Match.Value works" <| fun _ ->
        let str = "For more information, see Chapter 3.4.5.1"
        let m = Regex.Match(str, "Chapter \d+(\.\d)*")
        m.Value |> equal "Chapter 3.4.5.1"

    testCase "Regex.Matches indexer getter works" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
        ms.[8].Index |> equal 29

    testCase "Regex.Matches iteration works" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
        let count = ref 0
        for m in ms do count := !count + m.Value.Length
        equal 10 !count

    testCase "Regex.Matches iteration with casting works" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let ms = Regex.Matches(str, "[A-E]", RegexOptions.IgnoreCase)
        let count =
           ms |> Seq.cast<Match> |> Seq.fold(fun acc m -> acc + m.Value.Length) 0
        equal 10 count

    testCase "MatchCollection.Count works" <| fun _ ->
        let str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        let test pattern expected =
            let ms = Regex.Matches(str, pattern, RegexOptions.IgnoreCase)
            ms.Count |> equal expected
        test "[A-E]" 10
        test "(ZZ)+" 0

    testCase "Regex.Split works" <| fun _ ->
        let test str expected =
            let splits = Regex.Split(str, "[;,]")
            splits.Length |> equal expected
        test "plum;pear,orange" 3
        test "" 1

    testCase "Regex.Split with limit works" <| fun _ ->
        let s = "blah blah blah, blah blah blah"
        let r = Regex(" ")
        r.Split(s, 1).Length |> equal 1
        r.Split(s, 3).Length |> equal 3

    testCase "Regex.Split with limit and offset works" <| fun _ ->
        let s = "blah blah blah, blah blah blah"
        let r = Regex(" ")
        r.Split(s, 10, 0).Length |> equal 6
        r.Split(s, 10, 20).Length |> equal 3

    testCase "Regex.Replace works" <| fun _ ->
        let str = "Too   much   space"
        Regex.Replace(str, "\\s+", " ")
        |> equal "Too much space"
        Regex.Replace(str, "", " ")
        |> equal " T o o       m u c h       s p a c e "

    testCase "Regex.Replace with macros works" <| fun _ ->
        let str = "Alfonso Garcia-Caro"
        Regex.Replace(str, "([A-Za-z]+) ([A-Za-z\-]+)", "$2 $1") |> equal "Garcia-Caro Alfonso"
        Regex.Replace(str, "(fon)(so)", "$2 $1") |> equal "Also fon Garcia-Caro"

    testCase "Regex.Replace with limit works" <| fun _ ->
        let str = "Too   much   space"
        let r = Regex("\\s+")
        r.Replace(str, " ", count=1)
        |> equal "Too much   space"
        r.Replace(str, " ", count=2)
        |> equal "Too much space"

    testCase "Regex.Replace with limit and offset works" <| fun _ ->
        let str = "Too   much   space"
        let r = Regex("\\s+")
        r.Replace(str, " ", count=20, startat=0)
        |> equal "Too much space"
        r.Replace(str, " ", count=20, startat=5)
        |> equal "Too   much space"

    testCase "Regex.Replace with limit, offset and macros works" <| fun _ ->
        let str = "Alfonso Garcia-Caro"
        let re = Regex("([A-Za-z]+) ([A-Za-z\-]+)")
        re.Replace(str, "$2 $1", 1) |> equal "Garcia-Caro Alfonso"
        re.Replace(str, "$2 $1", 10, 5) |> equal "AlfonGarcia-Caro so"

    testCase "Regex.Replace with evaluator works" <| fun _ ->
        let str = "Alfonso García-Caro"
        let test pattern expected =
            Regex.Replace(str, pattern, fun (m: Match) ->
                m.Groups.[2].Value + " " + m.Groups.[1].Value)
            |> equal expected
        test "([A-Za-z]+) ([A-Za-z\-]+)" "Garc Alfonsoía-Caro"
        test "(fon)(so)" "Also fon García-Caro"

    testCase "Regex.Replace with evaluator and limit works" <| fun _ ->
        let str = "abcabcabcabcabcabcabcabc"
        let r = Regex("c")
        let test count expected =
            r.Replace(str, (fun (m: Match) -> string m.Index), count=count)
            |> equal expected
        test 1 "ab2abcabcabcabcabcabcabc"
        test 3 "ab2ab5ab8abcabcabcabcabc"

    testCase "Regex.Replace with evaluator, limit and offset works" <| fun _ ->
        let str = "abcCcabCCabcccabcabcabCCCcabcabc"
        let r = Regex("c+", RegexOptions.IgnoreCase)
        let test startat expected =
            r.Replace(str, (fun (m: Match) -> string m.Length), count=3, startat=startat)
            |> equal expected
        test 0 "ab3ab2ab3abcabcabCCCcabcabc"
        test 10 "abcCcabCCab3ab1ab1abCCCcabcabc"

    testCase "Replacing with $0 works" <| fun _ -> // See #1155
        let s = Regex.Replace("1234567890", ".{2}", "$0-")
        equal "12-34-56-78-90-" s

    // See #838
    testCase "Group values are correct and empty when not being matched" <| fun _ ->
        Regex.Matches("\n\n\n", @"(?:([^\n\r]+)|\r\n|\n\r|\n|\r)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Seq.forall (fun value -> value = "")
        |> equal true

    testCase "Group values can be converted to int" <| fun _ -> // See #1753
        let m = Regex.Match("ABC123", @"([A-Z]+)(\d+)")
        let group = m.Groups.[2]
        int (group.Value) |> equal 123
  ]