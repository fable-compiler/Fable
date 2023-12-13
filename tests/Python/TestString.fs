module Fable.Tests.String

open System
open System.Globalization

open Util.Testing

#nowarn "44" // This construct is deprecated. Uri.EscapeUriString can corrupt the Uri string in some cases. (code 44)

[<Literal>]
let formatCoordinateBody = "(%f,%f)"

[<Literal>]
let formatPrefix = "Person at coordinates"

[<Literal>]
let fullFormat = formatPrefix + formatCoordinateBody

let containsInOrder (substrings: string list) (str: string) =
    let mutable lastIndex = -1
    substrings |> List.forall (fun s ->
      let i = str.IndexOf(s)
      let success = i >= 0 && i > lastIndex
      lastIndex <- i
      success)

[<Fact>]
let ``test sprintf works`` () =
    // Immediately applied
    sprintf "%.2f %g" 0.5468989 5.
    |> equal "0.55 5"
    // Curried
    let printer = sprintf "Hi %s, good %s!"
    let printer = printer "Alfonso"
    printer "morning" |> equal "Hi Alfonso, good morning!"
    printer "evening" |> equal "Hi Alfonso, good evening!"

[<Fact>]
let ``test sprintf works II`` () =
      let printer2 = sprintf "Hi %s, good %s%s" "Maxime"
      let printer2 = printer2 "afternoon"
      printer2 "?" |> equal "Hi Maxime, good afternoon?"

[<Fact>]
let ``test sprintf with different decimal digits works`` () =
      sprintf "Percent: %.0f%%" 5.0 |> equal "Percent: 5%"
      sprintf "Percent: %.2f%%" 5. |> equal "Percent: 5.00%"
      sprintf "Percent: %.1f%%" 5.24 |> equal "Percent: 5.2%"
      sprintf "Percent: %.2f%%" 5.268 |> equal "Percent: 5.27%"
      sprintf "Percent: %f%%" 5.67 |> equal "Percent: 5.670000%"

[<Fact>]
let ``test sprintf displays sign correctly`` () =
      sprintf "%i" 1 |> equal "1"
      sprintf "%d" 1 |> equal "1"
      sprintf "%d" 1L |> equal "1"
      sprintf "%.2f" 1. |> equal "1.00"
      sprintf "%i" -1 |> equal "-1"
      sprintf "%d" -1 |> equal "-1"
      sprintf "%d" -1L |> equal "-1"
      sprintf "%.2f" -1. |> equal "-1.00"

[<Fact>]
let ``test format string can use and compose string literals`` =
    let renderedCoordinates = sprintf formatCoordinateBody 0.25 0.75
    let renderedText = sprintf fullFormat 0.25 0.75

    equal "(0.250000,0.750000)" renderedCoordinates
    equal "Person at coordinates(0.250000,0.750000)" renderedText

[<Fact>]
let ``test Print.sprintf works`` () =
    let res = Printf.sprintf "%s" "abc"
    equal "res: abc" ("res: " + res)

[<Fact>]
let ``test sprintf without arguments works`` () =
    sprintf "hello" |> equal "hello"

[<Fact>]
let ``test input of print format can be retrieved`` () =
    let pathScan (pf:PrintfFormat<_,_,_,_,'t>) =
        let formatStr = pf.Value
        formatStr

    equal "/hello/%s" (pathScan "/hello/%s")

[<Fact>]
let ``test interpolate works`` () =
    let name = "Phillip"
    let age = 29
    $"Name: {name}, Age: %i{age}"
    |> equal "Name: Phillip, Age: 29"

#if FABLE_COMPILER
[<Fact>]
let ``test string interpolation works with inline expressions`` () =
    $"I think {3.0 + 0.14} is close to %.8f{3.14159265}!"
    |> equal "I think 3.14 is close to 3.14159265!"
#endif

[<Fact>]
let ``test string interpolation works with anonymous records`` () =
    let person =
        {| Name = "John"
           Surname = "Doe"
           Age = 32
           Country = "The United Kingdom" |}
    $"Hi! My name is %s{person.Name} %s{person.Surname.ToUpper()}. I'm %i{person.Age} years old and I'm from %s{person.Country}!"
    |> equal "Hi! My name is John DOE. I'm 32 years old and I'm from The United Kingdom!"

[<Fact>]
let ``test Extended string interpolation syntax`` =
    let classAttr = "item-panel"
    let cssNew = $$""".{{classAttr}}:hover {background-color: #eee;}"""
    cssNew |> equal ".item-panel:hover {background-color: #eee;}"

[<Fact>]
let ``test sprintf \"%A\" with lists works`` () =
    let xs = ["Hi"; "Hello"; "Hola"]
    (sprintf "%A" xs).Replace("\"", "") |> equal "[Hi; Hello; Hola]"

[<Fact>]
let ``test sprintf \"%A\" with nested lists works`` () =
    let xs = [["Hi"]; ["Hello"]; ["Hola"]]
    (sprintf "%A" xs).Replace("\"", "") |> equal "[[Hi]; [Hello]; [Hola]]"

[<Fact>]
let ``test sprintf \"%A\" with sequences works`` () =
    let xs = seq { "Hi"; "Hello"; "Hola" }
    sprintf "%A" xs |> containsInOrder ["Hi"; "Hello"; "Hola"] |> equal true

[<Fact>]
let ``test Storing result of Seq.tail and printing the result several times works. Related to #1996`` () =
    let tweets = seq { "Hi"; "Hello"; "Hola" }
    let tweetsTailR: seq<string> = tweets |> Seq.tail

    let a = sprintf "%A" (tweetsTailR)
    let b = sprintf "%A" (tweetsTailR)

    containsInOrder ["Hello"; "Hola"] a |> equal true
    containsInOrder ["Hello"; "Hola"] b |> equal true

[<Fact>]
let ``test sprintf \"%X\" works`` () =
    //These should all be the Native JS Versions (except int64 / uint64)
    //See #1530 for more information.

    sprintf "255: %X" 255 |> equal "255: FF"
    sprintf "255: %x" 255 |> equal "255: ff"
    sprintf "-255: %X" -255 |> equal "-255: FFFFFF01"
    sprintf "4095L: %X" 4095L |> equal "4095L: FFF"
    sprintf "-4095L: %X" -4095L |> equal "-4095L: FFFFFFFFFFFFF001"
    sprintf "1 <<< 31: %x" (1 <<< 31) |> equal "1 <<< 31: 80000000"
    sprintf "1u <<< 31: %x" (1u <<< 31) |> equal "1u <<< 31: 80000000"
    sprintf "2147483649L: %x" 2147483649L |> equal "2147483649L: 80000001"
    sprintf "2147483650uL: %x" 2147483650uL |> equal "2147483650uL: 80000002"
    sprintf "1L <<< 63: %x" (1L <<< 63) |> equal "1L <<< 63: 8000000000000000"
    sprintf "1uL <<< 63: %x" (1uL <<< 63) |> equal "1uL <<< 63: 8000000000000000"

[<Fact>]
let ``test sprintf integers with sign and padding works`` () =
    sprintf "%+04i" 1 |> equal "+001"
    sprintf "%+04i" -1 |> equal "-001"
    sprintf "%5d" -5 |> equal "   -5"
    sprintf "%5d" -5L |> equal "   -5"
    sprintf "%- 4i" 5 |> equal " 5  "

// [<Fact>]
// let ``test parameterized padding works`` () =
//     sprintf "[%*s][%*s]" 6 "Hello" 5 "Foo"
//     |> equal "[ Hello][  Foo]"

[<Fact>]
let ``test String.Format combining padding and zeroes pattern works`` () =
    String.Format(CultureInfo.InvariantCulture, "{0:++0.00++}", -5000.5657) |> equal "-++5000.57++"
    String.Format(CultureInfo.InvariantCulture, "{0:000.00}foo", 5) |> equal "005.00foo"
    String.Format(CultureInfo.InvariantCulture, "{0,-8:000.00}foo", 12.456) |> equal "012.46  foo"

[<Fact>]
let ``test String.Format works`` () =
    let arg1, arg2, arg3 = "F#", "Fable", "Babel"
    String.Format(CultureInfo.InvariantCulture, "{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
    |> equal "Babel is to Fable what Fable is to F#"

[<Fact>]
let ``test String.Format with array works`` () =
    let args = [| "F#" :> obj; "Fable"; "Babel" |]
    String.Format("{2} is to {1} what {1} is to {0}", args=args)
    |> equal "Babel is to Fable what Fable is to F#"

[<Fact>]
let ``test StringBuilder works`` () =
    let sb = System.Text.StringBuilder()
    sb.Append "Hello" |> ignore
    sb.AppendLine () |> ignore
    sb.AppendLine "World!" |> ignore
    let expected = System.Text.StringBuilder()
                      .AppendFormat("Hello{0}World!{0}", Environment.NewLine)
                      .ToString()
    sb.ToString() |> equal expected

[<Fact>]
let ``test StringBuilder.Lengh works`` () =
    let sb = System.Text.StringBuilder()
    sb.Append("Hello") |> ignore
    // We don't test the AppendLine for Length because depending on the OS
    // the result is different. Unix \n VS Windows \r\n
    // sb.AppendLine() |> ignore
    equal 5 sb.Length

[<Fact>]
let ``test StringBuilder.ToString works with index and length`` () =
    let sb = System.Text.StringBuilder()
    sb.Append("Hello") |> ignore
    sb.AppendLine() |> ignore
    equal "ll" (sb.ToString(2, 2))

[<Fact>]
let ``test StringBuilder.Clear works`` () =
    let builder = new System.Text.StringBuilder()
    builder.Append("1111") |> ignore
    builder.Clear() |> ignore
    equal "" (builder.ToString())

[<Fact>]
let ``test StringBuilder.Append works with various overloads`` () =
    let builder = Text.StringBuilder()
                      .Append(Text.StringBuilder "aaa")
                      .Append("bcd".ToCharArray())
                      .Append('/')
                      .Append(true)
                      .AppendFormat(CultureInfo.InvariantCulture, "{0}", 5.2)
                      .Append(34)
    equal "aaabcd/true5.234" (builder.ToString().ToLower())

[<Fact>]
let ``test Conversion char to int works`` () =
    equal 97 (int 'a')
    equal 'a' (char 97)

[<Fact>]
let ``test Conversion string to char works`` () =
    equal 'a' (char "a")
    equal "a" (string 'a')

[<Fact>]
let ``test Conversion string to negative int8 works`` () =
    equal -5y (int8 "-5")
    equal "-5" (string -5y)

[<Fact>]
let ``test Conversion string to negative int16 works`` () =
    equal -5s (int16 "-5")
    equal "-5" (string -5s)

[<Fact>]
let ``test Conversion string to negative int32 works`` () =
    equal -5 (int32 "-5")
    equal "-5" (string -5)

[<Fact>]
let ``test Conversion string to negative int64 works`` () =
    equal -5L (int64 "-5")
    equal "-5" (string -5L)

[<Fact>]
let ``test Conversion string to int8 works`` () =
    equal 5y (int8 "5")
    equal "5" (string 5y)

[<Fact>]
let ``test Conversion string to int16 works`` () =
    equal 5s (int16 "5")
    equal "5" (string 5s)

[<Fact>]
let ``test Conversion string to int32 works`` () =
    equal 5 (int32 "5")
    equal "5" (string 5)

[<Fact>]
let ``test Conversion string to int64 works`` () =
    equal 5L (int64 "5")
    equal "5" (string 5L)

[<Fact>]
let ``test Conversion string to uint8 works`` () =
    equal 5uy (uint8 "5")
    equal "5" (string 5uy)

[<Fact>]
let ``test Conversion string to uint16 works`` () =
    equal 5us (uint16 "5")
    equal "5" (string 5us)

[<Fact>]
let ``test Conversion string to uint32 works`` () =
    equal 5u (uint32 "5")
    equal "5" (string 5u)

[<Fact>]
let ``test Conversion string to uint64 works`` () =
    equal 5uL (uint64 "5")
    equal "5" (string 5uL)

[<Fact>]
let ``test Conversion string to single works`` () =
    equal 5.f (float32 "5.0")
    equal -5.f (float32 "-5.0")
    (string 5.f).StartsWith("5") |> equal true
    equal 5.25f (float32 "5.25")
    (string 5.25f).StartsWith("5.25") |> equal true

[<Fact>]
let ``test Conversion string to double works`` () =
    equal 5. (float "5.0")
    equal -5. (float "-5.0")
    (string 5.).StartsWith("5") |> equal true
    equal 5.25 (float "5.25")
    (string 5.25).StartsWith("5.25") |> equal true


[<Fact>]
let ``test Conversion string to decimal works`` () =
    equal 5.m (decimal "5.0")
    equal -5.m (decimal "-5.0")
    (string 5.m).StartsWith("5") |> equal true
    equal 5.25m (decimal "5.25")
    (string 5.25m).StartsWith("5.25") |> equal true

// System.String - constructors

[<Fact>]
let ``test String.ctor(char[]) works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|])
    |> equal "fable"

[<Fact>]
let ``test String.ctor(char, int) works`` () =
    System.String('f', 5)
    |> equal "fffff"

[<Fact>]
let ``test String.ctor(char[], int, int) works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3)
    |> equal "abl"

// System.String - static methods

[<Fact>]
let ``test System.String.Equals works`` () =
    System.String.Equals("abc", "abc") |> equal true
    System.String.Equals("ABC", "abc") |> equal false
    System.String.Equals("abc", "abd") |> equal false
    "abc".Equals("abc") |> equal true
    "ABC".Equals("abc") |> equal false
    "abc".Equals("abd") |> equal false
    System.String.Equals("ABC", "abc", StringComparison.Ordinal) |> equal false
    System.String.Equals("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABC".Equals("abc", StringComparison.Ordinal) |> equal false
    "ABC".Equals("abc", StringComparison.OrdinalIgnoreCase) |> equal true

[<Fact>]
let ``test String.Compare works`` () =
    // TODO: "ABC".CompareTo("abc") > 0 |> equal true
    System.String.Compare("abc", "abc") |> equal 0
    // TODO: System.String.Compare("ABC", "abc") |> equal 1
    System.String.Compare("abc", "abd") |> equal -1
    System.String.Compare("bbc", "abd") |> equal 1
    // TODO: System.String.Compare("ABC", "abc", false) |> equal 1
    System.String.Compare("ABC", "abc", true) |> equal 0
    System.String.Compare("ABC", "abd", true) |> equal -1
    System.String.Compare("BBC", "abd", true) |> equal 1
    // TODO: System.String.Compare("ABC", "abc", StringComparison.CurrentCulture) > 0 |> equal true
    System.String.Compare("ABC", "abc", StringComparison.Ordinal) < 0 |> equal true
    System.String.Compare("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal 0
    // TODO: System.String.Compare("abc", 0, "bcd", 0, 3) |> equal -1
    // System.String.Compare("abc", 1, "bcd", 0, 2) |> equal 0
    // TODO: System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.CurrentCulture) > 0 |> equal true
    System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.Ordinal) < 0 |> equal true
    // TODO: System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.OrdinalIgnoreCase) |> equal 0


[<Fact>]
let ``test String.IsNullOrEmpty works`` () =
    let args = [("", true); (null, true); ("test", false); (" \t", false)]
    for arg in args do
          System.String.IsNullOrEmpty(fst arg)
          |> equal (snd arg)

[<Fact>]
let ``test String.IsNullOrWhiteSpace works`` () =
    let args = [("", true); (null, true); ("test", false); (" \t", true)]
    for arg in args do
          System.String.IsNullOrWhiteSpace(fst arg)
          |> equal (snd arg)

[<Fact>]
let ``test String.Contains works`` () =
    "ABC".Contains("B") |> equal true
    "ABC".Contains("Z") |> equal false

[<Fact>]
let ``test String.Split works`` () =
    "a b c  d".Split(' ')
    |> (=) [|"a";"b";"c";"";"d"|] |> equal true
    "a b c  d ".Split()
    |> (=) [|"a";"b";"c";"";"d";""|] |> equal true
    let array = "a;b,c".Split(',', ';')
    "abc" = array.[0] + array.[1] + array.[2]
    |> equal true
    "a--b-c".Split([|"--"|], StringSplitOptions.None)
    |> (=) [|"a";"b-c"|] |> equal true

[<Fact>]
let ``test String.Split with remove empties works`` () =
    "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    |> (=) [|"a";"b";"c";"d"|] |> equal true
    let array = ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
    "abc" = array.[0] + array.[1] + array.[2]
    |> equal true

[<Fact>]
let ``test String.Split with count works`` () =
    let array = "a b  c d".Split ([|' '|], 2)
    equal "a" array.[0]
    equal "b  c d" array.[1]
    "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.RemoveEmptyEntries)
    |> (=) [|"a";"b";"c;d"|] |> equal true

[<Fact>]
let ``test String.Replace works`` () =
    "abc abc abc".Replace("abc", "d") |> equal "d d d"
    // String.Replace does not get stuck in endless loop
    "...".Replace(".", "..") |> equal "......"

[<Fact>]
let ``test Access char by index works`` () =
    let c = "abcd".[2]
    equal 'c' c
    equal 'd' (char ((int c) + 1))

[<Fact>]
let ``test String.IndexOf char works`` () =
    "abcd".IndexOf('b') * 100 + "abcd".IndexOf('e')
    |> equal 99

[<Fact>]
let ``test String.IndexOf char works with offset`` () =
    "abcdbc".IndexOf('b', 3)
    |> equal 4

[<Fact>]
let ``test String.LastIndexOf char works`` () =
    "abcdbc".LastIndexOf('b') * 100 + "abcd".LastIndexOf('e')
    |> equal 399

[<Fact>]
let ``test String.LastIndexOf char works with offset`` () =
    "abcdbcebc".LastIndexOf('b', 3)
    |> equal 1

[<Fact>]
let ``test String.IndexOf works`` () =
    "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd")
    |> equal 99

[<Fact>]
let ``test String.IndexOf works with offset`` () =
    "abcdbc".IndexOf("bc", 3)
    |> equal 4

[<Fact>]
let ``test String.LastIndexOf works`` () =
    "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd")
    |> equal 399

[<Fact>]
let ``test String.LastIndexOf works with offset`` () =
    "abcdbcebc".LastIndexOf("bc", 3)
    |> equal 1


[<Fact>]
let ``test String.IndexOfAny works`` () =
    "abcdbcebc".IndexOfAny([|'b'|]) |> equal 1
    "abcdbcebc".IndexOfAny([|'b'|], 2) |> equal 4
    "abcdbcebc".IndexOfAny([|'b'|], 2, 2) |> equal -1
    "abcdbcebc".IndexOfAny([|'f';'e'|]) |> equal 6
    "abcdbcebc".IndexOfAny([|'f';'e'|], 2) |> equal 6
    "abcdbcebc".IndexOfAny([|'f';'e'|], 2, 4) |> equal -1
    "abcdbcebc".IndexOfAny([|'c';'b'|]) |> equal 1

[<Fact>]
let ``test String.StartsWith works`` () =
    let args = [("ab", true); ("cd", false); ("abcdx", false)]
    for arg in args do
          "abcd".StartsWith(fst arg)
          |> equal (snd arg)

[<Fact>]
let ``test String.StartsWith with StringComparison works`` () =
    let args = [("ab", true); ("cd", false); ("abcdx", false)]
    for arg in args do
          "ABCD".StartsWith(fst arg, StringComparison.OrdinalIgnoreCase)
          |> equal (snd arg)

[<Fact>]
let ``test String.EndsWith works`` () =
    let args = [("ab", false); ("cd", true); ("abcdx", false)]
    for arg in args do
          "abcd".EndsWith(fst arg)
          |> equal (snd arg)

[<Fact>]
let ``test String.Trim works`` () =
    "   abc   ".Trim()
    |> equal "abc"

[<Fact>]
let ``test String.Trim with chars works`` () =
    @"\\\abc///".Trim('\\','/')
    |> equal "abc"

[<Fact>]
let ``test String.Trim with special chars works`` () =
    @"()[]{}abc/.?*+-^$|\".Trim(@"()[]{}/.?*+-^$|\".ToCharArray())
    |> equal "abc"

[<Fact>]
let ``test String.TrimStart works`` () =
    "!!--abc   ".TrimStart('!','-')
    |> equal "abc   "

[<Fact>]
let ``test String.TrimStart with chars works`` () =
    "   abc   ".TrimStart()
    |> equal "abc   "

[<Fact>]
let ``test String.TrimEnd works`` () =
    "   abc   ".TrimEnd()
    |> equal "   abc"

[<Fact>]
let ``test String.TrimEnd with chars works`` () =
    "   abc??**".TrimEnd('*','?')
    |> equal "   abc"
    @"\foo\bar\".Replace("\\", "/").TrimEnd('/')
    |> equal "/foo/bar"

[<Fact>]
let ``test String.Empty works`` () =
    let s = String.Empty
    s |> equal ""

[<Fact>]
let ``test String.Chars works`` () =
    let input = "hello"
    input.Chars(2)
    |> equal 'l'

[<Fact>]
let ``test String.Substring works`` () =
    "abcdefg".Substring(2)
    |> equal "cdefg"

[<Fact>]
let ``test String.Substring works with length`` () =
    "abcdefg".Substring(2, 2)
    |> equal "cd"

[<Fact>]
let ``test String.Substring throws error if startIndex or length are out of bounds`` () =
    let throws f =
        try f () |> ignore; false
        with _ -> true
    throws (fun _ -> "abcdefg".Substring(20)) |> equal true
    throws (fun _ -> "abcdefg".Substring(2, 10)) |> equal true

[<Fact>]
let ``test String.ToUpper works`` () =
    "AbC".ToUpper() |> equal "ABC"

[<Fact>]
let ``test String.ToLower works`` () =
    "aBc".ToLower() |> equal "abc"


[<Fact>]
let ``test String.ToUpperInvariant works`` () =
    "AbC".ToUpperInvariant() |> equal "ABC"

[<Fact>]
let ``test String.ToLowerInvariant works`` () =
    "aBc".ToLowerInvariant() |> equal "abc"

[<Fact>]
let ``test String.Length works`` () =
    "AbC".Length |> equal 3

[<Fact>]
let ``test String item works`` () =
    "AbC".[1] |> equal 'b'

[<Fact>]
let ``test String.ToCharArray works`` () =
    let arr = "abcd".ToCharArray()
    equal "c" (string arr.[2])
    arr |> Array.map (fun _ -> 1) |> Array.sum
    |> equal arr.Length

// [<Fact>]
// let ``test String enumeration handles surrogates pairs`` () =
//     let unicodeString = ".\U0001f404."
//     unicodeString |> List.ofSeq |> Seq.length |> equal 4
//     String.length unicodeString |> equal 4
//     let mutable len = 0
//     for i in unicodeString do
//         len <- len + 1
//     equal 4 len

[<Fact>]
let ``test String.Join works`` () =
    String.Join("--", "a", "b", "c")
    |> equal "a--b--c"
    String.Join("--", seq { yield "a"; yield "b"; yield "c" })
    |> equal "a--b--c"

[<Fact>]
let ``test String.Join with indices works`` () =
    String.Join("**", [|"a"; "b"; "c"; "d"|], 1, 2)
    |> equal "b**c"
    String.Join("*", [|"a"; "b"; "c"; "d"|], 1, 3)
    |> equal "b*c*d"

[<Fact>]
let ``test String.Join works with chars`` () =
    String.Join("--", 'a', 'b', 'c')
    |> equal "a--b--c"
    String.Join("--", seq { yield 'a'; yield 'b'; yield 'c' })
    |> equal "a--b--c"
    [0..10]
    |> List.map (fun _ -> '*')
    |> fun chars -> String.Join("", chars)
    |> equal "***********"

[<Fact>]
let ``test String.Join with big integers works`` () =
    String.Join("--", [|3I; 5I|])
    |> equal "3--5"
    String.Join("--", 3I, 5I)
    |> equal "3--5"

[<Fact>]
let ``test String.Join with single argument works`` () =
    String.Join(",", "abc") |> equal "abc"
    String.Join(",", [|"abc"|]) |> equal "abc"
    String.Join(",", ["abc"]) |> equal "abc"

[<Fact>]
let ``test System.String.Concat works`` () =
    String.Concat("a", "b", "c")
    |> equal "abc"
    String.Concat(seq { yield "a"; yield "b"; yield "c" })
    |> equal "abc"

[<Fact>]
let ``test System.String.Join with long array works`` () =
    let n = 100_000
    let a = Array.init n (fun _i -> "a")
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Join with long seq works`` () =
    let n = 100_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Concat with long array works`` () =
    let n = 100_000
    let a = Array.init n (fun _i -> "a")
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Concat with long seq works`` () =
    let n = 100_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``test String.concat with long array works`` () =
    let n = 1_000_000
    let a = Array.init n (fun _i -> "a")
    let s = String.concat "" a
    s.Length |> equal n

[<Fact>]
let ``test String.concat with long seq works`` () =
    let n = 1_000_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.concat "" a
    s.Length |> equal n

[<Fact>]
let ``test String.Remove works`` () =
    "abcd".Remove(2)
    |> equal "ab"
    "abcd".Remove(1,2)
    |> equal "ad"
    "abcd".Remove(0,2)
    |> equal "cd"
    "abcd".Remove(0,4)
    |> equal ""
    "abcd".Remove(0,0)
    |> equal "abcd"

[<Fact>]
let ``test String.Insert work`` () =
    "foobar".Insert(3, " is ")
    |> equal "foo is bar"

[<Fact>]
let ``test Enumerating string works`` () =
    let mutable res = ""
    for c in "HELLO" |> Seq.rev do
          res <- res + (string c)
    equal "OLLEH" res

// String - F# module functions

[<Fact>]
let ``test String.concat works`` () =
    String.concat "--" ["a"; "b"; "c"] |> equal "a--b--c"
    seq { yield "a"; yield "b"; yield "c" }
    |> String.concat "-" |> equal "a-b-c"

[<Fact>]
let ``test String.forall and exists work`` () =
    "!!!" |> String.forall (fun c -> c = '!') |> equal true
    "a!a" |> String.forall (fun c -> c = '!') |> equal false
    "aaa" |> String.forall (fun c -> c = '!') |> equal false

[<Fact>]
let ``test String.init works`` () =
    String.init 3 (fun i -> "a")
    |> equal "aaa"

[<Fact>]
let ``test String.collect works`` () =
    "abc" |> String.collect (fun c -> "bcd")
    |> equal "bcdbcdbcd"

[<Fact>]
let ``test String.iter works`` () =
    let res = ref ""
    "Hello world!"
    |> String.iter (fun c -> res.Value <- res.Value + c.ToString())
    equal "Hello world!" res.Value

[<Fact>]
let ``test String.iteri works`` () =
    let mutable res = ""
    "Hello world!"
    |> String.iteri (fun c i -> res <- res + i.ToString() + c.ToString())
    equal "H0e1l2l3o4 5w6o7r8l9d10!11" res

[<Fact>]
let ``test String.length function works`` () =
    "AbC" |> String.length
    |> equal 3

[<Fact>]
let ``test String.map works`` () =
    "Hello world!" |> String.map (fun c -> if c = 'H' then '_' else c)
    |> equal "_ello world!"

[<Fact>]
let ``test String.mapi works`` () =
    "Hello world!" |> String.mapi (fun i c -> if i = 1 || c = 'H' then '_' else c)
    |> equal "__llo world!"

[<Fact>]
let ``test String.replicate works`` () =
    String.replicate 3 "hi there"
    |> equal "hi therehi therehi there"

[<Fact>]
let ``test String.IsNullOrWhiteSpace works on string with blanks`` () =
    String.IsNullOrWhiteSpace "Fri Jun 30 2017 12:30:00 GMT+0200 (Mitteleuropäische Sommerzeit)"
    |> equal false


[<Fact>]
let ``test String.IsNullOrWhiteSpace works on blank only string`` () =
    String.IsNullOrWhiteSpace "      "
    |> equal true

[<Fact>]
let ``test String.filter works`` () =
    String.filter (fun x -> x <> '.') "a.b.c"
    |> equal "abc"

[<Fact>]
let ``test String.filter works when predicate matches everything`` () =
    String.filter (fun x -> x <> '.') "abc"
    |> equal "abc"

[<Fact>]
let ``test String.filter works when predicate doesn't match`` () =
    String.filter (fun x -> x <> '.') "..."
    |> equal ""

[<Fact>]
let ``test String.filter works with empty string`` () =
    String.filter (fun x -> x <> '.') ""
    |> equal ""

#if FABLE_COMPILER
[<Fact>]
let ``test System.Environment.NewLine works`` () =
  System.Environment.NewLine
  |> equal "\n"
#endif

[<Fact>]
let ``test System.Uri.UnescapeDataString works`` () =
    System.Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"
    System.Uri.UnescapeDataString("http%3A%2F%2Fkvz.io%2F") |> equal "http://kvz.io/"
    System.Uri.UnescapeDataString("http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a")
    |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

[<Fact>]
let ``test System.Uri.EscapeDataString works`` () =
    System.Uri.EscapeDataString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld%21"
    System.Uri.EscapeDataString("http://kvz.io/") |> equal "http%3A%2F%2Fkvz.io%2F"
    System.Uri.EscapeDataString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
    |> equal "http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a"

[<Fact>]
let ``test System.Uri.EscapeUriString works`` () =
    System.Uri.EscapeUriString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld!"
    System.Uri.EscapeUriString("http://kvz.io/") |> equal "http://kvz.io/"
    System.Uri.EscapeUriString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
    |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

// See #1628, though I'm not sure if the compiled tests are passing just the function reference without wrapping it
[<Fact>]
let ``test Passing Char.IsDigit as a function reference does not make String.filter hang`` () =
    "Hello! 123" |> String.filter System.Char.IsDigit |> equal "123"

[<Fact>]
let ``test sprintf with double percent should be unescaped`` () =
    sprintf "%d%%" 100 |> equal "100%"

[<Fact>]
let ``test interpolated string with double percent should be unescaped`` () =
    $"{100}%%" |> equal "100%"

[<Fact>]
let ``test Can create FormattableString`` () =
    let orderAmount = 100
    let convert (s: FormattableString) = s
    let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
    s.Format |> equal "You owe: {0:N5} {1} {2}"
    s.ArgumentCount |> equal 3
    s.GetArgument(2) |> equal (box true)
    s.GetArguments() |> equal [|100; 3; true|]

#if FABLE_COMPILER
//[<Fact>]
//let ``test Can use FormattableString.GetStrings() extension`` () =
//    let orderAmount = 100
//    let convert (s: FormattableString) = s
//    let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
//    s.GetStrings() |> equal [|"You owe: "; " "; " "; ""|]
//    let s2: FormattableString = $"""{5 + 2}This is "{"really"}" awesome!"""
//    s2.GetStrings() |> equal [|""; "This is \""; "\" awesome!"|]
//    let s3: FormattableString = $"""I have no holes"""
//    s3.GetStrings() |> equal [|"I have no holes"|]
#endif

[<Fact>]
let ``test printing strings with unicode characters`` () =
    printfn "🚀 printing unicode ..."

[<Fact>]
let ``test calling ToString(CultureInfo.InvariantCulture) works`` () =
    (1).ToString(CultureInfo.InvariantCulture) |> equal "1"
    (7923209L).ToString(CultureInfo.InvariantCulture) |> equal "7923209"
    (7923209UL).ToString(CultureInfo.InvariantCulture) |> equal "7923209"
