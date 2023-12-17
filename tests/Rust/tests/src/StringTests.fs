module Fable.Tests.StringTests

open Util.Testing
open System
open System.Globalization

// module M =
//     let f x = nameof x

// LINE SEPARATOR char doesn't cause an error #1283
let LINE_SEPARATOR = "\u2028"

let [<Literal>] aLiteral = "foo"
let notALiteral = "foo"

[<Literal>]
let formatCoordinateBody = "(%f,%f)"

[<Literal>]
let formatPrefix = "Person at coordinates"

[<Literal>]
let fullFormat = formatPrefix + formatCoordinateBody

// type MyUnion = Bar of int * int | Foo1 of float | Foo3 | Foo4 of MyUnion

// type Test(i: int) =
//     override __.ToString() = string(i + i)

// let spr fmt =
//     let fmt = Printf.StringFormat<_>(fmt)
//     sprintf fmt

// let containsInOrder (substrings: string list) (str: string) =
//     let mutable lastIndex = -1
//     substrings
//     |> List.forall (fun s ->
//         let i = str.IndexOf(s)
//         let success = i >= 0 && i > lastIndex
//         lastIndex <- i
//         success)

[<Fact>]
let ``Adding strings works`` () =
    let a = "hello"
    let b = "world"
    let actual = a + " " + b
    a |> equal "hello" //bind out a to prevent inlining
    actual |> equal "hello world"

[<Fact>]
let ``String equality works`` () =
    let s1 = "hello"
    let s2 = "hello"
    let s3 = "helloo"
    s1 |> equal s2
    (s1 = s2) |> equal true
    (s1 = s3) |> equal false
    (s1 <> s3) |> equal true

[<Fact>]
let ``String literal addition is optimized`` () =
    "bar" + aLiteral |> equal "barfoo"
    "bar" + notALiteral |> equal "barfoo"

// [<Fact>]
// let ``String chunkBySize works`` () = // See #1296
//     "fffff" |> Seq.chunkBySize 3 |> Seq.map String |> Seq.toList
//     |> equal ["fff"; "ff"]

[<Fact>]
let ``StringBuilder works`` () =
    let sb = Text.StringBuilder()
    sb.Append("Hello") |> ignore
    sb.AppendLine() |> ignore
    sb.AppendLine("World!") |> ignore
    let expected = String.Format("Hello{0}World!{0}", Environment.NewLine)
    sb.ToString() |> equal expected

[<Fact>]
let ``StringBuilder.Length works`` () =
    let sb = Text.StringBuilder()
    sb.Append("Hello") |> ignore
    // We don't test the AppendLine for Length because depending on the OS
    // the result is different. Unix \n VS Windows \r\n
    // sb.AppendLine() |> ignore
    equal 5 sb.Length

[<Fact>]
let ``StringBuilder.ToString works with index and length`` () =
    let sb = Text.StringBuilder()
    sb.Append("Hello") |> ignore
    sb.AppendLine() |> ignore
    equal "ll" (sb.ToString(2, 2))

[<Fact>]
let ``StringBuilder.Clear works`` () =
    let builder = new Text.StringBuilder()
    builder.Append("1111") |> ignore
    builder.Clear() |> ignore
    equal "" (builder.ToString())

[<Fact>]
let ``StringBuilder.Append works with various overloads`` () =
    let sb =
        Text.StringBuilder()
            .Append(Text.StringBuilder("aaa"))
            .Append("bcd".ToCharArray())
            .Append('/')
            .Append(true)
            .Append(5.2)
            .Append(34)
    let actual = sb.ToString().Replace(",", ".").ToLower()
    actual |> equal "aaabcd/true5.234"

[<Fact>]
let ``StringBuilder.AppendFormat works`` () =
    let sb = Text.StringBuilder()
    sb.AppendFormat("Hello{0}World{1}", " ", "!") |> ignore
    sb.ToString() |> equal "Hello World!"

[<Fact>]
let ``StringBuilder.AppendFormat with provider works`` () =
    let sb = Text.StringBuilder()
    sb.AppendFormat(CultureInfo.InvariantCulture, "Hello{0}World{1}", " ", "!") |> ignore
    sb.ToString() |> equal "Hello World!"

[<Fact>]
let ``kprintf works`` () =
    let f (s:string) = s + "XX"
    Printf.kprintf f "hello" |> equal "helloXX"
    Printf.kprintf f "%X" 255 |> equal "FFXX"
    Printf.kprintf f "%.2f %g" 0.5468989 5. |> equal "0.55 5XX"

[<Fact>]
let ``ksprintf works`` () =
    let f (s:string) = s + "XX"
    Printf.ksprintf f "hello" |> equal "helloXX"
    Printf.ksprintf f "%X" 255 |> equal "FFXX"
    Printf.ksprintf f "%.2f %g" 0.5468989 5. |> equal "0.55 5XX"

// [<Fact>]
// let ``kprintf works indirectly`` () = // See #1204
//     let lines = ResizeArray<string>()
//     let linef fmt = Printf.ksprintf lines.Add fmt // broken
//     linef "open %s" "Foo"
//     lines |> Seq.toList |> equal ["open Foo"]

[<Fact>]
let ``kbprintf works`` () =
    let sb = Text.StringBuilder()
    let mutable i = 0
    let f () = i <- i + 1
    Printf.kbprintf f sb "Hello"
    Printf.kbprintf f sb " %s!" "world"
    i |> equal 2
    sb.ToString() |> equal "Hello world!"

[<Fact>]
let ``ksprintf curries correctly`` () =
    let append (a: string) b = b + a
    let step1 = Printf.ksprintf append "%d"
    let step2 = step1 42
    let result = step2 "The answer is: "
    result |> equal "The answer is: 42"

[<Fact>]
let ``bprintf works`` () =
    let sb = Text.StringBuilder(10)
    Printf.bprintf sb "Hello"
    Printf.bprintf sb " %s!" "world"
    sb.ToString() |> equal "Hello world!"

[<Fact>]
let ``sprintf works`` () =
    // Immediately applied
    let s = sprintf "%.2f %g" 0.5468989 5.
    s |> equal "0.55 5"
    // Curried
    let printer = sprintf "Hi %s, good %s!"
    let printer = printer "Alfonso"
    printer "morning" |> equal "Hi Alfonso, good morning!"
    printer "evening" |> equal "Hi Alfonso, good evening!"

[<Fact>]
let ``sprintf works II`` () =
    let printer2 = sprintf "Hi %s, good %s%s" "Maxime"
    let printer2 = printer2 "afternoon"
    printer2 "?" |> equal "Hi Maxime, good afternoon?"

[<Fact>]
let ``sprintf with different decimal digits works`` () = // See #1932
    sprintf "Percent: %.0f%%" 5.0 |> equal "Percent: 5%"
    sprintf "Percent: %.2f%%" 5. |> equal "Percent: 5.00%"
    sprintf "Percent: %.1f%%" 5.24 |> equal "Percent: 5.2%"
    sprintf "Percent: %.2f%%" 5.268 |> equal "Percent: 5.27%"
    sprintf "Percent: %f%%" 5.67 |> equal "Percent: 5.670000%"
    sprintf "Percent: %g%%" 5.67 |> equal "Percent: 5.67%"

[<Fact>]
let ``sprintf displays sign correctly`` () = // See #1937
    sprintf "%i" 1 |> equal "1"
    sprintf "%d" 1 |> equal "1"
    sprintf "%d" 1L |> equal "1"
    sprintf "%.2f" 1. |> equal "1.00"
    sprintf "%i" -1 |> equal "-1"
    sprintf "%d" -1 |> equal "-1"
    sprintf "%d" -1L |> equal "-1"
    sprintf "%.2f" -1. |> equal "-1.00"

[<Fact>]
let ``Print.sprintf works`` () = // See #1216
    let res = Printf.sprintf "%s" "abc"
    equal "res: abc" ("res: " + res)

[<Fact>]
let ``sprintf without arguments works`` () =
    sprintf "hello" |> equal "hello"

// [<Fact>]
// let ``input of print format can be retrieved`` () =
//     let pathScan (pf: PrintfFormat<_,_,_,_,'t>) =
//         let formatStr = pf.Value
//         formatStr
//     equal "/hello/%s" (pathScan "/hello/%s")

[<Fact>]
let ``sprintf with escaped percent symbols works`` () = // See #195
    let r, r1, r2 = "Ratio", 0.213849, 0.799898
    sprintf "%s1: %.2f%% %s2: %.2f%%" r (r1*100.) r (r2*100.)
    |> equal "Ratio1: 21.38% Ratio2: 79.99%"

[<Fact>]
let ``sprintf with percent symbols in arguments works`` () = // See #329
    let same s = sprintf "%s" s |> equal s
    same "%"
    same "%%"
    same "%%%"
    same "%%%%"
    same "% %"
    same "%% %%"
    same "%% % % %%"

[<Fact>]
let ``Fix #2398: Exception when two successive string format placeholders and value of first one ends in '%'`` () =
    sprintf "%c%s" '%' "text" |> equal "%text"

// [<Fact>]
// let ``Unions with sprintf %A`` () =
//     Bar(1,5) |> sprintf "%A" |> equal "Bar (1, 5)"
//     Foo1 4.5 |> sprintf "%A" |> equal "Foo1 4.5"
//     Foo4 Foo3 |> sprintf "%A" |> equal "Foo4 Foo3"
//     Foo4(Foo1 4.5) |> sprintf "%A" |> equal "Foo4 (Foo1 4.5)"
//     Foo3 |> sprintf "%A" |> equal "Foo3"

// [<Fact>]
// let ``Unions with string operator`` () =
//     Bar(1,5) |> string |> equal "Bar (1, 5)"
//     Foo1 4.5 |> string |> equal "Foo1 4.5"
//     Foo4 Foo3 |> string |> equal "Foo4 Foo3"
//     Foo4(Foo1 4.5) |>string |> equal "Foo4 (Foo1 4.5)"
//     Foo3 |> string |> equal "Foo3"

// [<Fact>]
// let ``sprintf \"%O\" with overloaded string works`` () =
//     let o = Test(5)
//     sprintf "%O" o |> equal "10"

// [<Fact>]
// let ``sprintf \"%A\" with overloaded string works`` () =
//     let o = Test(5)
//     (sprintf "%A" o).Replace("\"", "") |> equal "10"

// #if FABLE_COMPILER
// [<Fact>]
// let ``sprintf \"%A\" with circular references doesn't crash`` () = // See #338
//     let o = obj()
//     o?self <- o
//     sprintf "%A" o |> ignore
// #endif

module M =
    let f x = nameof x

[<Fact>]
let ``F# nameof works`` () =
    M.f 12 |> equal "x"
    nameof M |> equal "M"
    nameof M.f |> equal "f"

[<Fact>]
let ``string interpolation works`` () =
    let name = "Phillip"
    let age = 29
    $"Name: {name}, Age: %i{age}"
    |> equal "Name: Phillip, Age: 29"

[<Fact>]
let ``string interpolation works with inline expressions`` () =
    $"I think {3.0 + 0.14} is close to {float32 Math.PI}!"
    |> equal "I think 3.14 is close to 3.1415927!"

[<Fact>]
let ``string interpolation works with anonymous records`` () =
    let person =
        {|  Name = "John"
            Surname = "Doe"
            Age = 32
            Country = "The United Kingdom" |}
    $"Hi! My name is %s{person.Name} %s{person.Surname.ToUpper()}. I'm %i{person.Age} years old and I'm from %s{person.Country}!"
    |> equal "Hi! My name is John DOE. I'm 32 years old and I'm from The United Kingdom!"

[<Fact>]
let ``Interpolated strings keep empty lines`` () =
    let s1 = $"""1


    {1+1}


3"""
    let s2 = """1


    2


3"""
    equal s1 s2
    equal s1.Length s2.Length
    equal 13 s1.Length

[<Fact>]
let ``Can use backslash is interpolated strings`` () =
    $"\n{1+1}\n" |> equal """
2
"""

[<Fact>]
let ``Backslash is escaped in interpolated strings`` () = // See #2649
    $"\\" |> equal @"\"
    $"\\".Length |> equal 1
    $@"\" |> equal @"\"
    $@"\".Length |> equal 1
    @$"\ " |> equal @"\ "
    @$"\ ".Length |> equal 2
    $"\\{4}" |> equal @"\4"
    $"\\{4}".Length |> equal 2
    $@"\{4}" |> equal @"\4"
    $@"\{4}".Length |> equal 2
    @$"\{4}" |> equal @"\4"
    @$"\{4}".Length |> equal 2

[<Fact>]
let ``Extended string interpolation syntax`` =
    let classAttr = "item-panel"
    let cssNew = $$""".{{classAttr}}:hover {background-color: #eee;}"""
    cssNew |> equal ".item-panel:hover {background-color: #eee;}"

[<Fact>]
let ``interpolated string with double % should be unescaped`` () =
    $"{100}%%" |> equal "100%"

[<Fact>]
let ``interpolated string with format and double % should be unescaped`` () =
    $"%f{100.456667}%%" |> equal "100.456667%"

[<Fact>]
let ``interpolated string with double % should not interfere with holes afterwards `` () =
    $"%%{99. - 1.5}" |> equal "%97.5"

[<Fact>]
let ``interpolated string with double % should not interfere with format afterwards `` () =
    $"%%%g{99. - 1.5}" |> equal "%97.5"

[<Fact>]
let ``interpolated string with consecutive holes work`` () =
    $"""{"foo"}{5}""" |> equal "foo5"
    $"""%s{"foo"}%i{5}""" |> equal "foo5"
    $"""{"foo"}/{5}.fsi""" |> equal "foo/5.fsi"

[<Fact>]
let ``interpolated string with double braces should be unescaped`` () =
    $"{{ {100} }}" |> equal "{ 100 }"

[<Fact>]
let ``interpolated string with format and double braces should be unescaped`` () =
    $"{{ %.2f{100.4566666} }}" |> equal "{ 100.46 }"

[<Fact>]
let ``sprintf with double % should be unescaped`` () =
    sprintf "%d%%" 100 |> equal "100%"

// [<Fact>]
// let ``sprintf \"%A\" with lists works`` () =
//     let xs = ["Hi"; "Hello"; "Hola"]
//     (sprintf "%A" xs).Replace("\"", "") |> equal "[Hi; Hello; Hola]"

// [<Fact>]
// let ``sprintf \"%A\" with nested lists works`` () =
//     let xs = [["Hi"]; ["Hello"]; ["Hola"]]
//     (sprintf "%A" xs).Replace("\"", "") |> equal "[[Hi]; [Hello]; [Hola]]"

// [<Fact>]
// let ``sprintf \"%A\" with sequences works`` () =
//     let xs = seq { "Hi"; "Hello"; "Hola" }
//     sprintf "%A" xs |> containsInOrder ["Hi"; "Hello"; "Hola"] |> equal true

// [<Fact>]
// let ``Storing result of Seq.tail and printing the result several times works. Related to #1996`` () =
//     let tweets = seq { "Hi"; "Hello"; "Hola" }
//     let tweetsTailR: seq<string> = tweets |> Seq.tail

//     let a = sprintf "%A" (tweetsTailR)
//     let b = sprintf "%A" (tweetsTailR)

//     containsInOrder ["Hello"; "Hola"] a |> equal true
//     containsInOrder ["Hello"; "Hola"] b |> equal true

[<Fact>]
let ``sprintf \"%X\" works`` () =
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
let ``sprintf integers with sign and padding works`` () = // See #1931
    sprintf "%+04i" 1 |> equal "+001"
    sprintf "%+04i" -1 |> equal "-001"
    sprintf "%5d" -5 |> equal "   -5"
    sprintf "%5d" -5L |> equal "   -5"
    // sprintf "%- 4i" 5 |> equal " 5  " //TODO:

[<Fact>]
let ``test format string can use and compose string literals`` =
    let renderedCoordinates = sprintf formatCoordinateBody 0.25 0.75
    let renderedText = sprintf fullFormat 0.25 0.75

    equal "(0.250000,0.750000)" renderedCoordinates
    equal "Person at coordinates(0.250000,0.750000)" renderedText

// [<Fact>]
// let ``parameterized padding works`` () = // See #2336
//     sprintf "[%*s][%*s]" 6 "Hello" 5 "Foo"
//     |> equal "[ Hello][  Foo]"

// [<Fact>]
// let ``String.Format combining padding and zeroes pattern works`` () =
//     String.Format(CultureInfo.InvariantCulture, "{0:++0.00++}", -5000.5657) |> equal "-++5000.57++"
//     String.Format(CultureInfo.InvariantCulture, "{0:000.00}foo", 5) |> equal "005.00foo"
//     String.Format(CultureInfo.InvariantCulture, "{0,-8:000.00}foo", 12.456) |> equal "012.46  foo"

[<Fact>]
let ``String.Format {0:x} works`` () =
    //See above comment on expected values
    String.Format(CultureInfo.InvariantCulture, "255: {0:X}", 255) |> equal "255: FF"
    String.Format(CultureInfo.InvariantCulture, "255: {0:x}", 255) |> equal "255: ff"
    String.Format(CultureInfo.InvariantCulture, "-255: {0:X}", -255) |> equal "-255: FFFFFF01"
    String.Format(CultureInfo.InvariantCulture, "4095L: {0:X}", 4095L) |> equal "4095L: FFF"
    String.Format(CultureInfo.InvariantCulture, "-4095L: {0:X}", -4095L) |> equal "-4095L: FFFFFFFFFFFFF001"
    String.Format(CultureInfo.InvariantCulture, "1 <<< 31: {0:x}", (1 <<< 31)) |> equal "1 <<< 31: 80000000"
    String.Format(CultureInfo.InvariantCulture, "1u <<< 31: {0:x}", (1u <<< 31)) |> equal "1u <<< 31: 80000000"
    String.Format(CultureInfo.InvariantCulture, "2147483649L: {0:x}", 2147483649L) |> equal "2147483649L: 80000001"
    String.Format(CultureInfo.InvariantCulture, "2147483650uL: {0:x}", 2147483650uL) |> equal "2147483650uL: 80000002"
    String.Format(CultureInfo.InvariantCulture, "1L <<< 63: {0:x}", (1L <<< 63)) |> equal "1L <<< 63: 8000000000000000"
    String.Format(CultureInfo.InvariantCulture, "1uL <<< 63: {0:x}", (1uL <<< 63)) |> equal "1uL <<< 63: 8000000000000000"

// [<Fact>]
// let ``String.Format {0:x} with precision works`` () =
//     String.Format(CultureInfo.InvariantCulture, "#{0:X3}", 0xC149D) |> equal "#C149D"
//     String.Format(CultureInfo.InvariantCulture, "#{0:X6}", 0xC149D) |> equal "#0C149D"

// [<Fact>]
// let ``String.Format works with thousands separator`` () =
//     String.Format(CultureInfo.InvariantCulture, "{0}", 12343235354.6547757) |> equal "12343235354.654776"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##.000}", 12343235354.6547757) |> equal "12,343,235,354.655"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##.000}", 12343235354.6547757M) |> equal "12,343,235,354.655"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123.456) |> equal "123.46"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123.456M) |> equal "123.46"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123438192123.456M) |> equal "123,438,192,123.46"
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##}", 1.456M) |> equal "1"
//     String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 1.456M) |> equal "01"

// [<Fact>]
// let ``String.Format can omit decimal digits`` () =
//     String.Format(CultureInfo.InvariantCulture, "{0:#,##}", 12343235354.6547757) |> equal "12,343,235,355"
//     String.Format(CultureInfo.InvariantCulture, "{0:0,00}", 12343235354.6547757) |> equal "12,343,235,355"
//     String.Format(CultureInfo.InvariantCulture, "{0:0,00}", 12343235354.) |> equal "12,343,235,354"
//     String.Format(CultureInfo.InvariantCulture, "{0:#}", 12343235354.) |> equal "12343235354"
//     String.Format(CultureInfo.InvariantCulture, "{0:0}", 12343235354.) |> equal "12343235354"

//     String.Format(CultureInfo.InvariantCulture, "{0:#,#}", 12343235354.6547757M) |> equal "12,343,235,355"
//     String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 12343235354.6547757M) |> equal "12,343,235,355"

//     String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 1234323535) |> equal "1,234,323,535"
//     String.Format(CultureInfo.InvariantCulture, "{0:#}", 1234323535) |> equal "1234323535"
//     String.Format(CultureInfo.InvariantCulture, "{0:0}", 1234323535) |> equal "1234323535"

//     String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 12343235354M) |> equal "12,343,235,354"
//     String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 343235354M) |> equal "343,235,354"
//     String.Format(CultureInfo.InvariantCulture, "{0:#}", 12343235354M) |> equal "12343235354"
//     String.Format(CultureInfo.InvariantCulture, "{0:0}", 12343235354M) |> equal "12343235354"

// [<Fact>]
// let ``ToString formatted works with decimals`` () = // See #2276
//     let decimal = 78.6M
//     decimal.ToString("0.000").Replace(",", ".") |> equal "78.600"

// [<Fact>]
// let ``Printf works with generic argument`` () =
//     spr "bar %s" "a" |> equal "bar a"
//     spr "foo %i %i" 3 5 |> equal "foo 3 5"
//     let f1 = spr "foo %i %i"
//     let f2 = f1 2
//     f2 2 |> equal "foo 2 2"
//     let f1 = spr "foo %i %i %i"
//     let f2 = f1 2
//     let f3 = f2 2
//     f3 2 |> equal "foo 2 2 2"

[<Fact>]
let ``Printf in sequence is not erased`` () =
    let x = sprintf "Foo"
    let y = sprintf "B%sr" "a"
    x + y |> equal "FooBar"

[<Fact>]
let ``Strings can be indexed`` () =
    let s = "bar"
    let s2 = s.ToCharArray()
    let s3 = [|'b'; 'a'; 'r'|]
    s[0] |> equal 'b'
    s2[1] |> equal 'a'
    s3[2] |> equal 'r'
    "おはよう"[2] |> equal 'よ'

[<Fact>]
let ``Strings can be enumerated`` () =
    "おはよう" |> Seq.item 2 |> equal 'よ'

[<Fact>]
let ``String slicing works`` () =
    let s = "cat and dog"
    s[2..8] |> equal "t and d"
    s[2..] |> equal "t and dog"
    s[..8] |> equal "cat and d"
    s[0..-1] |> equal ""
    s[-50..50] |> equal "cat and dog"

[<Fact>]
let ``String.Format works without args`` () =
    String.Format("Hello") |> equal "Hello"
    String.Format($"""Hello {"World!"}""") |> equal "Hello World!"

[<Fact>]
let ``String.Format works`` () =
    let arg1, arg2, arg3 = "F#", "Fable", "Babel"
    String.Format("{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
    |> equal "Babel is to Fable what Fable is to F#"

[<Fact>]
let ``String.Format works II`` () =
    let arg1, arg2, arg3 = "F#", "Fable", "Babel"
    String.Format(CultureInfo.InvariantCulture, "{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
    |> equal "Babel is to Fable what Fable is to F#"

// [<Fact>]
// let ``String.Format with extra formatting works`` () =
//     let i = 0.5466788
//     let dt = DateTime(2014, 9, 26).AddMinutes(19.)
//     String.Format(CultureInfo.InvariantCulture, "{0:F2} {0:P2} {1:yyyy-MM-dd HH:mm}", i, dt)
//         .Replace(",", ".").Replace(" %", "%")
//     |> equal "0.55 54.67% 2014-09-26 00:19"

[<Fact>]
let ``Padding with sprintf works`` () =
    sprintf "%10.1f" 3.14   |> equal "       3.1"
    sprintf "%10.1f" -3.14  |> equal "      -3.1"
    sprintf "%-10.1f" 3.14  |> equal "3.1       "
    sprintf "%-10.1f" -3.14 |> equal "-3.1      "
    sprintf "%10i" 22       |> equal "        22"
    sprintf "%10i" -22      |> equal "       -22"
    sprintf "%-10i" 22      |> equal "22        "
    sprintf "%-10i" -22     |> equal "-22       "
    sprintf "%010i" 22      |> equal "0000000022"
    sprintf "%010i" -22     |> equal "-000000022"
    sprintf "%0-10i" 22     |> equal "22        "
    sprintf "%0-10i" -22    |> equal "-22       "
    sprintf "%+10i" 22      |> equal "       +22"
    sprintf "%+10i" -22     |> equal "       -22"
    sprintf "%+-10i" 22     |> equal "+22       "
    sprintf "%+-10i" -22    |> equal "-22       "
    sprintf "%+010i" 22     |> equal "+000000022"
    sprintf "%+010i" -22    |> equal "-000000022"
    sprintf "%+0-10i" 22    |> equal "+22       "
    sprintf "%+0-10i" -22   |> equal "-22       "

// [<Fact>]
// let ``Padding with String.Format works`` () =
//     String.Format(CultureInfo.InvariantCulture, "{0,10:F1}", 3.14)  |> equal "       3.1"
//     String.Format(CultureInfo.InvariantCulture, "{0,-10:F1}", 3.14) |> equal "3.1       "
//     String.Format(CultureInfo.InvariantCulture, "{0,10}", 22)       |> equal "        22"
//     String.Format(CultureInfo.InvariantCulture, "{0,-10}", -22)     |> equal "-22       "

// Conversions

[<Fact>]
let ``Conversion char to int works`` () =
    equal 97 (int 'a')
    equal 'a' (char 97)

[<Fact>]
let ``Conversion string to char works`` () =
    equal 'a' (char "a")
    equal "a" (string 'a')

[<Fact>]
let ``Conversion string to negative int8 works`` () =
    equal -5y (int8 "-5")
    equal "-5" (string -5y)

[<Fact>]
let ``Conversion string to negative int16 works`` () =
    equal -5s (int16 "-5")
    equal "-5" (string -5s)

[<Fact>]
let ``Conversion string to negative int32 works`` () =
    equal -5 (int32 "-5")
    equal "-5" (string -5)

[<Fact>]
let ``Conversion string to negative int64 works`` () =
    equal -5L (int64 "-5")
    equal "-5" (string -5L)

[<Fact>]
let ``Conversion string to int8 works`` () =
    equal 5y (int8 "5")
    equal "5" (string 5y)

[<Fact>]
let ``Conversion string to int16 works`` () =
    equal 5s (int16 "5")
    equal "5" (string 5s)

[<Fact>]
let ``Conversion string to int32 works`` () =
    equal 5 (int32 "5")
    equal "5" (string 5)

[<Fact>]
let ``Conversion string to int64 works`` () =
    equal 5L (int64 "5")
    equal "5" (string 5L)

[<Fact>]
let ``Conversion string to uint8 works`` () =
    equal 5uy (uint8 "5")
    equal "5" (string 5uy)

[<Fact>]
let ``Conversion string to uint16 works`` () =
    equal 5us (uint16 "5")
    equal "5" (string 5us)

[<Fact>]
let ``Conversion string to uint32 works`` () =
    equal 5u (uint32 "5")
    equal "5" (string 5u)

[<Fact>]
let ``Conversion string to uint64 works`` () =
    equal 5uL (uint64 "5")
    equal "5" (string 5uL)

[<Fact>]
let ``Conversion string to single works`` () =
    equal 5.f (float32 "5.0")
    equal -5.f (float32 "-5.0")
    (string 5.f).StartsWith("5") |> equal true
    equal 5.25f (float32 "5.25")
    (string 5.25f).StartsWith("5.25") |> equal true

[<Fact>]
let ``Conversion string to double works`` () =
    equal 5. (float "5.0")
    equal -5. (float "-5.0")
    (string 5.).StartsWith("5") |> equal true
    equal 5.25 (float "5.25")
    (string 5.25).StartsWith("5.25") |> equal true

[<Fact>]
let ``Conversion string to decimal works`` () =
    equal 5.m (decimal "5.0")
    equal -5.m (decimal "-5.0")
    (string 5.m).StartsWith("5") |> equal true
    equal 5.25m (decimal "5.25")
    (string 5.25m).StartsWith("5.25") |> equal true

// String - constructors

[<Fact>]
let ``String.ctor(char[]) works`` () =
    String([|'f'; 'a'; 'b'; 'l'; 'e'|])
    |> equal "fable"

[<Fact>]
let ``String.ctor(char, int) works`` () =
    String('f', 5)
    |> equal "fffff"

[<Fact>]
let ``String.ctor(char[], int, int) works`` () =
    String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3)
    |> equal "abl"

// String - static methods

[<Fact>]
let ``String.Equals works`` () =
    String.Equals("abc", "abc") |> equal true
    String.Equals("ABC", "abc") |> equal false
    String.Equals("abc", "abd") |> equal false
    "abc".Equals("abc") |> equal true
    "ABC".Equals("abc") |> equal false
    "abc".Equals("abd") |> equal false

[<Fact>]
let ``String.Equals with comparison works`` () =
    String.Equals("ABC", "abc", StringComparison.Ordinal) |> equal false
    String.Equals("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABC".Equals("abc", StringComparison.Ordinal) |> equal false
    "ABC".Equals("abc", StringComparison.OrdinalIgnoreCase) |> equal true

[<Fact>]
let ``String.CompareOrdinal works`` () =
    String.CompareOrdinal("abc", "abc") = 0 |> equal true
    String.CompareOrdinal("ABC", "abc") < 0 |> equal true
    String.CompareOrdinal("abc", "abd") < 0 |> equal true
    String.CompareOrdinal("bbc", "abd") > 0 |> equal true

[<Fact>]
let ``String.CompareOrdinal substring works`` () =
    String.CompareOrdinal("abc", 0, "bcd", 0, 3) < 0 |> equal true
    String.CompareOrdinal("abc", 1, "bcd", 0, 2) = 0 |> equal true
    String.CompareOrdinal("bbc", 0, "bcd", 0, 2) < 0 |> equal true

[<Fact>]
let ``String.CompareTo works`` () =
    // "ABC".CompareTo("abc") > 0 |> equal true // TODO: culture
    "abc".CompareTo("abc") = 0 |> equal true
    "abc".CompareTo("abd") < 0 |> equal true
    "bbc".CompareTo("abd") > 0 |> equal true

[<Fact>]
let ``String.Compare works`` () =
    // String.Compare("ABC", "abc") |> equal 1 // TODO: culture
    String.Compare("abc", "abc") |> equal 0
    String.Compare("abc", "abd") |> equal -1
    String.Compare("bbc", "abd") |> equal 1

[<Fact>]
let ``String.Compare case-insensitive works`` () =
    // String.Compare("ABC", "abc", false) |> equal 1 // TODO: culture
    String.Compare("ABC", "abc", true) |> equal 0
    String.Compare("ABC", "abd", true) |> equal -1
    String.Compare("BBC", "abd", true) |> equal 1

[<Fact>]
let ``String.Compare substring works`` () =
    String.Compare("abc", 0, "bcd", 0, 3) |> equal -1
    String.Compare("abc", 1, "bcd", 0, 2) |> equal 0

[<Fact>]
let ``String.Compare with comparison works`` () =
    // String.Compare("ABC", "abc", StringComparison.InvariantCulture) > 0 |> equal true
    String.Compare("ABC", "abc", StringComparison.Ordinal) < 0 |> equal true
    String.Compare("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal 0

[<Fact>]
let ``String.Compare substring with comparison works`` () =
    // String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.InvariantCulture) > 0 |> equal true
    String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.Ordinal) < 0 |> equal true
    String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.OrdinalIgnoreCase) |> equal 0

[<Fact>]
let ``String.IsNullOrEmpty works`` () =
    String.IsNullOrEmpty("") |> equal true
    String.IsNullOrEmpty(null) |> equal true
    String.IsNullOrEmpty("test") |> equal false
    String.IsNullOrEmpty(" \t") |> equal false

[<Fact>]
let ``String.IsNullOrWhiteSpace works`` () =
    String.IsNullOrWhiteSpace("") |> equal true
    String.IsNullOrWhiteSpace(null) |> equal true
    String.IsNullOrWhiteSpace("test") |> equal false
    String.IsNullOrWhiteSpace(" \t") |> equal true

// String - instance methods

[<Fact>]
let ``String.Contains works`` () =
    "ABC".Contains("B") |> equal true
    "ABC".Contains("Z") |> equal false

[<Fact>]
let ``String.PadLeft works`` () =
    "3.14".PadLeft(10) |> equal "      3.14"
    "333".PadLeft(1) |> equal "333"

[<Fact>]
let ``String.PadLeft with char works`` () =
    "22".PadLeft(10, '0') |> equal "0000000022"
    "22".PadLeft(1, '0') |> equal "22"

[<Fact>]
let ``String.PadRight works`` () =
    "3.14".PadRight(10) |> equal "3.14      "
    "333".PadRight(1) |> equal "333"

[<Fact>]
let ``String.PadRight with char works`` () =
    "-22".PadRight(10, 'x') |> equal "-22xxxxxxx"
    "-22".PadRight(1, 'x') |> equal "-22"

[<Fact>]
let ``String.Split with whitespace works`` () =
    "a b cd".Split() |> equal [|"a";"b";"cd"|]
    "a b c  d ".Split() |> equal [|"a";"b";"c";"";"d";""|]
    "a-b-c".Split() |> equal [|"a-b-c"|]
    "a-b-c".Split("") |> equal [|"a-b-c"|]
    "a\tb".Split() |> equal [|"a";"b"|]
    "a\nb".Split() |> equal [|"a";"b"|]
    "a\rb".Split() |> equal [|"a";"b"|]
    "a\u2003b".Split() |> equal [|"a";"b"|] // em space

[<Fact>]
let ``String.Split with null works`` () =
    "a b c  d".Split(null) |> equal [|"a";"b";"c";"";"d"|]
    "a\tb".Split(null) |> equal [|"a";"b"|]
    "a\u2003b".Split(null) |> equal [|"a";"b"|] // em space

[<Fact>]
let ``String.Split with single separator works`` () =
    "a b c  d".Split(' ') |> equal [|"a";"b";"c";"";"d"|]
    "---o---o---".Split("--", StringSplitOptions.None)
    |> equal [|""; "-o"; "-o"; "-"|];
    "a--b-c".Split([|"--"|], StringSplitOptions.None)
    |> equal [|"a";"b-c"|]

[<Fact>]
let ``String.Split with multiple char args works`` () =
    "a;b,c".Split(',', ';') |> equal [|"a"; "b"; "c"|]

// [<Fact>]
// let ``String.Split with string array works`` () =
//     "a;b,c".Split([|","; ";"|], StringSplitOptions.None)
//     |> equal [|"a"; "b"; "c"|]

[<Fact>]
let ``String.Split with RemoveEmptyEntries works`` () =
    "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    |> equal [|"a"; "b"; "c"; "d"|]
    " a-- b- c ".Split('-', StringSplitOptions.RemoveEmptyEntries)
    |> equal [|" a"; " b"; " c "|]
    "---o---o---".Split("--", StringSplitOptions.RemoveEmptyEntries)
    |> equal [|"-o"; "-o"; "-"|]
    ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
    |> equal [|"a"; "b"; "c"|]

[<Fact>]
let ``String.Split with TrimEntries works`` () =
    "a b c  d ".Split([|" "|], StringSplitOptions.TrimEntries)
    |> equal [|"a"; "b"; "c"; ""; "d"; ""|]
    " a-- b- c ".Split('-', StringSplitOptions.TrimEntries)
    |> equal [|"a"; ""; "b"; "c"|]
    "---o---o---".Split("--", StringSplitOptions.TrimEntries)
    |> equal [|""; "-o"; "-o"; "-"|]
    ";,a;b,c".Split([|','; ';'|], StringSplitOptions.TrimEntries)
    |> equal [|""; ""; "a"; "b"; "c"|]

[<Fact>]
let ``String.Split with RemoveEmptyEntries and TrimEntries works`` () =
    "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> equal [|"a"; "b"; "c"; "d"|]
    " a-- b- c ".Split('-', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> equal [|"a"; "b"; "c"|]
    "---o---o---".Split("--", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> equal [|"-o"; "-o"; "-"|]
    ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> equal [|"a"; "b"; "c"|]

[<Fact>]
let ``String.Split with count works`` () =
    "a b  c d".Split ([|' '|], 2)
    |> equal [|"a";"b  c d"|]
    " a-- b- c ".Split('-', 2, StringSplitOptions.None)
    |> equal [|" a"; "- b- c "|]
    "a-b-c".Split("-", 1)
    |> equal [|"a-b-c"|]
    "a-b-c".Split("", Int32.MaxValue)
    |> equal [|"a-b-c"|]

[<Fact>]
let ``String.Split with count and consecutive separators works`` () =
    "-----".Split("-", 4, StringSplitOptions.None)
    |> equal [|"";"";"";"--"|]
    "     ".Split(" ", 4, StringSplitOptions.TrimEntries)
    |> equal [|"";"";"";""|]
    "-----".Split("-", 4, StringSplitOptions.RemoveEmptyEntries)
    |> equal [||]
    "     ".Split(" ", 4, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> equal [||]

[<Fact>]
let ``String.Split with count and TrimEntries works`` () =
    " a-- b- c ".Split("-", 2, StringSplitOptions.TrimEntries)
    |> equal [|"a"; "- b- c"|]
    " a-- b- c ".Split('-', 3, StringSplitOptions.TrimEntries)
    |> equal [|"a"; ""; "b- c"|]
    "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.TrimEntries)
    |> equal [|"a"; ""; "b,c;d"|]

// [<Fact>]
// let ``String.Split with count and RemoveEmptyEntries works`` () =
//     " a-- b- c ".Split("-", 2, StringSplitOptions.RemoveEmptyEntries)
//     |> equal [|" a"; " b- c "|]
//     " a-- b- c ".Split('-', 3, StringSplitOptions.TrimEntries)
//     |> equal [|"a"; ""; "b- c"|]
//     "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.RemoveEmptyEntries)
//     |> equal [|"a";"b";"c;d"|]

// [<Fact>]
// let ``String.Split with count, RemoveEmptyEntries and TrimEntries works`` () =
//     " a-- b- c ".Split([| "-" |], 2, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
//     |> equal [|"a"; "b- c"|]
//     " a-- b- c ".Split([| '-' |], 3, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
//     |> equal  [|"a"; "b"; "c"|]
//     "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
//     |> equal [|"a";"b";"c;d"|]

[<Fact>]
let ``String.Replace works`` () =
    "abc abc abc".Replace("abc", "d") |> equal "d d d"
    // String.Replace does not get stuck in endless loop
    "...".Replace(".", "..") |> equal "......"

[<Fact>]
let ``Access char by index works`` () =
    let c = "abcd"[2]
    equal 'c' c
    equal 'd' (char ((int c) + 1))

[<Fact>]
let ``String.IndexOf char works`` () =
    "abcd".IndexOf('b') * 100 + "abcd".IndexOf('e')
    |> equal 99

[<Fact>]
let ``String.IndexOf char works with offset`` () =
    "abcdbc".IndexOf('b', 3)
    |> equal 4

[<Fact>]
let ``String.LastIndexOf char works`` () =
    "abcdbc".LastIndexOf('b') * 100 + "abcd".LastIndexOf('e')
    |> equal 399

[<Fact>]
let ``String.LastIndexOf char works with offset`` () =
    "abcdbcebc".LastIndexOf('b', 3)
    |> equal 1

[<Fact>]
let ``String.IndexOf works`` () =
    "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd")
    |> equal 99

[<Fact>]
let ``String.IndexOf works with offset`` () =
    "abcdbc".IndexOf("bc", 3)
    |> equal 4

[<Fact>]
let ``String.LastIndexOf works`` () =
    "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd")
    |> equal 399

[<Fact>]
let ``String.LastIndexOf works with offset`` () =
    "abcdbcebc".LastIndexOf("bc", 3)
    |> equal 1

[<Fact>]
let ``String.IndexOfAny works`` () =
    "abcdbcebc".IndexOfAny([|'b'|]) |> equal 1
    "abcdbcebc".IndexOfAny([|'b'|], 2) |> equal 4
    "abcdbcebc".IndexOfAny([|'b'|], 2, 2) |> equal -1
    "abcdbcebc".IndexOfAny([|'f';'e'|]) |> equal 6
    "abcdbcebc".IndexOfAny([|'f';'e'|], 2) |> equal 6
    "abcdbcebc".IndexOfAny([|'f';'e'|], 2, 4) |> equal -1
    "abcdbcebc".IndexOfAny([|'c';'b'|]) |> equal 1

[<Fact>]
let ``String.LastIndexOfAny works`` () =
    "abcdbcebc".LastIndexOfAny([|'b'|]) |> equal 7
    "abcdbcebc".LastIndexOfAny([|'b'|], 6) |> equal 4
    "abcdbcebc".LastIndexOfAny([|'b'|], 6, 2) |> equal -1
    "abcdbcebc".LastIndexOfAny([|'f';'e'|]) |> equal 6
    "abcdbcebc".LastIndexOfAny([|'f';'e'|], 6) |> equal 6
    "abcdbcebc".LastIndexOfAny([|'f';'e'|], 7, 1) |> equal -1

[<Fact>]
let ``String.StartsWith works`` () =
    "abcd".StartsWith("ab") |> equal true
    "abcd".StartsWith("cd") |> equal false
    "abcd".StartsWith("abcdx") |> equal false

[<Fact>]
let ``String.StartsWith char works`` () =
    "abcd".StartsWith('a') |> equal true
    "abcd".StartsWith('d') |> equal false

[<Fact>]
let ``String.StartsWith with StringComparison works`` () =
    let args = [("ab", true); ("cd", false); ("abcdx", false)]
    for arg in args do
        "ABCD".StartsWith(fst arg, StringComparison.OrdinalIgnoreCase)
        |> equal (snd arg)

[<Fact>]
let ``String.EndsWith works`` () =
    "abcd".EndsWith("ab") |> equal false
    "abcd".EndsWith("cd") |> equal true
    "abcd".EndsWith("abcdx") |> equal false

[<Fact>]
let ``String.EndsWith char works`` () =
    "abcd".EndsWith('a') |> equal false
    "abcd".EndsWith('d') |> equal true

[<Fact>]
let ``String.EndsWith with StringComparison works`` () =
    let args = [("ab", false); ("cd", true); ("abcdx", false)]
    for arg in args do
        "ABCD".EndsWith(fst arg, StringComparison.OrdinalIgnoreCase)
        |> equal (snd arg)

[<Fact>]
let ``String.Trim works`` () =
    "   abc   ".Trim()
    |> equal "abc"

[<Fact>]
let ``String.Trim with chars works`` () =
    @"\\\abc///".Trim('\\','/')
    |> equal "abc"

[<Fact>]
let ``String.Trim with special chars works`` () =
    @"()[]{}abc/.?*+-^$|\".Trim(@"()[]{}/.?*+-^$|\".ToCharArray())
    |> equal "abc"

[<Fact>]
let ``String.TrimStart works`` () =
    "!!--abc   ".TrimStart('!','-')
    |> equal "abc   "

[<Fact>]
let ``String.TrimStart with chars works`` () =
    "   abc   ".TrimStart()
    |> equal "abc   "

[<Fact>]
let ``String.TrimEnd works`` () =
    "   abc   ".TrimEnd()
    |> equal "   abc"

[<Fact>]
let ``String.TrimEnd with chars works`` () =
    "   abc??**".TrimEnd('*','?')
    |> equal "   abc"
    @"\foo\bar\".Replace("\\", "/").TrimEnd('/')
    |> equal "/foo/bar"

[<Fact>]
let ``String.Empty works`` () =
    let s = String.Empty
    s |> equal ""

[<Fact>]
let ``String.Chars works`` () =
    let input = "hello"
    input.Chars(2)
    |> equal 'l'

[<Fact>]
let ``String.Substring works`` () =
    "abcdefg".Substring(2)
    |> equal "cdefg"

[<Fact>]
let ``String.Substring works with length`` () =
    "abcdefg".Substring(2, 2)
    |> equal "cd"

[<Fact>]
let ``String.Substring throws error if startIndex or length are out of bounds`` () = // See #1955
    throwsAnyError (fun () -> "abcdefg".Substring(20))
    throwsAnyError (fun () -> "abcdefg".Substring(2, 10))

[<Fact>]
let ``String.ToUpper works`` () =
    "AbC".ToUpper() |> equal "ABC"

[<Fact>]
let ``String.ToLower works`` () =
    "aBc".ToLower() |> equal "abc"

[<Fact>]
let ``String.ToUpperInvariant works`` () =
    "AbC".ToUpperInvariant() |> equal "ABC"

[<Fact>]
let ``String.ToLowerInvariant works`` () =
    "aBc".ToLowerInvariant() |> equal "abc"

[<Fact>]
let ``String.Length works`` () =
    "AbC".Length |> equal 3
    "a\u2003b".Length |> equal 3
    ".\U0001f404.".Length
#if FABLE_COMPILER_RUST
    |> equal 3 // char is UTF32
#else
    |> equal 4 // char is UTF16
#endif

[<Fact>]
let ``String.Item works`` () =
    "AbC"[1] |> equal 'b'

[<Fact>]
let ``String.ToCharArray works`` () =
    let arr = "abcd".ToCharArray()
    arr |> equal [|'a';'b';'c';'d'|]

// [<Fact>]
// let ``String enumeration handles surrogates pairs`` () = // See #1279
//     let unicodeString = ".\U0001f404."
//     unicodeString |> List.ofSeq |> Seq.length |> equal 4
//     String.length unicodeString |> equal 4
//     let mutable len = 0
//     for i in unicodeString do
//         len <- len + 1
//     equal 4 len

[<Fact>]
let ``String.Join works`` () =
    String.Join("--", [|"a"; "b"; "c"|])
    |> equal "a--b--c"

[<Fact>]
let ``String.Join works II`` () =
    String.Join("--", "a", "b", "c")
    |> equal "a--b--c"

[<Fact>]
let ``String.Join with IEnumerable works`` () =
    String.Join("--", seq { yield "a"; yield "b"; yield "c" })
    |> equal "a--b--c"

[<Fact>]
let ``String.Join with indices works`` () =
    String.Join("**", [|"a"; "b"; "c"; "d"|], 1, 2)
    |> equal "b**c"
    String.Join("*", [|"a"; "b"; "c"; "d"|], 1, 3)
    |> equal "b*c*d"

// [<Fact>]
// let ``String.Join works with chars`` () = // See #1524
//     String.Join("--", 'a', 'b', 'c')
//     |> equal "a--b--c"
//     String.Join("--", seq { yield 'a'; yield 'b'; yield 'c' })
//     |> equal "a--b--c"
//     [0..10]
//     |> List.map (fun _ -> '*')
//     |> fun chars -> String.Join("", chars)
//     |> equal "***********"

// [<Fact>]
// let ``String.Join with big integers works`` () =
//     String.Join("--", [|3I; 5I|])
//     |> equal "3--5"
//     String.Join("--", 3I, 5I)
//     |> equal "3--5"

[<Fact>]
let ``String.Join with single argument works`` () = // See #1182
    String.Join(",", "abc") |> equal "abc"
    String.Join(",", [|"abc"|]) |> equal "abc"
    String.Join(",", ["abc"]) |> equal "abc"

[<Fact>]
let ``String.Concat works`` () =
    String.Concat([|"a"; "b"; "c"|])
    |> equal "abc"

[<Fact>]
let ``String.Concat works II`` () =
    String.Concat("a", "b", "c")
    |> equal "abc"

[<Fact>]
let ``String.Concat with IEnumerable works`` () =
    String.Concat(seq { yield "a"; yield "b"; yield "c" })
    |> equal "abc"

[<Fact>]
let ``String.Join with long array works`` () =
    let n = 1_000_000
    let a = [| for i in 1..n -> "a" |]
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``String.Join with long seq works`` () =
    let n = 1_000_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``String.Concat with long array works`` () =
    let n = 1_000_000
    let a = [| for i in 1..n -> "a" |]
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``String.Concat with long seq works`` () =
    let n = 1_000_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``String.concat with long array works`` () =
    let n = 1_000_000
    let a = [| for i in 1..n -> "a" |]
    let s = String.concat "" a
    s.Length |> equal n

[<Fact>]
let ``String.concat with long seq works`` () =
    let n = 1_000_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.concat "" a
    s.Length |> equal n

[<Fact>]
let ``String.Remove works`` () =
    "abcd".Remove(2) |> equal "ab"
    "abcd".Remove(1,2) |> equal "ad"
    "abcd".Remove(0,2) |> equal "cd"
    "abcd".Remove(0,4) |> equal ""
    "abcd".Remove(0,0) |> equal "abcd"

[<Fact>]
let ``String.Insert work`` () =
    "foobar".Insert(3, " is ")
    |> equal "foo is bar"

[<Fact>]
let ``String.IsNullOrWhiteSpace works on string with blanks`` () =
    String.IsNullOrWhiteSpace "Fri Jun 30 2017 12:30:00 GMT+0200 (Mitteleuropäische Sommerzeit)"
    |> equal false

[<Fact>]
let ``String.IsNullOrWhiteSpace works on blank only string`` () =
    String.IsNullOrWhiteSpace "    "
    |> equal true

// [<Fact>]
// let ``Enumerating string works`` () =
//     let mutable res = ""
//     for c in "HELLO" |> Seq.rev do
//         res <- res + (string c)
//     equal "OLLEH" res

// String - F# module functions

[<Fact>]
let ``String.concat works`` () =
    String.concat "--" ["a"; "b"; "c"] |> equal "a--b--c"
    seq { yield "a"; yield "b"; yield "c" }
    |> String.concat "-" |> equal "a-b-c"

[<Fact>]
let ``String.exists works`` () =
    "!!!" |> String.exists (fun c -> c = '!') |> equal true
    "a!a" |> String.exists (fun c -> c = '!') |> equal true
    "aaa" |> String.exists (fun c -> c = '!') |> equal false

[<Fact>]
let ``String.forall works`` () =
    "!!!" |> String.forall (fun c -> c = '!') |> equal true
    "a!a" |> String.forall (fun c -> c = '!') |> equal false
    "aaa" |> String.forall (fun c -> c = '!') |> equal false

[<Fact>]
let ``String.init works`` () =
    String.init 3 (fun i -> "a")
    |> equal "aaa"

[<Fact>]
let ``String.collect works`` () =
    "abc" |> String.collect (fun c -> "bcd")
    |> equal "bcdbcdbcd"

[<Fact>]
let ``String.iter works`` () =
    let mutable res = ""
    "Hello world!"
    |> String.iter (fun c -> res <- res + c.ToString())
    equal "Hello world!" res

[<Fact>]
let ``String.iteri works`` () =
    let mutable res = ""
    "Hello world!"
    |> String.iteri (fun c i -> res <- res + i.ToString() + c.ToString())
    equal "H0e1l2l3o4 5w6o7r8l9d10!11" res

[<Fact>]
let ``String.length works`` () =
    String.length "AbC" |> equal 3
    String.length "a\u2003b" |> equal 3
    String.length ".\U0001f404."
#if FABLE_COMPILER_RUST
    |> equal 3 // char is UTF32
#else
    |> equal 4 // char is UTF16
#endif

[<Fact>]
let ``String.map works`` () =
    "Hello world!" |> String.map (fun c -> if c = 'H' then '_' else c)
    |> equal "_ello world!"

[<Fact>]
let ``String.mapi works`` () =
    "Hello world!" |> String.mapi (fun i c -> if i = 1 || c = 'H' then '_' else c)
    |> equal "__llo world!"

[<Fact>]
let ``String.replicate works`` () =
    String.replicate 3 "hi there"
    |> equal "hi therehi therehi there"

[<Fact>]
let ``String.filter works`` () =
    String.filter (fun x -> x <> '.') "a.b.c"
    |> equal "abc"

[<Fact>]
let ``String.filter works when predicate matches everything`` () =
    String.filter (fun x -> x <> '.') "abc"
    |> equal "abc"

[<Fact>]
let ``String.filter works when predicate doesn't match`` () =
    String.filter (fun x -> x <> '.') "..."
    |> equal ""

[<Fact>]
let ``String.filter works with empty string`` () =
    String.filter (fun x -> x <> '.') ""
    |> equal ""

// See #1628, though I'm not sure if the compiled tests are passing just the function reference without wrapping it
[<Fact>]
let ``String.filter with Char.IsDigit as a predicate doesn't hang`` () =
    "Hello! 123" |> String.filter Char.IsDigit
    |> equal "123"

// #if FABLE_COMPILER
// [<Fact>]
// let ``Environment.NewLine works`` () =
//     Environment.NewLine
//     |> equal "\n"
// #endif

// [<Fact>]
// let ``Uri.UnescapeDataString works`` () =
//     Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"
//     Uri.UnescapeDataString("http%3A%2F%2Fkvz.io%2F") |> equal "http://kvz.io/"
//     Uri.UnescapeDataString("http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a")
//     |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

// [<Fact>]
// let ``Uri.EscapeDataString works`` () =
//     Uri.EscapeDataString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld%21"
//     Uri.EscapeDataString("http://kvz.io/") |> equal "http%3A%2F%2Fkvz.io%2F"
//     Uri.EscapeDataString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
//     |> equal "http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a"

// [<Fact>]
// let ``Uri.EscapeUriString works`` () =
//     Uri.EscapeUriString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld!"
//     Uri.EscapeUriString("http://kvz.io/") |> equal "http://kvz.io/"
//     Uri.EscapeUriString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
//     |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

// [<Fact>]
// let ``Can create FormattableString`` () =
//     let orderAmount = 100
//     let convert (s: FormattableString) = s
//     let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
//     s.Format |> equal "You owe: {0:N5} {1} {2}"
//     s.ArgumentCount |> equal 3
//     s.GetArgument(2) |> equal (box true)
//     s.GetArguments() |> equal [|100; 3; true|]
//     let s2: FormattableString = $"""{5 + 2}This is "{"really"}" awesome!"""
//     s2.Format |> equal "{0}This is \"{1}\" awesome!"
//     s2.GetArguments() |> equal [|box 7; box "really"|]
//     let s3: FormattableString = $"""I have no holes"""
//     s3.Format |> equal "I have no holes"
//     s3.GetArguments() |> equal [||]
//     let s4: FormattableString = $"I have `backticks`"
//     s4.Format |> equal "I have `backticks`"
//     let s5: FormattableString = $"I have {{escaped braces}} and %%percentage%%"
//     s5.Format |> equal "I have {{escaped braces}} and %percentage%"
//     ()

// #if FABLE_COMPILER
// [<Fact>]
// let ``Can use FormattableString.GetStrings() extension`` () =
//     let orderAmount = 100
//     let convert (s: FormattableString) = s
//     let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
//     s.GetStrings() |> equal [|"You owe: "; " "; " "; ""|]
//     let s2: FormattableString = $"""{5 + 2}This is "{"really"}" awesome!"""
//     s2.GetStrings() |> equal [|""; "This is \""; "\" awesome!"|]
//     let s3: FormattableString = $"""I have no holes"""
//     s3.GetStrings() |> equal [|"I have no holes"|]
// #endif
