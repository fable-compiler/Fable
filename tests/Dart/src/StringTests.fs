module Fable.Tests.Dart.String

open System
open Util

module M =
    let f x = nameof x

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

type MyUnion = Bar of int * int | Foo1 of float | Foo3 | Foo4 of MyUnion

type Test(i: int) =
      override _.ToString() = string(i + i)

//let spr fmt =
//    let fmt = Printf.StringFormat<_>(fmt)
//    sprintf fmt

let containsInOrder (substrings: string list) (str: string) =
    let mutable lastIndex = -1
    substrings |> List.forall (fun s ->
      let i = str.IndexOf(s)
      let success = i >= 0 && i > lastIndex
      lastIndex <- i
      success)

let tests() =
      // This test belongs to Char module, but it's the only char test we support for now
      testCase "Char addition works" <| fun _ ->
            'A' + 'B' |> int |> equal 131
            'A' + char 7 |> int |> equal 72

      // testCase "Char subtraction works" <| fun _ ->
      //       'B' - 'A' |> int |> equal 1
      //       char 9 - char 7 |> int |> equal 2

      // TODO: Char module
      // See #1628, though I'm not sure if the compiled tests are passing just the function reference without wrapping it
      // testCase "Passing Char.IsDigit as a function reference doesn't make String.filter hang" <| fun () ->
      //       "Hello! 123" |> String.filter System.Char.IsDigit |> equal "123"

      testCase "Strings can be indexed" <| fun () ->
        let s = "bar"
        let s2 = s.ToCharArray()
        let s3 = [|'b'; 'a'; 'r'|]
        let c = s[0]
        let c2 = s2[1]
        let c3 = s3[2]
        equal c 'b'
        equal c2 'a'
        equal c3 'r'
        "おはよう" |> Seq.item 2 |> equal 'よ'

      testCase "String slicing works" <| fun () ->
           let s = "cat and dog"
           s.[2..8] |> equal "t and d"
           s.[2..] |> equal "t and dog"
           s.[..8] |> equal "cat and d"

      testCase "String literal addition is optimized" <| fun () ->
            "bar" + aLiteral |> equal "barfoo"
            "bar" + notALiteral |> equal "barfoo"
            obj.ReferenceEquals("bar" + aLiteral, "barfoo") |> equal true
            obj.ReferenceEquals("bar" + notALiteral, "barfoo") |> equal false

      testCase "String chunkBySize works" <| fun () -> // See #1296
            "fffff" |> Seq.chunkBySize 3 |> Seq.map String |> Seq.toList
            |> equal ["fff"; "ff"]

    // TODO: StringBuilder
//      testCase "StringBuilder works" <| fun () ->
//            let sb = System.Text.StringBuilder()
//            sb.Append "Hello" |> ignore
//            sb.AppendLine () |> ignore
//            sb.AppendLine "World!" |> ignore
//            let expected = System.Text.StringBuilder()
//                              .AppendFormat("Hello{0}World!{0}", Environment.NewLine)
//                              .ToString()
//            sb.ToString() |> equal expected
//
//      testCase "StringBuilder.Lengh works" <| fun () ->
//            let sb = System.Text.StringBuilder()
//            sb.Append("Hello") |> ignore
//            // We don't test the AppendLine for Length because depending on the OS
//            // the result is different. Unix \n VS Windows \r\n
//            // sb.AppendLine() |> ignore
//            equal 5 sb.Length
//
//      testCase "StringBuilder.ToString works with index and length" <| fun () ->
//            let sb = System.Text.StringBuilder()
//            sb.Append("Hello") |> ignore
//            sb.AppendLine() |> ignore
//            equal "ll" (sb.ToString(2, 2))
//
//      testCase "StringBuilder.Clear works" <| fun () ->
//            let builder = new System.Text.StringBuilder()
//            builder.Append("1111") |> ignore
//            builder.Clear() |> ignore
//            equal "" (builder.ToString())
//
//      testCase "StringBuilder.Append works with various overloads" <| fun () ->
//            let builder = Text.StringBuilder()
//                              .Append(Text.StringBuilder "aaa")
//                              .Append("bcd".ToCharArray())
//                              .Append('/')
//                              .Append(true)
//                              .Append(5.2)
//                              .Append(34)
//            equal "aaabcd/true5.234" (builder.ToString().Replace(",", ".").ToLower())

      // TODO: Union string formatting
      // testCase "Unions with string operator" <| fun () ->
      //       Bar(1,5) |> string |> equal "Bar (1, 5)"
      //       Foo1 4.5 |> string |> equal "Foo1 4.5"
      //       Foo4 Foo3 |> string |> equal "Foo4 Foo3"
      //       Foo4(Foo1 4.5) |> string |> equal "Foo4 (Foo1 4.5)"
      //       Foo3 |> string |> equal "Foo3"

      testCase "F# nameof works" <| fun () ->
          M.f 12 |> equal "x"
          nameof M |> equal "M"
          nameof M.f |> equal "f"

      testCase "string interpolation works" <| fun () ->
          let name = "Phillip"
          let age = 29
          $"Name: {name}, Age: %i{age}"
          |> equal "Name: Phillip, Age: 29"

      testCase "string interpolation works with inline expressions" <| fun () ->
          $"I think {3.0 + 0.14} is close to {Math.PI.ToString().Substring(0, 10)}!".Replace(",", ".")
          |> equal "I think 3.14 is close to 3.14159265!"

      testCase "string interpolation works with anonymous records" <| fun () ->
          let person =
              {| Name = "John"
                 Surname = "Doe"
                 Age = 32
                 Country = "The United Kingdom" |}
          $"Hi! My name is %s{person.Name} %s{person.Surname.ToUpper()}. I'm %i{person.Age} years old and I'm from %s{person.Country}!"
          |> equal "Hi! My name is John DOE. I'm 32 years old and I'm from The United Kingdom!"

      testCase "Interpolated strings keep empty lines" <| fun () ->
        let s1 = $"""1


    {1+1}


3"""
        let s2 = """1


    2


3"""
        equal s1 s2
        equal s1.Length s2.Length
        equal 13 s1.Length

      testCase "Can use backslash is interpolated strings" <| fun () ->
        $"(\n{1+1}\n)" |> equal """(
2
)"""

      testCase "Backslash is escaped in interpolated strings" <| fun () -> // See #2649
            $"\\" |> equal @"\"
            $"\\".Length |> equal 1
            $@"\" |> equal @"\"
            $@"\".Length |> equal 1
            @$"\" |> equal @"\"
            @$"\".Length |> equal 1
            $"\\{4}" |> equal @"\4"
            $"\\{4}".Length |> equal 2
            $@"\{4}" |> equal @"\4"
            $@"\{4}".Length |> equal 2
            @$"\{4}" |> equal @"\4"
            @$"\{4}".Length |> equal 2

      // TODO: Formatting
//      testCase "kprintf works" <| fun () ->
//            let f (s:string) = s + "XX"
//            Printf.kprintf f "hello" |> equal "helloXX"
//            Printf.kprintf f "%X" 255 |> equal "FFXX"
//            Printf.kprintf f "%.2f %g" 0.5468989 5. |> equal "0.55 5XX"
//
//      testCase "kprintf works indirectly" <| fun () -> // See #1204
//            let lines = ResizeArray<string>()
//            let linef fmt = Printf.ksprintf lines.Add fmt // broken
//            linef "open %s" "Foo"
//            lines |> Seq.toList |> equal ["open Foo"]
//
//      testCase "kbprintf works" <| fun () ->
//            let sb = System.Text.StringBuilder()
//            let mutable i = 0
//            let f () = i <- i + 1
//            Printf.kbprintf f sb "Hello"
//            Printf.kbprintf f sb " %s!" "world"
//            i |> equal 2
//            sb.ToString() |> equal "Hello world!"
//
//      testCase "ksprintf curries correctly" <| fun () ->
//          let append (a: string) b = a + b
//
//          let step1 = Printf.ksprintf append "%d"
//          let step2 = step1 42
//          let result = step2 "The answer is: "
//          result |> equal "42The answer is: "
//
//      testCase "bprintf works" <| fun () ->
//            let sb = System.Text.StringBuilder(10)
//            Printf.bprintf sb "Hello"
//            Printf.bprintf sb " %s!" "world"
//            sb.ToString() |> equal "Hello world!"
//
//      testCase "sprintf works" <| fun () ->
//            // Immediately applied
//            sprintf "%.2f %g" 0.5468989 5.
//            |> equal "0.55 5"
//            // Curried
//            let printer = sprintf "Hi %s, good %s!"
//            let printer = printer "Alfonso"
//            printer "morning" |> equal "Hi Alfonso, good morning!"
//            printer "evening" |> equal "Hi Alfonso, good evening!"
//
//      testCase "sprintf works II" <| fun () ->
//            let printer2 = sprintf "Hi %s, good %s%s" "Maxime"
//            let printer2 = printer2 "afternoon"
//            printer2 "?" |> equal "Hi Maxime, good afternoon?"
//
//      testCase "sprintf with different decimal digits works" <| fun () -> // See #1932
//          sprintf "Percent: %.0f%%" 5.0 |> equal "Percent: 5%"
//          sprintf "Percent: %.2f%%" 5. |> equal "Percent: 5.00%"
//          sprintf "Percent: %.1f%%" 5.24 |> equal "Percent: 5.2%"
//          sprintf "Percent: %.2f%%" 5.268 |> equal "Percent: 5.27%"
//          sprintf "Percent: %f%%" 5.67 |> equal "Percent: 5.670000%"

//      testCase "sprintf with double % should be unescaped" <| fun () ->
//            sprintf "%d%%" 100 |> equal "100%"
//
//      testCase "sprintf displays sign correctly" <| fun () -> // See #1937
//          sprintf "%i" 1 |> equal "1"
//          sprintf "%d" 1 |> equal "1"
//          sprintf "%d" 1L |> equal "1"
//          sprintf "%.2f" 1. |> equal "1.00"
//          sprintf "%i" -1 |> equal "-1"
//          sprintf "%d" -1 |> equal "-1"
//          sprintf "%d" -1L |> equal "-1"
//          sprintf "%.2f" -1. |> equal "-1.00"
//
//      testCase "format string can use and compose string literals" <| fun () ->
//            let renderedCoordinates = sprintf formatCoordinateBody 0.25 0.75
//            let renderedText = sprintf fullFormat 0.25 0.75
//
//            equal "(0.250000,0.750000)" renderedCoordinates
//            equal "Person at coordinates(0.250000,0.750000)" renderedText
//
//      testCase "Print.sprintf works" <| fun () -> // See #1216
//            let res = Printf.sprintf "%s" "abc"
//            equal "res: abc" ("res: " + res)
//
//      testCase "sprintf without arguments works" <| fun () ->
//            sprintf "hello" |> equal "hello"
//
//      testCase "input of print format can be retrieved" <| fun () ->
//            let pathScan (pf:PrintfFormat<_,_,_,_,'t>) =
//                let formatStr = pf.Value
//                formatStr
//
//            equal "/hello/%s" (pathScan "/hello/%s")
//
//      testCase "sprintf with escaped percent symbols works" <| fun () -> // See #195
//            let r, r1, r2 = "Ratio", 0.213849, 0.799898
//            sprintf "%s1: %.2f%% %s2: %.2f%%" r (r1*100.) r (r2*100.)
//            |> equal "Ratio1: 21.38% Ratio2: 79.99%"
//
//      testCase "sprintf with percent symbols in arguments works" <| fun () -> // See #329
//            let same s = sprintf "%s" s |> equal s
//            same "%"
//            same "%%"
//            same "%%%"
//            same "%%%%"
//            same "% %"
//            same "%% %%"
//            same "%% % % %%"
//
//      testCase "Fix #2398: Exception when two successive string format placeholders and value of first one ends in `%`" <| fun () ->
//          sprintf "%c%s" '%' "text" |> equal "%text"
//
//      testCase "Unions with sprintf %A" <| fun () ->
//            Bar(1,5) |> sprintf "%A" |> equal "Bar (1, 5)"
//            Foo1 4.5 |> sprintf "%A" |> equal "Foo1 4.5"
//            Foo4 Foo3 |> sprintf "%A" |> equal "Foo4 Foo3"
//            Foo4(Foo1 4.5) |> sprintf "%A" |> equal "Foo4 (Foo1 4.5)"
//            Foo3 |> sprintf "%A" |> equal "Foo3"
//
//      testCase "sprintf \"%O\" with overloaded string works" <| fun () ->
//            let o = Test(5)
//            sprintf "%O" o |> equal "10"
//
//      testCase "sprintf \"%A\" with overloaded string works" <| fun () ->
//            let o = Test(5)
//            (sprintf "%A" o).Replace("\"", "") |> equal "10"
//
//      testCase "Extended string interpolation syntax" <| fun () ->
//            let classAttr = "item-panel"
//            let cssNew = $$""".{{classAttr}}:hover {background-color: #eee;}"""//
//            cssNew |> equal ".item-panel:hover {background-color: #eee;}"
//
//      testCase "sprintf \"%A\" with lists works" <| fun () ->
//            let xs = ["Hi"; "Hello"; "Hola"]
//            (sprintf "%A" xs).Replace("\"", "") |> equal "[Hi; Hello; Hola]"
//
//      testCase "sprintf \"%A\" with nested lists works" <| fun () ->
//            let xs = [["Hi"]; ["Hello"]; ["Hola"]]
//            (sprintf "%A" xs).Replace("\"", "") |> equal "[[Hi]; [Hello]; [Hola]]"
//
//      testCase "sprintf \"%A\" with sequences works" <| fun () ->
//            let xs = seq { "Hi"; "Hello"; "Hola" }
//            sprintf "%A" xs |> containsInOrder ["Hi"; "Hello"; "Hola"] |> equal true
//
//      testCase "Storing result of Seq.tail and printing the result several times works. Related to #1996" <| fun () ->
//            let tweets = seq { "Hi"; "Hello"; "Hola" }
//            let tweetsTailR: seq<string> = tweets |> Seq.tail
//
//            let a = sprintf "%A" (tweetsTailR)
//            let b = sprintf "%A" (tweetsTailR)
//
//            containsInOrder ["Hello"; "Hola"] a |> equal true
//            containsInOrder ["Hello"; "Hola"] b |> equal true
//
//      testCase "sprintf \"%X\" works" <| fun () ->
//            //These should all be the Native JS Versions (except int64 / uint64)
//            //See #1530 for more information.
//
//            sprintf "255: %X" 255 |> equal "255: FF"
//            sprintf "255: %x" 255 |> equal "255: ff"
//            sprintf "-255: %X" -255 |> equal "-255: FFFFFF01"
//            sprintf "4095L: %X" 4095L |> equal "4095L: FFF"
//            sprintf "-4095L: %X" -4095L |> equal "-4095L: FFFFFFFFFFFFF001"
//            sprintf "1 <<< 31: %x" (1 <<< 31) |> equal "1 <<< 31: 80000000"
//            sprintf "1u <<< 31: %x" (1u <<< 31) |> equal "1u <<< 31: 80000000"
//            sprintf "2147483649L: %x" 2147483649L |> equal "2147483649L: 80000001"
//            sprintf "2147483650uL: %x" 2147483650uL |> equal "2147483650uL: 80000002"
//            sprintf "1L <<< 63: %x" (1L <<< 63) |> equal "1L <<< 63: 8000000000000000"
//            sprintf "1uL <<< 63: %x" (1uL <<< 63) |> equal "1uL <<< 63: 8000000000000000"
//
//      testCase "sprintf integers with sign and padding works" <| fun () -> // See #1931
//          sprintf "%+04i" 1 |> equal "+001"
//          sprintf "%+04i" -1 |> equal "-001"
//          sprintf "%5d" -5 |> equal "   -5"
//          sprintf "%5d" -5L |> equal "   -5"
//          sprintf "%- 4i" 5 |> equal " 5  "
//
//      testCase "parameterized padding works" <| fun () -> // See #2336
//          sprintf "[%*s][%*s]" 6 "Hello" 5 "Foo"
//          |> equal "[ Hello][  Foo]"
//
//      testCase "String.Format should fail if there are less arguments than placeholders" <| fun () -> // See #2768
//            throwsAnyError <| fun () -> String.Format ("Hello {0}", args = Array.empty)
//
//      testCase "String.Format combining padding and zeroes pattern works" <| fun () ->
//          String.Format(CultureInfo.InvariantCulture, "{0:++0.00++}", -5000.5657) |> equal "-++5000.57++"
//          String.Format(CultureInfo.InvariantCulture, "{0:000.00}foo", 5) |> equal "005.00foo"
//          String.Format(CultureInfo.InvariantCulture, "{0,-8:000.00}foo", 12.456) |> equal "012.46  foo"
//
//      testCase "String.Format {0:x} works" <| fun () ->
//            //See above comment on expected values
//            String.Format(CultureInfo.InvariantCulture, "255: {0:X}", 255) |> equal "255: FF"
//            String.Format(CultureInfo.InvariantCulture, "255: {0:x}", 255) |> equal "255: ff"
//            String.Format(CultureInfo.InvariantCulture, "-255: {0:X}", -255) |> equal "-255: FFFFFF01"
//            String.Format(CultureInfo.InvariantCulture, "4095L: {0:X}", 4095L) |> equal "4095L: FFF"
//            String.Format(CultureInfo.InvariantCulture, "-4095L: {0:X}", -4095L) |> equal "-4095L: FFFFFFFFFFFFF001"
//            String.Format(CultureInfo.InvariantCulture, "1 <<< 31: {0:x}", (1 <<< 31)) |> equal "1 <<< 31: 80000000"
//            String.Format(CultureInfo.InvariantCulture, "1u <<< 31: {0:x}", (1u <<< 31)) |> equal "1u <<< 31: 80000000"
//            String.Format(CultureInfo.InvariantCulture, "2147483649L: {0:x}", 2147483649L) |> equal "2147483649L: 80000001"
//            String.Format(CultureInfo.InvariantCulture, "2147483650uL: {0:x}", 2147483650uL) |> equal "2147483650uL: 80000002"
//            String.Format(CultureInfo.InvariantCulture, "1L <<< 63: {0:x}", (1L <<< 63)) |> equal "1L <<< 63: 8000000000000000"
//            String.Format(CultureInfo.InvariantCulture, "1uL <<< 63: {0:x}", (1uL <<< 63)) |> equal "1uL <<< 63: 8000000000000000"
//
//      testCase "String.Format {0:x} with precision works" <| fun () ->
//            String.Format(CultureInfo.InvariantCulture, "#{0:X3}", 0xC149D) |> equal "#C149D"
//            String.Format(CultureInfo.InvariantCulture, "#{0:X6}", 0xC149D) |> equal "#0C149D"
//
//      testCase "String.Format works with thousands separator" <| fun () ->
//            String.Format(CultureInfo.InvariantCulture, "{0}", 12343235354.6547757) |> equal "12343235354.654776"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##.000}", 12343235354.6547757) |> equal "12,343,235,354.655"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##.000}", 12343235354.6547757M) |> equal "12,343,235,354.655"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123.456) |> equal "123.46"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123.456M) |> equal "123.46"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##.00}", 123438192123.456M) |> equal "123,438,192,123.46"
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##}", 1.456M) |> equal "1"
//            String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 1.456M) |> equal "01"
//
//      testCase "String.Format can omit decimal digits" <| fun () ->
//            String.Format(CultureInfo.InvariantCulture, "{0:#,##}", 12343235354.6547757) |> equal "12,343,235,355"
//            String.Format(CultureInfo.InvariantCulture, "{0:0,00}", 12343235354.6547757) |> equal "12,343,235,355"
//            String.Format(CultureInfo.InvariantCulture, "{0:0,00}", 12343235354.) |> equal "12,343,235,354"
//            String.Format(CultureInfo.InvariantCulture, "{0:#}", 12343235354.) |> equal "12343235354"
//            String.Format(CultureInfo.InvariantCulture, "{0:0}", 12343235354.) |> equal "12343235354"
//
//            String.Format(CultureInfo.InvariantCulture, "{0:#,#}", 12343235354.6547757M) |> equal "12,343,235,355"
//            String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 12343235354.6547757M) |> equal "12,343,235,355"
//
//            String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 1234323535) |> equal "1,234,323,535"
//            String.Format(CultureInfo.InvariantCulture, "{0:#}", 1234323535) |> equal "1234323535"
//            String.Format(CultureInfo.InvariantCulture, "{0:0}", 1234323535) |> equal "1234323535"
//
//            String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 12343235354M) |> equal "12,343,235,354"
//            String.Format(CultureInfo.InvariantCulture, "{0:0,0}", 343235354M) |> equal "343,235,354"
//            String.Format(CultureInfo.InvariantCulture, "{0:#}", 12343235354M) |> equal "12343235354"
//            String.Format(CultureInfo.InvariantCulture, "{0:0}", 12343235354M) |> equal "12343235354"
//
//      testCase "ToString formatted works with decimals" <| fun () -> // See #2276
//          let decimal = 78.6M
//          decimal.ToString("0.000").Replace(",", ".") |> equal "78.600"
//
//      testCase "Printf works with generic argument" <| fun () ->
//          spr "bar %s" "a" |> equal "bar a"
//          spr "foo %i %i" 3 5 |> equal "foo 3 5"
//          let f1 = spr "foo %i %i"
//          let f2 = f1 2
//          f2 2 |> equal "foo 2 2"
//          let f1 = spr "foo %i %i %i"
//          let f2 = f1 2
//          let f3 = f2 2
//          f3 2 |> equal "foo 2 2 2"
//
//      testCase "Printf in sequence is not erased" <| fun () ->
//          let x = sprintf "Foo"
//          let y = sprintf "B%sr" "a"
//          x + y |> equal "FooBar"
//
//      testCase "String.Format works" <| fun () ->
//            let arg1, arg2, arg3 = "F#", "Fable", "Babel"
//            String.Format(CultureInfo.InvariantCulture, "{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
//            |> equal "Babel is to Fable what Fable is to F#"
//
//      testCase "String.Format with extra formatting works" <| fun () ->
//            let i = 0.5466788
//            let dt = DateTime(2014, 9, 26).AddMinutes(19.)
//            String.Format(CultureInfo.InvariantCulture, "{0:F2} {0:P2} {1:yyyy-MM-dd HH:mm}", i, dt)
//                  .Replace(",", ".").Replace(" %", "%")
//            |> equal "0.55 54.67% 2014-09-26 00:19"
//
//      testCase "Padding works" <| fun () ->
//          "3.14".PadLeft(10)      |> equal "      3.14"
//          "3.14".PadRight(10)     |> equal "3.14      "
//          "22".PadLeft(10, '0')   |> equal "0000000022"
//          "-22".PadRight(10, 'X') |> equal "-22XXXXXXX"
//          "333".PadLeft(1) |> equal "333"
//
//      testCase "Padding with sprintf works" <| fun () ->
//          sprintf "%10.1f" 3.14  |> equal "       3.1"
//          sprintf "%-10.1f" 3.14 |> equal "3.1       "
//          sprintf "%+010i" 22    |> equal "+000000022"
//          sprintf "%+0-10i" -22  |> equal "-22       "
//
//      testCase "Padding with String.Format works" <| fun () ->
//          String.Format(CultureInfo.InvariantCulture, "{0,10:F1}", 3.14)  |> equal "       3.1"
//          String.Format(CultureInfo.InvariantCulture, "{0,-10:F1}", 3.14) |> equal "3.1       "
//          String.Format(CultureInfo.InvariantCulture, "{0,10}", 22)       |> equal "        22"
//          String.Format(CultureInfo.InvariantCulture, "{0,-10}", -22)     |> equal "-22       "

      // Conversions

      testCase "Conversion char to int works" <| fun () ->
            equal 97 (int 'a')
            equal 'a' (char 97)

      testCase "Conversion string to char works" <| fun () ->
            equal 'a' (char "a")
            equal "a" (string 'a')

      testCase "Conversion string to negative int8 works" <| fun () ->
            equal -5y (int8 "-5")
            equal "-5" (string -5y)

      testCase "Conversion string to negative int16 works" <| fun () ->
            equal -5s (int16 "-5")
            equal "-5" (string -5s)

      testCase "Conversion string to negative int32 works" <| fun () ->
            equal -5 (int32 "-5")
            equal "-5" (string -5)

      testCase "Conversion string to negative int64 works" <| fun () ->
            equal -5L (int64 "-5")
            equal "-5" (string -5L)

      testCase "Conversion string to int8 works" <| fun () ->
            equal 5y (int8 "5")
            equal "5" (string 5y)

      testCase "Conversion string to int16 works" <| fun () ->
            equal 5s (int16 "5")
            equal "5" (string 5s)

      testCase "Conversion string to int32 works" <| fun () ->
            equal 5 (int32 "5")
            equal "5" (string 5)

      testCase "Conversion string to int64 works" <| fun () ->
            equal 5L (int64 "5")
            equal "5" (string 5L)

      testCase "Conversion string to uint8 works" <| fun () ->
            equal 5uy (uint8 "5")
            equal "5" (string 5uy)

      testCase "Conversion string to uint16 works" <| fun () ->
            equal 5us (uint16 "5")
            equal "5" (string 5us)

      testCase "Conversion string to uint32 works" <| fun () ->
            equal 5u (uint32 "5")
            equal "5" (string 5u)

      testCase "Conversion string to uint64 works" <| fun () ->
            equal 5uL (uint64 "5")
            equal "5" (string 5uL)

      testCase "Conversion string to single works" <| fun () ->
            equal 5.f (float32 "5.0")
            equal -5.f (float32 "-5.0")
            (string 5.f).StartsWith("5") |> equal true
            equal 5.25f (float32 "5.25")
            (string 5.25f).StartsWith("5.25") |> equal true

      testCase "Conversion string to double works" <| fun () ->
            equal 5. (float "5.0")
            equal -5. (float "-5.0")
            (string 5.).StartsWith("5") |> equal true
            equal 5.25 (float "5.25")
            (string 5.25).StartsWith("5.25") |> equal true

      // testCase "Conversion string to decimal works" <| fun () ->
      //       equal 5.m (decimal "5.0")
      //       equal -5.m (decimal "-5.0")
      //       (string 5.m).StartsWith("5") |> equal true
      //       equal 5.25m (decimal "5.25")
      //       (string 5.25m).StartsWith("5.25") |> equal true

      // System.String - constructors

      testCase "String.ctor(char[]) works" <| fun () ->
            System.String([|'f'; 'a'; 'b'; 'l'; 'e'|])
            |> equal "fable"

      testCase "String.ctor(char, int) works" <| fun () ->
            System.String('f', 5)
            |> equal "fffff"

      testCase "String.ctor(char[], int, int) works" <| fun () ->
            System.String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3)
            |> equal "abl"

      // System.String - static methods

      testCase "System.String.Equals works" <| fun () ->
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

      // TODO: Dart seems to compare lower/uppercase strings differently as how .NET does
      testCase "String.Compare works" <| fun () ->
            // "ABC".CompareTo("abc") > 0 |> equal true
            // System.String.Compare("abc", "abc") |> equal 0
            // System.String.Compare("ABC", "abc") |> equal 1
            // System.String.Compare("abc", "abd") |> equal -1
            // System.String.Compare("bbc", "abd") |> equal 1
            // System.String.Compare("ABC", "abc", false) |> equal 1
            System.String.Compare("ABC", "abc", true) |> equal 0
            System.String.Compare("ABC", "abd", true) |> equal -1
            System.String.Compare("BBC", "abd", true) |> equal 1
            // System.String.Compare("ABC", "abc", StringComparison.CurrentCulture) > 0 |> equal true
            // System.String.Compare("ABC", "abc", StringComparison.Ordinal) < 0 |> equal true
            System.String.Compare("ABC", "abc", StringComparison.CurrentCultureIgnoreCase) |> equal 0
            System.String.Compare("ABC", "abc", StringComparison.InvariantCultureIgnoreCase) |> equal 0
            System.String.Compare("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal 0
            // System.String.Compare("abc", 0, "b", 0, 3) |> equal -1
            // System.String.Compare("abc", 1, "bcd", 0, 2) |> equal 0
            // System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.CurrentCulture) > 0 |> equal true
            // System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.Ordinal) < 0 |> equal true
            System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.CurrentCultureIgnoreCase) |> equal 0
            System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.InvariantCultureIgnoreCase) |> equal 0
            System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.OrdinalIgnoreCase) |> equal 0

      testCase "String.IsNullOrEmpty works" <| fun () ->
            let args = [("", true); (null, true); ("test", false); (" \t", false)]
            for arg in args do
                  System.String.IsNullOrEmpty(fst arg)
                  |> equal (snd arg)

      testCase "String.IsNullOrWhiteSpace works" <| fun () ->
            let args = [("", true); (null, true); ("test", false); (" \t", true)]
            for arg in args do
                  System.String.IsNullOrWhiteSpace(fst arg)
                  |> equal (snd arg)

      // System.String - instance methods

      testCase "String.Contains works" <| fun () ->
            "ABC".Contains("B") |> equal true
            "ABC".Contains("Z") |> equal false

      testCase "String.Split works" <| fun () ->
            "a b c  d".Split(' ')
            |> equal [|"a";"b";"c";"";"d"|]
            "a b c  d ".Split()
            |> equal [|"a";"b";"c";"";"d";""|]
            "a-b-c".Split()
            |> equal [|"a-b-c"|]
            "a-b-c".Split("")
            |> equal [|"a-b-c"|]
            "a\tb".Split()
            |> equal [|"a";"b"|]
            "a\nb".Split()
            |> equal [|"a";"b"|]
            "a\rb".Split()
            |> equal [|"a";"b"|]
            "a\u2003b".Split() // em space
            |> equal [|"a";"b"|]
            // "a b c  d".Split(null)
            // |> equal [|"a";"b";"c";"";"d"|]
            // "a\tb".Split(null)
            // |> equal [|"a";"b"|]
            // "a\u2003b".Split(null) // em space
            // |> equal [|"a";"b"|]
            let array = "a;b,c".Split(',', ';')
            "abc" = array.[0] + array.[1] + array.[2]
            |> equal true
            "a--b-c".Split([|"--"|], StringSplitOptions.None)
            |> equal [|"a";"b-c"|]
            " a-- b- c ".Split('-', 2, StringSplitOptions.None)
            |> equal [|" a"; "- b- c "|]
            "---o---o---".Split("--", StringSplitOptions.None)
            |> equal [|""; "-o"; "-o"; "-"|];

      testCase "String.Split with remove empties works" <| fun () ->
            "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
            |> (=) [|"a";"b";"c";"d"|] |> equal true
            " a-- b- c ".Split("-", 2, StringSplitOptions.RemoveEmptyEntries)
            |> (=) [|" a"; " b- c "|] |> equal true
            "---o---o---".Split("--", StringSplitOptions.RemoveEmptyEntries)
            |> (=) [|"-o"; "-o"; "-"|] |> equal true
            let array = ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
            "abc" = array.[0] + array.[1] + array.[2]
            |> equal true

      testCase "String.Split with count works" <| fun () ->
            let array = "a b  c d".Split ([|' '|], 2)
            equal "a" array.[0]
            equal "b  c d" array.[1]
            "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.RemoveEmptyEntries)
            |> (=) [|"a";"b";"c;d"|] |> equal true
            "a-b-c".Split("", System.Int32.MaxValue)
            |> (=) [|"a-b-c"|] |> equal true

      testCase "String.Split with empty works" <| fun () ->
            let array = "a b cd".Split()
            array |> equal [| "a"; "b"; "cd" |]

      testCase "String.Split with trim entries works" <| fun () ->
            " a-- b- c ".Split('-', 2, StringSplitOptions.TrimEntries)
            |> (=) [|"a"; "- b- c"|] |> equal true
            " a-- b- c ".Split('-', 3, StringSplitOptions.TrimEntries)
            |> (=) [|"a"; ""; "b- c"|] |> equal true

      testCase "String.Split with trim and remove entries works" <| fun () ->
            " a-- b- c ".Split([| "-" |], 2, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> equal [|"a"; "b- c"|]
            " a-- b- c ".Split([| '-' |], 3, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> equal  [|"a"; "b"; "c"|]

      testCase "String.Split with consecutive separators works" <| fun () ->
            "       ".Split(" ", 4,  StringSplitOptions.None)
            |> equal [|"";"";"";"    "|]
            "       ".Split(" ", 4,  StringSplitOptions.RemoveEmptyEntries)
            |> equal [||]
            "       ".Split(" ", 4,  StringSplitOptions.TrimEntries)
            |> equal [|"";"";"";""|]
            "       ".Split(" ", 4,  StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> equal [||]

      testCase "String.Replace works" <| fun () ->
            "abc abc abc".Replace("abc", "d") |> equal "d d d"
            // String.Replace does not get stuck in endless loop
            "...".Replace(".", "..") |> equal "......"

      testCase "Access char by index works" <| fun () ->
            let c = "abcd".[2]
            equal 'c' c
            equal 'd' (char ((int c) + 1))

      testCase "String.IndexOf char works" <| fun () ->
            "abcd".IndexOf('b') * 100 + "abcd".IndexOf('e')
            |> equal 99

      testCase "String.IndexOf char works with offset" <| fun () ->
            "abcdbc".IndexOf('b', 3)
            |> equal 4

      testCase "String.LastIndexOf char works" <| fun () ->
            "abcdbc".LastIndexOf('b') * 100 + "abcd".LastIndexOf('e')
            |> equal 399

      testCase "String.LastIndexOf char works with offset" <| fun () ->
            "abcdbcebc".LastIndexOf('b', 3)
            |> equal 1

      testCase "String.IndexOf works" <| fun () ->
            "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd")
            |> equal 99

      testCase "String.IndexOf works with offset" <| fun () ->
            "abcdbc".IndexOf("bc", 3)
            |> equal 4

      testCase "String.LastIndexOf works" <| fun () ->
            "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd")
            |> equal 399

      testCase "String.LastIndexOf works with offset" <| fun () ->
            "abcdbcebc".LastIndexOf("bc", 3)
            |> equal 1

      testCase "String.IndexOfAny works" <| fun () ->
            "abcdbcebc".IndexOfAny([|'b'|]) |> equal 1
            "abcdbcebc".IndexOfAny([|'b'|], 2) |> equal 4
            "abcdbcebc".IndexOfAny([|'b'|], 2, 2) |> equal -1
            "abcdbcebc".IndexOfAny([|'f';'e'|]) |> equal 6
            "abcdbcebc".IndexOfAny([|'f';'e'|], 2) |> equal 6
            "abcdbcebc".IndexOfAny([|'f';'e'|], 2, 4) |> equal -1
            // failing
            //"abcdbcebc".IndexOfAny([|'c';'b'|]) |> equal 1

      testCase "String.StartsWith works" <| fun () ->
            let args = [("ab", true); ("cd", false); ("abcdx", false)]
            for arg in args do
                  "abcd".StartsWith(fst arg)
                  |> equal (snd arg)

      // TODO: StartsWith with StringComparison
      // testCase "String.StartsWith with StringComparison works" <| fun () ->
      //       let args = [("ab", true); ("cd", false); ("abcdx", false)]
      //       for arg in args do
      //             "ABCD".StartsWith(fst arg, StringComparison.OrdinalIgnoreCase)
      //             |> equal (snd arg)

      testCase "String.EndsWith works" <| fun () ->
            let args = [("ab", false); ("cd", true); ("abcdx", false)]
            for arg in args do
                  "abcd".EndsWith(fst arg)
                  |> equal (snd arg)

      testCase "String.Trim works" <| fun () ->
            "   abc   ".Trim()
            |> equal "abc"

      testCase "String.Trim with chars works" <| fun () ->
            @"\\\abc///".Trim('\\','/')
            |> equal "abc"

      testCase "String.Trim with special chars works" <| fun () ->
            @"()[]{}abc/.?*+-^$|\".Trim(@"()[]{}/.?*+-^$|\".ToCharArray())
            |> equal "abc"

      testCase "String.TrimStart works" <| fun () ->
            "!!--abc   ".TrimStart('!','-')
            |> equal "abc   "

      testCase "String.TrimStart with chars works" <| fun () ->
            "   abc   ".TrimStart()
            |> equal "abc   "

      testCase "String.TrimEnd works" <| fun () ->
            "   abc   ".TrimEnd()
            |> equal "   abc"

      testCase "String.TrimEnd with chars works" <| fun () ->
            "   abc??**".TrimEnd('*','?')
            |> equal "   abc"
            @"\foo\bar\".Replace("\\", "/").TrimEnd('/')
            |> equal "/foo/bar"

      testCase "String.Empty works" <| fun () ->
            let s = String.Empty
            s |> equal ""

      testCase "String.Chars works" <| fun () ->
            let input = "hello"
            input.Chars(2)
            |> equal 'l'

      testCase "String.Substring works" <| fun () ->
            "abcdefg".Substring(2)
            |> equal "cdefg"

      testCase "String.Substring works with length" <| fun () ->
            "abcdefg".Substring(2, 2)
            |> equal "cd"

      testCase "String.Substring throws error if startIndex or length are out of bounds" <| fun () -> // See #1955
            let throws f =
                try f () |> ignore; false
                with _ -> true
            throws (fun _ -> "abcdefg".Substring(20)) |> equal true
            throws (fun _ -> "abcdefg".Substring(2, 10)) |> equal true

      testCase "String.ToUpper works" <| fun () ->
            "AbC".ToUpper() |> equal "ABC"

      testCase "String.ToLower works" <| fun () ->
            "aBc".ToLower() |> equal "abc"

      testCase "String.ToUpperInvariant works" <| fun () ->
            "AbC".ToUpperInvariant() |> equal "ABC"

      testCase "String.ToLowerInvariant works" <| fun () ->
            "aBc".ToLowerInvariant() |> equal "abc"

      testCase "String.Length works" <| fun () ->
            "AbC".Length |> equal 3

      testCase "String item works" <| fun () ->
            "AbC".[1] |> equal 'b'

      testCase "String.ToCharArray works" <| fun () ->
            let arr = "abcd".ToCharArray()
            equal "c" (string arr.[2])
            arr |> Array.map (fun _ -> 1) |> Array.sum
            |> equal arr.Length

      // TODO: surrogate pairs
      // testCase "String enumeration handles surrogates pairs" <| fun () -> // See #1279
      //     let unicodeString = ".\U0001f404."
      //     unicodeString |> List.ofSeq |> Seq.length |> equal 4
      //     String.length unicodeString |> equal 4
      //     let mutable len = 0
      //     for i in unicodeString do
      //         len <- len + 1
      //     equal 4 len

      testCase "String.Join works" <| fun () ->
            String.Join("--", "a", "b", "c")
            |> equal "a--b--c"
            String.Join("--", seq { yield "a"; yield "b"; yield "c" })
            |> equal "a--b--c"

      testCase "String.Join with indices works" <| fun () ->
            String.Join("**", [|"a"; "b"; "c"; "d"|], 1, 2)
            |> equal "b**c"
            String.Join("*", [|"a"; "b"; "c"; "d"|], 1, 3)
            |> equal "b*c*d"

      testCase "String.Join works with chars" <| fun () -> // See #1524
            // TODO: It's difficult to make this work because the type
            // of ParamArray is obj[] so we need to check each arg one by one.
            // String.Join("--", 'a', 'b', 'c') |> equal "a--b--c"

            String.Join("--", seq { yield 'a'; yield 'b'; yield 'c' })
            |> equal "a--b--c"
            [0..10]
            |> List.map (fun _ -> '*')
            |> fun chars -> String.Join("", chars)
            |> equal "***********"

      // TODO: BigInts
      // testCase "String.Join with big integers works" <| fun () ->
      //       String.Join("--", [|3I; 5I|])
      //       |> equal "3--5"
      //       String.Join("--", 3I, 5I)
      //       |> equal "3--5"

      testCase "String.Join with single argument works" <| fun () -> // See #1182
            String.Join(",", "abc") |> equal "abc"
            String.Join(",", [|"abc"|]) |> equal "abc"
            String.Join(",", ["abc"]) |> equal "abc"

      testCase "System.String.Concat works" <| fun () ->
            String.Concat("a", "b", "c")
            |> equal "abc"
            String.Concat(seq { yield "a"; yield "b"; yield "c" })
            |> equal "abc"

      testCase "System.String.Join with long array works" <| fun () ->
            let n = 1_000_000
            let a = Array.init n (fun _i -> "a")
            let s = String.Join("", a)
            s.Length |> equal n

      testCase "System.String.Join with long seq works" <| fun () ->
            let n = 1_000_000
            let a = seq { for i in 1..n -> "a" }
            let s = String.Join("", a)
            s.Length |> equal n

      testCase "System.String.Concat with long array works" <| fun () ->
            let n = 1_000_000
            let a = Array.init n (fun _i -> "a")
            let s = String.Concat(a)
            s.Length |> equal n

      testCase "System.String.Concat with long seq works" <| fun () ->
            let n = 1_000_000
            let a = seq { for i in 1..n -> "a" }
            let s = String.Concat(a)
            s.Length |> equal n

      testCase "String.concat with long array works" <| fun () ->
            let n = 1_000_000
            let a = Array.init n (fun _i -> "a")
            let s = String.concat "" a
            s.Length |> equal n

      testCase "String.concat with long seq works" <| fun () ->
            let n = 1_000_000
            let a = seq { for i in 1..n -> "a" }
            let s = String.concat "" a
            s.Length |> equal n

      testCase "String.Remove works" <| fun () ->
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

      testCase "String.Insert work" <| fun () ->
            "foobar".Insert(3, " is ")
            |> equal "foo is bar"

      testCase "Enumerating string works" <| fun () ->
            let mutable res = ""
            for c in "HELLO" |> Seq.rev do
                  res <- res + (string c)
            equal "OLLEH" res

      // String - F# module functions

      testCase "String.concat works" <| fun () ->
            String.concat "--" ["a"; "b"; "c"] |> equal "a--b--c"
            seq { yield "a"; yield "b"; yield "c" }
            |> String.concat "-" |> equal "a-b-c"

      testCase "String.forall and exists work" <| fun () ->
            "!!!" |> String.forall (fun c -> c = '!') |> equal true
            "a!a" |> String.forall (fun c -> c = '!') |> equal false
            "aaa" |> String.forall (fun c -> c = '!') |> equal false

      testCase "String.init works" <| fun () ->
            String.init 3 (fun i -> "a")
            |> equal "aaa"

      testCase "String.collect works" <| fun () ->
            "abc" |> String.collect (fun c -> "bcd")
            |> equal "bcdbcdbcd"

      testCase "String.iter works" <| fun () ->
            let mutable res = ""
            "Hello world!"
            |> String.iter (fun c -> res <- res + c.ToString())
            equal "Hello world!" res

      testCase "String.iteri works" <| fun () ->
            let mutable res = ""
            "Hello world!"
            |> String.iteri (fun c i -> res <- res + i.ToString() + c.ToString())
            equal "H0e1l2l3o4 5w6o7r8l9d10!11" res

      testCase "String.length (function) works" <| fun () ->
            "AbC" |> String.length
            |> equal 3

      testCase "String.map works" <| fun () ->
            "Hello world!" |> String.map (fun c -> if c = 'H' then '_' else c)
            |> equal "_ello world!"

      testCase "String.mapi works" <| fun () ->
            "Hello world!" |> String.mapi (fun i c -> if i = 1 || c = 'H' then '_' else c)
            |> equal "__llo world!"

      testCase "String.replicate works" <| fun () ->
            String.replicate 3 "hi there"
            |> equal "hi therehi therehi there"

      testCase "String.IsNullOrWhiteSpace works on string with blanks" <| fun () ->
            String.IsNullOrWhiteSpace "Fri Jun 30 2017 12:30:00 GMT+0200 (Mitteleuropäische Sommerzeit)"
            |> equal false

      testCase "String.IsNullOrWhiteSpace works on blank only string" <| fun () ->
            String.IsNullOrWhiteSpace "      "
            |> equal true

      testCase "String.filter works" <| fun () ->
            String.filter (fun x -> x <> '.') "a.b.c"
            |> equal "abc"

      testCase "String.filter works when predicate matches everything" <| fun () ->
            String.filter (fun x -> x <> '.') "abc"
            |> equal "abc"

      testCase "String.filter works when predicate doesn't match" <| fun () ->
            String.filter (fun x -> x <> '.') "..."
            |> equal ""

      testCase "String.filter works with empty string" <| fun () ->
            String.filter (fun x -> x <> '.') ""
            |> equal ""

      // TODO: Environment
//      #if FABLE_COMPILER
//      testCase "System.Environment.NewLine works" <| fun () ->
//          System.Environment.NewLine
//          |> equal "\n"
//      #endif

      // TODO: Uri
//      testCase "System.Uri.UnescapeDataString works" <| fun () ->
//        System.Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"
//        System.Uri.UnescapeDataString("http%3A%2F%2Fkvz.io%2F") |> equal "http://kvz.io/"
//        System.Uri.UnescapeDataString("http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a")
//        |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"
//
//      testCase "System.Uri.EscapeDataString works" <| fun () ->
//        System.Uri.EscapeDataString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld%21"
//        System.Uri.EscapeDataString("http://kvz.io/") |> equal "http%3A%2F%2Fkvz.io%2F"
//        System.Uri.EscapeDataString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
//        |> equal "http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a"
//
//      testCase "System.Uri.EscapeUriString works" <| fun () ->
//        System.Uri.EscapeUriString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld!"
//        System.Uri.EscapeUriString("http://kvz.io/") |> equal "http://kvz.io/"
//        System.Uri.EscapeUriString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
//        |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

      testCase "interpolated string with double % should be unescaped" <| fun () ->
          $"{100}%%" |> equal "100%"

      testCase "interpolated string with double % should not interfere with holes afterwards " <| fun () ->
          $"%%{99. - 1.5}".Replace(",", ".") |> equal "%97.5"

      testCase "interpolated string with double braces should be unescaped" <| fun () ->
          $"{{ {100} }}" |> equal "{ 100 }"

      // TODO: interpolation with formatting
      // testCase "interpolated string with format and double % should be unescaped" <| fun () ->
      //     $"%.2f{100.4566666}%%" |> equal "100.46%"

      // testCase "interpolated string with format and double braces should be unescaped" <| fun () ->
      //     $"{{ %.2f{100.4566666} }}" |> equal "{ 100.46 }"

      testCase "interpolated string with consecutive holes work" <| fun () ->
            $"""{"foo"}{5}""" |> equal "foo5"
            $"""%s{"foo"}%i{5}""" |> equal "foo5"
            $"""{"foo"}/{5}.fsi""" |> equal "foo/5.fsi"

      // TODO: FormattableString
//      testCase "Can create FormattableString" <| fun () ->
//          let orderAmount = 100
//          let convert (s: FormattableString) = s
//          let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
//          s.Format |> equal "You owe: {0:N5} {1} {2}"
//          s.ArgumentCount |> equal 3
//          s.GetArgument(2) |> equal (box true)
//          s.GetArguments() |> equal [|100; 3; true|]
//          let s2: FormattableString = $"""{5 + 2}This is "{"really"}" awesome!"""
//          s2.Format |> equal "{0}This is \"{1}\" awesome!"
//          s2.GetArguments() |> equal [|box 7; box "really"|]
//          let s3: FormattableString = $"""I have no holes"""
//          s3.Format |> equal "I have no holes"
//          s3.GetArguments() |> equal [||]
//          let s4: FormattableString = $"I have `backticks`"
//          s4.Format |> equal "I have `backticks`"
//          let s5: FormattableString = $"I have {{escaped braces}} and %%percentage%%"
//          s5.Format |> equal "I have {{escaped braces}} and %percentage%"
