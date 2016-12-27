[<Util.Testing.TestFixture>]
#if !DOTNETCORE && !FABLE_COMPILER
[<NUnit.Framework.SetCulture("en-US")>]
#endif
module Fable.Tests.Strings
open System
open Util.Testing
open Fable.Tests.Util

// Format

[<Test>]
let ``kprintf works``() =
      let f s = s
      Printf.kprintf f "hello" |> equal "hello"
      Printf.kprintf f "%X" 255 |> equal "FF"
      Printf.kprintf f "%.2f %g" 0.5468989 5. |> equal "0.55 5"

[<Test>]
let ``sprintf works``() =
      // Immediately applied
      sprintf "%.2f %g" 0.5468989 5.
      |> equal "0.55 5"
      // Curried
      let printer = sprintf "Hi %s, good %s!"
      let printer = printer "Alfonso"
      printer "morning" |> equal "Hi Alfonso, good morning!"
      printer "evening" |> equal "Hi Alfonso, good evening!"

[<Test>]
let ``sprintf without arguments works``() =
      sprintf "hello" |> equal "hello"

[<Test>]
let ``sprintf with escaped percent symbols works``() = // See #195
      let r, r1, r2 = "Ratio", 0.213849, 0.799898
      sprintf "%s1: %.2f%% %s2: %.2f%%" r (r1*100.) r (r2*100.)
      |> equal "Ratio1: 21.38% Ratio2: 79.99%"

[<Test>]
let ``sprintf with percent symbols in arguments works``() = // See #329
      let same s = sprintf "%s" s |> equal s
      same "%"
      same "%%"
      same "%%%"
      same "%%%%"
      same "% %"
      same "%% %%"
      same "%% % % %%"

type Test(i: int) =
      override __.ToString() = string(i + i)

[<Test>]
let ``sprintf "%O" with overloaded string works``() =
      let o = Test(5)
      sprintf "%O" o |> equal "10"

[<Test>]
let ``sprintf "%A" with overloaded string works``() =
      let o = Test(5)
      (sprintf "%A" o).Replace("\"", "") |> equal "10"

#if FABLE_COMPILER
open Fable.Core.JsInterop

[<Test>]
let ``sprintf "%A" with circular references doesn't crash``() = // See #338
      let o = obj()
      o?self <- o
      sprintf "%A" o |> ignore
#endif

[<Test>]
let ``sprintf "%X" works``() =
      sprintf "%X" 255 |> equal "FF"
      sprintf "%x" 255 |> equal "ff"
      sprintf "%X" -255 |> equal "FFFFFF01"
      String.Format("{0:X}", 255) |> equal "FF"
      String.Format("{0:x}", 255) |> equal "ff"
      String.Format("{0:X}", -255) |> equal "FFFFFF01"

[<Test>]
let ``String slicing works``() =
      let s = "cat and dog"
      sprintf "%s" s.[2..8] |> equal "t and d"
      sprintf "%s" s.[2..] |> equal "t and dog"
      sprintf "%s" s.[..8] |> equal "cat and d"

[<Test>]
let ``String.Format works``() =
      let arg1, arg2, arg3 = "F#", "Fable", "Babel"
      String.Format("{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
      |> equal "Babel is to Fable what Fable is to F#"

[<Test>]
let ``String.Format with extra formatting works``() =
      let i = 0.5466788
      let dt = DateTime(2014, 9, 26).AddMinutes(19.)
      String.Format("{0:F2} {0:P2} {1:yyyy-MM-dd HH:mm}", i, dt)
      |> equal "0.55 54.67 % 2014-09-26 00:19"

[<Test>]
let ``Padding works``() =
    "3.14".PadLeft(10)      |> equal "      3.14"
    "3.14".PadRight(10)     |> equal "3.14      "
    "22".PadLeft(10, '0')   |> equal "0000000022"
    "-22".PadRight(10, 'X') |> equal "-22XXXXXXX"
    "333".PadLeft(1) |> equal "333"

[<Test>]
let ``Padding with sprintf works``() =
    sprintf "%10.1f" 3.14  |> equal "       3.1"
    sprintf "%-10.1f" 3.14 |> equal "3.1       "
    sprintf "%+010i" 22    |> equal "+000000022"
    sprintf "%+0-10i" -22  |> equal "-22       "

[<Test>]
let ``Padding with String.Format works``() =
    String.Format("{0,10:F1}", 3.14)  |> equal "       3.1"
    String.Format("{0,-10:F1}", 3.14) |> equal "3.1       "
    String.Format("{0,10}", 22)       |> equal "        22"
    String.Format("{0,-10}", -22)     |> equal "-22       "

// Conversions

[<Test>]
let ``Conversion string to char works``() =
      let c1 = char "h"
      equal "h" (string c1)

[<Test>]
let ``Conversion int to char works``() =
      let c2 = char 97
      equal "a" (string c2)

[<Test>]
let ``Conversion char to int works``() =
      equal 97 (int 'a')
      equal 'a' (char 97)

[<Test>]
let ``Conversion string to int works``() =
      equal 5 (int "5")
      equal "5" (string 5)

[<Test>]
let ``Conversion string to int8 works``() =
      equal 5y (int8 "5")
      equal "5" (string 5)

[<Test>]
let ``Conversion string to int16 works``() =
      equal 5s (int16 "5")
      equal "5" (string 5s)

[<Test>]
let ``Conversion string to int32 works``() =
      equal 5 (int32 "5")
      equal "5" (string 5)

[<Test>]
let ``Conversion string to int64 works``() =
      equal 5L (int64 "5")
      equal "5" (string 5L)

[<Test>]
let ``Conversion string to uint8 works``() =
      equal 5uy (uint8 "5")
      equal "5" (string 5uy)

[<Test>]
let ``Conversion string to uint16 works``() =
      equal 5us (uint16 "5")
      equal "5" (string 5us)

[<Test>]
let ``Conversion string to uint32 works``() =
      equal 5u (uint32 "5")
      equal "5" (string 5u)

[<Test>]
let ``Conversion string to uint64 works``() =
      equal 5uL (uint64 "5")
      equal "5" (string 5uL)

[<Test>]
let ``Conversion string to single works``() =
      equal 5.f (float32 "5.0")
      (string 5.f).StartsWith("5") |> equal true
      equal 5.25f (float32 "5.25")
      (string 5.25f).StartsWith("5.25") |> equal true

[<Test>]
let ``Conversion string to double works``() =
      equal 5. (float "5.0")
      (string 5.).StartsWith("5") |> equal true
      equal 5.25 (float "5.25")
      (string 5.25).StartsWith("5.25") |> equal true

[<Test>]
let ``Conversion string to decimal works``() =
      equal 5.m (decimal "5.0")
      (string 5.m).StartsWith("5") |> equal true
      equal 5.25m (decimal "5.25")
      (string 5.25m).StartsWith("5.25") |> equal true

// System.String - constructors

[<Test>]
let ``String.ctor(char[]) works``() =
      System.String([|'f'; 'a'; 'b'; 'l'; 'e'|])
      |> equal "fable"

let ``String.ctor(char, int) works``() =
      System.String('f', 5)
      |> equal "fffff"

let ``String.ctor(char[], int, int) works``() =
      System.String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3)
      |> equal "abl"

// System.String - static methods

[<Test>]
let ``String.Compare works``() =
      System.String.Compare("abc", "abc") |> equal 0
      System.String.Compare("ABC", "abc") |> equal 1
      System.String.Compare("abc", "abd") |> equal -1
      System.String.Compare("bbc", "abd") |> equal 1
      System.String.Compare("ABC", "abc", false) |> equal 1
      System.String.Compare("ABC", "abc", true) |> equal 0
      System.String.Compare("ABC", "abd", true) |> equal -1
      System.String.Compare("BBC", "abd", true) |> equal 1
      System.String.Compare("ABC", "abc", StringComparison.CurrentCulture) > 0 |> equal true
      System.String.Compare("ABC", "abc", StringComparison.Ordinal) < 0 |> equal true
      System.String.Compare("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal 0
      System.String.Compare("abc", 0, "bcd", 0, 3) |> equal -1
      System.String.Compare("abc", 1, "bcd", 0, 2) |> equal 0
      System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.CurrentCulture) > 0 |> equal true
      System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.Ordinal) < 0 |> equal true
      System.String.Compare("ABC", 1, "bcd", 0, 2, StringComparison.OrdinalIgnoreCase) |> equal 0

[<Test>]
let ``String.IsNullOrEmpty works``() =
      let args = [("", true); (null, true); ("test", false); (" \t", false)]
      for arg in args do
            System.String.IsNullOrEmpty(fst arg)
            |> equal (snd arg)

[<Test>]
let ``String.IsNullOrWhiteSpace works``() =
      let args = [("", true); (null, true); ("test", false); (" \t", true)]
      for arg in args do
            System.String.IsNullOrWhiteSpace(fst arg)
            |> equal (snd arg)

// System.String - instance methods

[<Test>]
let ``String.Contains works``() =
      "ABC".Contains("B") |> equal true
      "ABC".Contains("Z") |> equal false

[<Test>]
let ``String.Split works``() =
      "a b c  d".Split(' ')
      |> (=) [|"a";"b";"c";"";"d"|] |> equal true
      "a b c  d ".Split()
      |> (=) [|"a";"b";"c";"";"d";""|] |> equal true
      let array = "a;b,c".Split(',', ';')
      "abc" = array.[0] + array.[1] + array.[2]
      |> equal true
      "a--b-c".Split([|"--"|], StringSplitOptions.None)
      |> (=) [|"a";"b-c"|] |> equal true

[<Test>]
let ``String.Split with remove empties works``() =
      "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
      |> (=) [|"a";"b";"c";"d"|] |> equal true
      let array = ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
      "abc" = array.[0] + array.[1] + array.[2]
      |> equal true

[<Test>]
let ``String.Split with count works``() =
      let array = "a b  c d".Split ([|' '|], 2)
      equal "a" array.[0]
      equal "b  c d" array.[1]
      "a;,b,c;d".Split([|','; ';'|], 3, StringSplitOptions.RemoveEmptyEntries)
      |> (=) [|"a";"b";"c;d"|] |> equal true

[<Test>]
let ``String.Replace works``() =
      "abc abc abc".Replace("abc", "d") |> equal "d d d"
      // String.Replace does not get stuck in endless loop
      "...".Replace(".", "..") |> equal "......"

[<Test>]
let ``Access char by index works``() =
      let c = "abcd".[2]
      equal 'c' c
      equal 'd' (char ((int c) + 1))

[<Test>]
let ``String.IndexOf char works``() =
      "abcd".IndexOf('b') * 100 + "abcd".IndexOf('e')
      |> equal 99

[<Test>]
let ``String.IndexOf char works with offset``() =
      "abcdbc".IndexOf('b', 3)
      |> equal 4

[<Test>]
let ``String.LastIndexOf char works``() =
      "abcdbc".LastIndexOf('b') * 100 + "abcd".LastIndexOf('e')
      |> equal 399

[<Test>]
let ``String.LastIndexOf char works with offset``() =
      "abcdbcebc".LastIndexOf('b', 3)
      |> equal 1

[<Test>]
let ``String.IndexOf works``() =
      "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd")
      |> equal 99

[<Test>]
let ``String.IndexOf works with offset``() =
      "abcdbc".IndexOf("bc", 3)
      |> equal 4

[<Test>]
let ``String.LastIndexOf works``() =
      "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd")
      |> equal 399

[<Test>]
let ``String.LastIndexOf works with offset``() =
      "abcdbcebc".LastIndexOf("bc", 3)
      |> equal 1

[<Test>]
let ``String.StartsWith works``() =
      let args = [("ab", true); ("cd", false); ("abcdx", false)]
      for arg in args do
            "abcd".StartsWith(fst arg)
            |> equal (snd arg)

[<Test>]
let ``String.EndsWith works``() =
      let args = [("ab", false); ("cd", true); ("abcdx", false)]
      for arg in args do
            "abcd".EndsWith(fst arg)
            |> equal (snd arg)

[<Test>]
let ``String.Trim works``() =
      "   abc   ".Trim()
      |> equal "abc"

[<Test>]
let ``String.Trim with chars works``() =
      @"\\\abc///".Trim('\\','/')
      |> equal "abc"

[<Test>]
let ``String.TrimStart works``() =
      "!!--abc   ".TrimStart('!','-')
      |> equal "abc   "

[<Test>]
let ``String.TrimStart with chars works``() =
      "   abc   ".TrimStart()
      |> equal "abc   "

[<Test>]
let ``String.TrimEnd works``() =
      "   abc   ".TrimEnd()
      |> equal "   abc"

[<Test>]
let ``String.TrimEnd with chars works``() =
      "   abc??**".TrimEnd('*','?')
      |> equal "   abc"

[<Test>]
let ``String.Empty works``() =
      let s = String.Empty
      s |> equal ""

[<Test>]
let ``String.Substring works``() =
      "abcdefg".Substring(2)
      |> equal "cdefg"

[<Test>]
let ``String.Substring works with length``() =
      "abcdefg".Substring(2, 2)
      |> equal "cd"

[<Test>]
let ``String.ToUpper and String.ToLower work``() =
      "AbC".ToUpper() + "aBc".ToLower()
      |> equal "ABCabc"

[<Test>]
let ``String.Length works``() =
      "AbC".Length |> equal 3

[<Test>]
let ``String item works``() =
      "AbC".[1] |> equal 'b'

[<Test>]
let ``String.ToCharArray works``() =
      let arr = "abcd".ToCharArray()
      equal "c" (string arr.[2])
      arr |> Array.map (fun _ -> 1) |> Array.sum
      |> equal arr.Length

[<Test>]
let ``String.Join works``() =
      String.Join("--", "a", "b", "c")
      |> equal "a--b--c"
      String.Join("--", seq { yield "a"; yield "b"; yield "c" })
      |> equal "a--b--c"

[<Test>]
let ``System.String.Concat works``() =
      String.Concat("a", "b", "c")
      |> equal "abc"
      String.Concat(seq { yield "a"; yield "b"; yield "c" })
      |> equal "abc"

[<Test>]
let ``String.Remove works``() =
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

[<Test>]
let ``String.Insert work``() =
      "foobar".Insert(3, " is ")
      |> equal "foo is bar"

[<Test>]
let ``Enumerating string works``() =
      let mutable res = ""
      for c in "HELLO" |> Seq.rev do
            res <- res + (string c)
      equal "OLLEH" res

// String - F# module functions

[<Test>]
let ``String.concat works``() =
      String.concat "--" ["a"; "b"; "c"] |> equal "a--b--c"
      seq { yield "a"; yield "b"; yield "c" }
      |> String.concat "-" |> equal "a-b-c"

[<Test>]
let ``String.forall and exists work``() =
      "!!!" |> String.forall (fun c -> c = '!') |> equal true
      "a!a" |> String.forall (fun c -> c = '!') |> equal false
      "aaa" |> String.forall (fun c -> c = '!') |> equal false

[<Test>]
let ``String.init works``() =
      String.init 3 (fun i -> "a")
      |> equal "aaa"

[<Test>]
let ``String.collect works``() =
      "abc" |> String.collect (fun c -> "bcd")
      |> equal "bcdbcdbcd"

[<Test>]
let ``String.iter works``() =
      let res = ref ""
      "Hello world!"
      |> String.iter (fun c -> res := !res + c.ToString())
      equal "Hello world!" !res

[<Test>]
let ``String.iteri works``() =
      let mutable res = ""
      "Hello world!"
      |> String.iteri (fun c i -> res <- res + i.ToString() + c.ToString())
      equal "H0e1l2l3o4 5w6o7r8l9d10!11" res

[<Test>]
let ``String.length (function) works``() =
      "AbC" |> String.length
      |> equal 3

[<Test>]
let ``String.map works``() =
      "Hello world!" |> String.map (fun c -> if c = 'H' then '_' else c)
      |> equal "_ello world!"

[<Test>]
let ``String.mapi works``() =
      "Hello world!" |> String.mapi (fun i c -> if i = 1 || c = 'H' then '_' else c)
      |> equal "__llo world!"

[<Test>]
let ``String.replicate works``() =
      String.replicate 3 "hi there"
      |> equal "hi therehi therehi there"

// String to number convertion

[<Test>]
let ``System.Convert.ToSByte works``() =
      let x = "101"
      Convert.ToSByte(x) |> equal 101y
      Convert.ToSByte(x, 2) |> equal 5y
      Convert.ToSByte(x, 8) |> equal 65y
      Convert.ToSByte(x, 10) |> equal 101y

[<Test>]
let ``System.Convert.ToInt16 works``() =
      let x = "101"
      Convert.ToInt16(x) |> equal 101s
      Convert.ToInt16(x, 2) |> equal 5s
      Convert.ToInt16(x, 8) |> equal 65s
      Convert.ToInt16(x, 10) |> equal 101s
      Convert.ToInt16(x, 16) |> equal 257s

[<Test>]
let ``System.Convert.ToInt32 works``() =
      let x = "101"
      Convert.ToInt32(x) |> equal 101
      Convert.ToInt32(x, 2) |> equal 5
      Convert.ToInt32(x, 8) |> equal 65
      Convert.ToInt32(x, 10) |> equal 101
      Convert.ToInt32(x, 16) |> equal 257

[<Test>]
let ``System.Convert.ToInt64 works``() =
      let x = "101"
      Convert.ToInt64(x) |> equal 101L
      Convert.ToInt64(x, 2) |> equal 5L
      Convert.ToInt64(x, 8) |> equal 65L
      Convert.ToInt64(x, 10) |> equal 101L
      Convert.ToInt64(x, 16) |> equal 257L

[<Test>]
let ``System.Convert.ToByte works``() =
      let x = "101"
      Convert.ToByte(x) |> equal 101uy
      Convert.ToByte(x, 2) |> equal 5uy
      Convert.ToByte(x, 8) |> equal 65uy
      Convert.ToByte(x, 10) |> equal 101uy

[<Test>]
let ``System.Convert.ToUInt16 works``() =
      let x = "101"
      Convert.ToUInt16(x) |> equal 101us
      Convert.ToUInt16(x, 2) |> equal 5us
      Convert.ToUInt16(x, 8) |> equal 65us
      Convert.ToUInt16(x, 10) |> equal 101us
      Convert.ToUInt16(x, 16) |> equal 257us

[<Test>]
let ``System.Convert.ToUInt32 works``() =
      let x = "101"
      Convert.ToUInt32(x) |> equal 101u
      Convert.ToUInt32(x, 2) |> equal 5u
      Convert.ToUInt32(x, 8) |> equal 65u
      Convert.ToUInt32(x, 10) |> equal 101u
      Convert.ToUInt32(x, 16) |> equal 257u

[<Test>]
let ``System.Convert.ToUInt64 works``() =
      let x = "101"
      Convert.ToUInt64(x) |> equal 101uL
      Convert.ToUInt64(x, 2) |> equal 5uL
      Convert.ToUInt64(x, 8) |> equal 65uL
      Convert.ToUInt64(x, 10) |> equal 101uL
      Convert.ToUInt64(x, 16) |> equal 257uL

[<Test>]
let ``System.Convert.ToSingle works``() =
      let x = "101."
      Convert.ToSingle(x) |> equal 101.f

[<Test>]
let ``System.Convert.ToDouble works``() =
      let x = "101."
      Convert.ToDouble(x) |> equal 101.

[<Test>]
let ``System.Convert.ToDecimal works``() =
      let x = "101."
      Convert.ToDecimal(x) |> equal 101.m
