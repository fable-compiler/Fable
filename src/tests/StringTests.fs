[<NUnit.Framework.TestFixture>]
#if !DOTNETCORE
[<NUnit.Framework.SetCultureAttribute("en-US")>]
#endif
module Fable.Tests.Strings
open System
open NUnit.Framework
open Fable.Tests.Util

// Format

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

#if MOCHA
open Fable.Core.JsInterop

[<Test>]
let ``sprintf "%A" with circular references doesn't crash``() = // See #338
      let o = obj()
      o?self <- o
      sprintf "%A" o |> ignore
#endif

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
let ``Conversion to char works``() =
      let c1 = char "h"
      equal "h" (string c1)

      let c2 = char 97
      equal "a" (string c2)

[<Test>]
let ``Conversion to int works``() =
      equal 5 (int "5")
      equal "5" (string 5)

[<Test>]
let ``Conversion to float works``() =
      equal 5. (float "5.0")
      (string 5.).StartsWith("5") |> equal true
      equal 5.25 (float "5.25")
      (string 5.25).StartsWith("5.25") |> equal true

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
let ``String.Contains works``() =
      "ABC".Contains("B") |> equal true
      "ABC".Contains("Z") |> equal false

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
      String.Concat("--", seq { yield "a"; yield "b"; yield "c" })
      |> equal "--abc"

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

// [<Test>]
// let ``System.Convert.ToString works``() =
//       let x = 45
//       Convert.ToString(x) |> equal "45"
//       Convert.ToString(x, 2) |> equal "101101"
//       Convert.ToString(x, 16) |> equal "2d"
