[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Strings
open System
open NUnit.Framework
open Fabel.Tests.Util

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
let ``String.Format works``() =
      let arg1, arg2, arg3 = "F#", "Fabel", "Babel"
      String.Format("{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
      |> equal "Babel is to Fabel what Fabel is to F#"

// TODO: Implement DateTime first
// [<Test>]
// let ``String.Format with extra formatting works``() =
//       let i = 0.5466788
//       let dt = DateTime(2014, 9, 26).AddMinutes(19.)
//       String.Format("{0:F2} {0:P2} {1:yy/MM/dd HH:mm}", i, dt)
//       |> equal "0.55 54.67 % 14/09/26 00:19"

// TODO: Conversions char, string, int, float

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
      let array = "a;b,c".Split(',', ';')
      "abc" = array.[0] + array.[1] + array.[2]
      |> equal true

[<Test>]
let ``String.Split with remove empties works``() =
      let array = ";,a;b,c".Split([|','; ';'|], StringSplitOptions.RemoveEmptyEntries)
      "abc" = array.[0] + array.[1] + array.[2]
      |> equal true

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
      arr.[3] |> int
      |> equal 100
      arr |> Array.map int |> Array.sum
      |> equal 394

[<Test>]
let ``String.Join works``() =
      String.Join("--", "a", "b", "c")
      |> equal "a--b--c"
      String.Join("--", seq { yield "a"; yield "b"; yield "c" })
      |> equal "a--b--c"

// String - F# module functions
 
// [<TestCase("!!!"); TestCase("a!a"); TestCase("aaa")>]
// let ``String.forall and exists work``(str) =
//       str |> String.forall (fun c -> charToInt c = charToInt '!') 
//       |> equal true 
// 
// [<Test>]
// let ``String.init works``() =
//       String.init 3 (fun i -> "a")
//       |> equal true 
// 
// [<Test>]
// let ``String.collect works``() =
//       "abc" |> String.collect (fun c -> "bcd")
//       |> equal true 
// 
// [<Test>]
// let ``String.iter works``() =
//       let res = ref ""
//       "Hello world!" |> String.iter (fun c -> res := !res + c.ToString())
//       !res
//       |> equal true 
// 
// [<Test>]
// let ``String.iteri works``() =
//       let res = ref ""
//       "Hello world!" |> String.iteri (fun c i -> res := !res + i.ToString() + c.ToString())
//       !res
//       |> equal true 
// 
// [<Test>]
// let ``String.length (function) works``() =
//    check
//       <@@ 
//       "AbC" |> String.length |> float
//       |> equal true
// 
// [<Test>]
// let ``String.map works``() =
//       "Hello world!" |> String.map (fun c -> if charToInt c = charToInt 'H' then '_' else c)
//       |> equal true 
// 
// [<Test>]
// let ``String.mapi works``() =
//       "Hello world!" |> String.mapi (fun i c -> if i = 1 || charToInt c = charToInt 'H' then '_' else c)
//       |> equal true 
// 
// [<Test>]
// let ``String.replicate works``() =
//       String.replicate 10 "hi there"
//       |> equal true 