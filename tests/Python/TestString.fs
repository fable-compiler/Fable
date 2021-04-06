module Fable.Tests.String

open System
open Util.Testing

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
let ``sprintf displays sign correctly`` () =
      sprintf "%i" 1 |> equal "1"
      sprintf "%d" 1 |> equal "1"
      sprintf "%d" 1L |> equal "1"
      sprintf "%.2f" 1. |> equal "1.00"
      sprintf "%i" -1 |> equal "-1"
      sprintf "%d" -1 |> equal "-1"
      sprintf "%d" -1L |> equal "-1"
      sprintf "%.2f" -1. |> equal "-1.00"

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
let ``test interpolated string with double % should be unescaped`` () =
    $"{100}%%" |> equal "100%"

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

// [<Fact>]
// let ``test Storing result of Seq.tail and printing the result several times works. Related to #1996`` () =
//     let tweets = seq { "Hi"; "Hello"; "Hola" }
//     let tweetsTailR: seq<string> = tweets |> Seq.tail

//     let a = sprintf "%A" (tweetsTailR)
//     let b = sprintf "%A" (tweetsTailR)

//     containsInOrder ["Hello"; "Hola"] a |> equal true
//     containsInOrder ["Hello"; "Hola"] b |> equal true

// [<Fact>]
// let ``test sprintf \"%X\" works`` () =
//     //These should all be the Native JS Versions (except int64 / uint64)
//     //See #1530 for more information.

//     sprintf "255: %X" 255 |> equal "255: FF"
//     sprintf "255: %x" 255 |> equal "255: ff"
//     sprintf "-255: %X" -255 |> equal "-255: FFFFFF01"
//     sprintf "4095L: %X" 4095L |> equal "4095L: FFF"
//     sprintf "-4095L: %X" -4095L |> equal "-4095L: FFFFFFFFFFFFF001"
//     sprintf "1 <<< 31: %x" (1 <<< 31) |> equal "1 <<< 31: 80000000"
//     sprintf "1u <<< 31: %x" (1u <<< 31) |> equal "1u <<< 31: 80000000"
//     sprintf "2147483649L: %x" 2147483649L |> equal "2147483649L: 80000001"
//     sprintf "2147483650uL: %x" 2147483650uL |> equal "2147483650uL: 80000002"
//     sprintf "1L <<< 63: %x" (1L <<< 63) |> equal "1L <<< 63: 8000000000000000"
//     sprintf "1uL <<< 63: %x" (1uL <<< 63) |> equal "1uL <<< 63: 8000000000000000"

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
