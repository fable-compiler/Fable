#r "nuget:Fable.Python"

open Fable.Core
open Fable.Core.Testing
open Fable.Core.PyInterop
open Fable.Python.Builtins
open System
open System.Globalization

let equal expected actual =
    // According the console log arguments are reversed
    Assert.AreEqual(actual, expected)

[<EntryPoint>]
let main argv =
    let name = Array.tryHead argv |> Option.defaultValue "Guest"
    printfn $"Hello {name}!"

    // Open file with builtin `open`
    // use file = builtins.``open``(StringPath "data.txt")
    // file.read() |> printfn "File contents: %s"

    DateTime(2014, 10, 5).DayOfWeek |> equal DayOfWeek.Sunday
    DateTime(2014, 10, 6).DayOfWeek |> equal DayOfWeek.Monday
    DateTime(2014, 10, 7).DayOfWeek |> equal DayOfWeek.Tuesday
    DateTime(2014, 10, 8).DayOfWeek |> equal DayOfWeek.Wednesday
    DateTime(2014, 10, 9).DayOfWeek |> equal DayOfWeek.Thursday
    DateTime(2014, 10, 10).DayOfWeek |> equal DayOfWeek.Friday
    DateTime(2014, 10, 11).DayOfWeek |> equal DayOfWeek.Saturday


    // let t = DayOfWeek.Thursday

    let d = DateTime(2014, 10, 9)
    DateTime(2014, 10, 9).DayOfYear |> equal 282

    DateTime(2020, 10, 9).DayOfYear |> equal 283

    0
