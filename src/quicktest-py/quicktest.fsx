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
    // Summer time (allowed to detect invalid utcoffset for Europe/Paris)
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.Hour |> equal 13

    // Winter time
    let d = DateTime(2014, 1, 9, 13, 23, 30, DateTimeKind.Local)
    d.Hour |> equal 13

    // let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    // d.Hour |> equal 13

    0


// TryParse(String, DateTime)
// TryParse(String, IFormatProvider, DateTime)
// TryParse(String, IFormatProvider, DateTimeStyles, DateTime)
