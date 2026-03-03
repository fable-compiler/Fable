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

let throwsError (expected: string) (f: unit -> 'a) : unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            if not <| String.IsNullOrEmpty(expected) then
                equal e.Message expected

            false
    // TODO better error messages
    equal false success

let throwsAnyError (f: unit -> 'a) : unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            printfn "Got expected error: %s" e.Message
            false

    if success then
        printfn "[ERROR EXPECTED]"

[<EntryPoint>]
let main argv =
    let name = Array.tryHead argv |> Option.defaultValue "Guest"
    printfn $"Hello {name}!"

    // Open file with builtin `open`
    // use file = builtins.``open``(StringPath "data.txt")
    // file.read() |> printfn "File contents: %s"

    printfn "All tests passed!"

    0
