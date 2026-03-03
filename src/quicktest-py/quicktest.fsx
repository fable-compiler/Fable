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

// Reproduction: exception variable captured in a deferred closure (not immediately called)
let testExCapturedInDeferredClosure () =
    let getMsg =
        try
            failwith "boom"
            fun () -> "no error"
        with ex ->
            fun () -> ex.Message // closure captures ex, not immediately called

    getMsg () // called AFTER the try/with block

[<EntryPoint>]
let main argv =
    let name = Array.tryHead argv |> Option.defaultValue "Guest"
    printfn $"Hello {name}!"

    let msg = testExCapturedInDeferredClosure ()
    printfn $"Caught: {msg}"

    printfn "All tests passed!"

    0
