module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let log (o: obj) = JS.console.log (o)
// printfn "%A" o

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual

    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

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

let testCase (msg: string) f : unit =
    try
        printfn "%s" msg
        f ()
    with ex ->
        printfn "%s" ex.Message

        if
            ex.Message <> null
            && ex.Message.StartsWith("[ASSERT ERROR]", StringComparison.Ordinal)
               |> not
        then
            printfn "%s" (ex.StackTrace ??= "")

    printfn ""

let testCaseAsync msg f =
    testCase
        msg
        (fun () ->
            async {
                try
                    do! f ()
                with ex ->
                    printfn "%s" ex.Message

                    if
                        ex.Message <> null
                        && ex.Message.StartsWith(
                            "[ASSERT ERROR]",
                            StringComparison.Ordinal
                           )
                           |> not
                    then
                        printfn "%s" (ex.StackTrace ??= "")
            }
            |> Async.StartImmediate
        )

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

let measureTime (f: unit -> unit) : unit =
    emitJsStatement
        ()
        """
   //js
   const startTime = process.hrtime();
   f();
   const elapsed = process.hrtime(startTime);
   console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
   //!js
"""

printfn "Running quick tests..."

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4
