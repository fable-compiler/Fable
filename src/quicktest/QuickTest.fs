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
open System.Globalization

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
            && ex.Message.StartsWith("[ASSERT ERROR]", StringComparison.Ordinal) |> not
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
                        && ex.Message.StartsWith("[ASSERT ERROR]", StringComparison.Ordinal) |> not
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

testCase "Path methods work"
<| fun () ->
    System.IO.Path.GetDirectoryName("temp/test.txt") |> equal "temp"
    System.IO.Path.GetExtension("temp/test.txt") |> equal ".txt"
    System.IO.Path.GetFileName("temp/test.txt") |> equal "test.txt"
    System.IO.Path.GetFileNameWithoutExtension("temp/test.txt") |> equal "test"
    System.IO.Path.HasExtension("temp/test.txt") |> equal true
    System.IO.Path.HasExtension("temp/test") |> equal false
    (System.IO.Path.GetFullPath("temp/test.txt")).Length > 0 |> equal true
    (System.IO.Path.GetTempPath()).Length > 0 |> equal true
    (System.IO.Path.GetRandomFileName()).Length > 0 |> equal true

testCase "File methods work"
<| fun () ->
    let file =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.IO.Path.GetRandomFileName())

    System.IO.File.Exists(file) |> equal false

    System.IO.File.WriteAllText(file, "Hello World")
    System.IO.File.Exists(file) |> equal true
    System.IO.File.ReadAllText(file) |> equal "Hello World"

    System.IO.File.WriteAllLines(file, [| "Hello"; "World" |])
    let lines = System.IO.File.ReadAllLines(file)
    lines |> equal [| "Hello"; "World" |]

    let bytes = System.IO.File.ReadAllBytes(file)
    bytes.Length > 0 |> equal true
    System.IO.File.WriteAllBytes(file, bytes)

    let file2 = file + ".copy"
    System.IO.File.Copy(file, file2)
    System.IO.File.Exists(file2) |> equal true

    let file3 = file + ".moved"
    System.IO.File.Move(file2, file3)
    System.IO.File.Exists(file2) |> equal false
    System.IO.File.Exists(file3) |> equal true

    System.IO.File.Delete(file)
    System.IO.File.Delete(file3)
    System.IO.File.Exists(file) |> equal false
    System.IO.File.Exists(file3) |> equal false

testCase "Path.GetTempFileName works"
<| fun () ->
    let file1 = System.IO.Path.GetTempFileName()
    let file2 = System.IO.Path.GetTempFileName()
    System.IO.File.Exists(file1) |> equal true
    System.IO.File.Exists(file2) |> equal true
    (file1 = file2) |> equal false
    System.IO.File.Delete(file1)
    System.IO.File.Delete(file2)

testCase "Environment/Console methods work"
<| fun () ->
    System.Environment.GetEnvironmentVariable("__FABLE_NON_EXISTENT_VAR__")
    |> equal null

    System.Environment.CurrentDirectory.Length > 0 |> equal true

    System.Console.Out.WriteLine("Hello stdout")
    System.Console.Error.WriteLine("Hello stderr")
