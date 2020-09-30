module QuickTest

// Run `npm run build quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let log (o: obj) =
    printfn "%O" o

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

let throwsError (expected: string) (f: unit -> 'a): unit =
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

let testCase (msg: string) f: unit =
    try
        printfn "%s" msg
        f ()
    with ex ->
        printfn "%s" ex.Message
        if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
            printfn "%s" ex.StackTrace
    printfn ""

let testCaseAsync msg f =
    testCase msg (fun () ->
        async {
            try
                do! f ()
            with ex ->
                printfn "%s" ex.Message
                if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
                    printfn "%s" ex.StackTrace
        } |> Async.StartImmediate)

let measureTime (f: unit -> unit) = emitJsStatement f """
    //js
    const startTime = process.hrtime();
    $0();
    const elapsed = process.hrtime(startTime);
    console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
    //!js
"""

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

let areEqual (x: obj) (y: obj) =
    x = y

type MyUnion1 = Foo of int * int | Bar of float | Baz
type MyUnion2 = Foo of int * int
    with override _.ToString() = "ffff"

type MyRecord1 = { Foo: int; Bar: string }
type MyRecord2 = { Foo: int; Bar: string }

testCase "Two unions of different type with same shape are not equal" <| fun () ->
    areEqual (MyUnion1.Foo(1,2)) (MyUnion2.Foo(1,2)) |> equal false
    areEqual (MyUnion1.Foo(1,2)) (MyUnion1.Foo(1,2)) |> equal true

testCase "Two records of different type with same shape are not equal" <| fun () ->
    areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord2.Foo = 2; Bar = "oh" } |> equal false
    areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord1.Foo = 2; Bar = "oh" } |> equal true

testCase "Union to string" <| fun () ->
    MyUnion1.Foo(1,2) |> string |> equal "Foo (1,2)"
    MyUnion1.Bar 4.5 |> string |> equal "Bar 4.5"
    MyUnion1.Baz |> string |> equal "Baz"
    MyUnion1.Foo(1,2) |> sprintf "%A" |> equal "Foo (1,2)"
    MyUnion1.Bar 4.5 |> sprintf "%A" |> equal "Bar 4.5"
    MyUnion1.Baz |> sprintf "%A" |> equal "Baz"

// testCase "Record to string" <| fun () ->
//     { MyRecord1.Foo = 2; Bar = "oh" } |> string |> equal "{Foo = 2; Bar = oh}"
//     { MyRecord1.Foo = 2; Bar = "oh" } |> sprintf "%A" |> equal "{Foo = 2; Bar = oh}"
