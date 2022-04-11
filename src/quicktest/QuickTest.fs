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

let log (o: obj) =
    JS.console.log(o)
    // printfn "%A" o

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

let throwsAnyError (f: unit -> 'a): unit =
    let success =
        try
            f() |> ignore
            true
        with e ->
            printfn "Got expected error: %s" e.Message
            false
    if success then
        printfn "[ERROR EXPECTED]"

let measureTime (f: unit -> unit) = emitJsStatement () """
   //js
   const startTime = process.hrtime();
   f();
   const elapsed = process.hrtime(startTime);
   console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
   //!js
"""

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

module Enumerator =
    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member x.GetEnumerator() = f()
        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = f() :> System.Collections.IEnumerator
        override xs.ToString() =
            let maxCount = 4
            let mutable i = 0
            let mutable str = "seq ["
            use e = (xs :> IEnumerable<'T>).GetEnumerator()
            while (i < maxCount && e.MoveNext()) do
                if i > 0 then str <- str + "; "
                str <- str + (string e.Current)
                i <- i + 1
            if i = maxCount then
                str <- str + "; ..."
            str + "]"

let checkNonNull argName arg = if isNull arg then nullArg argName

let mkSeq (f: unit -> IEnumerator<'T>): seq<'T> =
    Enumerator.Enumerable(f) :> IEnumerable<'T>

let ofSeq (xs: seq<'T>): IEnumerator<'T> =
    checkNonNull "source" xs
    xs.GetEnumerator()


let compareWith (comparer: 'T -> 'T -> int) (xs: seq<'T>) (ys: seq<'T>): int =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable c = 0
    let mutable b1 = e1.MoveNext()
    let mutable b2 = e2.MoveNext()
    while c = 0 && b1 && b2 do
        c <- comparer e1.Current e2.Current
        if c = 0 then
            b1 <- e1.MoveNext()
            b2 <- e2.MoveNext()
    if c <> 0 then c
    elif b1 then 1
    elif b2 then -1
    else 0
