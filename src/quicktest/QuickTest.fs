module QuickTest

// Run `npm run build quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler and fable-library.
// When everything works, move the tests to the appropriate file in tests/Main.
// You can check the compiled JS in the "bin" folder within this directory.
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

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

type Ideable =
    { Id: int; Name: string }
    static member (+) (x: Ideable, y: Ideable) = x.Name.Length + y.Name.Length
    // with override this.ToString() = this.Name

type Ideable2 =
    { Id: int; Foo: int }
    static member (+) (x: Ideable2, y: Ideable2) = x.Foo + y.Foo

type Ideable3 =
    { Bar: int }
    member this.Id = this.Bar * 4
    static member (+) (x: Ideable3, y: Ideable3) = x.Bar * y.Bar

let inline dupId< ^t when ^t : (member Id : int)> (x: ^t) =
    let id = (^t : (member Id : int) x)
    id + id

let inline dupIdAndSum x y =
    (dupId x) * (x + y)

let test() =
    let i1 = dupIdAndSum { Id = 5; Name = "Test" } { Id = 5; Name = "ooooooo" }
    let i2 = dupIdAndSum { Id = 5; Foo = 4 } { Id = 5; Foo = 9 }
    let i3 = dupIdAndSum { Bar = 3 } { Bar = 10 }
    i1 + i2 + i3

test() |> printfn "%i"