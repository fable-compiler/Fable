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

[<TaggedUnion("kind")>]
type TypeScriptDU =
    | [<Case("foo")>] Foo of {| kind: string; foo: string |}
    | [<Case("bar")>] Bar of {| kind: string; bar: string |}
    | [<Case("baz")>] Baz of {| kind: string; baz: string |}

testCase "Case testing with TypeScript-style discriminated unions works" <| fun () ->
    let describe = function
        | Foo x -> "foo: " + x.foo
        | Bar x -> "bar: " + x.bar
        | Baz x -> "baz: " + x.baz
    Foo {| kind = "foo"; foo = "42" |} |> describe |> equal "foo: 42"
    Bar {| kind = "bar"; bar = "hello" |} |> describe |> equal "bar: hello"
    Baz {| kind = "baz"; baz = "world" |} |> describe |> equal "baz: world"