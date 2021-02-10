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
   printfn "%A" o

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

let func (f: {| foo: 'a -> 'b|}) (a: 'a) = f.foo a

testCase "Currying/uncurrying works with generic records returning lambdas" <| fun () ->
    let f = func {| foo = fun x y -> x ** y |} 5.
    f 3. |> equal 125.

type Fn = bool -> int -> string
type Thing =
    | In of Fn
    | Out of Fn

let findThing (things:Thing list) =
    let folder (a : Fn option) t =
        match t with
        | In x -> Some x // Found an In, set accumulator
        | _ -> a // pass accumulator through unchanged

    things |> List.fold folder None  // Searching for an "In x"

testCase "Curried functions being mangled via DU, List.fold and match combination #2356" <| fun _ ->
    let testData = [ In (fun b i -> "fly"); Out (fun b i -> "fade")]

    let test = match findThing testData with
                        | Some f -> f true 1
                        | None -> "nothing"
    test |> equal "fly"

testCase "Option uncurrying #2116" <| fun _ ->
    let optionFn = Some (fun x y -> x + y)

    let list = List.choose id [optionFn]
    List.length list |> equal 1
    let x =
        match list with
        | [f] -> f 3 4
        | _ -> -1
    equal 7 x

let fortyTwo () () =
    42

for someFun in [fortyTwo] do
    printfn "%A" <| someFun () ()