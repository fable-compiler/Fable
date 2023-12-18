module Fable.Tests.Main
open System

let allTests =
  [|
    Adaptive.tests
  |]

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.JsInterop

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative


let rec flattenTest (test: Util.Testing.TestKind) : unit =
    match test with
    |  Util.Testing.TestKind.TestList(name, tests) ->
        describe name (fun () ->
          for t in tests do
            flattenTest t)
    |  Util.Testing.TestKind.TestCase (name, test) ->
        it name (unbox test)


let run () =
    for t in allTests do
        flattenTest t
run()

#else

open Expecto

[<EntryPoint>]
let main args =
    Array.toList allTests
    |> testList "All"
    |> runTestsWithCLIArgs [] args

#endif
