module Fable.Tests.TypeScript.Main

open System
open Fable.Tests

let allTests =
  [|
    Arithmetic.tests
  |]

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.JsInterop

// Import a polyfill for atob and btoa, used by fable-library
// but not available in node.js runtime
importSideEffects "../Js/Main/js/polyfill.js"

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative

// TODO: Emit declarations automatically when there are global variables
emitJsStatement () """
declare var it: any;
declare var describe: any;
"""

let rec flattenTest (test: Util.Testing.TestKind) : unit =
    match test with
    | Util.Testing.TestKind.TestList(name, tests) ->
        describe name (fun () ->
          for t in tests do
            flattenTest t)
    | Util.Testing.TestKind.TestCase (name, test) ->
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
    |> runTestsWithArgs defaultConfig args

#endif
