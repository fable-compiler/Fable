module Fable.Tests.Main

let allTests =
  [|
     Applicative.tests
     Arithmetic.tests
     Async.tests
     Arrays.tests
     Char.tests
     Convert.tests
     Comparison.tests
     DateTime.tests
     DateTimeOffset.tests
     Dictionaries.tests
     Enumerable.tests
     Enum.tests
     Event.tests
     HashSets.tests
     Import.tests
     JsInterop.tests
     Lists.tests
     Observable.tests
     RecordTypes.tests
     Regex.tests
     Seqs.tests
     ``Seq Expressions``.tests
     Sets.tests
     Maps.tests
     Option.tests
  |]

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.JsInterop

// Import a polyfill for atob and btoa, used by fable-core
// but not available in node.js runtime
importSideEffects "./js/polyfill"

let [<Global>] describe (name: string) (f: unit->unit) = jsNative
let [<Global>] it (msg: string) (f: unit->unit) = jsNative

let run () =
    for (name, tests) in allTests do
        describe name (fun () ->
            for (msg, test) in tests do
                it msg (unbox test))
run()

#else

open Expecto

[<EntryPoint>]
let main args =
  Array.toList allTests
  |> testList "All"
  |> runTestsWithArgs defaultConfig args

#endif
