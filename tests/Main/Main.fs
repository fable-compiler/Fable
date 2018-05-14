module Fable.Tests.Main

let allTests =
  [|
    // TODO: Misc and remaining tests in
    // Applicable, Arithmetic, seqs, lists and arrays

    Applicative.tests
    Arithmetic.tests
    Async.tests
    Arrays.tests
    Char.tests
    Convert.tests
    Comparison.tests
    CustomOperators.tests
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
    Maps.tests
    Observable.tests
    RecordTypes.tests
    Regex.tests
    Result.tests
    Seqs.tests
    ``Seq Expressions``.tests
    Sets.tests
    Strings.tests
    Sudoku.tests
    TailCalls.tests
    TupleTypes.tests
    TypeTests.tests
    Option.tests
    UnionTypes.tests
    #if FABLE_COMPILER
    ElmishParser.tests
    #endif
    Json.tests
    Reflection.tests
    Misc.tests
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
