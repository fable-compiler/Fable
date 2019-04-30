module Fable.Tests.Main
open System

let allTests =
  [|
    Applicative.tests
    Arithmetic.tests
    Arrays.tests
    Async.tests
    Chars.tests
    Comparison.tests
    Convert.tests
    CustomOperators.tests
    DateTimeOffset.tests
    DateTime.tests
    Dictionaries.tests
#if FABLE_COMPILER
    ElmishParser.tests
#endif
    Enumerable.tests
    Enum.tests
    Event.tests
    HashSets.tests
    Import.tests
    JsInterop.tests
    Lists.tests
    Maps.tests
    Misc.tests
    Observable.tests
    Option.tests
    RecordTypes.tests
    Reflection.tests
    Regex.tests
    ResizeArrays.tests
    Result.tests
    SeqExpressions.tests
    Seqs.tests
    Sets.tests
    SystemNumerics.tests
    Strings.tests
    Sudoku.tests
    TailCalls.tests
    TimeSpan.tests
    TupleTypes.tests
    TypeTests.tests
    UnionTypes.tests
    Uri.tests
  |]

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.JsInterop

// Import a polyfill for atob and btoa, used by fable-library
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
