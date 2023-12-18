module Fable.Tests.Main
open System

let allTests =
  [|
    UtilTests.tests
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
    DateOnly.tests
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
    NestedAndRecursivePatternTests.tests
    Observable.tests
    Option.tests
    Queue.tests
    RecordTypes.tests
    Reflection.tests
    Regex.tests
    ResizeArrays.tests
    Result.tests
    SeqExpressions.tests
    Seqs.tests
    Sets.tests
    Stack.tests
    Strings.tests
    Sudoku.tests
    TailCalls.tests
    TimeOnly.tests
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
importSideEffects "./js/polyfill.js"

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative


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
    |> runTestsWithCLIArgs [] args

#endif
