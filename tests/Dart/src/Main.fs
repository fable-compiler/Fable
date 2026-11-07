module Fable.Tests.Dart.Main

let allTests =
    [|
        "Arithmetic", Arithmetic.tests
        "Async", Async.tests
        "Random", RandomTests.tests
        "Array", Array.tests
        "Comparison", Comparison.tests
        "CustomOperator", CustomOperator.tests
        "DateTime", DateTime.tests
        "Dictionary", Dictionary.tests
        "Enum", Enum.tests
        "Enumerable", Enumerable.tests
        "HashSet", HashSet.tests
        "InlineIfLambda", InlineIfLambda.tests
        "List", List.tests
        "Map", Map.tests
        "Misc", Misc.tests
        "Nullable", Nullable.tests
        "Option", Option.tests
        "Quotation", Quotation.tests
        "Record", Record.tests
        "Regex", Regex.tests
        "ResizeArray", ResizeArray.tests
        "Result", Result.tests
        "Rune", Rune.tests
        "Seq", Seq.tests
        "SeqExpression", SeqExpression.tests
        "Set", Set.tests
        "String", String.tests
        "Sudoku", Sudoku.tests
        "TailCall", TailCall.tests
        "TimeSpan", TimeSpan.tests
        "Tuple", Tuple.tests
        "Type", Type.tests
        "Union", Union.tests
    |]

#if FABLE_COMPILER
let main () =
    for _, test in allTests do
        test ()
    0
#else
open Expecto

[<EntryPoint>]
let main args =
    for name, test in allTests do
        Util.collectExpectoModule name test
    Util.getExpectoTests ()
    |> runTestsWithCLIArgs [] args
#endif