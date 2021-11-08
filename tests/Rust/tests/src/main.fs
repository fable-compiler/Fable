module Tests

#if FABLE_COMPILER
open Fable.Core.JsInterop

let tests: unit[] = [|
    importAll "AnonRecordTests.rs"
    importAll "ArithmeticTests.rs"
    importAll "ArrayTests.rs"
    importAll "ClassTests.rs"
    importAll "ClosureTests.rs"
    importAll "ControlFlowTests.rs"
    importAll "EnumTests.rs"
    importAll "InterfaceTests.rs"
    importAll "ListTests.rs"
    importAll "NBodyTests.rs"
    importAll "OptionTests.rs"
    importAll "RecordTests.rs"
    importAll "ResizeArrayTests.rs"
    importAll "SeqTests.rs"
    importAll "StringTests.rs"
    importAll "TupleTests.rs"
    importAll "UnionTests.rs"
    importAll "UnitTests.rs"
|]

#endif

[<EntryPoint>]
let main _args =
    0
