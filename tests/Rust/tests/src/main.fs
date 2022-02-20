module Tests

#if FABLE_COMPILER
open Fable.Core.JsInterop

let tests: unit[] = [|
    importAll "AnonRecordTests.rs"
    importAll "ArithmeticTests.rs"
    importAll "ArrayTests.rs"
    importAll "CharTests.rs"
    importAll "ClassTests.rs"
    importAll "ClosureTests.rs"
    importAll "ControlFlowTests.rs"
    importAll "DictionaryTests.rs"
    importAll "EnumTests.rs"
    importAll "HashSetTests.rs"
    importAll "InterfaceTests.rs"
    importAll "ListTests.rs"
    importAll "MapTests.rs"
    importAll "MiscTests.rs"
    importAll "NBodyTests.rs"
    importAll "OptionTests.rs"
    importAll "RecordTests.rs"
    importAll "ResizeArrayTests.rs"
    importAll "ResultTests.rs"
    importAll "SeqTests.rs"
    importAll "SetTests.rs"
    importAll "StringTests.rs"
    importAll "TailCallTests.rs"
    importAll "TupleTests.rs"
    importAll "UnionTests.rs"
|]

#endif

[<EntryPoint>]
let main _args =
    0
