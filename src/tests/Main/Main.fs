module Fable.Tests.Main

// This is necessary to make webpack collect all test files

#if FABLE_COMPILER
open Fable.Core.JsInterop
importAll "./ApplicativeTests.fs" |> ignore
importAll "./ArithmeticTests.fs" |> ignore
importAll "./ArrayTests.fs" |> ignore
importAll "./AsyncTests.fs" |> ignore
importAll "./ComparisonTests.fs" |> ignore
importAll "./ConvertTests.fs" |> ignore
importAll "./DateTimeTests.fs" |> ignore
importAll "./DictionaryTests.fs" |> ignore
importAll "./EnumerableTests.fs" |> ignore
importAll "./EnumTests.fs" |> ignore
importAll "./EventTests.fs" |> ignore
importAll "./HashSetTests.fs" |> ignore
importAll "./ImportTests.fs" |> ignore
importAll "./JsInteropTests.fs" |> ignore
importAll "./JsonTests.fs" |> ignore
importAll "./ListTests.fs" |> ignore
importAll "./MapTests.fs" |> ignore
importAll "./MiscTests.fs" |> ignore
importAll "./ObservableTests.fs" |> ignore
importAll "./RecordTypeTests.fs" |> ignore
importAll "./ReflectionTests.fs" |> ignore
importAll "./RegexTests.fs" |> ignore
importAll "./ResizeArrayTests.fs" |> ignore
importAll "./SeqExpressionTests.fs" |> ignore
importAll "./SeqTests.fs" |> ignore
importAll "./SetTests.fs" |> ignore
importAll "./StringTests.fs" |> ignore
importAll "./SudokuTest.fs" |> ignore
importAll "./TailCallTests.fs" |> ignore
importAll "./TupleTypeTests.fs" |> ignore
importAll "./TypeTests.fs" |> ignore
importAll "./UnionTypeTests.fs" |> ignore
#endif