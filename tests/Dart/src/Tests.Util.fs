module Fable.Tests.Dart.Util

#if FABLE_COMPILER

open Fable.Core
open Fable.Core.Dart

type Assertion =
    interface end

[<ImportAll("package:test/test.dart")>]
type Test =
    static member test(msg: string, f: unit -> unit): unit = nativeOnly
    static member test(msg: string, f: unit -> Future<unit>): unit = nativeOnly
    static member expect(actual: obj, assertion: Assertion): unit = nativeOnly
    static member equals(value: obj): Assertion = nativeOnly
    static member isNot(assertion: Assertion): Assertion = nativeOnly
    static member throwsException: Assertion = nativeOnly

let testCase (msg: string) (f: unit -> unit) = Test.test(msg, f)
let testCaseAsync (msg: string) (f: unit -> Async<unit>) = Test.test(msg, fun () -> f () |> Async.StartAsFuture)
let testList (msg: string) (tests: unit list) = ()
let equal (expected: 'T) (actual: 'T) = Test.expect(actual, Test.equals(expected))
let notEqual (expected: 'T) (actual: 'T) = Test.expect(actual, Test.isNot(Test.equals(expected)))
let throwsAnyError (f: unit -> 'a): unit = Test.expect(f, Test.throwsException)

#else

open Expecto
let private expectoTestCase = testCase
let private expectoTestCaseAsync = testCaseAsync
let private expectoTestList = testList

let mutable private registeredTests = []
let mutable private registeredTestNames = Map.empty

let private uniqueTestName (name: string) =
    match Map.tryFind name registeredTestNames with
    | Some count ->
        registeredTestNames <- Map.add name (count + 1) registeredTestNames
        $"{name} ({count + 1})"
    | None ->
        registeredTestNames <- Map.add name 1 registeredTestNames
        name

let private register test =
    registeredTests <- test :: registeredTests

let testCase (msg: string) (f: unit -> unit) =
    register (expectoTestCase (uniqueTestName msg) f)

let testCaseAsync (msg: string) (f: unit -> Async<unit>) =
    register (expectoTestCaseAsync (uniqueTestName msg) (f ()))

let testList (msg: string) (tests: unit list) =
    let testCount = List.length tests
    let childTests = registeredTests |> List.take testCount |> List.rev
    registeredTests <- registeredTests |> List.skip testCount
    register (expectoTestList (uniqueTestName msg) childTests)

let getExpectoTests () = expectoTestList "All" (List.rev registeredTests)
let collectExpectoModule (name: string) (registerTests: unit -> unit) =
    let previousTests = registeredTests
    let previousNames = registeredTestNames
    registeredTests <- []
    registeredTestNames <- Map.empty
    registerTests ()
    let moduleTests = registeredTests |> List.rev
    registeredTests <- expectoTestList name moduleTests :: previousTests
    registeredTestNames <- previousNames

let equal expected actual: unit = Expecto.Expect.equal actual expected ""
let notEqual expected actual: unit = Expecto.Expect.notEqual actual expected ""
let throwsAnyError (f: unit -> 'a): unit = Expecto.Expect.throws (fun () -> f () |> ignore) ""

#endif

/// Matches the most specific type first, so an ArgumentOutOfRangeException is not reported as ArgumentException.
let raisedExceptionType (f: unit -> 'a): string =
    try
        f () |> ignore
        "no exception"
    with
    | :? System.ArgumentOutOfRangeException -> "ArgumentOutOfRangeException"
    | :? System.ArgumentNullException -> "ArgumentNullException"
    | :? System.ArgumentException -> "ArgumentException"
    | :? System.InvalidOperationException -> "InvalidOperationException"
    | :? System.Collections.Generic.KeyNotFoundException -> "KeyNotFoundException"
    | :? System.IndexOutOfRangeException -> "IndexOutOfRangeException"
    | :? System.FormatException -> "FormatException"
    | e -> "Unexpected exception: " + e.Message

let private run (f: unit -> 'a) =
    try
        f()
        |> Ok
    with e ->
        Error e.Message

module private RunResult =
    let wasError expected actual =
        match actual with
        | Error _ when System.String.IsNullOrEmpty expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value -> equal (Error expected) (Ok value)

    let wasErrorContaining expected actual =
        match actual with
        | Error _ when System.String.IsNullOrEmpty expected -> ()
        | Error (actual: string) when actual.Contains expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value -> equal (Error expected) (Ok value)

let throwsError (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasError expected

/// While `throwsError` tests exception message for equality,
/// this checks if error message CONTAINS passed message
let throwsErrorContaining (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasErrorContaining expected
