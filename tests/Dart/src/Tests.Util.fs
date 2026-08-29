module Fable.Tests.Dart.Util

open Fable.Core

type Assertion =
    interface end

[<ImportAll("package:test/test.dart")>]
type Test =
    static member test(msg: string, f: unit -> unit): unit = nativeOnly
    static member expect(actual: obj, assertion: Assertion): unit = nativeOnly
    static member equals(value: obj): Assertion = nativeOnly
    static member isNot(assertion: Assertion): Assertion = nativeOnly
    static member throwsException: Assertion = nativeOnly

let testCase (msg: string) (f: unit -> unit) = Test.test(msg, f)
let testList (msg: string) (tests: unit list) = ()
let equal (expected: 'T) (actual: 'T) = Test.expect(actual, Test.equals(expected))
let notEqual (expected: 'T) (actual: 'T) = Test.expect(actual, Test.isNot(Test.equals(expected)))
let throwsAnyError (f: unit -> 'a): unit = Test.expect(f, Test.throwsException)

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
