module Fable.Tests.Dart.Util

open Fable.Core

type Assertion =
    interface end

[<ImportAll("package:test/test.dart")>]
type Test =
    static member test(msg: string, f: unit -> unit): unit = nativeOnly
    static member expect(actual: obj, assertion: Assertion): unit = nativeOnly
    static member equals(value: obj): Assertion = nativeOnly
    static member throwsException: Assertion = nativeOnly

let testCase (msg: string) (f: unit -> unit) = Test.test(msg, f)
let equal (expected: obj) (actual: obj) = Test.expect(actual, Test.equals(expected))
let throwsAnyError (f: unit -> 'a): unit = Test.expect(f, Test.throwsException)
