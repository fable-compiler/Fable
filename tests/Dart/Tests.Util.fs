module Fable.Tests.Dart.Util

open Fable.Core

type Assertion<'T> =
    interface end

[<ImportAll("package:test/test.dart")>]
type Test =
    static member test(msg: string, f: unit -> unit): unit = nativeOnly
    static member expect(actual: 'T, assertion: Assertion<'T>): unit = nativeOnly
    static member equals(value: 'T): Assertion<'T> = nativeOnly

let testCase msg f = Test.test(msg, f)
let equal expected actual = Test.expect(actual, Test.equals(expected))
