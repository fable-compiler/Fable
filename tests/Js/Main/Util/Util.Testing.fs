module Util.Testing

open System

#if FABLE_COMPILER
type TestKind =
| TestList of string * TestKind seq
| TestCase of (string*obj)

open Fable.Core
open Fable.Core.Testing

let testList (name: string) (tests: TestKind seq) = TestList( name, tests )
let testCase (msg: string) (test: unit->unit) = TestCase( msg, box test )
let testCaseAsync (msg: string) (test: unit->Async<unit>) = TestCase( msg, box(fun () -> test () |> Async.StartAsPromise) )

let equal expected actual: unit = Assert.AreEqual(actual, expected)
let notEqual expected actual: unit = Assert.NotEqual(actual, expected)

#else

open Expecto

let testList name tests = testList name tests
let testCase msg test = testCase msg test
let testCaseAsync msg test = testCaseAsync msg (test ())

let equal expected actual: unit = Expect.equal actual expected ""
let notEqual expected actual: unit = Expect.notEqual actual expected ""
#endif
