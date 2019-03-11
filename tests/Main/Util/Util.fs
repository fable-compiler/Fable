module Fable.Tests.Util

open System

module Testing =
#if FABLE_COMPILER
    open Fable.Core
    open Fable.Core.Testing

    let testList (name: string) (tests: (string * obj) seq) = name, tests
    let testCase (msg: string) (test: unit->unit) = msg, box test
    let testCaseAsync (msg: string) (test: unit->Async<unit>) = msg, box(fun () -> test () |> Async.StartAsPromise)

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

#if FABLE_COMPILER
let foo: string = Fable.Core.JsInterop.importMember "../js/1foo.js"

[<Fable.Core.Import("foo", "../js/1foo.js")>]
let foo2: string = failwith "JS only"

let apply (f:Func<int,int,int>) (x:int) (y:int): int = Fable.Core.JsInterop.importMember "../js/1foo.js"
#else
let foo = "foo"
let foo2 = "foo"
let apply (f:Func<int,int,int>) (x:int) (y:int): int = f.Invoke(x, y)
#endif

// Idents starting with a digit doesn't cause an error
let ``1foo`` = 5

open Testing

let throwsError (expected: string) (f: unit -> 'a): unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            if not <| String.IsNullOrEmpty(expected) then
                equal expected e.Message
            false
    // TODO better error messages
    equal false success

let rec sumFirstSeq (zs: seq<float>) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> Seq.head zs
   | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

let f2 a b = a + b

// Assignment block as expression outside a function is NOT optimized
f2(let mutable x = 5 in let mutable y = 6 in x + y) |> ignore

let mutable a = 10

module B =
  let c = a
  a <- a + 5
  let mutable a = 20
  let d = f2 2 2
  let f2 a b = a - b

  module D =
    let d = a
    a <- a + 5
    let e = f2 2 2

// Test members with names conflicting with JS
let Int32Array = [|1;2|]

module Float64Array =
    let Float64Array = [|3.;4.|]

type private R = { a: int }
let mutable private x = 5

// Check that variables with same name (the compiler generated `matchValue` here)
// at module level don't conflict. See https://github.com/fable-compiler/Fable/issues/718#issuecomment-281533299
match { a = 5 } with
| { a = 3 } -> ()
| _ -> x <- 4

match { a = 2 } with
| { a = 2 } -> x <- 2
| _ -> x <- 4

module Foo =
    let update () = ()

module Bar =
    let rec nestedRecursive i = update (i+2)
    and update i = i + 5

let rec nonNestedRecursive s = update s
and update s = String.replicate 3 s

let mutable mutableValue = 1

let getValueTimes2() = mutableValue * 2

module Nested =
    let mutable nestedMutableValue = "a"

    let getValueTimes2() = nestedMutableValue + nestedMutableValue

    let getOuterValueTimes4() = mutableValue * 4

let getNestedValueTimes3() = Nested.nestedMutableValue + Nested.nestedMutableValue + Nested.nestedMutableValue
