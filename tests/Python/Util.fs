module Fable.Tests.Util

open System
open Fable.Core.Testing

module Testing =
#if FABLE_COMPILER
    open Fable.Core
    open Fable.Core.PyInterop

    let equal expected actual: unit = Assert.AreEqual(actual, expected)
    let notEqual expected actual: unit = Assert.NotEqual(actual, expected)

    type Fact() = inherit System.Attribute()
#else
    open Xunit
    type FactAttribute = Xunit.FactAttribute

    let equal<'T> (expected: 'T) (actual: 'T): unit = Assert.Equal(expected, actual)
    let notEqual<'T> (expected: 'T) (actual: 'T) : unit = Assert.NotEqual(expected, actual)
#endif

    let rec sumFirstSeq (zs: seq<float>) (n: int): float =
       match n with
       | 0 -> 0.
       | 1 -> Seq.head zs
       | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

open Testing

module private RunResult =
#if FABLE_COMPILER
    let private stringify result =
        let stringifyValue (value: obj) =
            match value with
            | _ when value = Fable.Core.JS.undefined -> "" // probably `unit`
            | :? string as value -> sprintf "\"%s\"" value
            | _ -> sprintf "%A" value
        let stringifyHead head value =
            match stringifyValue value with
            | "" -> head
            | str -> sprintf "%s %s" head str

        match result with
        | Ok value -> stringifyHead "Ok" value
        | Error msg -> stringifyHead "Error" msg
#endif

    let private equal expected actual =
#if FABLE_COMPILER
        // In JS: `{ "fields": [ "Error msg" ], "tag": 1 }`
        // -> stringify for prettier formatting and diff
        if expected <> actual then
            equal (stringify expected) (stringify actual)
#else
        equal expected actual
#endif

    let wasSuccess actual =
        match actual with
        | Ok _ -> ()
        | Error actual ->
            equal (Ok ()) (Error actual)

    let wasAnyError actual =
        match actual with
        | Error _ -> ()
        | Ok value ->
            equal (Error ()) (Ok value)

    let wasError expected actual =
        match actual with
        | Error _ when String.IsNullOrEmpty expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value  -> equal (Error expected) (Ok value)

    let wasErrorContaining expected actual =
        match actual with
        | Error _ when String.IsNullOrEmpty expected -> ()
        | Error (actual: string) when actual.Contains expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value  -> equal (Error expected) (Ok value)

let private run (f: unit -> 'a) =
    try
        f()
        |> Ok
    with e ->
        Error e.Message

let throwsError (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasError expected

/// While `throwsError` tests exception message for equality,
/// this checks if error message CONTAINS passed message
let throwsErrorContaining (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasErrorContaining expected

let throwsAnyError (f: unit -> 'a): unit =
    run f
    |> RunResult.wasAnyError

let doesntThrow (f: unit -> 'a): unit =
    run f
    |> RunResult.wasSuccess

let Int32Array = [|1;2|]

module Float64Array =
    let Float64Array = [|3.;4.|]

module Foo =
    let update () = ()

module Bar =
    let rec nestedRecursive i = update (i+2)
    and update i = i + 5

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

let rec nonNestedRecursive s = update s
and update s = String.replicate 3 s

let mutable mutableValue = 1
let mutable mutableValueOpt = Some 1

let getValueTimes2() = mutableValue * 2

module Nested =
    let mutable nestedMutableValue = "a"

    let getValueTimes2() = nestedMutableValue + nestedMutableValue

    let getOuterValueTimes4() = mutableValue * 4

let getNestedValueTimes3() = Nested.nestedMutableValue + Nested.nestedMutableValue + Nested.nestedMutableValue
