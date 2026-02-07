module Fable.Tests.Util

open System
open Fable.Core.Testing

module Testing =
#if FABLE_COMPILER
    open Fable.Core

    let equal expected actual: unit = Assert.AreEqual(actual, expected)
    let notEqual expected actual: unit = Assert.NotEqual(actual, expected)

    type FactAttribute() = inherit System.Attribute()
#else
    open Xunit
    type FactAttribute = Xunit.FactAttribute

    let equal<'T> (expected: 'T) (actual: 'T): unit = Assert.Equal(expected, actual)
    let notEqual<'T> (expected: 'T) (actual: 'T) : unit = Assert.NotEqual(expected, actual)
#endif

open Testing

let private run (f: unit -> 'a) =
    try
        f()
        |> Ok
    with e ->
        Error e.Message

module private RunResult =
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

let throwsError (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasError expected

let throwsErrorContaining (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasErrorContaining expected

let throwsAnyError (f: unit -> 'a): unit =
    run f
    |> RunResult.wasAnyError

let doesntThrow (f: unit -> 'a): unit =
    run f
    |> RunResult.wasSuccess
