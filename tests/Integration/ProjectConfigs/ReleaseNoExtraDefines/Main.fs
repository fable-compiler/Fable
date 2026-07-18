module Fable.Tests.DefineConstants.CustomConfiguration

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let inline describe (name: string) (f: unit->unit) : unit = import "describe" "node:test"
let inline it (msg: string) (f: unit->unit) : unit = import "it" "node:test"

let equals expected actual = Assert.AreEqual(actual, expected)

describe "ReleaseNoExtraDefines" (fun () ->
    let isDefined = "is defined"
    let notDefined = "not defined"

    it "should NOT define DEBUG" (fun () ->
        #if DEBUG
        isDefined
        #else
        notDefined
        #endif
        |> equals notDefined
    )

    it "should define RELEASE" (fun () ->
        #if RELEASE
        isDefined
        #else
        notDefined
        #endif
        |> equals isDefined
    )
)
