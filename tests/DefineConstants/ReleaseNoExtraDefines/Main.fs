module Fable.Tests.DefineConstants.CustomConfiguration

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative
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
