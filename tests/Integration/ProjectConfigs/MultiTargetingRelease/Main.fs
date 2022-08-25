module Fable.Tests.MultiTargetingRelease

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative
let equals expected actual = Assert.AreEqual(actual, expected)

describe "Multi-Targeting Release" (fun () ->
    let isDefined = "is defined"
    let notDefined = "not defined"

    it "should not define DEBUG" (fun () ->
        #if DEBUG
        isDefined
        #else
        notDefined
        #endif
        |> equals notDefined
    )

    it "should define first listed framework" (fun () ->
        #if NET5_0
        isDefined
        #else
        notDefined
        #endif
        |> equals isDefined
    )
)
