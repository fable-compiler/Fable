// module/namespace declaration can be missing in last files of apps, see #2576

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let inline describe (name: string) (f: unit->unit) : unit = import "describe" "node:test"
let inline it (msg: string) (f: unit->unit) : unit = import "it" "node:test"

let equals expected actual = Assert.AreEqual(actual, expected)

describe "ConsoleApp" (fun () ->
    it "Last file in console apps can omit module/namespace declaration" (fun () ->
        2 + 2 |> equals 4
    )
)
