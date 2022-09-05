#load "LoadedFile.fs"
open LoadedFile

#r "nuget: Thoth.Json"
open Thoth.Json

#r "nuget: Fable.Core"
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let [<Global>] describe (name: string) (f: unit->unit) : unit = jsNative
let [<Global>] it (msg: string) (f: unit->unit) : unit = jsNative
let equals expected actual = Assert.AreEqual(actual, expected)

describe "FSharpScript" (fun () ->
    it "should load referenced files" (fun () ->
        [ loadedFileStr ] |> equals [ "loadedFile" ]
    )

    it "should load nuget packages" (fun () ->
        Decode.fromString (Decode.array Decode.int) "[1, 2, 3]" |> equals (Ok [| 1; 2; 3 |])
    )
)
