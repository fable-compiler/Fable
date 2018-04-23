module QuickTest

// Use this template to make quick tests when adding new features to Fable.
// You must run a full build at least once (from repo root directory,
// type `sh build.sh` on OSX/Linux or just `build` on Windows). Then:
// - When making changes to Fable.Compiler run `build QuickFableCompilerTest`
// - When making changes to fable-core run `build QuickFableCoreTest`

// Please don't add this file to your commits

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open Fable.Import

let log (o: obj) =
    printfn "%O" o

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "Expected %A but got %A" expected actual

let testCase (msg: string) f: unit =
    printfn "%s" msg; f (); printfn ""

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

testCase "Map.toSeq works" <| fun () ->
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = Map.toSeq xs
    Seq.isEmpty ys |> equal false

testCase "Map.map works" <| fun () ->
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = xs |> Map.map (fun _ v -> v * 2.)
    ys.[3] |> equal 18.

testCase "Map.tryFindKey works" <| fun () ->
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.tryFindKey (fun k _ -> k = 3)
    |> Option.isSome
    |> equal true

testCase "Map.toArray works" <| fun () ->
    let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
    let mutable ys = Map.ofArray xs
    let mutable zs = Map.toArray ys
    snd xs.[2] = snd zs.[2]
    |> equal true
