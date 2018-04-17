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
testCase "Set.IsEmpty works" <| fun () ->
    let xs = Set.empty<int>
    xs.IsEmpty |> equal true
    let xxs = Set.empty<string>
    xxs.IsEmpty |> equal true
    let ys = set [1; 1]
    ys.IsEmpty |> equal false
    let zs = set ["1"]
    zs.IsEmpty |> equal false
