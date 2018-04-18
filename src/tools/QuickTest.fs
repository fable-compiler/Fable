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

testCase "Set.IsProperSubset works" <| fun () ->
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsProperSubsetOf xs
    |> equal true

testCase "Set.isProperSuperset works" <| fun () ->
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSuperset xs ys
    |> equal true

testCase "Set.IsProperSuperset works" <| fun () ->
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsProperSupersetOf ys
    |> equal true

testCase "Set.ofList works" <| fun () ->
    let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

testCase "Set.ofArray works" <| fun () ->
    let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
    xs.Count |> equal 4

testCase "Set.ofSeq works" <| fun () ->
    let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

testCase "Set.toList works" <| fun () ->
    let xs = [1.; 2.; 3.; 4.]
    let ys = Set.ofList xs
    let zs = Set.toList ys
    xs.[2] = zs.[2]
    |> equal true

testCase "Set.toArray works" <| fun () ->
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = Set.ofArray xs
    let zs = Set.toArray ys
    xs.[2] = zs.[2]
    |> equal true

testCase "Set.toSeq works" <| fun () ->
    let xs = seq [1.; 2.; 3.; 4.]
    let ys = Set.ofSeq xs
    let zs = Set.toSeq ys
    (Seq.item 2 xs) = (Seq.item 2 zs)
    |> equal true
