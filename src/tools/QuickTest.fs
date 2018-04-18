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


// I think this test is now invalid because of how Set module build it's tree
// It's important to note that here we hare comparing the Set object representation, it is not an equality check between 2 Set
testCase "Set.union works II" <| fun () ->
    Set.union (set [1; 2]) (set [3; 4; 5])
    |> equal (set [1; 2; 3; 4; 5])

// I think we should write it like that
// But this also means we need to reactivate: override __.Equals(_) = from Set type
// Line 604
testCase "Set.union works II" <| fun () ->
    let xs = Set.union (set [1; 2]) (set [3; 4; 5])
    let ys = (set [1; 2; 3; 4; 5])
    xs = ys |> equal true

testCase "Set.toSeq works" <| fun () ->
    let xs = seq [1.; 2.; 3.; 4.]
    let ys = Set.ofSeq xs
    let zs = Set.toSeq ys
    (Seq.item 2 xs) = (Seq.item 2 zs)
    |> equal true
