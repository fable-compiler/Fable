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

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "Expected %A but got %A" expected actual

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// [<Test>]
// let ``My Test``() =
//     Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
//     Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4

// You'll have to run your test manually, sorry!
// ``My Test``()

let makeSome (x: 'a): 'a option =
    Some x

let ``Generic options work``() =
    let x1 = makeSome ()
    let x2 = makeSome None
    let x3 = makeSome null |> makeSome
    Option.isSome x1 |> equal true
    Option.isNone x1 |> equal false
    x1.IsSome |> equal true
    x1.IsNone |> equal false
    match x1 with Some _ -> true | None -> false
    |> equal true
    Option.isSome x2 |> equal true
    Option.isNone x2 |> equal false
    x2.IsSome |> equal true
    x2.IsNone |> equal false
    match x2 with
    | Some(Some _) -> 0
    | Some(None) -> 1
    | None -> 2
    |> equal 1
    Option.isSome x3 |> equal true
    Option.isNone x3 |> equal false
    x3.IsSome |> equal true
    x3.IsNone |> equal false
    match x3 with
    | None -> 0
    | Some(None) -> 1
    | Some(Some _) -> 2
    |> equal 2

let ``Nested options work``() =
    let x1 = Some(Some 5)
    let x2 = Some(Some ())
    let x3 = Some(None)
    Option.isSome x1 |> equal true
    Option.isNone x1 |> equal false
    x1.IsSome |> equal true
    x1.IsNone |> equal false
    match x1 with
    | Some(Some 5) -> 0
    | Some(Some _) -> 1
    | Some(None) -> 2
    | None -> 3
    |> equal 0
    Option.isSome x2 |> equal true
    Option.isNone x2 |> equal false
    x2.IsSome |> equal true
    x2.IsNone |> equal false
    match x2 with
    | Some(None) -> 0
    | Some(Some _) -> 1
    | None -> 2
    |> equal 1
    Option.isSome x3 |> equal true
    Option.isNone x3 |> equal false
    x3.IsSome |> equal true
    x3.IsNone |> equal false
    match x3 with
    | None -> 0
    | Some(Some _) -> 1
    | Some(None) -> 2
    |> equal 2

``Generic options work``()
``Nested options work``()

type Option2<'T> =
    | Some2 of 'T
    | None2

module Option =
    let apply x f =
        match (x, f) with
        | Some x, Some f -> Some (f x)
        | _ -> None

    let apply2 x f =
        match (x, f) with
        | Some2 x, Some2 f -> Some2 (f x)
        | _ -> None2

    module Operators =
        let inline (<*>) m x = apply x m
        let inline (<**>) m x = apply2 x m

open Option.Operators

[<Test>]
let ``Option.apply (<*>) auto curried`` () =
    let f x y = x + y
    let r = Some f <*> Some 2 <*> Some 3
    r |> equal (Some 5)

let ``Option.apply (<**>) auto curried`` () =
    let f x y = x + y
    let r = Some2 f <**> Some2 2 <**> Some2 3
    r |> equal (Some2 5)

``Option.apply (<*>) auto curried`` ()
``Option.apply (<**>) auto curried`` ()