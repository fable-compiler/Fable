module Fable.Tests.Dart.Array

open Util

let tests () =
    testCase "Array.length works" <| fun () ->
        let xs = [|"a"; "a"; "a"; "a"|]
        Array.length xs |> equal 4

    // testCase "Array.countBy works" <| fun () ->
    //     let xs = [|1; 2; 3; 4|]
    //     xs |> Array.countBy (fun x -> x % 2)
    //     |> Array.length |> equal 2

    testCase "Array.map works" <| fun () ->
        let xs = [|1.|]
        let ys = xs |> Array.map (fun x -> x * 2.)
        ys.[0] |> equal 2.

    testCase "Array.map doesn't execute side effects twice" <| fun () -> // See #1140
        let mutable c = 0
        let i () = c <- c + 1; c
        [| i (); i (); i () |] |> Array.map (fun x -> x + 1) |> ignore
        equal 3 c

    testCase "Array.map2 works" <| fun () ->
        let xs = [|1.|]
        let ys = [|2.|]
        let zs = Array.map2 (*) xs ys
        zs.[0] |> equal 2.

    testCase "Array.map3 works" <| fun () ->
        let value1 = [|1.|]
        let value2 = [|2.|]
        let value3 = [|3.|]
        let zs = Array.map3 (fun a b c -> a * b * c) value1 value2 value3
        zs.[0] |> equal 6.

    testCase "Array.mapi works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.mapi (fun i x -> float i + x)
        ys.[1] |> equal 3.

    testCase "Array.mapi2 works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = [|2.; 3.|]
        let zs = Array.mapi2 (fun i x y -> float i + x * y) xs ys
        zs.[1] |> equal 7.

    testCase "Array.find works" <| fun () ->
        let xs = [|1us; 2us; 3us; 4us|]
        xs |> Array.find ((=) 2us)
        |> equal 2us

    testCase "Array.findIndex works" <| fun () ->
        let xs = [|1.f; 2.f; 3.f; 4.f|]
        xs |> Array.findIndex ((=) 2.f)
        |> equal 1

    testCase "Array.findBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.find ((>) 4.) |> equal 1.
        xs |> Array.findBack ((>) 4.) |> equal 3.

    testCase "Array.findIndexBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.findIndex ((>) 4.) |> equal 0
        xs |> Array.findIndexBack ((>) 4.) |> equal 2

    testCase "Array.tryFindBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.tryFind ((>) 4.) |> equal (Some 1.)
        xs |> Array.tryFindBack ((>) 4.) |> equal (Some 3.)
        xs |> Array.tryFindBack ((=) 5.) |> equal None

    testCase "Array.tryFindIndexBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.tryFindIndex ((>) 4.) |> equal (Some 0)
        xs |> Array.tryFindIndexBack ((>) 4.) |> equal (Some 2)
        xs |> Array.tryFindIndexBack ((=) 5.) |> equal None

    testCase "Array.fold works" <| fun () ->
        let xs = [|1y; 2y; 3y; 4y|]
        let total = xs |> Array.fold (+) 0y
        total |> equal 10y

    testCase "Array.fold2 works" <| fun () ->
        let xs = [|1uy; 2uy; 3uy; 4uy|]
        let ys = [|1uy; 2uy; 3uy; 4uy|]
        let total = Array.fold2 (fun x y z -> x + y + z) 0uy xs ys
        total |> equal 20uy

    testCase "Array.foldBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let total = Array.foldBack (fun x acc -> acc - x) xs 0.
        total |> equal -10.

    testCase "Array.foldBack2 works" <| fun () ->
        let xs = [|1; 2; 3; 4|]
        let ys = [|1; 2; 3; 4|]
        let total = Array.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
        total |> equal -4
