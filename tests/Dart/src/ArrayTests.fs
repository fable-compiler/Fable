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
