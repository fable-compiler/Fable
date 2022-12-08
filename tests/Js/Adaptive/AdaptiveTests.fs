module Fable.Tests.Adaptive

open System
open Util.Testing
open FSharp.Data.Adaptive

let simple () =
    let x = AVal.init 3
    let mappedX = x |> AVal.map (fun x' -> x' + 1)
    let adaptiveList =
        mappedX
        |> AList.single
        |> AList.mapA id
    let firstForce = adaptiveList |> AList.force
    transact (fun () -> x.Value <- 0)
    let secondForce = adaptiveList |> AList.force
    firstForce, secondForce

let tests = testList "Adaptive" [
    testCase "FSharp.Data.Adaptive works" <| fun () -> // See #2291
        let first, second = simple ()
        equal 4 first.[0]
        equal 1 second.[0]
]
