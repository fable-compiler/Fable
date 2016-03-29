[<NUnit.Framework.TestFixture>] 
module Fable.Tests.SystemSets
open System
open System.Collections.Generic
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``HashSet creation works``() =
    let hs = HashSet<_>()
    equal 0 hs.Count

[<Test>]
let ``HashSet iteration works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore

    let i = ref 0.
    for v in hs do
       i := v + !i
    equal 385. !i

[<Test>]
let ``HashSet folding works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore
    hs |> Seq.fold (fun acc item -> acc + item) 0.
    |> equal 385.

[<Test>]
let ``HashSet.Count works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore
    hs.Count
    |> equal 10

[<Test>]
let ``HashSet.Add works``() =
    let hs = HashSet<_>()
    hs.Add("A", "Hello") |> ignore
    hs.Add("B", "World!") |> ignore
    hs.Count |> equal 2

[<Test>]
let ``HashSet.Clear works``() =
    let hs = HashSet<_>()
    hs.Add("A", 1) |> ignore
    hs.Add("B", 2) |> ignore
    hs.Clear()
    hs.Count |> equal 0

[<Test>]
let ``HashSet.Contains works``() =
    let hs = HashSet<_>()
    hs.Add("Hello") |> ignore
    hs.Add("World!") |> ignore
    hs.Contains("Hello") |> equal true
    hs.Contains("Everybody!") |> equal false

[<Test>]
let ``HashSet.CopyTo works``() =
    let hs = HashSet<_>()
    for i in 1 .. 9 do hs.Add(i) |> ignore

    let arr1 = Array.zeroCreate 9
    let arr2 = Array.zeroCreate 11
    let arr3 = Array.zeroCreate 7

    hs.CopyTo(arr1)         // [|1;2;3;4;5;6;7;8;9|]
    hs.CopyTo(arr2, 2)      // [|0;0;1;2;3;4;5;6;7;8;9|]
    hs.CopyTo(arr3, 3, 4)   // [|0;0;0;1;2;3;4|]

    let sum = fun acc item -> acc + item
    arr1 |> Seq.fold sum 0 |> equal 45
    arr1.Length |> equal 9

    arr2 |> Seq.fold sum 0 |> equal 45
    arr2.Length |> equal 11

    arr3 |> Seq.fold sum 0 |> equal 10
    arr3.Length |> equal 7

[<Test>]
let ``HashSet.Remove works``() =
    let hs = HashSet<_>()
    hs.Add("A") |> ignore
    hs.Add("B") |> ignore
    hs.Remove("A") |> equal true
    hs.Remove("C") |> equal false
