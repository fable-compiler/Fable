module Fable.Tests.HashSets

open Util.Testing
open System.Collections.Generic

type MyRefType(i: int) =
    member x.Value = i

let inline set l =
    let xs = HashSet<_>()
    for x in l do
        xs.Add x |> ignore
    xs

type MyRecord = { a: int }

type R = { i: int; s: string }

type Apa<'t when 't : equality>() =
    let state = HashSet<'t>()
    member _.Add t = state.Add t |> ignore
    member _.Contains t = state.Contains t

let tests =
  testList "HashSets" [
    testCase "HashSet ctor from Enumerable works" <| fun () ->
        let s = List.toSeq [1;2;2;3]
        let xs = HashSet<int>(s)
        xs |> Seq.toList
        |> equal [1;2;3]

    testCase "HashSet ctor creates empty HashSet" <| fun () ->
        let xs = HashSet<int>()
        xs |> Seq.isEmpty
        |> equal true

    testCase "HashSets with IEqualityComparer work" <| fun () ->
        let x = MyRefType(4)
        let y = MyRefType(4)
        let z = MyRefType(6)
        let set = HashSet<_>()
        set.Add(x) |> equal true
        set.Contains(x) |> equal true
        set.Contains(y) |> equal false

        let comparer =
            { new IEqualityComparer<MyRefType> with
                member _.Equals(x, y) = x.Value = y.Value
                member _.GetHashCode(x) = x.Value }
        let set2 = HashSet<_>(comparer)
        set2.Add(x) |> equal true
        set2.Contains(x) |> equal true
        set2.Contains(y) |> equal true
        set2.Contains(z) |> equal false

    testCase "HashSet.Add returns true if not present" <| fun () ->
        let xs = set []
        xs.Add(1) |> equal true
        xs.Count |> equal 1

    testCase "HashSet.Add returns false if already present" <| fun () ->
        let xs = set [1]
        xs.Add(1) |> equal false
        xs.Count |> equal 1

    testCase "HashSet.Remove works when item is present" <| fun () ->
        let xs = set [1]
        xs.Remove 1 |> equal true
        xs.Count |> equal 0

    testCase "HashSet.Remove works when item is not present" <| fun () ->
        let xs = set [1; 2]
        xs.Remove 3 |> equal false
        xs.Count |> equal 2

    testCase "HashSet.UnionWith works" <| fun () ->
        let xs = set [1; 2]
        let ys = set [2; 4]
        xs.UnionWith ys
        (xs.Contains 1 && xs.Contains 2 && xs.Contains 4)
        |> equal true

    testCase "HashSet.IntersectWith works" <| fun () ->
        let xs = set [1; 2]
        let ys = set [2; 4]
        xs.IntersectWith ys
        xs.Contains 1 |> equal false
        xs.Contains 2 |> equal true

    testCase "IntersectWith works with custom comparison" <| fun () -> // See #2566
        let ignoreCase =
            { new IEqualityComparer<string> with
                member _.Equals(s1: string, s2: string) =
                    s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
                member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
        let set = new HashSet<string>(["Foo"; "bar"], ignoreCase)
        set.Contains("foo") |> equal true
        set.Contains("Foo") |> equal true
        set.Contains("bar") |> equal true
        set.Contains("Bar") |> equal true
        set.IntersectWith(["foo"; "bar"])
        set.Count |> equal 2
        set.IntersectWith(["Foo"; "Bar"])
        set.Count |> equal 2

    testCase "HashSet.ExceptWith works" <| fun () ->
        let xs = set [1; 2]
        let ys = set [2; 4]
        xs.ExceptWith ys
        xs.Contains 1 |> equal true
        xs.Contains 2 |> equal false

    testCase "HashSet creation works" <| fun () ->
        let hs = HashSet<_>()
        equal 0 hs.Count

    testCase "HashSet iteration works" <| fun () ->
        let hs = HashSet<_>()
        for i in 1. .. 10. do hs.Add(i*i) |> ignore

        let i = ref 0.
        for v in hs do
           i := v + !i
        equal 385. !i

    testCase "HashSet folding works" <| fun () ->
        let hs = HashSet<_>()
        for i in 1. .. 10. do hs.Add(i*i) |> ignore
        hs |> Seq.fold (fun acc item -> acc + item) 0.
        |> equal 385.

    testCase "HashSet.Count works" <| fun () ->
        let hs = HashSet<_>()
        for i in 1. .. 10. do hs.Add(i*i) |> ignore
        hs.Count
        |> equal 10
        let xs = set []
        xs.Count |> equal 0
        let ys = set [1]
        ys.Count |> equal 1
        let zs = set [1; 1]
        zs.Count |> equal 1
        let zs' = set [1; 2]
        zs'.Count |> equal 2

    testCase "HashSet.Add works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add("A") |> equal true
        hs.Add("B") |> equal true
        hs.Count |> equal 2

    testCase "HashSet.Add with records works" <| fun () ->
        let x1 = { a = 5 }
        let x2 = { a = 5 }
        let x3 = { a = 10 }
        let hs = HashSet<_>()
        hs.Add(x1) |> equal true
        hs.Add(x2) |> equal false
        hs.Add(x3) |> equal true
        hs.Count |> equal 2

    testCase "HashSet.Clear works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add(1) |> equal true
        hs.Add(2) |> equal true
        hs.Clear()
        hs.Count |> equal 0

    testCase "HashSet.Contains works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add("Hello") |> equal true
        hs.Add("World!") |> equal true
        hs.Contains("Hello") |> equal true
        hs.Contains("Everybody!") |> equal false

    // TODO!!!
    // testCase "HashSet.CopyTo works" <| fun () ->
    //     let hs = HashSet<_>()
    //     for i in 1 .. 9 do hs.Add(i) |> ignore

    //     let arr1 = Array.zeroCreate 9
    //     let arr2 = Array.zeroCreate 11
    //     let arr3 = Array.zeroCreate 7

    //     hs.CopyTo(arr1)         // [|1;2;3;4;5;6;7;8;9|]
    //     hs.CopyTo(arr2, 2)      // [|0;0;1;2;3;4;5;6;7;8;9|]
    //     hs.CopyTo(arr3, 3, 4)   // [|0;0;0;1;2;3;4|]

    //     let sum = fun acc item -> acc + item
    //     arr1 |> Seq.fold sum 0 |> equal 45
    //     arr1.Length |> equal 9

    //     arr2 |> Seq.fold sum 0 |> equal 45
    //     arr2.Length |> equal 11

    //     arr3 |> Seq.fold sum 0 |> equal 10
    //     arr3.Length |> equal 7

    testCase "HashSet.Remove works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add("A") |> ignore
        hs.Add("B") |> ignore
        hs.Remove("A") |> equal true
        hs.Remove("C") |> equal false

    testCase "HashSet.Remove with records works" <| fun () ->
        let x1 = { a = 5 }
        let x2 = { a = 5 }
        let x3 = { a = 10 }
        let hs = HashSet<_>()
        hs.Add(x1) |> ignore
        hs.Add(x2) |> ignore
        hs.Count |> equal 1
        hs.Remove(x3) |> equal false
        hs.Count |> equal 1
        hs.Remove(x1) |> equal true
        hs.Count |> equal 0

    testCase "HashSet equality works with generics" <| fun () -> // See #1712
        let apa = Apa<R>()
        apa.Add({ i = 5; s = "foo"})
        apa.Contains ({ i = 5; s = "foo"}) |> equal true
        apa.Contains ({ i = 5; s = "fo"}) |> equal false

  ]