module Fable.Tests.HashSets

open System.Collections.Generic
open Util.Testing

type MyRefType(i: int) =
    member x.Value = i

type MyRefTypeComparer() =
    interface IEqualityComparer<MyRefType> with
        member _.Equals(x, y) = x.Value = y.Value
        member _.GetHashCode(x) = x.Value

type IgnoreCaseComparer() =
    interface IEqualityComparer<string> with
        member _.Equals(s1, s2) =
            System.String.Equals(s1, s2, System.StringComparison.InvariantCultureIgnoreCase)

        member _.GetHashCode(s) =
            if System.String.IsNullOrEmpty(s) then 0 else s.ToLowerInvariant().GetHashCode()

let inline hashSet l =
    let xs = HashSet<_>()
    for x in l do
        xs.Add x |> ignore
    xs

[<ReferenceEquality>]
type RefEqRecord = { a: int; b: int }

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

    testCase "HashSet ctor with capacity works" <| fun () ->
        let xs = HashSet<int>(10)
        xs.Count |> equal 0

    testCase "HashSet with IEqualityComparer works" <| fun () ->
        let x = MyRefType(4)
        let y = MyRefType(4)
        let z = MyRefType(6)
        let set1 = HashSet<_>()
        set1.Add(x) |> equal true
        set1.Contains(x) |> equal true
        set1.Contains(y) |> equal false

        let set2 = HashSet<_>(MyRefTypeComparer())
        set2.Add(x) |> equal true
        set2.Contains(x) |> equal true
        set2.Contains(y) |> equal true
        set2.Contains(z) |> equal false

    testCase "HashSet.Add returns true if not present" <| fun () ->
        let xs = hashSet []
        xs.Add(1) |> equal true
        xs.Count |> equal 1

    testCase "HashSet.Add returns false if already present" <| fun () ->
        let xs = hashSet [1]
        xs.Add(1) |> equal false
        xs.Count |> equal 1

    testCase "HashSet.Remove works when item is present" <| fun () ->
        let xs = hashSet [1]
        xs.Remove 1 |> equal true
        xs.Count |> equal 0

    testCase "HashSet.Remove works when item is not present" <| fun () ->
        let xs = hashSet [1; 2]
        xs.Remove 3 |> equal false
        xs.Count |> equal 2

    testCase "HashSet.UnionWith works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [2; 4]
        xs.UnionWith ys
        (xs.Contains 1 && xs.Contains 2 && xs.Contains 4)
        |> equal true

    testCase "HashSet.IntersectWith works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [2; 4]
        xs.IntersectWith ys
        xs.Contains 1 |> equal false
        xs.Contains 2 |> equal true

    testCase "HashSet.IntersectWith works with custom comparison" <| fun () -> // See #2566
        let xs = new HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
        xs.Contains("foo") |> equal true
        xs.Contains("Foo") |> equal true
        xs.Contains("bar") |> equal true
        xs.Contains("Bar") |> equal true
        xs.IntersectWith(["foo"; "bar"])
        xs.Count |> equal 2
        xs.IntersectWith(["Foo"; "Bar"])
        xs.Count |> equal 2

    testCase "HashSet.ExceptWith works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [2; 4]
        xs.ExceptWith ys
        xs.Contains 1 |> equal true
        xs.Contains 2 |> equal false

    testCase "HashSet.ExceptWith works with custom comparison" <| fun () ->
        let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
        xs.ExceptWith(["foo"; "baz"])
        xs.Count |> equal 1
        xs.Contains("foo") |> equal false
        xs.Contains("bar") |> equal true
        xs.Contains("baz") |> equal false

    testCase "HashSet.SymmetricExceptWith works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [2; 4]
        xs.SymmetricExceptWith ys
        xs.Count |> equal 2
        xs.Contains 1 |> equal true
        xs.Contains 2 |> equal false
        xs.Contains 4 |> equal true

    testCase "HashSet.SymmetricExceptWith works with custom comparison" <| fun () ->
        let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
        xs.SymmetricExceptWith(["foo"; "baz"])
        xs.Count |> equal 2
        xs.Contains("foo") |> equal false
        xs.Contains("bar") |> equal true
        xs.Contains("baz") |> equal true

    testCase "HashSet.Overlaps works" <| fun () ->
        let xs = hashSet [1; 2]
        xs.Overlaps [2; 4] |> equal true
        xs.Overlaps [3; 4] |> equal false

    testCase "HashSet.Overlaps works with custom comparison" <| fun () ->
        let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
        xs.Overlaps ["foo"; "baz"] |> equal true
        xs.Overlaps ["baz"; "qux"] |> equal false

    testCase "HashSet.SetEquals works" <| fun () ->
        let xs = hashSet [1; 2]
        xs.SetEquals [2; 1; 1] |> equal true
        xs.SetEquals [2; 3] |> equal false

    testCase "HashSet.SetEquals works with custom comparison" <| fun () ->
        let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
        xs.SetEquals ["foo"; "BAR"; "foo"] |> equal true
        xs.SetEquals ["foo"; "baz"] |> equal false

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

    testCase "HashSet.Count works II" <| fun () ->
        let xs = hashSet []
        xs.Count |> equal 0
        let ys = hashSet [1]
        ys.Count |> equal 1
        let zs = hashSet [1; 1]
        zs.Count |> equal 1
        let zs' = hashSet [1; 2]
        zs'.Count |> equal 2

    testCase "HashSet.Add works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add(1) |> equal true
        hs.Add(3) |> equal true
        hs.Count |> equal 2

    testCase "HashSet.Add works II" <| fun () ->
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

    // testCase "HashSet.Add with reference records works" <| fun () ->
    //     let x1 = { a = 5; b = 10 }
    //     let x2 = { a = 5; b = 10 }
    //     let x3 = { a = 10; b = 20 }
    //     let hs = HashSet<RefEqRecord>()
    //     hs.Add(x1) |> equal true
    //     hs.Add(x2) |> equal true
    //     hs.Add(x3) |> equal true
    //     hs.Count |> equal 3

    testCase "HashSet.Clear works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add(1) |> equal true
        hs.Add(2) |> equal true
        hs.Clear()
        hs.Count |> equal 0

    testCase "HashSet.Contains works" <| fun () ->
        let hs = HashSet<_>()
        hs.Add(1) |> equal true
        hs.Add(2) |> equal true
        hs.Contains(1) |> equal true
        hs.Contains(3) |> equal false

    testCase "HashSet.Contains works II" <| fun () ->
        let hs = HashSet<_>()
        hs.Add("Hello") |> equal true
        hs.Add("World!") |> equal true
        hs.Contains("Hello") |> equal true
        hs.Contains("Everybody!") |> equal false

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

    testCase "HashSet.IsSubsetOf works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [1; 2; 3]
        xs.IsSubsetOf(ys) |> equal true
        ys.IsSubsetOf(xs) |> equal false
        xs.IsSubsetOf(xs) |> equal true

    testCase "HashSet.IsSupersetOf works" <| fun () ->
        let xs = hashSet [1; 2; 3]
        let ys = hashSet [1; 2]
        xs.IsSupersetOf(ys) |> equal true
        ys.IsSupersetOf(xs) |> equal false
        xs.IsSupersetOf(xs) |> equal true

    testCase "HashSet.IsProperSubsetOf works" <| fun () ->
        let xs = hashSet [1; 2]
        let ys = hashSet [1; 2; 3]
        xs.IsProperSubsetOf(ys) |> equal true
        ys.IsProperSubsetOf(xs) |> equal false
        xs.IsProperSubsetOf(xs) |> equal false

    testCase "HashSet.IsProperSupersetOf works" <| fun () ->
        let xs = hashSet [1; 2; 3]
        let ys = hashSet [1; 2]
        xs.IsProperSupersetOf(ys) |> equal true
        ys.IsProperSupersetOf(xs) |> equal false
        xs.IsProperSupersetOf(xs) |> equal false

    testCase "HashSet.CopyTo works" <| fun () ->
        let xs = HashSet<_>()
        for i in 1 .. 9 do xs.Add(i) |> ignore

        let arr1 = Array.zeroCreate 9
        let arr2 = Array.zeroCreate 11
        let arr3 = Array.zeroCreate 7

        xs.CopyTo(arr1)         // [|x;x;x;x;x;x;x;x;x|]
        xs.CopyTo(arr2, 2)      // [|0;0;x;x;x;x;x;x;x;x;x|]
        xs.CopyTo(arr3, 3, 4)   // [|0;0;0;x;x;x;x|]

        let sum = fun acc item -> acc + item
        arr1 |> Seq.fold sum 0 |> equal 45
        arr1.Length |> equal 9

        arr2 |> Seq.fold sum 0 |> equal 45
        arr2.Length |> equal 11

        // The exact order of the copied items is not guaranteed,
        // but they must all exist in the hash set and be distinct.
        // arr3 |> Seq.fold sum 0 |> equal 10 // not guaranteed to work
        arr3[0..2] |> Array.forall ((=) 0) |> equal true
        arr3[3..] |> Array.forall xs.Contains |> equal true
        arr3[3..] |> Array.distinct |> Array.length |> equal 4
        arr3.Length |> equal 7

    testCase "HashSet IReadOnlyCollection.Count works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> IReadOnlyCollection<_>
        coll.Count |> equal 3

    testCase "HashSet ICollection.IsReadOnly works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.IsReadOnly |> equal false

    testCase "HashSet ICollection.Count works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.Count |> equal 3

    testCase "HashSet ICollection.Contains works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.Contains(("B", 3)) |> equal false
        coll.Contains(("D", 3)) |> equal false
        coll.Contains(("B", 2)) |> equal true

    testCase "HashSet ICollection.CopyTo works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        let ys = [| ("D", 4); ("E", 5); ("F", 6) |]
        coll.CopyTo(ys, 0)
        ys = xs |> equal true

    testCase "HashSet ICollection.Clear works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.Clear()
        coll.Count |> equal 0

    testCase "HashSet ICollection.Add works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.Add(("A", 1))
        coll.Add(("A", 2))
        coll.Add(("D", 4))
        coll.Count |> equal 5

    testCase "HashSet ICollection.Remove works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (HashSet xs) :> ICollection<_>
        coll.Remove(("B", 3)) |> equal false
        coll.Remove(("D", 3)) |> equal false
        coll.Remove(("B", 2)) |> equal true
        coll.Count |> equal 2
  ]