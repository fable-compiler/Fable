module Fable.Tests.HashSetTests

open Util.Testing
open System.Collections.Generic

// type MyRefType(i: int) =
//     member x.Value = i

let inline private hashSet xs =
    let res = HashSet<_>()
    xs |> List.iter (fun x -> res.Add x |> ignore)
    res

type MyRecord = { a: int }

type R = { i: int; s: string }

type Apa<'t when 't : equality>() =
    let state = HashSet<'t>()
    member _.Add t = state.Add t |> ignore
    member _.Contains t = state.Contains t

[<Fact>]
let ``HashSet ctor works`` () =
    let xs = HashSet<int>()
    xs.Count |> equal 0

[<Fact>]
let ``HashSet ctor with capacity works`` () =
    let xs = HashSet<int>(10)
    xs.Count |> equal 0

[<Fact>]
let ``HashSet ctor from IEnumerable works`` () =
    let s = List.toSeq [1;2;2;3]
    let xs = HashSet<int>(s)
    xs.Count |> equal 3

// [<Fact>]
// let ``HashSets with IEqualityComparer work`` () =
//     let x = MyRefType(4)
//     let y = MyRefType(4)
//     let z = MyRefType(6)
//     let set1 = HashSet<_>()
//     set1.Add(x) |> equal true
//     set1.Contains(x) |> equal true
//     set1.Contains(y) |> equal false
//     let comparer =
//         { new IEqualityComparer<MyRefType> with
//             member _.Equals(x, y) = x.Value = y.Value
//             member _.GetHashCode(x) = x.Value }
//     let set2 = HashSet<_>(comparer)
//     set2.Add(x) |> equal true
//     set2.Contains(x) |> equal true
//     set2.Contains(y) |> equal true
//     set2.Contains(z) |> equal false

[<Fact>]
let ``HashSet.Add returns true if not present`` () =
    let xs = HashSet<int>()
    xs.Add 1 |> equal true
    xs.Count |> equal 1

[<Fact>]
let ``HashSet.Add returns false if already present`` () =
    let xs = HashSet<int>()
    xs.Add 1 |> equal true
    xs.Add 1 |> equal false
    xs.Count |> equal 1

[<Fact>]
let ``HashSet.Remove works when item is present`` () =
    let xs = HashSet<int>()
    xs.Add 1 |> equal true
    xs.Remove 1 |> equal true
    xs.Count |> equal 0

[<Fact>]
let ``HashSet.Remove works when item is not present`` () =
    let xs = HashSet<int>()
    xs.Add 1 |> equal true
    xs.Add 2 |> equal true
    xs.Remove 3 |> equal false
    xs.Count |> equal 2

[<Fact>]
let ``HashSet.UnionWith works`` () =
    let xs = hashSet [1; 2]
    let ys = hashSet [2; 4]
    xs.UnionWith ys
    (xs.Contains 1 && xs.Contains 2 && xs.Contains 4)
    |> equal true

[<Fact>]
let ``HashSet.IntersectWith works`` () =
    let xs = hashSet [1; 2]
    let ys = hashSet [2; 4]
    xs.IntersectWith ys
    xs.Contains 1 |> equal false
    xs.Contains 2 |> equal true

[<Fact>]
let ``HashSet.IntersectWith works with custom comparison`` () = // See #2566
    let ignoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(s1: string, s2: string) =
                s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
            member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
    let xs = new HashSet<string>(["Foo"; "bar"], ignoreCase)
    xs.Contains("foo") |> equal true
    xs.Contains("Foo") |> equal true
    xs.Contains("bar") |> equal true
    xs.Contains("Bar") |> equal true
    xs.IntersectWith(["foo"; "bar"])
    xs.Count |> equal 2
    xs.IntersectWith(["Foo"; "Bar"])
    xs.Count |> equal 2

[<Fact>]
let ``HashSet.ExceptWith works`` () =
    let xs = hashSet [1; 2]
    let ys = hashSet [2; 4]
    xs.ExceptWith ys
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false

[<Fact>]
let ``HashSet.ExceptWith works with custom comparison`` () =
    let ignoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(s1: string, s2: string) =
                s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
            member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
    let xs = HashSet<string>(["Foo"; "bar"], ignoreCase)
    xs.ExceptWith(["foo"; "baz"])
    xs.Count |> equal 1
    xs.Contains("foo") |> equal false
    xs.Contains("bar") |> equal true
    xs.Contains("baz") |> equal false

[<Fact>]
let ``HashSet.SymmetricExceptWith works`` () =
    let xs = hashSet [1; 2]
    let ys = hashSet [2; 4]
    xs.SymmetricExceptWith ys
    xs.Count |> equal 2
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false
    xs.Contains 4 |> equal true

[<Fact>]
let ``HashSet.SymmetricExceptWith works with custom comparison`` () =
    let ignoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(s1: string, s2: string) =
                s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
            member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
    let xs = HashSet<string>(["Foo"; "bar"], ignoreCase)
    xs.SymmetricExceptWith(["foo"; "baz"])
    xs.Count |> equal 2
    xs.Contains("foo") |> equal false
    xs.Contains("bar") |> equal true
    xs.Contains("baz") |> equal true

[<Fact>]
let ``HashSet.Overlaps works`` () =
    let xs = hashSet [1; 2]
    xs.Overlaps [2; 4] |> equal true
    xs.Overlaps [3; 4] |> equal false

[<Fact>]
let ``HashSet.Overlaps works with custom comparison`` () =
    let ignoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(s1: string, s2: string) =
                s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
            member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
    let xs = HashSet<string>(["Foo"; "bar"], ignoreCase)
    xs.Overlaps ["foo"; "baz"] |> equal true
    xs.Overlaps ["baz"; "qux"] |> equal false

[<Fact>]
let ``HashSet.SetEquals works`` () =
    let xs = hashSet [1; 2]
    xs.SetEquals [2; 1; 1] |> equal true
    xs.SetEquals [2; 3] |> equal false

[<Fact>]
let ``HashSet.SetEquals works with custom comparison`` () =
    let ignoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(s1: string, s2: string) =
                s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
            member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
    let xs = HashSet<string>(["Foo"; "bar"], ignoreCase)
    xs.SetEquals ["foo"; "BAR"; "foo"] |> equal true
    xs.SetEquals ["foo"; "baz"] |> equal false

[<Fact>]
let ``HashSet iteration works`` () =
    let xs = HashSet<_>()
    for i in 1 .. 10 do
        xs.Add(i*i) |> ignore
    let mutable i = 0
    for v in xs do
        i <- v + i
    equal 385 i

[<Fact>]
let ``HashSet folding works`` () =
    let xs = HashSet<_>()
    for i in 1 .. 10 do xs.Add(i*i) |> ignore
    xs |> Seq.fold (fun acc item -> acc + item) 0
    |> equal 385

[<Fact>]
let ``HashSet.Count works`` () =
    let xs = HashSet<_>()
    for i in 1 .. 10 do
        xs.Add(i*i) |> ignore
    xs.Count |> equal 10

[<Fact>]
let ``HashSet.Count works II`` () =
    let xs = hashSet ([]: int list)
    xs.Count |> equal 0
    let ys = hashSet [1]
    ys.Count |> equal 1
    let zs = hashSet [1; 1]
    zs.Count |> equal 1
    let zs' = hashSet [1; 2]
    zs'.Count |> equal 2

[<Fact>]
let ``HashSet.Add works`` () =
    let xs = HashSet<_>()
    xs.Add 1 |> equal true
    xs.Add 3 |> equal true
    xs.Count |> equal 2

[<Fact>]
let ``HashSet.Add works ||`` () =
    let xs = HashSet<_>()
    xs.Add("A") |> equal true
    xs.Add("B") |> equal true
    xs.Count |> equal 2

[<Fact>]
let ``HashSet.Add with records works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let xs = HashSet<_>()
    xs.Add x1 |> equal true
    xs.Add x2 |> equal false
    xs.Add x3 |> equal true
    xs.Count |> equal 2

[<Fact>]
let ``HashSet.Clear works`` () =
    let xs = HashSet<_>()
    xs.Add 1 |> equal true
    xs.Add 4 |> equal true
    xs.Clear()
    xs.Count |> equal 0

[<Fact>]
let ``HashSet.Contains works`` () =
    let xs = HashSet<_>()
    xs.Add 1 |> equal true
    xs.Add 2 |> equal true
    xs.Contains 1 |> equal true
    xs.Contains 3 |> equal false

[<Fact>]
let ``HashSet.Contains works II`` () =
    let xs = HashSet<_>()
    xs.Add("Hello") |> equal true
    xs.Add("World!") |> equal true
    xs.Contains("Hello") |> equal true
    xs.Contains("Everybody!") |> equal false

[<Fact>]
let ``HashSet.CopyTo works`` () =
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

[<Fact>]
let ``HashSet.Remove works`` () =
    let xs = HashSet<_>()
    xs.Add 1 |> equal true
    xs.Add 2 |> equal true
    xs.Remove 1 |> equal true
    xs.Remove 3 |> equal false
    xs.Count |> equal 1

[<Fact>]
let ``HashSet.Remove with records works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let xs = HashSet<_>()
    xs.Add x1 |> equal true
    xs.Add x2 |> equal false
    xs.Remove x3 |> equal false
    xs.Remove x1 |> equal true
    xs.Count |> equal 0

[<Fact>]
let ``HashSet equality works with generics`` () = // See #1712
    let apa = Apa<R>()
    apa.Add({ i = 5; s = "foo"})
    apa.Contains ({ i = 5; s = "foo"}) |> equal true
    apa.Contains ({ i = 5; s = "fo"}) |> equal false
