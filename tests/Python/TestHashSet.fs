module Fable.Tests.HashSet

open System.Collections.Generic
open Util.Testing

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

[<Fact>]
let ``test HashSet ctor from Enumerable works`` () =
    let s = List.toSeq [1;2;2;3]
    let xs = HashSet<int>(s)
    xs |> Seq.toList
    |> equal [1;2;3]

[<Fact>]
let ``test HashSet ctor creates empty HashSet`` () =
    let xs = HashSet<int>()
    xs |> Seq.isEmpty
    |> equal true

[<Fact>]
let ``test HashSets with IEqualityComparer work`` () =
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

[<Fact>]
let ``test HashSet.Add returns true if not present`` () =
    let xs = set []
    xs.Add(1) |> equal true
    xs.Count |> equal 1

[<Fact>]
let ``test HashSet.Add returns false if already present`` () =
    let xs = set [1]
    xs.Add(1) |> equal false
    xs.Count |> equal 1

[<Fact>]
let ``test HashSet.Remove works when item is present`` () =
    let xs = set [1]
    xs.Remove 1 |> equal true
    xs.Count |> equal 0

[<Fact>]
let ``test HashSet.Remove works when item is not present`` () =
    let xs = set [1; 2]
    xs.Remove 3 |> equal false
    xs.Count |> equal 2

[<Fact>]
let ``test HashSet.UnionWith works`` () =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.UnionWith ys
    (xs.Contains 1 && xs.Contains 2 && xs.Contains 4)
    |> equal true

[<Fact>]
let ``test HashSet.IntersectWith works`` () =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.IntersectWith ys
    xs.Contains 1 |> equal false
    xs.Contains 2 |> equal true

// TODO: IntersectWith with custom comparison needs IEqualityComparer
// support in intersect_with_set helper
// [<Fact>]
// let ``test IntersectWith works with custom comparison`` () = // See #2566
//     let ignoreCase =
//         { new IEqualityComparer<string> with
//             member _.Equals(s1: string, s2: string) =
//                 s1.Equals(s2, System.StringComparison.InvariantCultureIgnoreCase)
//             member _.GetHashCode(s: string) = s.ToLowerInvariant().GetHashCode() }
//     let set = new HashSet<string>(["Foo"; "bar"], ignoreCase)
//     set.Contains("foo") |> equal true
//     set.Contains("Foo") |> equal true
//     set.Contains("bar") |> equal true
//     set.Contains("Bar") |> equal true
//     set.IntersectWith(["foo"; "bar"])
//     set.Count |> equal 2
//     set.IntersectWith(["Foo"; "Bar"])
//     set.Count |> equal 2

[<Fact>]
let ``test HashSet.ExceptWith works`` () =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.ExceptWith ys
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false

[<Fact>]
let ``test HashSet creation works`` () =
    let hs = HashSet<_>()
    equal 0 hs.Count

[<Fact>]
let ``test HashSet iteration works`` () =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore

    let i = ref 0.
    for v in hs do
       i := v + !i
    equal 385. !i

[<Fact>]
let ``test HashSet folding works`` () =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore
    hs |> Seq.fold (fun acc item -> acc + item) 0.
    |> equal 385.

[<Fact>]
let ``test HashSet.Count works`` () =
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

[<Fact>]
let ``test HashSet.Add works`` () =
    let hs = HashSet<_>()
    hs.Add("A") |> equal true
    hs.Add("B") |> equal true
    hs.Count |> equal 2

[<Fact>]
let ``test HashSet.Add with records works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let hs = HashSet<_>()
    hs.Add(x1) |> equal true
    hs.Add(x2) |> equal false
    hs.Add(x3) |> equal true
    hs.Count |> equal 2

[<Fact>]
let ``test HashSet.Clear works`` () =
    let hs = HashSet<_>()
    hs.Add(1) |> equal true
    hs.Add(2) |> equal true
    hs.Clear()
    hs.Count |> equal 0

[<Fact>]
let ``test HashSet.Contains works`` () =
    let hs = HashSet<_>()
    hs.Add("Hello") |> equal true
    hs.Add("World!") |> equal true
    hs.Contains("Hello") |> equal true
    hs.Contains("Everybody!") |> equal false

[<Fact>]
let ``test HashSet.Remove works`` () =
    let hs = HashSet<_>()
    hs.Add("A") |> ignore
    hs.Add("B") |> ignore
    hs.Remove("A") |> equal true
    hs.Remove("C") |> equal false

[<Fact>]
let ``test HashSet.Remove with records works`` () =
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

[<Fact>]
let ``test HashSet equality works with generics`` () = // See #1712
    let apa = Apa<R>()
    apa.Add({ i = 5; s = "foo"})
    apa.Contains ({ i = 5; s = "foo"}) |> equal true
    apa.Contains ({ i = 5; s = "fo"}) |> equal false

// TODO: ICollection interface tests are disabled for Python because ICollection
// interface dispatch has a separate issue with comparer arguments.
// See: HashSet ICollection.Contains, ICollection.Remove, etc.

// [<Fact>]
// let ``test HashSet ICollection.IsReadOnly works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.IsReadOnly |> equal false

// [<Fact>]
// let ``test HashSet ICollection.Count works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.Count |> equal 3

// [<Fact>]
// let ``test HashSet ICollection.Contains works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.Contains(("B", 3)) |> equal false
//     coll.Contains(("D", 3)) |> equal false
//     coll.Contains(("B", 2)) |> equal true

// [<Fact>]
// let ``test HashSet ICollection.Clear works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.Clear()
//     coll.Count |> equal 0

// [<Fact>]
// let ``test HashSet ICollection.Add works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.Add(("A", 1))
//     coll.Add(("A", 2))
//     coll.Add(("D", 4))
//     coll.Count |> equal 5

// [<Fact>]
// let ``test HashSet ICollection.Remove works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     coll.Remove(("B", 3)) |> equal false
//     coll.Remove(("D", 3)) |> equal false
//     coll.Remove(("B", 2)) |> equal true
//     coll.Count |> equal 2
