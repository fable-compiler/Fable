module Fable.Tests.HashSets

open System.Collections.Generic
open Fable.Tests.Util
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

[<ReferenceEquality>]
type RefEqRecord = { a: int; b: int }

type MyRecord = { a: int }

type R = { i: int; s: string }

type Apa<'t when 't : equality>() =
    let state = HashSet<'t>()
    member _.Add t = state.Add t |> ignore
    member _.Contains t = state.Contains t

// --- Construction ---

[<Fact>]
let ``test HashSet ctor creates empty HashSet`` () =
    let xs = HashSet<int>()
    xs.Count |> equal 0

// [<Fact>]
// let ``test HashSet with IEqualityComparer works`` () =
//     // Beam ignores IEqualityComparer — custom comparer behaviour is not supported
//     let x = MyRefType(4)
//     let y = MyRefType(4)
//     let z = MyRefType(6)
//     let set1 = HashSet<_>()
//     set1.Add(x) |> equal true
//     set1.Contains(x) |> equal true
//     set1.Contains(y) |> equal false
//     let set2 = HashSet<_>(MyRefTypeComparer())
//     set2.Add(x) |> equal true
//     set2.Contains(x) |> equal true
//     set2.Contains(y) |> equal true
//     set2.Contains(z) |> equal false

[<Fact>]
let ``test HashSet ctor from Enumerable works`` () =
    let xs = HashSet<int>([1; 2; 2; 3])
    xs.Count |> equal 3
    xs.Contains(1) |> equal true
    xs.Contains(2) |> equal true
    xs.Contains(3) |> equal true

// --- Add ---

[<Fact>]
let ``test HashSet.Add returns true if not present`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> equal true
    xs.Count |> equal 1

[<Fact>]
let ``test HashSet.Add returns false if already present`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Add(1) |> equal false
    xs.Count |> equal 1

[<Fact>]
let ``test HashSet.Add works`` () =
    let hs = HashSet<int>()
    hs.Add(1) |> equal true
    hs.Add(3) |> equal true
    hs.Count |> equal 2

[<Fact>]
let ``test HashSet.Add works II`` () =
    let hs = HashSet<string>()
    hs.Add("A") |> equal true
    hs.Add("B") |> equal true
    hs.Count |> equal 2

[<Fact>]
let ``test HashSet.Add with records works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let hs = HashSet<MyRecord>()
    hs.Add(x1) |> equal true
    hs.Add(x2) |> equal false
    hs.Add(x3) |> equal true
    hs.Count |> equal 2

// [<Fact>]
// let ``test HashSet.Add with reference records works`` () =
//     let x1 = { a = 5; b = 10 }
//     let x2 = { a = 5; b = 10 }
//     let x3 = { a = 10; b = 20 }
//     let hs = HashSet<RefEqRecord>()
//     hs.Add(x1) |> equal true
//     hs.Add(x2) |> equal true
//     hs.Add(x3) |> equal true
//     hs.Count |> equal 3

// --- Remove ---

[<Fact>]
let ``test HashSet.Remove works when item is present`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Remove(1) |> equal true
    xs.Count |> equal 0

[<Fact>]
let ``test HashSet.Remove works when item is not present`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Add(2) |> ignore
    xs.Remove(3) |> equal false
    xs.Count |> equal 2

[<Fact>]
let ``test HashSet.Remove works`` () =
    let hs = HashSet<string>()
    hs.Add("A") |> ignore
    hs.Add("B") |> ignore
    hs.Remove("A") |> equal true
    hs.Remove("C") |> equal false

[<Fact>]
let ``test HashSet.Remove with records works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let hs = HashSet<MyRecord>()
    hs.Add(x1) |> ignore
    hs.Add(x2) |> ignore
    hs.Count |> equal 1
    hs.Remove(x3) |> equal false
    hs.Count |> equal 1
    hs.Remove(x1) |> equal true
    hs.Count |> equal 0

// --- Contains ---

[<Fact>]
let ``test HashSet.Contains works`` () =
    let hs = HashSet<int>()
    hs.Add(1) |> ignore
    hs.Add(2) |> ignore
    hs.Contains(1) |> equal true
    hs.Contains(3) |> equal false

[<Fact>]
let ``test HashSet.Contains works II`` () =
    let hs = HashSet<string>()
    hs.Add("Hello") |> ignore
    hs.Add("World!") |> ignore
    hs.Contains("Hello") |> equal true
    hs.Contains("Everybody!") |> equal false

// --- Count ---

[<Fact>]
let ``test HashSet.Count works`` () =
    let hs = HashSet<int>()
    for i in 1 .. 10 do hs.Add(i * i) |> ignore
    hs.Count |> equal 10

[<Fact>]
let ``test HashSet.Count works II`` () =
    let xs = HashSet<int>()
    xs.Count |> equal 0
    let ys = HashSet<int>([1])
    ys.Count |> equal 1
    let zs = HashSet<int>([1; 1])
    zs.Count |> equal 1
    let zs' = HashSet<int>([1; 2])
    zs'.Count |> equal 2

// --- Clear ---

[<Fact>]
let ``test HashSet.Clear works`` () =
    let hs = HashSet<int>()
    hs.Add(1) |> ignore
    hs.Add(2) |> ignore
    hs.Clear()
    hs.Count |> equal 0

// --- Set Operations ---

[<Fact>]
let ``test HashSet.UnionWith works`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Add(2) |> ignore
    let ys = HashSet<int>()
    ys.Add(2) |> ignore
    ys.Add(4) |> ignore
    xs.UnionWith(ys)
    (xs.Contains(1) && xs.Contains(2) && xs.Contains(4))
    |> equal true

[<Fact>]
let ``test HashSet.IntersectWith works`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Add(2) |> ignore
    let ys = HashSet<int>()
    ys.Add(2) |> ignore
    ys.Add(4) |> ignore
    xs.IntersectWith(ys)
    xs.Contains(1) |> equal false
    xs.Contains(2) |> equal true

// [<Fact>]
// let ``test HashSet.IntersectWith works with custom comparison`` () = // See #2566
//     // Beam ignores IEqualityComparer — this would not behave as expected
//     let xs = new HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
//     xs.Contains("foo") |> equal true
//     xs.IntersectWith(["foo"; "bar"])
//     xs.Count |> equal 2

// [<Fact>]
// let ``test HashSet.ExceptWith works with custom comparison`` () =
//     // Beam ignores IEqualityComparer — this would not behave as expected
//     let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
//     xs.ExceptWith(["foo"; "baz"])
//     xs.Count |> equal 1
//     xs.Contains("foo") |> equal false
//     xs.Contains("bar") |> equal true

// [<Fact>]
// let ``test HashSet.SymmetricExceptWith works with custom comparison`` () =
//     // Beam ignores IEqualityComparer — this would not behave as expected
//     let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
//     xs.SymmetricExceptWith(["foo"; "baz"])
//     xs.Count |> equal 2
//     xs.Contains("foo") |> equal false
//     xs.Contains("bar") |> equal true
//     xs.Contains("baz") |> equal true

// [<Fact>]
// let ``test HashSet.Overlaps works with custom comparison`` () =
//     // Beam ignores IEqualityComparer — this would not behave as expected
//     let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
//     xs.Overlaps ["foo"; "baz"] |> equal true
//     xs.Overlaps ["baz"; "qux"] |> equal false

// [<Fact>]
// let ``test HashSet.SetEquals works with custom comparison`` () =
//     // Beam ignores IEqualityComparer — this would not behave as expected
//     let xs = HashSet<string>(["Foo"; "bar"], IgnoreCaseComparer())
//     xs.SetEquals ["foo"; "BAR"; "foo"] |> equal true
//     xs.SetEquals ["foo"; "baz"] |> equal false

[<Fact>]
let ``test HashSet.ExceptWith works`` () =
    let xs = HashSet<int>()
    xs.Add(1) |> ignore
    xs.Add(2) |> ignore
    let ys = HashSet<int>()
    ys.Add(2) |> ignore
    ys.Add(4) |> ignore
    xs.ExceptWith(ys)
    xs.Contains(1) |> equal true
    xs.Contains(2) |> equal false

[<Fact>]
let ``test HashSet.SymmetricExceptWith works`` () =
    let xs = HashSet<int>([1; 2])
    let ys = HashSet<int>([2; 4])
    xs.SymmetricExceptWith(ys)
    xs.Count |> equal 2
    xs.Contains(1) |> equal true
    xs.Contains(2) |> equal false
    xs.Contains(4) |> equal true

[<Fact>]
let ``test HashSet.Overlaps works`` () =
    let xs = HashSet<int>([1; 2])
    xs.Overlaps([2; 4]) |> equal true
    xs.Overlaps([3; 4]) |> equal false

[<Fact>]
let ``test HashSet.SetEquals works`` () =
    let xs = HashSet<int>([1; 2])
    xs.SetEquals([2; 1; 1]) |> equal true
    xs.SetEquals([2; 3]) |> equal false

// --- Iteration ---

[<Fact>]
let ``test HashSet iteration works`` () =
    let xs = HashSet<_>()
    for i in 1 .. 10 do
        xs.Add(i*i) |> ignore
    let mutable i = 0
    for v in xs do
        i <- v + i
    equal 385 i

[<Fact>]
let ``test HashSet folding works`` () =
    let xs = HashSet<_>()
    for i in 1 .. 10 do xs.Add(i*i) |> ignore
    xs |> Seq.fold (fun acc item -> acc + item) 0
    |> equal 385

// --- Subset / Superset ---

[<Fact>]
let ``test HashSet.IsSubsetOf works`` () =
    let xs = HashSet<int>([1; 2])
    let ys = HashSet<int>([1; 2; 3])
    xs.IsSubsetOf(ys) |> equal true
    ys.IsSubsetOf(xs) |> equal false
    xs.IsSubsetOf(xs) |> equal true

[<Fact>]
let ``test HashSet.IsSupersetOf works`` () =
    let xs = HashSet<int>([1; 2; 3])
    let ys = HashSet<int>([1; 2])
    xs.IsSupersetOf(ys) |> equal true
    ys.IsSupersetOf(xs) |> equal false
    xs.IsSupersetOf(xs) |> equal true

[<Fact>]
let ``test HashSet.IsProperSubsetOf works`` () =
    let xs = HashSet<int>([1; 2])
    let ys = HashSet<int>([1; 2; 3])
    xs.IsProperSubsetOf(ys) |> equal true
    ys.IsProperSubsetOf(xs) |> equal false
    xs.IsProperSubsetOf(xs) |> equal false

[<Fact>]
let ``test HashSet.IsProperSupersetOf works`` () =
    let xs = HashSet<int>([1; 2; 3])
    let ys = HashSet<int>([1; 2])
    xs.IsProperSupersetOf(ys) |> equal true
    ys.IsProperSupersetOf(xs) |> equal false
    xs.IsProperSupersetOf(xs) |> equal false

[<Fact>]
let ``test HashSet ctor with capacity works`` () =
    let xs = HashSet<int>(10)
    xs.Count |> equal 0

[<Fact>]
let ``test HashSet equality works with generics`` () = // See #1712
    let apa = Apa<R>()
    apa.Add({ i = 5; s = "foo"})
    apa.Contains ({ i = 5; s = "foo"}) |> equal true
    apa.Contains ({ i = 5; s = "fo"}) |> equal false

[<Fact>]
let ``test HashSet.CopyTo works`` () =
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

    arr3[0..2] |> Array.forall ((=) 0) |> equal true
    arr3[3..] |> Array.forall xs.Contains |> equal true
    arr3[3..] |> Array.distinct |> Array.length |> equal 4
    arr3.Length |> equal 7

// [<Fact>]
// let ``test HashSet IReadOnlyCollection.Count works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> IReadOnlyCollection<_>
//     coll.Count |> equal 3

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
// let ``test HashSet ICollection.CopyTo works`` () =
//     let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
//     let coll = (HashSet xs) :> ICollection<_>
//     let ys = [| ("D", 4); ("E", 5); ("F", 6) |]
//     coll.CopyTo(ys, 0)
//     ys = xs |> equal true

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
