module Fable.Tests.HashSets

open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

type MyRecord = { a: int }

// --- Construction ---

[<Fact>]
let ``test HashSet ctor creates empty HashSet`` () =
    let xs = HashSet<int>()
    xs.Count |> equal 0

[<Fact>]
let ``test HashSet ctor from list works`` () =
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
let ``test HashSet creation works`` () =
    let hs = HashSet<int>()
    equal 0 hs.Count

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

// --- Iteration ---

[<Fact>]
let ``test HashSet iteration with for-in works`` () =
    let hs = HashSet<int>()
    hs.Add(1) |> ignore
    hs.Add(2) |> ignore
    hs.Add(3) |> ignore
    let mutable sum = 0
    for v in hs do
        sum <- sum + v
    sum |> equal 6

// TODO: Seq.fold on HashSet requires IEnumerable conversion
// [<Fact>]
// let ``test HashSet folding works`` () =
//     let hs = HashSet<int>()
//     for i in 1 .. 10 do hs.Add(i) |> ignore
//     hs |> Seq.fold (fun acc item -> acc + item) 0
//     |> equal 55

[<Fact>]
let ``test HashSet sum via iteration works`` () =
    let hs = HashSet<int>()
    for i in 1 .. 10 do hs.Add(i) |> ignore
    let mutable sum = 0
    for item in hs do
        sum <- sum + item
    sum |> equal 55
