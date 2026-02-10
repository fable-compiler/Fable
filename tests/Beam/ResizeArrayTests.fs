module Fable.Tests.ResizeArray

open System
open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

// --- Construction ---

[<Fact>]
let ``test ResizeArray empty constructor works`` () =
    let ra = ResizeArray<int>()
    ra.Count |> equal 0

[<Fact>]
let ``test ResizeArray with capacity constructor works`` () =
    let ra = ResizeArray<int>(10)
    ra.Count |> equal 0

[<Fact>]
let ``test ResizeArray from list works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    ra.Count |> equal 3

// --- Add ---

[<Fact>]
let ``test ResizeArray.Add works`` () =
    let ra = ResizeArray<int>()
    ra.Add(1)
    ra.Add(2)
    ra.Add(3)
    ra.Count |> equal 3

// --- Indexer ---

[<Fact>]
let ``test ResizeArray indexer get works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    ra.[0] |> equal 10
    ra.[1] |> equal 20
    ra.[2] |> equal 30

[<Fact>]
let ``test ResizeArray indexer set works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    ra.[1] <- 99
    ra.[1] |> equal 99

// --- Contains ---

[<Fact>]
let ``test ResizeArray.Contains works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    ra.Contains(2) |> equal true
    ra.Contains(5) |> equal false

// --- IndexOf ---

[<Fact>]
let ``test ResizeArray.IndexOf works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    ra.IndexOf(20) |> equal 1
    ra.IndexOf(99) |> equal -1

// --- Remove ---

[<Fact>]
let ``test ResizeArray.Remove works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    let removed = ra.Remove(2)
    removed |> equal true
    ra.Count |> equal 2
    ra.[0] |> equal 1
    ra.[1] |> equal 3

[<Fact>]
let ``test ResizeArray.Remove returns false for missing item`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    let removed = ra.Remove(99)
    removed |> equal false
    ra.Count |> equal 3

// --- RemoveAt ---

[<Fact>]
let ``test ResizeArray.RemoveAt works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    ra.RemoveAt(1)
    ra.Count |> equal 2
    ra.[0] |> equal 10
    ra.[1] |> equal 30

// --- Insert ---

[<Fact>]
let ``test ResizeArray.Insert works`` () =
    let ra = ResizeArray<int>([1; 3])
    ra.Insert(1, 2)
    ra.Count |> equal 3
    ra.[0] |> equal 1
    ra.[1] |> equal 2
    ra.[2] |> equal 3

// --- Clear ---

[<Fact>]
let ``test ResizeArray.Clear works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    ra.Clear()
    ra.Count |> equal 0

// --- Reverse ---

[<Fact>]
let ``test ResizeArray.Reverse works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    ra.Reverse()
    ra.[0] |> equal 3
    ra.[1] |> equal 2
    ra.[2] |> equal 1

// --- Sort ---

[<Fact>]
let ``test ResizeArray.Sort works`` () =
    let ra = ResizeArray<int>([3; 1; 2])
    ra.Sort()
    ra.[0] |> equal 1
    ra.[1] |> equal 2
    ra.[2] |> equal 3

// --- ToArray ---

[<Fact>]
let ``test ResizeArray.ToArray works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    let arr = ra.ToArray()
    arr.Length |> equal 3
    arr.[0] |> equal 1

// --- Iteration ---

[<Fact>]
let ``test ResizeArray iteration with for-in works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    let mutable sum = 0
    for x in ra do
        sum <- sum + x
    sum |> equal 60

// --- Exists ---

[<Fact>]
let ``test ResizeArray.Exists works`` () =
    let ra = ResizeArray<int>([1; 2; 3])
    ra.Exists(fun x -> x = 2) |> equal true
    ra.Exists(fun x -> x = 5) |> equal false

// --- FindIndex ---

[<Fact>]
let ``test ResizeArray.FindIndex works`` () =
    let ra = ResizeArray<int>([10; 20; 30])
    ra.FindIndex(fun x -> x = 20) |> equal 1
    ra.FindIndex(fun x -> x = 99) |> equal -1

// --- AddRange ---

[<Fact>]
let ``test ResizeArray.AddRange works`` () =
    let ra = ResizeArray<int>([1])
    ra.AddRange([2; 3; 4])
    ra.Count |> equal 4
    ra.[3] |> equal 4
