module Fable.Tests.Stacks

open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

// --- Construction ---

[<Fact>]
let ``test Empty stack constructor works`` () =
    let s = Stack<int>()
    s.Count |> equal 0

[<Fact>]
let ``test Stack can be created from an IEnumerable`` () =
    let s = Stack<int>([1; 2; 3])
    s.Count |> equal 3

// --- Push / Pop ---

[<Fact>]
let ``test Stack works for simple cases`` () =
    let s = Stack<int>()
    s.Push(1)
    s.Push(2)
    s.Push(3)
    s.Count |> equal 3
    let v1 = s.Pop()
    let v2 = s.Pop()
    let v3 = s.Pop()
    v1 |> equal 3
    v2 |> equal 2
    v3 |> equal 1

// --- Peek ---

[<Fact>]
let ``test Stack Peek works`` () =
    let s = Stack<int>()
    s.Push(1)
    s.Push(2)
    let v = s.Peek()
    v |> equal 2
    s.Count |> equal 2

// --- Contains ---

[<Fact>]
let ``test Stack Contains works as expected`` () =
    let s = Stack<string>()
    s.Push("A")
    s.Push("B")
    s.Contains("A") |> equal true
    s.Contains("C") |> equal false

// --- ToArray ---

[<Fact>]
let ``test Stack ToArray works as expected`` () =
    let s = Stack<int>()
    s.Push(1)
    s.Push(2)
    s.Push(3)
    let arr = s.ToArray()
    arr.Length |> equal 3
    // ToArray returns in LIFO order (top first)
    arr.[0] |> equal 3
    arr.[1] |> equal 2
    arr.[2] |> equal 1

// --- TryPeek / TryPop ---

[<Fact>]
let ``test Stack TryPeek works as expected`` () =
    let s = Stack<int>()
    let success1, _v1 = s.TryPeek()
    success1 |> equal false
    s.Push(1)
    let success2, v2 = s.TryPeek()
    success2 |> equal true
    v2 |> equal 1
    s.Count |> equal 1

[<Fact>]
let ``test Stack TryPop works as expected`` () =
    let s = Stack<int>()
    let success1, _v1 = s.TryPop()
    success1 |> equal false
    s.Push(1)
    s.Push(2)
    let success2, v2 = s.TryPop()
    success2 |> equal true
    v2 |> equal 2
    s.Count |> equal 1

// --- Clear ---

[<Fact>]
let ``test Stack Clear works`` () =
    let s = Stack<int>()
    s.Push(1)
    s.Push(2)
    s.Clear()
    s.Count |> equal 0
