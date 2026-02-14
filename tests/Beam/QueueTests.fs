module Fable.Tests.Queues

open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

// --- Construction ---

[<Fact>]
let ``test Empty queue constructor works`` () =
    let q = Queue<int>()
    q.Count |> equal 0

[<Fact>]
let ``test Queue constructed with list works`` () =
    let q = Queue<int>([1; 2; 3])
    q.Count |> equal 3

// --- Enqueue / Dequeue ---

[<Fact>]
let ``test Enqueue to default queue works`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    q.Enqueue(3)
    q.Count |> equal 3

[<Fact>]
let ``test Enqueue Dequeue works`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    q.Enqueue(3)
    let v1 = q.Dequeue()
    let v2 = q.Dequeue()
    let v3 = q.Dequeue()
    v1 |> equal 1
    v2 |> equal 2
    v3 |> equal 3
    q.Count |> equal 0

[<Fact>]
let ``test Can enqueue to queue constructed from list`` () =
    let q = Queue<int>([1; 2; 3])
    q.Enqueue(4)
    q.Count |> equal 4

// --- Peek ---

[<Fact>]
let ``test Peek works and doesn't change the queue`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    let v = q.Peek()
    v |> equal 1
    q.Count |> equal 2

// --- TryDequeue / TryPeek ---

[<Fact>]
let ``test TryDequeue works with non-empty queue`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    let success, value = q.TryDequeue()
    success |> equal true
    value |> equal 1
    q.Count |> equal 1

[<Fact>]
let ``test TryDequeue works with empty queue`` () =
    let q = Queue<int>()
    let success, _value = q.TryDequeue()
    success |> equal false

[<Fact>]
let ``test TryPeek works with non-empty queue`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    let success, value = q.TryPeek()
    success |> equal true
    value |> equal 1
    q.Count |> equal 1

[<Fact>]
let ``test TryPeek works with empty queue`` () =
    let q = Queue<int>()
    let success, _value = q.TryPeek()
    success |> equal false

// --- Contains ---

[<Fact>]
let ``test Contains finds entries in queue`` () =
    let q = Queue<string>()
    q.Enqueue("A")
    q.Enqueue("B")
    q.Contains("A") |> equal true
    q.Contains("C") |> equal false

// --- Clear ---

[<Fact>]
let ``test Clear queue removes all entries`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    q.Clear()
    q.Count |> equal 0

// --- ToArray ---

[<Fact>]
let ``test Queue can be converted to array`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)
    q.Enqueue(3)
    let arr = q.ToArray()
    arr.Length |> equal 3
    arr.[0] |> equal 1
    arr.[2] |> equal 3

// --- Throws ---

[<Fact>]
let ``test Peek throws on empty queue`` () =
    let q = Queue<int>()
    let mutable threw = false
    try
        q.Peek() |> ignore
    with _ ->
        threw <- true
    threw |> equal true

[<Fact>]
let ``test Dequeue throws on empty queue`` () =
    let q = Queue<int>()
    let mutable threw = false
    try
        q.Dequeue() |> ignore
    with _ ->
        threw <- true
    threw |> equal true

// --- Enumeration ---

[<Fact>]
let ``test Queue can be enumerated with for-in`` () =
    let q = Queue<int>()
    q.Enqueue(10)
    q.Enqueue(20)
    q.Enqueue(30)
    let mutable sum = 0
    for item in q do
        sum <- sum + item
    sum |> equal 60

[<Fact>]
let ``test Queue enumeration preserves order`` () =
    let q = Queue<string>(["a"; "b"; "c"])
    let mutable result = ""
    for item in q do
        result <- result + item
    result |> equal "abc"
