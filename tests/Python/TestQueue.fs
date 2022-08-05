module Fable.Tests.Queue

open System
open System.Collections.Generic
open Util.Testing

[<Fact>]
let ``test Empty queue constructor works`` () =
    let q = Queue<int>()
    q.Count |> equal 0

[<Fact>]
let ``test Pre-sized queue constructor works`` () =
    let q = Queue<int>(16)
    q.Count |> equal 0

[<Fact>]
let ``test Enqueue to default queue works`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Count |> equal 1

[<Fact>]
let ``test Enqueue to zero-size queue works`` () =
    let q = Queue<int>(0)
    q.Enqueue(1)
    q.Count |> equal 1

[<Fact>]
let ``test Queue implements IEnumerable<'T>`` () =
    let q = Queue<int>()

    q |> List.ofSeq |> equal []

    [1..5] |> List.iter (q.Enqueue)

    q |> List.ofSeq |> equal [1;2;3;4;5]

[<Fact>]
let ``test Queue can be converted to array`` () =
    let q = Queue<int>()
    let q2 = Queue<int>(100)

    q.ToArray() |> equal [| |]
    q2.ToArray() |> equal [| |]

    [1..5] |> List.iter q.Enqueue

    q.ToArray() |> equal [|1;2;3;4;5|]

[<Fact>]
let ``test Queue constructed with list works`` () =
    let q = Queue<int>( [ 1;2;3 ] )
    q.Count |> equal 3 // Size matches input
    q |> Seq.toList |> equal [1;2;3] // Collections match
    q.Dequeue() |> equal 1 // Ordering matches Dequeue semantics

[<Fact>]
let ``test Can enqueue to queue constructed from list`` () =
    let q = Queue<int>( [ 1;2;3 ] )
    q.Enqueue(4)
    q.Count |> equal 4
    q |> Seq.toList |> equal [1;2;3;4]

[<Fact>]
let ``test Enqueue / Dequeue works`` () =
    let q = Queue<int>()
    q.Enqueue(1)
    q.Enqueue(2)

    q |> Seq.toList |> equal [1;2]

    q.Count |> equal 2
    q.Dequeue() |> equal 1
    q.Count |> equal 1

    q |> Seq.toList |> equal [2]
    q.Dequeue() |> equal 2
    q.Count |> equal 0
    q |> Seq.toList |> equal []

[<Fact>]
let ``test TrimExcess works`` () =
    // This is testing the internal implementation details (circular buffer). If they change then this
    // test is no longer (as) valid.
    let q = Queue<int>(0)
    [0..10] |> List.iter (q.Enqueue)
    q |> Seq.toList |> equal [0; 1;2;3;4;5;6;7;8;9;10]

    q.Dequeue() |> equal 0

    q |> Seq.toList |> equal [1;2;3;4;5;6;7;8;9;10]

    // Drain to 50%
    [1..5] |> List.iter (fun _ -> q.Dequeue() |> ignore)

    // Sanity check
    q |> Seq.toList |> equal [6;7;8;9;10]

    // Wrap the circular buffer into the newly-freed space, to 70%
    // count is now 7
    // contents.length is still 10
    // head is < tail (wrapped)
    [11..12] |> List.iter (q.Enqueue)

    q |> Seq.toList |> equal [6;7;8;9;10;11;12]

    q.TrimExcess()

    q.Count |> equal 7
    q |> Seq.toList |> equal [6;7;8;9;10;11;12]

[<Fact>]
let ``test Peek works and doesn't change the queue`` () =
    let q = System.Collections.Generic.Queue<int>();
    q.Enqueue(1);
    q.Enqueue(2);
    q.Peek() |> equal 1
    q |> Seq.toList |> equal [1;2]

[<Fact>]
let ``test TryDequeue works with empty and non-empty queues`` () =
    let q = System.Collections.Generic.Queue<int>()

    (match q.TryDequeue() with (false,_) -> None|(true,n) -> Some n) |> equal None

    q.Enqueue(9)
    (match q.TryDequeue() with (false,_) -> None|(true,n) -> Some n) |> equal (Some 9)

    (match q.TryDequeue() with (false,_) -> None|(true,n) -> Some n) |> equal None

[<Fact>]
let ``test TryPeek works with empty and non-empty queues`` () =
    let q = System.Collections.Generic.Queue<int>()

    (match q.TryPeek() with (false,_) -> None|(true,n) -> Some n) |> equal None

    q.Enqueue(9)
    (match q.TryPeek() with (false,_) -> None|(true,n) -> Some n) |> equal (Some 9)

    (match q.TryPeek() with (false,_) -> None|(true,n) -> Some n) |> equal (Some 9)

[<Fact>]
let ``test Peek throws on empty queue`` () =
    let q = Queue(0);
    (
        try
            q.Peek() |> ignore
            false
        with
        | _ -> true
    ) |> equal true

[<Fact>]
let ``test Dequeue throws on empty queue`` () =
    let q = Queue(0);
    (
        try
            q.Dequeue() |> ignore
            false
        with
        | _ -> true
    ) |> equal true

[<Fact>]
let ``test Clear queue removes all entries`` () =
    let q = Queue([1;2;3;4])

    q.ToArray() |> equal [|1;2;3;4|]

    q.Clear()

    q.Count |> equal(0)

[<Fact>]
let ``test Contains finds entries in queue`` () =
    let q = Queue([1;2;3;4])
    q.Contains(0) |> equal false
    q.Contains(1) |> equal true
    q.Contains(2) |> equal true
    q.Contains(3) |> equal true
    q.Contains(4) |> equal true
    q.Contains(5) |> equal false