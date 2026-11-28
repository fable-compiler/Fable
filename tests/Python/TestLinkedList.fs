module Fable.Tests.LinkedList

open System.Collections.Generic
open Util.Testing
open Fable.Tests.Util

[<Fact>]
let ``test Empty LinkedList constructor works`` () =
    let xs = LinkedList<int>()
    xs.Count |> equal 0
    isNull xs.First |> equal true
    isNull xs.Last |> equal true
    List.ofSeq xs |> equal []

[<Fact>]
let ``test LinkedList constructed from a sequence keeps its order`` () =
    let xs = LinkedList<int>([ 1; 2; 3 ])
    xs.Count |> equal 3
    xs.First.Value |> equal 1
    xs.Last.Value |> equal 3
    List.ofSeq xs |> equal [ 1; 2; 3 ]

[<Fact>]
let ``test LinkedList keeps the order of mixed adds`` () =
    let xs = LinkedList<int>()
    let two = xs.AddLast(2)
    xs.AddFirst(1) |> ignore
    let four = xs.AddLast(4)
    xs.AddBefore(four, 3) |> ignore
    xs.AddAfter(four, 5) |> ignore
    xs.AddAfter(two, LinkedListNode<int>(25))
    xs.AddBefore(two, LinkedListNode<int>(15))
    xs.AddFirst(LinkedListNode<int>(0))
    xs.AddLast(LinkedListNode<int>(6))
    List.ofSeq xs |> equal [ 0; 1; 15; 2; 25; 3; 4; 5; 6 ]
    xs.Count |> equal 9
    xs.First.Value |> equal 0
    xs.Last.Value |> equal 6

[<Fact>]
let ``test LinkedList AddFirst returns the node Remove takes`` () =
    let xs = LinkedList<string>()
    let a = xs.AddFirst("a")
    let b = xs.AddFirst("b")
    a.Value |> equal "a"
    obj.ReferenceEquals(xs.First, b) |> equal true
    obj.ReferenceEquals(xs.Last, a) |> equal true
    xs.Remove(a)
    List.ofSeq xs |> equal [ "b" ]
    xs.Count |> equal 1
    isNull a.List |> equal true

[<Fact>]
let ``test LinkedList can move a node to the front`` () =
    let xs = LinkedList<int * string>()
    let a = xs.AddFirst((1, "a"))
    let b = xs.AddFirst((2, "b"))
    let c = xs.AddFirst((3, "c"))
    xs.Remove(a)
    xs.AddFirst(a)
    List.ofSeq xs |> equal [ (1, "a"); (3, "c"); (2, "b") ]
    obj.ReferenceEquals(xs.First, a) |> equal true
    fst xs.Last.Value |> equal 2
    xs.RemoveLast()
    List.ofSeq xs |> equal [ (1, "a"); (3, "c") ]
    snd c.Value |> equal "c"
    isNull b.List |> equal true

[<Fact>]
let ``test LinkedListNode Next, Previous and List work`` () =
    let xs = LinkedList<int>([ 1; 2; 3 ])
    let first = xs.First
    let middle = first.Next
    let last = xs.Last
    middle.Value |> equal 2
    obj.ReferenceEquals(middle.Previous, first) |> equal true
    obj.ReferenceEquals(middle.Next, last) |> equal true
    isNull first.Previous |> equal true
    isNull last.Next |> equal true
    obj.ReferenceEquals(middle.List, xs) |> equal true
    xs.Remove(middle)
    isNull middle.List |> equal true
    isNull middle.Next |> equal true
    isNull middle.Previous |> equal true
    obj.ReferenceEquals(first.Next, last) |> equal true
    obj.ReferenceEquals(last.Previous, first) |> equal true

[<Fact>]
let ``test LinkedListNode constructor and Value setter work`` () =
    let node = LinkedListNode<string>("a")
    node.Value |> equal "a"
    isNull node.List |> equal true
    isNull node.Next |> equal true
    isNull node.Previous |> equal true
    node.Value <- "b"
    node.Value |> equal "b"
    let xs = LinkedList<string>()
    xs.AddLast(node)
    obj.ReferenceEquals(node.List, xs) |> equal true
    isNull node.Next |> equal true
    isNull node.Previous |> equal true
    xs.First.Value <- "c"
    List.ofSeq xs |> equal [ "c" ]

[<Fact>]
let ``test LinkedList Remove by value removes the first match`` () =
    let xs = LinkedList<int>([ 1; 2; 1; 3 ])
    xs.Remove(1) |> equal true
    List.ofSeq xs |> equal [ 2; 1; 3 ]
    xs.Remove(4) |> equal false
    xs.Count |> equal 3

[<Fact>]
let ``test LinkedList RemoveFirst and RemoveLast work`` () =
    let xs = LinkedList<int>([ 1; 2; 3 ])
    xs.RemoveFirst()
    xs.RemoveLast()
    List.ofSeq xs |> equal [ 2 ]
    xs.RemoveLast()
    xs.Count |> equal 0
    isNull xs.First |> equal true
    isNull xs.Last |> equal true

[<Fact>]
let ``test LinkedList Find, FindLast and Contains work`` () =
    let xs = LinkedList<string>([ "a"; "b"; "a" ])
    obj.ReferenceEquals(xs.Find("a"), xs.First) |> equal true
    obj.ReferenceEquals(xs.FindLast("a"), xs.Last) |> equal true
    isNull (xs.Find("c")) |> equal true
    isNull (xs.FindLast("c")) |> equal true
    xs.Contains("b") |> equal true
    xs.Contains("c") |> equal false
    let ys = LinkedList<int * string>([ (1, "a"); (2, "b") ])
    obj.ReferenceEquals(ys.Find((2, "b")), ys.Last) |> equal true
    ys.Contains((1, "a")) |> equal true

[<Fact>]
let ``test LinkedList Clear works`` () =
    let xs = LinkedList<int>([ 1; 2 ])
    let first = xs.First
    xs.Clear()
    xs.Count |> equal 0
    isNull xs.First |> equal true
    isNull xs.Last |> equal true
    List.ofSeq xs |> equal []
    isNull first.List |> equal true
    isNull first.Next |> equal true
    xs.AddLast(first)
    List.ofSeq xs |> equal [ 1 ]

[<Fact>]
let ``test LinkedList CopyTo works`` () =
    let xs = LinkedList<int>([ 1; 2; 3 ])
    let target = Array.zeroCreate<int> 5
    xs.CopyTo(target, 1)
    target |> equal [| 0; 1; 2; 3; 0 |]
    raisedExceptionType (fun () -> xs.CopyTo(target, 3)) |> equal "ArgumentException"
    raisedExceptionType (fun () -> xs.CopyTo(target, -1)) |> equal "ArgumentOutOfRangeException"
    raisedExceptionType (fun () -> xs.CopyTo(target, 6)) |> equal "ArgumentOutOfRangeException"

[<Fact>]
let ``test LinkedList can be enumerated in a for loop`` () =
    let xs = LinkedList<int>([ 1; 2; 3 ])
    let mutable acc = 0
    for x in xs do
        acc <- acc * 10 + x
    acc |> equal 123

[<Fact>]
let ``test LinkedList RemoveFirst and RemoveLast raise on an empty list`` () =
    let xs = LinkedList<int>()
    raisedExceptionType (fun () -> xs.RemoveFirst()) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.RemoveLast()) |> equal "InvalidOperationException"

[<Fact>]
let ``test LinkedList raises when adding a node that belongs to a list`` () =
    let xs = LinkedList<int>()
    let ys = LinkedList<int>()
    let node = xs.AddFirst(1)
    let other = ys.AddFirst(2)
    raisedExceptionType (fun () -> xs.AddFirst(node)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> ys.AddLast(node)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.AddBefore(node, other)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.AddAfter(node, other)) |> equal "InvalidOperationException"
    List.ofSeq xs |> equal [ 1 ]
    List.ofSeq ys |> equal [ 2 ]

[<Fact>]
let ``test LinkedList raises when given a node of another list`` () =
    let xs = LinkedList<int>()
    let ys = LinkedList<int>()
    let node = ys.AddFirst(1)
    raisedExceptionType (fun () -> xs.Remove(node)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.AddBefore(node, 2)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.AddAfter(node, 2)) |> equal "InvalidOperationException"
    raisedExceptionType (fun () -> xs.Remove(LinkedListNode<int>(3))) |> equal "InvalidOperationException"
    xs.Count |> equal 0
    List.ofSeq ys |> equal [ 1 ]

[<Fact>]
let ``test LinkedList raises on a null node or collection`` () =
    let xs = LinkedList<int>([ 1 ])
    let nullNode: LinkedListNode<int> = null
    raisedExceptionType (fun () -> xs.AddFirst(nullNode)) |> equal "ArgumentNullException"
    raisedExceptionType (fun () -> xs.AddLast(nullNode)) |> equal "ArgumentNullException"
    raisedExceptionType (fun () -> xs.Remove(nullNode)) |> equal "ArgumentNullException"
    raisedExceptionType (fun () -> xs.AddBefore(nullNode, 2)) |> equal "ArgumentNullException"
    raisedExceptionType (fun () -> xs.AddAfter(xs.First, nullNode)) |> equal "ArgumentNullException"
    raisedExceptionType (fun () -> LinkedList<int>(null: IEnumerable<int>)) |> equal "ArgumentNullException"
    List.ofSeq xs |> equal [ 1 ]
