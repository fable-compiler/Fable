module Fable.Tests.ResizeArray

open System
open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

type Animal = Duck of int | Dog of int

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

// --- Additional tests ported from Python ---

[<Fact>]
let ``test ResizeArray creation with seq works`` () =
    let li = ResizeArray<_>(seq{1..5})
    Seq.sum li |> equal 15

[<Fact>]
let ``test ResizeArray creation with literal array works`` () =
    let li = ResizeArray<_> [|1;2;3;4;5|]
    Seq.sum li |> equal 15

[<Fact>]
let ``test ResizeArray creation with literal list works`` () =
    let li = ResizeArray<_> [1;2;3;4;5]
    Seq.sum li |> equal 15

// TODO: Casting ResizeArray to seq<_> and assigning to variable loses type info for ref unwrapping
// [<Fact>]
// let ``test ResizeArray casting to seq works`` () =
//     let xs = ResizeArray<_>(seq{1..5}) :> seq<_>
//     Seq.sum xs |> equal 15

[<Fact>]
let ``test ResizeArray iteration with index works`` () =
    let li = ResizeArray<_>()
    for i = 1 to 4 do
       li.Add(i)
    let mutable x = 0
    for i = 0 to li.Count - 1 do
       x <- x + li.[i]
    equal 10 x

[<Fact>]
let ``test ResizeArray folding works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li |> Seq.fold (fun acc item -> acc + item) 0.
    |> equal 15.

[<Fact>]
let ``test ResizeArray.Count works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    equal 5 li.Count

[<Fact>]
let ``test ResizeArray.Find works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    System.Predicate<_> (fun x -> x = 1.)  |> li.Find |> equal 1.
    System.Predicate<_> (fun x -> x = -1.) |> li.Find |> equal 0.

[<Fact>]
let ``test ResizeArray.FindAll works`` () =
    let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
    System.Predicate<_> (fun x -> x <= 3.) |> xs.FindAll |> (fun l -> l.Count) |> equal 3
    System.Predicate<_> (fun x -> x = 5.) |> xs.FindAll |> (fun l -> l.Count) |> equal 0

[<Fact>]
let ``test ResizeArray.FindLast works`` () =
    let li = ResizeArray<_>()
    li.Add(1.,0.); li.Add(2.,0.); li.Add(3.,0.); li.Add(4.,0.); li.Add(5.,0.); li.Add(1.,1.)
    System.Predicate<_> (fun (x, _) -> x = 1.)  |> li.FindLast |> snd |> equal 1.

[<Fact>]
let ``test ResizeArray.FindLastIndex works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
    System.Predicate<_> (fun x -> x = 2.) |> li.FindLastIndex |> equal 3
    System.Predicate<_> (fun x -> x = 0.) |> li.FindLastIndex |> equal -1

[<Fact>]
let ``test ResizeArray.ForEach works`` () =
    let li = ResizeArray<_>()
    let mutable sum = 0
    li.Add(1); li.Add(2); li.Add(3); li.Add(4); li.Add(5)
    System.Action<_> (fun x -> sum <- sum + x) |> li.ForEach
    sum |> equal 15

[<Fact>]
let ``test ResizeArray.InsertRange works`` () =
    let li = ResizeArray<_>()
    li.Add(1); li.Add(2); li.Add(5)
    li.InsertRange(2, [3;4])
    Seq.toList li |> equal [1;2;3;4;5]

[<Fact>]
let ``test ResizeArray.GetRange works`` () =
    let li = ResizeArray<_>()
    li.AddRange [1;2;3]
    let sub = li.GetRange(1, 2)
    sub.Count |> equal 2
    sub.Contains(1) |> equal false

[<Fact>]
let ``test ResizeArray.RemoveAll works`` () =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Add("ab")
    System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 2
    System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 0
    li.[0] |> equal "ch"

// TODO: RemoveRange not supported by Fable
// [<Fact>]
// let ``test ResizeArray.RemoveRange works`` () =
//     let xs = ResizeArray<int>()
//     for x in [1 .. 5] do xs.Add(x)
//     xs.RemoveRange(1, 2)
//     equal 1 xs[0]
//     equal 4 xs[1]
//     equal 5 xs[2]

[<Fact>]
let ``test ResizeArray.Item is undefined when index is out of range`` () =
    let xs = ResizeArray [0]
    try (xs.Item 1) |> ignore; false with _ -> true
    |> equal true

[<Fact>]
let ``test ResizeArray.Remove works with non-primitive types`` () =
    let myResizeArray = ResizeArray<Animal>()
    myResizeArray.Add (Duck 5)
    myResizeArray.Remove (Duck 3) |> ignore
    myResizeArray.Count |> equal 1
    myResizeArray.Remove (Dog 5) |> ignore
    myResizeArray.Count |> equal 1
    myResizeArray.Remove (Duck 5) |> ignore
    myResizeArray.Count |> equal 0

[<Fact>]
let ``test ResizeArray.Contains works with non-primitive types`` () =
    let myResizeArray = ResizeArray<Animal>()
    myResizeArray.Add (Duck 5)
    myResizeArray.Contains (Duck 3) |> equal false
    myResizeArray.Contains (Dog 5) |> equal false
    myResizeArray.Contains (Duck 5) |> equal true

[<Fact>]
let ``test ResizeArray works with Seq.item`` () =
    let li = ResizeArray([1; 2; 3])
    let firstItem = Seq.item 0 li
    equal 1 firstItem
    let secondItem = Seq.item 1 li
    equal 2 secondItem
    let thirdItem = Seq.item 2 li
    equal 3 thirdItem
