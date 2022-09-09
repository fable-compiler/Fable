module Fable.Tests.ResizeArrays

open Util.Testing
open System.Text

type Animal = Duck of int | Dog of int

[<Fact>]
let ``test ResizeArray zero creation works`` () =
    let li = ResizeArray<float>()
    equal 0 li.Count

[<Fact>]
let ``test ResizeArray zero creation with size works`` () =
    let li = ResizeArray<string>(5)
    equal 0 li.Count

[<Fact>]
let ``test ResizeArray creation with seq works`` () =
    let li = ResizeArray<_>(seq{1..5})
    Seq.sum li
    |> equal 15

[<Fact>]
let ``test ResizeArray creation with literal array works`` () =
    let li = ResizeArray<_> [|1;2;3;4;5|]
    Seq.sum li
    |> equal 15

[<Fact>]
let ``test ResizeArray creation with literal list works`` () =
    let li = ResizeArray<_> [1;2;3;4;5]
    Seq.sum li
    |> equal 15

[<Fact>]
let ``test ResizeArray casting to seq works`` () =
    let xs = ResizeArray<_>(seq{1..5}) :> seq<_>
    Seq.sum xs
    |> equal 15

[<Fact>]
let ``test ResizeArray iteration works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    let acc = ref 0.
    for i in li do
       acc.Value <- acc.Value + i
    equal 15. acc.Value

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
let ``test ResizeArray.Find with option works`` () =
    let li = ResizeArray<_>()
    li.Add(Some 1); li.Add(None);
    System.Predicate<_> (fun _ -> true)  |> li.Find |> equal (Some 1)
    System.Predicate<_> (fun _ -> false)  |> li.Find |> equal None
    System.Predicate<_> Option.isNone  |> li.Find |> equal None
    System.Predicate<_> Option.isSome  |> li.Find |> equal (Some 1)

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
let ``test ResizeArray.FindLast with option works`` () =
    let li = ResizeArray<_>()
    li.Add(Some 1); li.Add(None);
    System.Predicate<_> (fun _ -> true)  |> li.FindLast |> equal None
    System.Predicate<_> (fun _ -> false)  |> li.FindLast |> equal None
    System.Predicate<_> Option.isSome  |> li.FindLast |> equal (Some 1)

[<Fact>]
let ``test ResizeArray.FindIndex works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
    System.Predicate<_> (fun x -> x = 2.) |> li.FindIndex |> equal 1
    System.Predicate<_> (fun x -> x = 0.) |> li.FindIndex |> equal -1

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
let ``test ResizeArray indexer getter works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    equal 2. li.[1]

[<Fact>]
let ``test ResizeArray indexer setter works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.[3] <- 10.
    equal 10. li.[3]

[<Fact>]
let ``test ResizeArray.Clear works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Clear()
    equal 0 li.Count

[<Fact>]
let ``test ResizeArray.Add works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Add(6.)
    equal 6 li.Count

[<Fact>]
let ``test ResizeArray.AddRange works`` () =
    let li = ResizeArray<_>()
    li.AddRange [1;2;3]
    equal 3 li.Count

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
let ``test ResizeArray.Contains works`` () =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Contains("ab") |> equal true
    li.Contains("cd") |> equal false

[<Fact>]
let ``test ResizeArray.IndexOf works`` () =
    let li = ResizeArray<_>()
    li.Add("ch")
    li.Add("ab")
    li.IndexOf("ab") |> equal 1
    li.IndexOf("cd") |> equal -1

[<Fact>]
let ``test ResizeArray.Remove works`` () =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Remove("ab") |> equal true
    li.Remove("cd") |> equal false

[<Fact>]
let ``test ResizeArray.RemoveAll works`` () =
    let li = ResizeArray<_>()
    li.Add("ab")
    li.Add("ch")
    li.Add("ab")
    System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 2
    System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 0
    li.[0] |> equal "ch"

[<Fact>]
let ``test ResizeArray.RemoveRange works`` () =
    let xs = ResizeArray<int>()
    for x in [1 .. 5] do xs.Add(x)
    xs.RemoveRange(1, 2) // [1;2;3;4;5] -> [1;4;5]
    equal 1 xs[0]
    equal 4 xs[1]
    equal 5 xs[2]

[<Fact>]
let ``test ResizeArray.Exists works`` () =
    let xs = ResizeArray<int>()
    for x in [1 .. 5] do xs.Add(x)
    xs.Exists (fun a -> a > 5) |> equal false
    xs.Exists (fun a -> a = 5) |> equal true
    xs.Exists (fun a -> a > 1) |> equal true
    xs.Exists (fun a -> a = 1) |> equal true
    xs.Exists (fun a -> a < 1) |> equal false
    xs.Exists (fun a -> a = 3) |> equal true

[<Fact>]
let ``test ResizeArray.RemoveAt works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.RemoveAt(2)
    equal 4. li.[2]
[<Fact>]
let ``test ResizeArray.Insert works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Insert(2, 8.)
    equal 8. li.[2]
[<Fact>]
let ``test ResizeArray.ReverseInPlace works`` () =
    let li = ResizeArray<_>()
    li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
    li.Reverse()
    equal 2. li.[3]

[<Fact>]
let ``test ResizeArray.SortInPlace works`` () =
    let li = ResizeArray<_>()
    li.Add("Ana"); li.Add("Pedro"); li.Add("Lucía"); li.Add("Paco")
    li.Sort()
    equal "Paco" li.[2]
    let li2 = ResizeArray [1;3;10;2]
    li2.Sort()
    equal 2 li2.[1]

(*
[<Fact>]
let ``test ResizeArray.SortInPlaceWith works`` () =
    let li = ResizeArray<_>()
    li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
    li.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
    equal 4. li.[3]

[<Fact>]
let ``test ResizeArray.SortInPlaceWith works with custom comparison function`` () =
    let ns = ResizeArray<int> [1;3;2]
    ns.Sort(fun x y -> if x < y then 1 else -1)
    Seq.toList ns |> equal [3; 2; 1]
    ns.Sort(compare)
    Seq.toList ns |> equal [1;2;3]

[<Fact>]
let ``test ResizeArray.SortInPlaceWith works with custom comparer`` () =
    let ns = ResizeArray<int> [1;3;2]
    let comparer = System.Collections.Generic.Comparer<int>.Default
    ns.Sort(comparer)
    Seq.toList ns |> equal [1;2;3]
*)
[<Fact>]
let ``test ResizeArray.ToArray works`` () =
    let li = ResizeArray<_>()
    li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
    equal 5 li.Count
    let ar = li.ToArray()
    Array.length ar |> equal li.Count
    ar[0] <- 2.
    equal 3. li[0]
    equal 2. ar[0]

[<Fact>]
let ``test ResizeArray<byte>.ToArray works`` () =
    let li = ResizeArray<byte>()
    li.Add(0x66uy); li.Add(0x61uy); li.Add(0x62uy); li.Add(0x6cuy); li.Add(0x65uy)
    let ar = li.ToArray()
    let text = ar |> Encoding.UTF8.GetString
    equal text "fable"

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
let ``test ResizeArray.IndexOf works with non-primitive types`` () =
    let myResizeArray = ResizeArray<Animal>()
    myResizeArray.Add (Duck 5)
    myResizeArray.IndexOf (Duck 3) |> equal -1
    myResizeArray.IndexOf (Dog 5) |> equal -1
    myResizeArray.IndexOf (Duck 5) |> equal 0
    myResizeArray.Add(Dog 3)
    myResizeArray.IndexOf(Dog 3) |> equal 1
    myResizeArray.IndexOf(Dog 3, 0, 1) |> equal -1
    myResizeArray.IndexOf(Duck 5, 1) |> equal -1