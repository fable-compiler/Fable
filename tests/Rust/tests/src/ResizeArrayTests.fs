module Fable.Tests.ResizeArrayTests

open Util.Testing

[<Fact>]
let ``ResizeArray creation works`` () =
    let xs = ResizeArray<float>()
    xs.Count |> equal 0

[<Fact>]
let ``ResizeArray creation with capacity works`` () =
    let xs = ResizeArray<string>(5)
    xs.Count |> equal 0

[<Fact>]
let ``ResizeArray creation with seq works`` () =
    let xs = ResizeArray<_>(seq{1;2;3;4;5})
    Seq.sum xs |> equal 15

[<Fact>]
let ``ResizeArray creation with literal array works`` () =
    let xs = ResizeArray<_> [|1;2;3;4;5|]
    Seq.sum xs |> equal 15

[<Fact>]
let ``ResizeArray creation with literal list works`` () =
    let xs = ResizeArray<_> [1;2;3;4;5]
    Seq.sum xs |> equal 15

[<Fact>]
let ``ResizeArray casting to seq works`` () =
    let xs = ResizeArray<_>(seq{1;2;3;4;5}) :> seq<_>
    Seq.sum xs |> equal 15

// [<Fact>]
// let ``ResizeArray iteration works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
//     let mutable acc = 0.
//     for i in xs do
//         acc <- acc + i
//     acc |> equal 15.

// [<Fact>]
// let ``ResizeArray iteration with index works`` () =
//     let xs = ResizeArray<_>()
//     for i = 1 to 4 do
//         xs.Add(i)
//     let mutable x = 0
//     for i = 0 to xs.Count - 1 do
//         x <- x + xs[i]
//     x |> equal 10

// [<Fact>]
// let ``ResizeArray folding works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
//     xs |> Seq.fold (fun acc item -> acc + item) 0.
//     |> equal 15.

[<Fact>]
let ``ResizeArray.Count works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Count |> equal 5

// [<Fact>]
// let ``ResizeArray.ConvertAll works`` () =
//     let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
//     let ys = xs.ConvertAll(System.Converter(fun x -> int x))
//     ys |> Seq.toList |> equal [1;2;3;4]

// [<Fact>]
// let ``ResizeArray.Find works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
//     System.Predicate<_> (fun x -> x = 1.)  |> xs.Find |> equal 1.
//     System.Predicate<_> (fun x -> x = -1.) |> xs.Find |> equal 0.

// [<Fact>]
// let ``ResizeArray.Find with option works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(Some 1); xs.Add(None);
//     System.Predicate<_> (fun _ -> true)  |> xs.Find |> equal (Some 1)
//     System.Predicate<_> (fun _ -> false)  |> xs.Find |> equal None
//     System.Predicate<_> Option.isNone  |> xs.Find |> equal None
//     System.Predicate<_> Option.isSome  |> xs.Find |> equal (Some 1)

// [<Fact>]
// let ``ResizeArray.FindAll works`` () =
//     let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
//     System.Predicate<_> (fun x -> x <= 3.) |> xs.FindAll |> (fun l -> l.Count) |> equal 3
//     System.Predicate<_> (fun x -> x = 5.) |> xs.FindAll |> (fun l -> l.Count) |> equal 0

// [<Fact>]
// let ``ResizeArray.FindLast works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.,0.); xs.Add(2.,0.); xs.Add(3.,0.); xs.Add(4.,0.); xs.Add(5.,0.); xs.Add(1.,1.)
//     System.Predicate<_> (fun (x, _) -> x = 1.)  |> xs.FindLast |> snd |> equal 1.

// [<Fact>]
// let ``ResizeArray.FindLast with option works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(Some 1); xs.Add(None);
//     System.Predicate<_> (fun _ -> true)  |> xs.FindLast |> equal None
//     System.Predicate<_> (fun _ -> false)  |> xs.FindLast |> equal None
//     System.Predicate<_> Option.isSome  |> xs.FindLast |> equal (Some 1)

// [<Fact>]
// let ``ResizeArray.FindIndex works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(2.); xs.Add(5.)
//     System.Predicate<_> (fun x -> x = 2.) |> xs.FindIndex |> equal 1
//     System.Predicate<_> (fun x -> x = 0.) |> xs.FindIndex |> equal -1

// [<Fact>]
// let ``ResizeArray.FindLastIndex works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(2.); xs.Add(5.)
//     System.Predicate<_> (fun x -> x = 2.) |> xs.FindLastIndex |> equal 3
//     System.Predicate<_> (fun x -> x = 0.) |> xs.FindLastIndex |> equal -1

// [<Fact>]
// let ``ResizeArray.ForEach works`` () =
//     let xs = ResizeArray<_>()
//     let mutable sum = 0
//     xs.Add(1); xs.Add(2); xs.Add(3); xs.Add(4); xs.Add(5)
//     System.Action<_> (fun x -> sum <- sum + x) |> xs.ForEach
//     sum |> equal 15

[<Fact>]
let ``ResizeArray indexer getter works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs[1] |> equal 2.

[<Fact>]
let ``ResizeArray indexer setter works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs[3] <- 10.
    xs[3] |> equal 10.

[<Fact>]
let ``ResizeArray.Clear works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Clear()
    xs.Count |> equal 0

[<Fact>]
let ``ResizeArray.Add works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.)
    equal 3 xs.Count

// [<Fact>]
// let ``ResizeArray.AddRange works`` () =
//     let xs = ResizeArray<_>()
//     xs.AddRange [1;2;3]
//     equal 3 xs.Count

// [<Fact>]
// let ``ResizeArray.InsertRange works`` () =
//     let xs = ResizeArray<_>()
//     let mutable sum = 0
//     xs.Add(1); xs.Add(2); xs.Add(5)
//     xs.InsertRange(2, [3;4])
//     Seq.toList xs |> equal [1;2;3;4;5]

// [<Fact>]
// let ``ResizeArray.GetRange works`` () =
//     let xs = ResizeArray<_>()
//     xs.AddRange [1;2;3]
//     let sub = xs.GetRange(1, 2)
//     sub.Count |> equal 2
//     sub.Contains(1) |> equal false

[<Fact>]
let ``ResizeArray.Contains works`` () =
    let xs = ResizeArray<_>()
    xs.Add("ab")
    xs.Add("ch")
    xs.Contains("ab") |> equal true
    xs.Contains("cd") |> equal false

[<Fact>]
let ``ResizeArray.IndexOf works`` () =
    let xs = ResizeArray<_>()
    xs.Add("ch")
    xs.Add("ab")
    xs.IndexOf("ab") |> equal 1
    xs.IndexOf("cd") |> equal -1

// [<Fact>]
// let ``ResizeArray.Remove works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add("ab")
//     xs.Add("ch")
//     xs.Remove("ab") |> equal true
//     xs.Remove("cd") |> equal false

// [<Fact>]
// let ``ResizeArray.RemoveAll works`` () =
//     let xs = ResizeArray<_>()
//     xs.Add("ab")
//     xs.Add("ch")
//     xs.Add("ab")
//     System.Predicate<_> (fun x -> x = "ab") |> xs.RemoveAll |> equal 2
//     System.Predicate<_> (fun x -> x = "ab") |> xs.RemoveAll |> equal 0
//     xs[0] |> equal "ch"

// [<Fact>]
// let ``ResizeArray.RemoveRange works`` () =
//     let xs = ResizeArray<int>()
//     for x in [1 .. 5] do xs.Add(x)
//     xs.RemoveRange(1, 2) // [1;2;3;4;5] -> [1;4;5]
//     equal 1 xs[0]
//     equal 4 xs[1]
//     equal 5 xs[2]

[<Fact>]
let ``ResizeArray.Exists works`` () =
    let xs = ResizeArray<int>()
    for x = 1 to 5 do xs.Add(x)
    xs.Exists (fun a -> a > 5) |> equal false
    xs.Exists (fun a -> a = 5) |> equal true
    xs.Exists (fun a -> a > 1) |> equal true
    xs.Exists (fun a -> a = 1) |> equal true
    xs.Exists (fun a -> a < 1) |> equal false
    xs.Exists (fun a -> a = 3) |> equal true

[<Fact>]
let ``ResizeArray.RemoveAt works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.RemoveAt(2)
    xs.Count |> equal 4
    xs[2] |> equal 4.

[<Fact>]
let ``ResizeArray.Insert works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Insert(2, 8.)
    xs.Count |> equal 6
    xs[2] |> equal 8.

[<Fact>]
let ``ResizeArray.ReverseInPlace works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Reverse()
    xs.ToArray() |> equal [|5.;4.;3.;2.;1.|]

[<Fact>]
let ``ResizeArray.SortInPlace works`` () =
    let xs = ResizeArray<_>()
    xs.Add("Ana"); xs.Add("Pedro"); xs.Add("Lucía"); xs.Add("Paco")
    xs.Sort()
    xs.ToArray() |> equal [|"Ana";"Lucía";"Paco";"Pedro"|]

[<Fact>]
let ``ResizeArray.SortInPlace works II`` () =
    let xs = ResizeArray [1;3;10;2]
    xs.Sort()
    xs.ToArray() |> equal [|1;2;3;10|]

[<Fact>]
let ``ResizeArray.SortInPlace works III`` () =
    let xs = ResizeArray [1.;3.;10.;2.]
    xs.Sort()
    xs.ToArray() |> equal [|1.;2.;3.;10.|]

[<Fact>]
let ``ResizeArray.SortInPlaceWith works`` () =
    let xs = ResizeArray<_>()
    xs.Add(3.); xs.Add(6.); xs.Add(5.); xs.Add(4.); xs.Add(8.)
    xs.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
    xs.ToArray() |> equal [|8.;6.;5.;4.;3.|]

[<Fact>]
let ``ResizeArray.SortInPlaceWith works with custom comparison function`` () = // See #1386
    let xs = ResizeArray<int> [1;3;2]
    xs.Sort(fun x y -> if x < y then 1 else -1)
    xs.ToArray() |> equal [|3;2;1|]
    xs.Sort(compare)
    xs.ToArray() |> equal [|1;2;3|]

// [<Fact>]
// let ``ResizeArray.SortInPlaceWith works with custom comparer`` () = // See #1386
//     let xs = ResizeArray<int> [1;3;2]
//     let comparer = System.Collections.Generic.Comparer<int>.Default
//     xs.Sort(comparer)
//     Seq.toList xs |> equal [1;2;3]

[<Fact>]
let ``ResizeArray.ToArray works`` () =
    let xs = ResizeArray<_>()
    xs.Add(3.); xs.Add(6.); xs.Add(5.); xs.Add(4.); xs.Add(8.)
    xs.Count |> equal 5
    let ar = xs.ToArray()
    Array.length ar |> equal 5
    ar[0] <- 2.
    equal 3. xs[0]
    equal 2. ar[0]

[<Fact>]
let ``ResizeArray.Item is undefined when index is out of range`` () =
    let xs = ResizeArray [0]
#if FABLE_COMPILER_JAVASCRIPT
    isNull <| box (xs.Item 1) |> equal true
#else
    throwsAnyError (fun () -> (xs.Item 1) |> ignore)
#endif
