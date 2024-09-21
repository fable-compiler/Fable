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

[<Fact>]
let ``ResizeArray iteration works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    let mutable acc = 0.
    for i in xs do
        acc <- acc + i
    acc |> equal 15.

[<Fact>]
let ``ResizeArray iteration with index works`` () =
    let xs = ResizeArray<_>()
    for i = 1 to 4 do
        xs.Add(i)
    let mutable x = 0
    for i = 0 to xs.Count - 1 do
        x <- x + xs[i]
    x |> equal 10

[<Fact>]
let ``ResizeArray folding works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    let sum = xs |> Seq.fold (fun acc item -> acc + item) 0.
    sum |> equal 15.

[<Fact>]
let ``ResizeArray.Capacity works`` () =
    let xs = ResizeArray<int>(42)
    xs.Capacity |> equal 42
    xs.Count |> equal 0

[<Fact>]
let ``ResizeArray.Count works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Count |> equal 5

[<Fact>]
let ``ResizeArray.ConvertAll works`` () =
    let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
    let ys = xs.ConvertAll(System.Converter(fun x -> int x))
    ys |> Seq.toList |> equal [1;2;3;4]

[<Fact>]
let ``ResizeArray.Find works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    System.Predicate<_> (fun x -> x = 1.)  |> xs.Find |> equal 1.
    System.Predicate<_> (fun x -> x = -1.) |> xs.Find |> equal 0.

[<Fact>]
let ``ResizeArray.Find with option works`` () =
    let xs = ResizeArray<_>()
    xs.Add(Some 1); xs.Add(None);
    System.Predicate<_> (fun _ -> true)  |> xs.Find |> equal (Some 1)
    System.Predicate<_> (fun _ -> false)  |> xs.Find |> equal None
    System.Predicate<_> Option.isNone  |> xs.Find |> equal None
    System.Predicate<_> Option.isSome  |> xs.Find |> equal (Some 1)

[<Fact>]
let ``ResizeArray.FindAll works`` () =
    let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
    System.Predicate<_> (fun x -> x <= 3.) |> xs.FindAll |> (fun l -> l.Count) |> equal 3
    System.Predicate<_> (fun x -> x = 5.) |> xs.FindAll |> (fun l -> l.Count) |> equal 0

[<Fact>]
let ``ResizeArray.FindLast works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.,0.); xs.Add(2.,0.); xs.Add(3.,0.); xs.Add(4.,0.); xs.Add(5.,0.); xs.Add(1.,1.)
    System.Predicate<_> (fun (x, _) -> x = 1.)  |> xs.FindLast |> snd |> equal 1.

[<Fact>]
let ``ResizeArray.FindLast with option works`` () =
    let xs = ResizeArray<_>()
    xs.Add(Some 1); xs.Add(None);
    System.Predicate<_> (fun _ -> true)  |> xs.FindLast |> equal None
    System.Predicate<_> (fun _ -> false)  |> xs.FindLast |> equal None
    System.Predicate<_> Option.isSome  |> xs.FindLast |> equal (Some 1)

[<Fact>]
let ``ResizeArray.FindIndex works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(2.); xs.Add(5.)
    System.Predicate<_> (fun x -> x = 2.) |> xs.FindIndex |> equal 1
    System.Predicate<_> (fun x -> x = 0.) |> xs.FindIndex |> equal -1

[<Fact>]
let ``ResizeArray.FindLastIndex works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(2.); xs.Add(5.)
    System.Predicate<_> (fun x -> x = 2.) |> xs.FindLastIndex |> equal 3
    System.Predicate<_> (fun x -> x = 0.) |> xs.FindLastIndex |> equal -1

[<Fact>]
let ``ResizeArray.ForEach works`` () =
    let xs = ResizeArray<_>()
    let mutable sum = 0
    xs.Add(1); xs.Add(2); xs.Add(3); xs.Add(4); xs.Add(5)
    System.Action<_> (fun x -> sum <- sum + x) |> xs.ForEach
    sum |> equal 15

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
    xs.ToArray() |> equal [|1.;2.;3.;10.;5.|]

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
    xs.ToArray() |> equal [|1;2;3|]

[<Fact>]
let ``ResizeArray.AddRange works`` () =
    let xs = ResizeArray<_>()
    xs.AddRange [1;2;3]
    xs.ToArray() |> equal [|1;2;3|]

[<Fact>]
let ``ResizeArray.Insert works`` () =
    let xs = ResizeArray<_>([|1;2;3;4;5|])
    xs.Insert(2, 8)
    xs.ToArray() |> equal [|1;2;8;3;4;5|]

[<Fact>]
let ``ResizeArray.InsertRange works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1); xs.Add(2); xs.Add(5)
    xs.InsertRange(2, [3;4])
    xs.ToArray() |> equal [|1;2;3;4;5|]

[<Fact>]
let ``ResizeArray.GetRange works`` () =
    let xs = ResizeArray<_>([|1;2;3;4;5|])
    let ys = xs.GetRange(1, 2)
    ys.ToArray() |> equal [|2;3|]

[<Fact>]
let ``ResizeArray.Slice works`` () =
    let xs = ResizeArray<_>([|1;2;3;4;5|])
    let ys = xs.Slice(1, 2)
    ys.ToArray() |> equal [|2;3|]

[<Fact>]
let ``ResizeArray.Contains works`` () =
    let xs = ResizeArray<_>()
    xs.Add("ab")
    xs.Add("ch")
    xs.Contains("ab") |> equal true
    xs.Contains("cd") |> equal false

[<Fact>]
let ``ResizeArray.IndexOf works`` () =
    let xs = ResizeArray<_>([|3;2;1;3;2|])
    xs.IndexOf(2) |> equal 1
    xs.IndexOf(3) |> equal 0
    xs.IndexOf(4) |> equal -1

[<Fact>]
let ``ResizeArray.IndexOf works II`` () =
    let xs = ResizeArray<_>([|"T";"A";"M";"B";"D";"T";"C"|])
    xs.IndexOf("T") |> equal 0
    xs.IndexOf("T", 3) |> equal 5
    xs.IndexOf("T", 2, 2) |> equal -1

[<Fact>]
let ``ResizeArray.LastIndexOf works`` () =
    let xs = ResizeArray<_>([|3;2;1;3;2|])
    xs.LastIndexOf(2) |> equal 4
    xs.LastIndexOf(3) |> equal 3
    xs.LastIndexOf(4) |> equal -1

[<Fact>]
let ``ResizeArray.LastIndexOf works II`` () =
    let xs = ResizeArray<_>([|"T";"A";"M";"B";"D";"T";"C"|])
    xs.LastIndexOf("T") |> equal 5
    xs.LastIndexOf("T", 3) |> equal 0
    xs.LastIndexOf("T", 4, 4) |> equal -1

[<Fact>]
let ``ResizeArray.Remove works`` () =
    let xs = ResizeArray<_>()
    xs.Add("ab")
    xs.Add("ch")
    xs.Remove("ab") |> equal true
    xs.Remove("cd") |> equal false

[<Fact>]
let ``ResizeArray.RemoveAll works`` () =
    let xs = ResizeArray<_>()
    xs.Add("ab")
    xs.Add("ch")
    xs.Add("ab")
    System.Predicate<_> (fun x -> x = "ab") |> xs.RemoveAll |> equal 2
    System.Predicate<_> (fun x -> x = "ab") |> xs.RemoveAll |> equal 0
    xs[0] |> equal "ch"

[<Fact>]
let ``ResizeArray.RemoveRange works`` () =
    let xs = ResizeArray<int>()
    for x in [1 .. 5] do xs.Add(x)
    xs.RemoveRange(1, 2) // [1;2;3;4;5] -> [1;4;5]
    equal 1 xs[0]
    equal 4 xs[1]
    equal 5 xs[2]

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
    xs.ToArray() |> equal [|1.;2.;4.;5.|]

[<Fact>]
let ``ResizeArray.Reverse works`` () =
    let xs = ResizeArray<_>()
    xs.Add(1.); xs.Add(2.); xs.Add(3.); xs.Add(4.); xs.Add(5.)
    xs.Reverse()
    xs.ToArray() |> equal [|5.;4.;3.;2.;1.|]

[<Fact>]
let ``ResizeArray.Reverse works II`` () =
    let xs = ResizeArray<_>([|1;2;3;4;5|])
    xs.Reverse(2, 3)
    xs.Reverse(1, 2)
    xs.ToArray() |> equal [|1;5;2;4;3|]

[<Fact>]
let ``ResizeArray.Sort works`` () =
    let xs = ResizeArray<_>()
    xs.Add("Ana"); xs.Add("Pedro"); xs.Add("Lucía"); xs.Add("Paco")
    xs.Sort()
    xs.ToArray() |> equal [|"Ana";"Lucía";"Paco";"Pedro"|]

[<Fact>]
let ``ResizeArray.Sort works II`` () =
    let xs = ResizeArray [1;3;10;2]
    xs.Sort()
    xs.ToArray() |> equal [|1;2;3;10|]

[<Fact>]
let ``ResizeArray.Sort works III`` () =
    let xs = ResizeArray [1.;3.;10.;2.]
    xs.Sort()
    xs.ToArray() |> equal [|1.;2.;3.;10.|]

[<Fact>]
let ``ResizeArray.Sort works with custom comparison`` () =
    let xs = ResizeArray<_>()
    xs.Add(3.); xs.Add(6.); xs.Add(5.); xs.Add(4.); xs.Add(8.)
    xs.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
    xs.ToArray() |> equal [|8.;6.;5.;4.;3.|]

[<Fact>]
let ``ResizeArray.Sort works with custom comparison II`` () = // See #1386
    let xs = ResizeArray<int> [1;5;2;3;4]
    xs.Sort(fun x y -> if x < y then 1 else -1)
    xs.ToArray() |> equal [|5;4;3;2;1|]
    xs.Sort(compare)
    xs.ToArray() |> equal [|1;2;3;4;5|]

[<Fact>]
let ``ResizeArray.Sort works with custom comparer`` () = // See #1386
    let xs = ResizeArray<_> [|1;5;2;3;4|]
    let comparer = System.Collections.Generic.Comparer<int>.Default
    xs.Sort(comparer)
    xs.ToArray() |> equal [|1;2;3;4;5|]

[<Fact>]
let ``ResizeArray.Sort works with custom comparer II`` () = // See #1386
    let xs = ResizeArray<_> [|1;5;2;3;4|]
    let comparer = System.Collections.Generic.Comparer<int>.Default
    xs.Sort(1,3, comparer)
    xs.ToArray() |> equal [|1;2;3;5;4|]

[<Fact>]
let ``ResizeArray.ToArray works`` () =
    let xs = ResizeArray<_>()
    xs.Add(3.); xs.Add(6.); xs.Add(5.); xs.Add(4.); xs.Add(8.)
    xs.Count |> equal 5
    xs.ToArray() |> equal [|3.;6.;5.;4.;8.|]

[<Fact>]
let ``ResizeArray.TrimExcess works`` () =
    let xs = ResizeArray<_>(42)
    xs.Add(3.); xs.Add(6.); xs.Add(5.); xs.Add(4.); xs.Add(8.)
    xs.TrimExcess()
    xs.Count |> equal 5
    xs.Capacity |> equal 5

[<Fact>]
let ``ResizeArray.TrueForAll works`` () =
    let xs = ResizeArray<int>()
    for x = 1 to 5 do xs.Add(x)
    xs.TrueForAll (fun a -> a < 6) |> equal true
    xs.TrueForAll (fun a -> a > 5) |> equal false

[<Fact>]
let ``ResizeArray.Item throws when index is out of range`` () =
    let xs = ResizeArray [|1;2;3|]
    throwsAnyError (fun () -> (xs.Item 3) |> ignore)

[<Fact>]
let ``ResizeArray.BinarySearch works`` () =
    let xs = ResizeArray [|1;4;5;7;10;13|]
    xs.BinarySearch(7) |> equal 3
    xs.BinarySearch(8) |> equal ~~~4

[<Fact>]
let ``ResizeArray.BinarySearch works II`` () =
    let xs = ResizeArray [|1;4;5;7;10;13|]
    let comparer = System.Collections.Generic.Comparer<int>.Default
    xs.BinarySearch(7, comparer) |> equal 3
    xs.BinarySearch(8, comparer) |> equal ~~~4

[<Fact>]
let ``ResizeArray.BinarySearch works III`` () =
    let xs = ResizeArray [|1;4;5;7;10;13|]
    let comparer = System.Collections.Generic.Comparer<int>.Default
    xs.BinarySearch(1, 3, 7, comparer) |> equal 3
    xs.BinarySearch(1, 2, 8, comparer) |> equal ~~~3

[<Fact>]
let ``ResizeArray.CopyTo works`` () =
    let xs = ResizeArray<_> [|1;2;3;4|]
    let ys = [|5;6;7;8;9|]
    xs.CopyTo(ys)
    ys |> equal [|1;2;3;4;9|]

[<Fact>]
let ``ResizeArray.CopyTo works II`` () =
    let xs = ResizeArray<_> [|1;2;3;4|]
    let ys = [|5;6;7;8;9|]
    xs.CopyTo(ys, 1)
    ys |> equal [|5;1;2;3;4|]

[<Fact>]
let ``ResizeArray.CopyTo works III`` () =
    let xs = ResizeArray<_> [|1;2;3;4|]
    let ys = [|5;6;7;8;9|]
    xs.CopyTo(2, ys, 1, 2)
    ys |> equal [|5;3;4;8;9|]
