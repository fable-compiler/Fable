module Fable.Tests.ResizeArrays

open System.Collections.Generic
open Util.Testing

type Animal = Duck of int | Dog of int

let tests =
  testList "ResizeArrays" [
    testCase "ResizeArray zero creation works" <| fun () ->
        let li = ResizeArray<float>()
        equal 0 li.Count

    testCase "ResizeArray zero creation with size works" <| fun () ->
        let li = ResizeArray<string>(5)
        equal 0 li.Count

    testCase "ResizeArray creation with seq works" <| fun () ->
        let li = ResizeArray<_>(seq{1..5})
        Seq.sum li
        |> equal 15

    testCase "ResizeArray creation with literal array works" <| fun () ->
        let li = ResizeArray<_> [|1;2;3;4;5|]
        Seq.sum li
        |> equal 15

    testCase "ResizeArray creation with literal list works" <| fun () ->
        let li = ResizeArray<_> [1;2;3;4;5]
        Seq.sum li
        |> equal 15

    testCase "ResizeArray casting to seq works" <| fun () ->
        let xs = ResizeArray<_>(seq{1..5}) :> seq<_>
        Seq.sum xs
        |> equal 15

    testCase "ResizeArray iteration works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        let acc = ref 0.
        for i in li do
           acc := !acc + i
        equal 15. !acc

    testCase "ResizeArray iteration with index works" <| fun () ->
        let li = ResizeArray<_>()
        for i = 1 to 4 do
           li.Add(i)
        let mutable x = 0
        for i = 0 to li.Count - 1 do
           x <- x + li.[i]
        equal 10 x

    testCase "ResizeArray folding works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li |> Seq.fold (fun acc item -> acc + item) 0.
        |> equal 15.

    testCase "ResizeArray.Count works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        equal 5 li.Count

    testCase "ResizeArray.ConvertAll works" <| fun () ->
        let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
        let ys = xs.ConvertAll(System.Converter(fun x -> int x))
        ys |> Seq.toList |> equal [1;2;3;4]

    testCase "ResizeArray.Find works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        System.Predicate<_> (fun x -> x = 1.)  |> li.Find |> equal 1.
        System.Predicate<_> (fun x -> x = -1.) |> li.Find |> equal 0.

    testCase "ResizeArray.Find with option works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(Some 1); li.Add(None);
        System.Predicate<_> (fun _ -> true)  |> li.Find |> equal (Some 1)
        System.Predicate<_> (fun _ -> false)  |> li.Find |> equal None
        System.Predicate<_> Option.isNone  |> li.Find |> equal None
        System.Predicate<_> Option.isSome  |> li.Find |> equal (Some 1)

    testCase "ResizeArray.FindAll works" <| fun () ->
        let xs = ResizeArray<_> [1.; 2.; 3.; 4.]
        System.Predicate<_> (fun x -> x <= 3.) |> xs.FindAll |> (fun l -> l.Count) |> equal 3
        System.Predicate<_> (fun x -> x = 5.) |> xs.FindAll |> (fun l -> l.Count) |> equal 0

    testCase "ResizeArray.FindLast works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.,0.); li.Add(2.,0.); li.Add(3.,0.); li.Add(4.,0.); li.Add(5.,0.); li.Add(1.,1.)
        System.Predicate<_> (fun (x, _) -> x = 1.)  |> li.FindLast |> snd |> equal 1.

    testCase "ResizeArray.FindLast with option works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(Some 1); li.Add(None);
        System.Predicate<_> (fun _ -> true)  |> li.FindLast |> equal None
        System.Predicate<_> (fun _ -> false)  |> li.FindLast |> equal None
        System.Predicate<_> Option.isSome  |> li.FindLast |> equal (Some 1)

    testCase "ResizeArray.FindIndex works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
        System.Predicate<_> (fun x -> x = 2.) |> li.FindIndex |> equal 1
        System.Predicate<_> (fun x -> x = 0.) |> li.FindIndex |> equal -1

    testCase "ResizeArray.FindLastIndex works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(2.); li.Add(5.)
        System.Predicate<_> (fun x -> x = 2.) |> li.FindLastIndex |> equal 3
        System.Predicate<_> (fun x -> x = 0.) |> li.FindLastIndex |> equal -1

    testCase "ResizeArray.ForEach works" <| fun () ->
        let li = ResizeArray<_>()
        let mutable sum = 0
        li.Add(1); li.Add(2); li.Add(3); li.Add(4); li.Add(5)
        System.Action<_> (fun x -> sum <- sum + x) |> li.ForEach
        sum |> equal 15

    testCase "ResizeArray indexer getter works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        equal 2. li.[1]

    testCase "ResizeArray indexer setter works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.[3] <- 10.
        equal 10. li.[3]

    testCase "ResizeArray.Clear works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.Clear()
        equal 0 li.Count

    testCase "ResizeArray.Add works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.Add(6.)
        equal 6 li.Count

    testCase "ResizeArray.AddRange works" <| fun () ->
        let li = ResizeArray<_>()
        li.AddRange [1;2;3]
        equal 3 li.Count

    testCase "ResizeArray.InsertRange works" <| fun () ->
        let li = ResizeArray<_>()
        let mutable sum = 0
        li.Add(1); li.Add(2); li.Add(5)
        li.InsertRange(2, [3;4])
        Seq.toList li |> equal [1;2;3;4;5]

    testCase "ResizeArray.GetRange works" <| fun () ->
        let li = ResizeArray<_>()
        li.AddRange [1;2;3]
        let sub = li.GetRange(1, 2)
        sub.Count |> equal 2
        sub.Contains(1) |> equal false

    testCase "ResizeArray.Contains works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add("ab")
        li.Add("ch")
        li.Contains("ab") |> equal true
        li.Contains("cd") |> equal false

    testCase "ResizeArray.IndexOf works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add("ch")
        li.Add("ab")
        li.IndexOf("ab") |> equal 1
        li.IndexOf("cd") |> equal -1

    testCase "ResizeArray.Remove works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add("ab")
        li.Add("ch")
        li.Remove("ab") |> equal true
        li.Remove("cd") |> equal false

    testCase "ResizeArray.RemoveAll works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add("ab")
        li.Add("ch")
        li.Add("ab")
        System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 2
        System.Predicate<_> (fun x -> x = "ab") |> li.RemoveAll |> equal 0
        li.[0] |> equal "ch"

    testCase "ResizeArray.RemoveRange works" <| fun () ->
        let xs = ResizeArray<int>()
        for x in [1 .. 5] do xs.Add(x)
        xs.RemoveRange(1, 2) // [1;2;3;4;5] -> [1;4;5]
        equal 1 xs.[0]
        equal 4 xs.[1]
        equal 5 xs.[2]

    testCase "ResizeArray.Exists works" <| fun () ->
        let xs = ResizeArray<int>()
        for x in [1 .. 5] do xs.Add(x)
        xs.Exists (fun a -> a > 5) |> equal false
        xs.Exists (fun a -> a = 5) |> equal true
        xs.Exists (fun a -> a > 1) |> equal true
        xs.Exists (fun a -> a = 1) |> equal true
        xs.Exists (fun a -> a < 1) |> equal false
        xs.Exists (fun a -> a = 3) |> equal true

    testCase "ResizeArray.RemoveAt works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.RemoveAt(2)
        equal 4. li.[2]

    testCase "ResizeArray.Insert works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.Insert(2, 8.)
        equal 8. li.[2]

    testCase "ResizeArray.ReverseInPlace works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(1.); li.Add(2.); li.Add(3.); li.Add(4.); li.Add(5.)
        li.Reverse()
        equal 2. li.[3]

    testCase "ResizeArray.SortInPlace works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add("Ana"); li.Add("Pedro"); li.Add("Lucía"); li.Add("Paco")
        li.Sort()
        equal "Paco" li.[2]
        let li2 = ResizeArray [1;3;10;2]
        li2.Sort()
        equal 2 li2.[1]

    testCase "ResizeArray.SortInPlaceWith works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
        li.Sort(fun x y -> if x > y then -1 elif x < y then 1 else 0)
        equal 4. li.[3]

    testCase "ResizeArray.SortInPlaceWith works with custom comparison function" <| fun () -> // See #1386
        let ns = ResizeArray<int> [1;3;2]
        ns.Sort(fun x y -> if x < y then 1 else -1)
        Seq.toList ns |> equal [3; 2; 1]
        ns.Sort(compare)
        Seq.toList ns |> equal [1;2;3]

    testCase "ResizeArray.SortInPlaceWith works with custom comparer" <| fun () -> // See #1386
        let ns = ResizeArray<int> [1;3;2]
        let comparer = System.Collections.Generic.Comparer<int>.Default
        ns.Sort(comparer)
        Seq.toList ns |> equal [1;2;3]

    testCase "ResizeArray.ToArray works" <| fun () ->
        let li = ResizeArray<_>()
        li.Add(3.); li.Add(6.); li.Add(5.); li.Add(4.); li.Add(8.)
        equal 5 li.Count
        let ar = li.ToArray()
        Array.length ar |> equal li.Count
        ar.[0] <- 2.
        equal 3. li.[0]
        equal 2. ar.[0]

    testCase "ResizeArray.Item is undefined when index is out of range" <| fun () ->
        let xs = ResizeArray [0]
        #if FABLE_COMPILER
        isNull <| box (xs.Item 1)
        #else
        try (xs.Item 1) |> ignore; false with _ -> true
        #endif
        |> equal true

    testCase "ResizeArray.Remove works with non-primitive types" <| fun _ ->
        let myResizeArray = new ResizeArray<Animal>()
        myResizeArray.Add (Duck 5)
        myResizeArray.Remove (Duck 3) |> ignore
        myResizeArray.Count |> equal 1
        myResizeArray.Remove (Dog 5) |> ignore
        myResizeArray.Count |> equal 1
        myResizeArray.Remove (Duck 5) |> ignore
        myResizeArray.Count |> equal 0

    testCase "ResizeArray.Contains works with non-primitive types" <| fun _ ->
        let myResizeArray = new ResizeArray<Animal>()
        myResizeArray.Add (Duck 5)
        myResizeArray.Contains (Duck 3) |> equal false
        myResizeArray.Contains (Dog 5) |> equal false
        myResizeArray.Contains (Duck 5) |> equal true

    testCase "ResizeArray.IndexOf works with non-primitive types" <| fun _ ->
        let myResizeArray = new ResizeArray<Animal>()
        myResizeArray.Add (Duck 5)
        myResizeArray.IndexOf (Duck 3) |> equal -1
        myResizeArray.IndexOf (Dog 5) |> equal -1
        myResizeArray.IndexOf (Duck 5) |> equal 0
        myResizeArray.Add(Dog 3)
        myResizeArray.IndexOf(Dog 3) |> equal 1
        myResizeArray.IndexOf(Dog 3, 0, 1) |> equal -1
        myResizeArray.IndexOf(Duck 5, 1) |> equal -1

    testCase "ResizeArray.CopyTo works" <| fun _ ->
        let xs = ResizeArray<_> [|1;2;3;4|]
        let ys = [|5;6;7;8;9|]
        xs.CopyTo(ys)
        ys |> equal [|1;2;3;4;9|]

    testCase "ResizeArray.CopyTo works II" <| fun _ ->
        let xs = ResizeArray<_> [|1;2;3;4|]
        let ys = [|5;6;7;8;9|]
        xs.CopyTo(ys, 1)
        ys |> equal [|5;1;2;3;4|]

    testCase "ResizeArray.CopyTo works III" <| fun _ ->
        let xs = ResizeArray<_> [|1;2;3;4|]
        let ys = [|5;6;7;8;9|]
        xs.CopyTo(2, ys, 1, 2)
        ys |> equal [|5;3;4;8;9|]

    testCase "ResizeArray ICollection.IsReadOnly works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.IsReadOnly |> equal false

    testCase "ResizeArray ICollection.Count works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.Count |> equal 3

    testCase "ResizeArray ICollection.Contains works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.Contains(("B", 3)) |> equal false
        coll.Contains(("D", 3)) |> equal false
        coll.Contains(("B", 2)) |> equal true

    testCase "Dictionary ICollection.CopyTo works" <| fun _ -> // See #3914
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        let ys = [| ("D", 4); ("E", 5); ("F", 6) |]
        coll.CopyTo(ys, 0)
        ys = xs |> equal true

    testCase "ResizeArray ICollection.Clear works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.Clear()
        coll.Count |> equal 0

    testCase "ResizeArray ICollection.Add works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.Add(("A", 1))
        coll.Add(("A", 2))
        coll.Add(("D", 4))
        coll.Count |> equal 6

    testCase "ResizeArray ICollection.Remove works" <| fun _ ->
        let xs = [| ("A", 1); ("B", 2); ("C", 3) |]
        let coll = (ResizeArray xs) :> ICollection<_>
        coll.Remove(("B", 3)) |> equal false
        coll.Remove(("D", 3)) |> equal false
        coll.Remove(("B", 2)) |> equal true
        coll.Count |> equal 2
  ]