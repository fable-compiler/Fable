module Fable.Tests.Dart.Map

open System.Collections.Generic
open Util

type R = { i: int; s: string }
//type R2 = { kv: KeyValuePair<string,int> }

[<CustomEquality; CustomComparison>]
type R3 =
    { Bar: string
      Foo: int }
    interface System.IComparable with
        member this.CompareTo(x) =
            match x with
            | :? R3 as x -> compare this.Bar x.Bar
            | _ -> -1
    override this.GetHashCode() = hash this.Bar
    override this.Equals(x) =
        match x with
        | :? R3 as x -> this.Bar = x.Bar
        | _ -> false

type R4 =
    { Bar: string
      Baz: int }

let tests() =
    testCase "Map construction from lists works" <| fun () ->
        let xs = Map [1,1; 2,2]
        xs |> Seq.isEmpty
        |> equal false

    testCase "Map.isEmpty works" <| fun () ->
        let xs: Map<int, int> = Map []
        Map.isEmpty xs |> equal true
        let ys = Map [1,1]
        Map.isEmpty ys |> equal false

    // TODO
//    testCase "Map.IsEmpty works" <| fun () ->
//        let xs = Map.empty<int, int>
//        xs.IsEmpty |> equal true
//        let ys = Map [1,1; 2,2]
//        ys.IsEmpty |> equal false
//
//    testCase "Map.Count works" <| fun () ->
//        let xs = Map.empty<int, int>
//        xs.Count
//        |> equal 0

    testCase "Map.count works" <| fun () ->
        let m1 = Map.empty<int, int>
        let c1 = Map.count m1
        equal 0 c1

    testCase "Map.add works" <| fun () ->
        let xs = Map.empty |> Map.add 1 1
        xs.Count
        |> equal 1

    // TODO
//    testCase "Map.Add works" <| fun () ->
//        let xs = Map.empty.Add(1, 1)
//        xs.Count
//        |> equal 1
//
//    testCase "Map.TryGetValue works" <| fun () ->
//        let xs = Map.empty.Add(1, 1)
//        xs.TryGetValue(1)
//        |> equal (true, 1)

    testCase "Map.containsKey works" <| fun () ->
        let xs = Map.empty |> Map.add 1 1
        xs |> Map.containsKey 1 |> equal true
        xs |> Map.containsKey 2 |> equal false

    // TODO
//    testCase "Map.ContainsKey works" <| fun () ->
//        let xs = Map.empty |> Map.add 1 1
//        xs.ContainsKey 1 |> equal true
//        xs.ContainsKey 2 |> equal false

    testCase "Map.remove works" <| fun () ->
        let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
        xs.IsEmpty |> equal true

    // TODO
//    testCase "Map.Remove works" <| fun () ->
//        let xs = (Map.empty |> Map.add 1 1).Remove 1
//        xs.IsEmpty |> equal true

    testCase "Map.iter works" <| fun () ->
        let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
        let mutable total = 0.
        xs |> Map.iter (fun x y -> total <- total + x + y)
        total |> equal 40.

    testCase "Map.forAll works" <| fun () ->
        let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
        xs |> Map.forall (fun x y -> x < 5.)
        |> equal true

    testCase "Map.exists works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.exists (fun k v -> k = 2)
        |> equal true

    testCase "Map.filter works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        let ys = xs |> Map.filter (fun x y -> x % 2 = 0)
        ys.Count |> equal 2
        ys.[4] |> equal 16.

    testCase "Map.partition works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        let ys, zs = xs |> Map.partition (fun x y -> x % 2 = 0)
        ys.Count + zs.Count
        |> equal 4

    testCase "Map.fold works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.fold (fun acc k v -> v - acc) 0.
        |> equal 10.

    testCase "Map.foldBack works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        Map.foldBack (fun k v acc -> v - acc) xs 0.
        |> equal -10.

    testCase "Map.map works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        let ys = xs |> Map.map (fun k v -> v * 2.)
        ys.[3] |> equal 18.

    testCase "Map.find works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.find 2
        |> equal 4.

//    testCase "Map.find with option works" <| fun () ->
//        let xs = Map [1,Some 1.; 2,None]
//        xs |> Map.find 2
//        |> Option.isNone
//        |> equal true
//        xs |> Map.find 1 |> (fun x -> x.Value)
//        |> equal 1.
//
//    testCase "Map.tryFind with option works" <| fun () ->
//        let xs = Map [1,Some 1.; 2,None]
//        xs |> Map.tryFind 2 |> (fun x -> x.Value)
//        |> Option.isNone
//        |> equal true
//        xs |> Map.tryFind 1 |> (fun x -> x.Value.Value)
//        |> equal 1.
//        xs |> Map.tryFind 3
//        |> equal None

    testCase "Map.tryFind works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.tryFind 0
        |> Option.isNone
        |> equal true

    testCase "Map.TryFind works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs.TryFind 3
        |> Option.isSome
        |> equal true

    testCase "Map.findKey works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.findKey (fun _ v -> v = 9.)
        |> equal 3

    testCase "Map.tryFindKey works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        xs |> Map.tryFindKey (fun _ v -> v > 10.) |> equal (Some 4)
        xs |> Map.tryFindKey (fun _ v -> v > 20.) |> equal None

    testCase "Map.pick works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        let y = xs |> Map.pick (fun k v ->
           match k with
           | 3 -> Some 10
           | _ -> None)
        equal 10 y

    testCase "Map.tryPick works" <| fun () ->
        let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
        let y = xs |> Map.tryPick (fun k v ->
           match k with
           | 3 -> Some 11
           | _ -> None)
        equal (Some 11) y

    testCase "Map.ofList works" <| fun () ->
        let xs = Map.ofList [1,1.; 2,4.; 3,9.; 4,16.]
        equal 4 xs.Count

    testCase "Map.ofArray works" <| fun () ->
        let xs = Map.ofArray [|1,1.; 2,4.; 3,9.; 4,16.|]
        equal 4 xs.Count

    testCase "Map.ofSeq works" <| fun () ->
        let xs = Map.ofSeq [1,1.; 2,4.; 3,9.; 4,16.]
        equal 4 xs.Count

    testCase "Map.toList works" <| fun () ->
        let xs = [1,1.; 2,4.; 3,9.; 4,16.]
        let ys = Map.ofList xs
        let zs = Map.toList ys
        snd xs.[2] = snd zs.[2]
        |> equal true

    testCase "Map.toArray works" <| fun () ->
        let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
        let ys = Map.ofArray xs
        let zs = Map.toArray ys
        snd xs.[2] = snd zs.[2]
        |> equal true

    testCase "Map.toSeq works" <| fun () ->
        let xs = seq [1,1.; 2,4.; 3,9.; 4,16.]
        let ys = Map.ofSeq xs
        let zs = Map.toSeq ys
        (Seq.item 2 xs |> snd) = (Seq.item 2 zs |> snd)
        |> equal true

    testCase "Map.toSeq generates sequences that can be iterated multiple times" <| fun () -> // See #2242
        let pr (sequence: seq<int * string>) =
            sequence
            |> Seq.map (fun (i, v) -> v)
            |> String.concat ", "

        let someSeq = Map.ofList [(1, "a"); (2, "b")] |> Map.toSeq
        pr someSeq |> equal "a, b"
        pr someSeq |> equal "a, b"

    testCase "Map.keys works" <| fun () ->
        let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
        let ys = Map.ofArray xs
        let zs = Map.keys ys
        zs.Count |> equal xs.Length
        Seq.item 2 zs |> equal (fst xs.[2])

    testCase "Map.values works" <| fun () ->
        let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
        let ys = Map.ofArray xs
        let zs = Map.values ys
        zs.Count |> equal xs.Length
        Seq.item 2 zs |> equal (snd xs.[2])

    testCase "Map.minKeyValue works" <| fun () ->
        let xs = [(5, "e"); (2, "b"); (3, "c"); (4, "d")]
        let ys = [("e", 5); ("b", 2); ("c", 3); ("d", 4)]
        xs |> Map.ofList |> Map.minKeyValue |> equal (2, "b")
        ys |> Map.ofList |> Map.minKeyValue |> equal ("b", 2)

    testCase "Map.maxKeyValue works" <| fun () ->
        let xs = [(2, "b"); (5, "e"); (3, "c"); (4, "d")]
        let ys = [("b", 2); ("e", 5); ("c", 3); ("d", 4)]
        xs |> Map.ofList|> Map.maxKeyValue |> equal (5, "e")
        ys |> Map.ofList|> Map.maxKeyValue |> equal ("e", 5)

    // TODO
//    testCase "Map can be casted to IDictionary" <| fun () -> // See #1729, #1857
//        let map = Map [ "a", 1; "b", 2; "c", 3]
//        let dic = map :> IDictionary<_,_>
//        dic.TryGetValue("c") |> equal (true,3)
//        dic.TryGetValue("d") |> fst |> equal false
//        dic.Keys |> Seq.toList |> equal ["a"; "b"; "c"]
//        dic.Values |> Seq.toList |> equal [1; 2; 3]

//    testCase "KeyValuePair can be referenced" <| fun () ->
//        let r2 = { kv = new KeyValuePair<_,_>("bar",25) }
//        r2.kv.Key |> equal "bar"
//        r2.kv.Value |> equal 25

    testCase "Map.change works" <| fun () ->
        let m = Map["a",1; "b",2]
        let m2 = Map["a",1; "b",3]
        m |> Map.change "c" (Option.map (fun v -> v + 1)) |> equal m
        m |> Map.change "b" (Option.map (fun v -> v + 1)) |> equal m2

    testCase "Map works with keys with custom comparison" <| fun () ->
        Map.empty
        |> Map.add { Bar = "a"; Baz = 5 } 1
        |> Map.add { Bar = "a"; Baz = 10 } 2
        |> Map.count
        |> equal 2

        Map.empty
        |> Map.add { Bar = "a"; Foo = 5 } 1
        |> Map.add { Bar = "a"; Foo = 10 } 2
        |> Map.count
        |> equal 1
