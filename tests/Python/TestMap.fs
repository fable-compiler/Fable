module Fable.Tests.Maps

open System.Collections.Generic
open Util.Testing

type R = { i: int; s: string }
type R2 = { kv: KeyValuePair<string,int> }

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

[<Fact>]
let ``test Map construction from lists works`` () =
    let xs = Map [1,1; 2,2]
    xs |> Seq.isEmpty
    |> equal false

[<Fact>]
let ``test Map.isEmpty works`` () =
    let xs = Map []
    Map.isEmpty xs |> equal true
    let ys = Map [1,1]
    Map.isEmpty ys |> equal false

[<Fact>]
let ``test Map.IsEmpty works II`` () =
    let xs = Map.empty<int, int>
    xs.IsEmpty |> equal true
    let ys = Map [1,1; 2,2]
    ys.IsEmpty |> equal false

[<Fact>]
let ``test xs.Count works`` () =
    let xs = Map.empty<int, int>
    xs.Count
    |> equal 0

[<Fact>]
let ``test Map.count works`` () =
    let m1 = Map.empty
    let c1 = Map.count m1
    equal 0 c1

[<Fact>]
let ``test Map.add works`` () =
    let xs = Map.empty |> Map.add 1 1
    xs.Count
    |> equal 1

[<Fact>]
let ``test xs.Add works`` () =
    let xs = Map.empty.Add(1, 1)
    xs.Count
    |> equal 1

[<Fact>]
let ``test Map.TryGetValue works`` () =
    let xs = Map.empty.Add(1, 1)
    xs.TryGetValue(1)
    |> equal (true, 1)

[<Fact>]
let ``test Map.containsKey works`` () =
    let xs = Map.empty |> Map.add 1 1
    xs |> Map.containsKey 1 |> equal true
    xs |> Map.containsKey 2 |> equal false

[<Fact>]
let ``test xs.ContainsKey works`` () =
    let xs = Map.empty |> Map.add 1 1
    xs.ContainsKey 1 |> equal true
    xs.ContainsKey 2 |> equal false

[<Fact>]
let ``test Map.remove works`` () =
    let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
    xs.IsEmpty |> equal true

[<Fact>]
let ``test xs.Remove works`` () =
    let xs = (Map.empty |> Map.add 1 1).Remove 1
    xs.IsEmpty |> equal true

[<Fact>]
let ``test Map.iter works`` () =
    let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
    let total = ref 0.
    xs |> Map.iter (fun x y -> total.Value <- total.Value + x + y)
    total.Value |> equal 40.

[<Fact>]
let ``test Map.forAll works`` () =
    let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
    xs |> Map.forall (fun x y -> x < 5.)
    |> equal true

[<Fact>]
let ``test Map.exists works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.exists (fun k v -> k = 2)
    |> equal true

[<Fact>]
let ``test Map.filter works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = xs |> Map.filter (fun x y -> x % 2 = 0)
    ys.Count |> equal 2
    ys.[4] |> equal 16.

[<Fact>]
let ``test Map.partition works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys, zs = xs |> Map.partition (fun x y -> x % 2 = 0)
    ys.Count + zs.Count
    |> equal 4

[<Fact>]
let ``test Map.fold works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.fold (fun acc k v -> v - acc) 0.
    |> equal 10.

[<Fact>]
let ``test Map.foldBack works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    Map.foldBack (fun k v acc -> v - acc) xs 0.
    |> equal -10.

[<Fact>]
let ``test Map.map works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = xs |> Map.map (fun k v -> v * 2.)
    ys.[3] |> equal 18.

[<Fact>]
let ``test Map.find works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.find 2
    |> equal 4.

[<Fact>]
let ``test Map.find with option works`` () =
    let xs = Map [1,Some 1.; 2,None]
    xs |> Map.find 2
    |> Option.isNone
    |> equal true
    xs |> Map.find 1 |> (fun x -> x.Value)
    |> equal 1.

[<Fact>]
let ``test Map.tryFind with option works`` () =
    let xs = Map [1,Some 1.; 2,None]
    xs |> Map.tryFind 2 |> (fun x -> x.Value)
    |> Option.isNone
    |> equal true
    xs |> Map.tryFind 1 |> (fun x -> x.Value.Value)
    |> equal 1.
    xs |> Map.tryFind 3
    |> equal None

[<Fact>]
let ``test Map.tryFind works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.tryFind 0
    |> Option.isNone
    |> equal true

[<Fact>]
let ``test xs.TryFind works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs.TryFind 3
    |> Option.isSome
    |> equal true

[<Fact>]
let ``test Map.findKey works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.findKey (fun _ v -> v = 9.)
    |> equal 3

[<Fact>]
let ``test Map.tryFindKey works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.tryFindKey (fun _ v -> v > 10.) |> equal (Some 4)
    xs |> Map.tryFindKey (fun _ v -> v > 20.) |> equal None

[<Fact>]
let ``test Map.pick works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let y = xs |> Map.pick (fun k v ->
       match k with
       | 3 -> Some 10
       | _ -> None)
    equal 10 y

[<Fact>]
let ``test Map.tryPick works`` () =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let y = xs |> Map.tryPick (fun k v ->
       match k with
       | 3 -> Some 11
       | _ -> None)
    equal 11 y.Value

[<Fact>]
let ``test Map.ofList works`` () =
    let xs = Map.ofList [1,1.; 2,4.; 3,9.; 4,16.]
    equal 4 xs.Count

[<Fact>]
let ``test Map.ofArray works`` () =
    let xs = Map.ofArray [|1,1.; 2,4.; 3,9.; 4,16.|]
    equal 4 xs.Count

[<Fact>]
let ``test Map.ofSeq works`` () =
    let xs = Map.ofSeq [1,1.; 2,4.; 3,9.; 4,16.]
    equal 4 xs.Count

[<Fact>]
let ``test Map.toList works`` () =
    let xs = [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = Map.ofList xs
    let zs = Map.toList ys
    snd xs.[2] = snd zs.[2]
    |> equal true

[<Fact>]
let ``test Map.toArray works`` () =
    let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
    let ys = Map.ofArray xs
    let zs = Map.toArray ys
    snd xs.[2] = snd zs.[2]
    |> equal true

[<Fact>]
let ``test Map.toSeq works`` () =
    let xs = seq [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = Map.ofSeq xs
    let zs = Map.toSeq ys
    (Seq.item 2 xs |> snd) = (Seq.item 2 zs |> snd)
    |> equal true

[<Fact>]
let ``test Map.toSeq generates sequences that can be iterated multiple times`` () =
    let pr (sequence: seq<int * string>) =
        sequence
        |> Seq.map (fun (i, v) -> v)
        |> String.concat ", "

    let someSeq = Map.ofList [(1, "a"); (2, "b")] |> Map.toSeq
    pr someSeq |> equal "a, b"
    pr someSeq |> equal "a, b"

[<Fact>]
let ``test Map.keys works`` () =
    let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
    let ys = Map.ofArray xs
    let zs = Map.keys ys
    zs.Count |> equal xs.Length
    Seq.item 2 zs |> equal (fst xs.[2])

[<Fact>]
let ``test Map.values works`` () =
    let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
    let ys = Map.ofArray xs
    let zs = Map.values ys
    zs.Count |> equal xs.Length
    Seq.item 2 zs |> equal (snd xs.[2])

[<Fact>]
let ``test Map.minKeyValue works`` () =
    let xs = [(5, "e"); (2, "b"); (3, "c"); (4, "d")]
    let ys = [("e", 5); ("b", 2); ("c", 3); ("d", 4)]
    xs |> Map.ofList |> Map.minKeyValue |> equal (2, "b")
    ys |> Map.ofList |> Map.minKeyValue |> equal ("b", 2)

[<Fact>]
let ``test Map.maxKeyValue works`` () =
    let xs = [(2, "b"); (5, "e"); (3, "c"); (4, "d")]
    let ys = [("b", 2); ("e", 5); ("c", 3); ("d", 4)]
    xs |> Map.ofList|> Map.maxKeyValue |> equal (5, "e")
    ys |> Map.ofList|> Map.maxKeyValue |> equal ("e", 5)

[<Fact>]
let ``test Map can be casted to IDictionary`` () =
     let map = Map [ "a", 1; "b", 2; "c", 3]
     let dic = map :> System.Collections.Generic.IDictionary<_,_>
     dic.TryGetValue("c") |> equal (true,3)
     dic.TryGetValue("d") |> fst |> equal false
     dic.Keys |> Seq.toList |> equal ["a"; "b"; "c"]
     dic.Values |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test KeyValuePair can be referenced`` () =
    let r2 = { kv = new KeyValuePair<_,_>("bar",25) }
    r2.kv.Key |> equal "bar"
    r2.kv.Value |> equal 25

[<Fact>]
let ``test Map.change works`` () =
    let m = Map["a",1; "b",2]
    let m2 = Map["a",1; "b",3]
    m |> Map.change "c" (Option.map (fun v -> v + 1)) |> equal m
    m |> Map.change "b" (Option.map (fun v -> v + 1)) |> equal m2

[<Fact>]
let ``test Map works with keys with custom comparison`` () =
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
