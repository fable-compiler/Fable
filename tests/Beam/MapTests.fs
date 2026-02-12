module Fable.Tests.Map

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Map ofList works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    Map.count m |> equal 3

[<Fact>]
let ``test Map add works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    let m2 = Map.add 3 "c" m
    Map.count m2 |> equal 3

[<Fact>]
let ``test Map find works`` () =
    let m = Map.ofList [ (1, "hello"); (2, "world") ]
    Map.find 1 m |> equal "hello"
    Map.find 2 m |> equal "world"

[<Fact>]
let ``test Map tryFind with existing key works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    Map.tryFind 1 m |> equal (Some "a")

[<Fact>]
let ``test Map tryFind with missing key works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    Map.tryFind 99 m |> equal None

[<Fact>]
let ``test Map containsKey works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    Map.containsKey 1 m |> equal true
    Map.containsKey 99 m |> equal false

[<Fact>]
let ``test Map remove works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    let m2 = Map.remove 2 m
    Map.count m2 |> equal 2
    Map.containsKey 2 m2 |> equal false

[<Fact>]
let ``test Map isEmpty works`` () =
    Map.isEmpty (Map.ofList []) |> equal true
    Map.isEmpty (Map.ofList [ (1, "a") ]) |> equal false

[<Fact>]
let ``test Map count works`` () =
    Map.count (Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]) |> equal 3
    Map.count (Map.ofList []) |> equal 0

[<Fact>]
let ``test Map toList works`` () =
    let m = Map.ofList [ (2, "b"); (1, "a") ]
    let lst = Map.toList m
    List.length lst |> equal 2

[<Fact>]
let ``test Map map works`` () =
    let m = Map.ofList [ (1, 10); (2, 20) ]
    let m2 = Map.map (fun _k v -> v * 2) m
    Map.find 1 m2 |> equal 20
    Map.find 2 m2 |> equal 40

[<Fact>]
let ``test Map filter works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30) ]
    let m2 = Map.filter (fun _k v -> v > 15) m
    Map.count m2 |> equal 2
    Map.containsKey 1 m2 |> equal false

[<Fact>]
let ``test Map fold works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30) ]
    let total = Map.fold (fun acc _k v -> acc + v) 0 m
    total |> equal 60

[<Fact>]
let ``test Map exists works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30) ]
    Map.exists (fun _k v -> v > 25) m |> equal true
    Map.exists (fun _k v -> v > 100) m |> equal false

[<Fact>]
let ``test Map forall works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30) ]
    Map.forall (fun _k v -> v > 0) m |> equal true
    Map.forall (fun _k v -> v > 15) m |> equal false

[<Fact>]
let ``test Map empty works`` () =
    let m = Map.empty<int, string>
    Map.isEmpty m |> equal true
    Map.count m |> equal 0

[<Fact>]
let ``test Map instance Item works`` () =
    let m = Map.ofList [ (1, "hello"); (2, "world") ]
    m.[1] |> equal "hello"
    m.[2] |> equal "world"

[<Fact>]
let ``test Map instance ContainsKey works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    m.ContainsKey(1) |> equal true
    m.ContainsKey(99) |> equal false

[<Fact>]
let ``test Map instance Add works`` () =
    let m = Map.ofList [ (1, "a") ]
    let m2 = m.Add(2, "b")
    Map.count m2 |> equal 2
    m2.[2] |> equal "b"

[<Fact>]
let ``test Map instance Remove works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    let m2 = m.Remove(1)
    Map.count m2 |> equal 1
    m2.ContainsKey(1) |> equal false

[<Fact>]
let ``test Map instance Count works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    m.Count |> equal 3

// --- New tests ---

[<Fact>]
let ``test Map iter works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30) ]
    let mutable total = 0
    Map.iter (fun _k v -> total <- total + v) m
    total |> equal 60

[<Fact>]
let ``test Map partition works`` () =
    let m = Map.ofList [ (1, 10); (2, 20); (3, 30); (4, 40) ]
    let yes, no = Map.partition (fun _k v -> v > 20) m
    Map.count yes |> equal 2
    Map.count no |> equal 2
    Map.containsKey 3 yes |> equal true
    Map.containsKey 1 no |> equal true

[<Fact>]
let ``test Map foldBack works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    let result = Map.foldBack (fun _k v acc -> acc + v) m ""
    // foldBack processes all entries, order may vary for maps
    String.length result |> equal 3

[<Fact>]
let ``test Map findKey works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    Map.findKey (fun _k v -> v = "b") m |> equal 2

[<Fact>]
let ``test Map tryFindKey works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    Map.tryFindKey (fun _k v -> v = "b") m |> equal (Some 2)
    Map.tryFindKey (fun _k v -> v = "z") m |> equal None

[<Fact>]
let ``test Map pick works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    Map.pick (fun k v -> if v = "b" then Some (k * 10) else None) m |> equal 20

[<Fact>]
let ``test Map tryPick works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    Map.tryPick (fun k v -> if v = "b" then Some (k * 10) else None) m |> equal (Some 20)
    Map.tryPick (fun _k v -> if v = "z" then Some 0 else None) m |> equal None

[<Fact>]
let ``test Map ofArray works`` () =
    let m = Map.ofArray [| (1, "a"); (2, "b") |]
    Map.count m |> equal 2
    Map.find 1 m |> equal "a"

[<Fact>]
let ``test Map ofSeq works`` () =
    let m = Map.ofSeq (seq { yield (1, "a"); yield (2, "b") })
    Map.count m |> equal 2
    Map.find 2 m |> equal "b"

[<Fact>]
let ``test Map toArray works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    let arr = Map.toArray m
    Array.length arr |> equal 2

[<Fact>]
let ``test Map toSeq works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    let s = Map.toSeq m
    Seq.length s |> equal 3

[<Fact>]
let ``test Map keys works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    let keys = Map.keys m |> Seq.toList |> List.sort
    keys |> equal [ 1; 2; 3 ]

[<Fact>]
let ``test Map values works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b"); (3, "c") ]
    let values = Map.values m |> Seq.toList |> List.sort
    values |> equal [ "a"; "b"; "c" ]

[<Fact>]
let ``test Map minKeyValue works`` () =
    let m = Map.ofList [ (3, "c"); (1, "a"); (2, "b") ]
    let k, v = Map.minKeyValue m
    k |> equal 1
    v |> equal "a"

[<Fact>]
let ``test Map maxKeyValue works`` () =
    let m = Map.ofList [ (3, "c"); (1, "a"); (2, "b") ]
    let k, v = Map.maxKeyValue m
    k |> equal 3
    v |> equal "c"

[<Fact>]
let ``test Map change works`` () =
    let m = Map.ofList [ (1, 10); (2, 20) ]
    // Update existing key
    let m2 = Map.change 1 (fun v -> v |> Option.map (fun x -> x + 5)) m
    Map.find 1 m2 |> equal 15
    // Add new key
    let m3 = Map.change 3 (fun _ -> Some 30) m
    Map.find 3 m3 |> equal 30
    Map.count m3 |> equal 3
    // Remove existing key
    let m4 = Map.change 2 (fun _ -> None) m
    Map.count m4 |> equal 1
    Map.containsKey 2 m4 |> equal false

[<Fact>]
let ``test Map instance IsEmpty works`` () =
    let m1 = Map.ofList<int, string> []
    let m2 = Map.ofList [ (1, "a") ]
    m1.IsEmpty |> equal true
    m2.IsEmpty |> equal false

[<Fact>]
let ``test Map instance TryFind works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    m.TryFind(1) |> equal (Some "a")
    m.TryFind(99) |> equal None

[<Fact>]
let ``test Map instance TryGetValue works`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    let found, value = m.TryGetValue(1)
    found |> equal true
    value |> equal "a"
    let notFound, _ = m.TryGetValue(99)
    notFound |> equal false

[<Fact>]
let ``test Map find with option type values works`` () =
    let m = Map.ofList [ (1, Some "a"); (2, None); (3, Some "c") ]
    Map.find 1 m |> equal (Some "a")
    Map.find 2 m |> equal None
    Map.find 3 m |> equal (Some "c")

// TODO: Nested options lossy in Beam (Some None = undefined = None)
// [<Fact>]
// let ``test Map tryFind with option type values works`` () =
//     let m = Map.ofList [ (1, Some "a"); (2, None) ]
//     Map.tryFind 1 m |> equal (Some(Some "a"))
//     Map.tryFind 99 m |> equal None

[<Fact>]
let ``test Map toSeq generates sequences that can be iterated multiple times`` () =
    let m = Map.ofList [ (1, "a"); (2, "b") ]
    let s = Map.toSeq m
    Seq.length s |> equal 2
    Seq.length s |> equal 2
