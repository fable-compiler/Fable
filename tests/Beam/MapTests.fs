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
