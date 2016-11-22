[<Util.Testing.TestFixture>]
module Fable.Tests.HashSets
open Util.Testing
open Fable.Tests.Util
open System.Collections.Generic

[<Test>]
let ``HashSet ctor creates empty HashSet``() =
    let xs = HashSet<int>()
    xs |> Seq.isEmpty
    |> equal true

let set l =
    let xs = HashSet<_>()
    for x in l do
        xs.Add x |> ignore
    xs

[<Test>]
let ``HashSet.Add returns true if not present``() =
    let xs = set []
    xs.Add(1) |> equal true
    xs.Count |> equal 1

[<Test>]
let ``HashSet.Add returns false if already present``() =
    let xs = set [1]
    xs.Add(1) |> equal false
    xs.Count |> equal 1

[<Test>]
let ``HashSet.Remove works when item is present``() =
    let xs = set [1]
    xs.Remove 1 |> equal true
    xs.Count |> equal 0

[<Test>]
let ``HashSet.Remove works when item is not present``() =
    let xs = set [1; 2]
    xs.Remove 3 |> equal false
    xs.Count |> equal 2

[<Test>]
let ``HashSet.UnionWith works``() =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.UnionWith ys
    (xs.Contains 1 && xs.Contains 2 && xs.Contains 4)
    |> equal true

[<Test>]
let ``HashSet.IntersectWith works``() =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.IntersectWith ys
    xs.Contains 1 |> equal false
    xs.Contains 2 |> equal true

[<Test>]
let ``HashSet.ExceptWith works``() =
    let xs = set [1; 2]
    let ys = set [2; 4]
    xs.ExceptWith ys
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false

[<Test>]
let ``HashSet creation works``() =
    let hs = HashSet<_>()
    equal 0 hs.Count

[<Test>]
let ``HashSet iteration works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore

    let i = ref 0.
    for v in hs do
       i := v + !i
    equal 385. !i

[<Test>]
let ``HashSet folding works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore
    hs |> Seq.fold (fun acc item -> acc + item) 0.
    |> equal 385.

[<Test>]
let ``HashSet.Count works``() =
    let hs = HashSet<_>()
    for i in 1. .. 10. do hs.Add(i*i) |> ignore
    hs.Count
    |> equal 10
    let xs = set []
    xs.Count |> equal 0
    let ys = set [1]
    ys.Count |> equal 1
    let zs = set [1; 1]
    zs.Count |> equal 1
    let zs' = set [1; 2]
    zs'.Count |> equal 2

[<Test>]
let ``HashSet.Add works``() =
    let hs = HashSet<_>()
    hs.Add("A", "Hello") |> equal true
    hs.Add("B", "World!") |> equal true
    hs.Count |> equal 2

[<Test>]
let ``HashSet.Clear works``() =
    let hs = HashSet<_>()
    hs.Add("A", 1) |> equal true
    hs.Add("B", 2) |> equal true
    hs.Clear()
    hs.Count |> equal 0

[<Test>]
let ``HashSet.Contains works``() =
    let hs = HashSet<_>()
    hs.Add("Hello") |> equal true
    hs.Add("World!") |> equal true
    hs.Contains("Hello") |> equal true
    hs.Contains("Everybody!") |> equal false

[<Test>]
let ``HashSet.CopyTo works``() =
    let hs = HashSet<_>()
    for i in 1 .. 9 do hs.Add(i) |> ignore

    let arr1 = Array.zeroCreate 9
    let arr2 = Array.zeroCreate 11
    let arr3 = Array.zeroCreate 7

    hs.CopyTo(arr1)         // [|1;2;3;4;5;6;7;8;9|]
    hs.CopyTo(arr2, 2)      // [|0;0;1;2;3;4;5;6;7;8;9|]
    hs.CopyTo(arr3, 3, 4)   // [|0;0;0;1;2;3;4|]

    let sum = fun acc item -> acc + item
    arr1 |> Seq.fold sum 0 |> equal 45
    arr1.Length |> equal 9

    arr2 |> Seq.fold sum 0 |> equal 45
    arr2.Length |> equal 11

    arr3 |> Seq.fold sum 0 |> equal 10
    arr3.Length |> equal 7

[<Test>]
let ``HashSet.Remove works``() =
    let hs = HashSet<_>()
    hs.Add("A") |> ignore
    hs.Add("B") |> ignore
    hs.Remove("A") |> equal true
    hs.Remove("C") |> equal false

type R = { i: int; s: string }

[<Test>]
let ``HashSet can be JSON serialized forth and back``() =
    let x = HashSet<_>()
    x.Add(1) |> ignore
    x.Add(2) |> ignore
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<HashSet<int>> json
    x2.IsSubsetOf x |> equal true
    (0, x2) ||> Seq.fold (fun acc v -> acc + v) |> equal 3
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<HashSet<int>> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<HashSet<int>> json
    #endif
    x2.IsSubsetOf x |> equal true
    (0, x2) ||> Seq.fold (fun acc v -> acc + v) |> equal 3

[<Test>]
let ``HashSet serialized with Json.NET can be deserialized``() =
    // let x = HashSet<_>()
    // x.Add({ i=1; s="1" }) |> ignore
    // x.Add({ i=2; s="2" }) |> ignore
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"System.Collections.Generic.HashSet`1[[Fable.Tests.HashSets+R, Fable.Tests]], FSharp.Core","$values":[{"$type":"Fable.Tests.HashSets+R, Fable.Tests","i":1,"s":"1"},{"$type":"Fable.Tests.HashSets+R, Fable.Tests","i":2,"s":"2"}]}"""
    #if FABLE_COMPILER
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<HashSet<R>> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<HashSet<R>> json
    #endif
    (0, x2) ||> Seq.fold (fun acc v -> acc + v.i) |> equal 3
