[<Util.Testing.TestFixture>]
module Fable.Tests.Maps
open Util.Testing
open Fable.Tests.Util

[<Test>]
let ``Map construction from lists works``() =
    let xs = Map [1,1; 2,2]
    xs |> Seq.isEmpty
    |> equal false

[<Test>]
let ``Map.isEmpty works``() =
    let xs = Map []
    Map.isEmpty xs |> equal true
    let ys = Map [1,1]
    Map.isEmpty ys |> equal false

[<Test>]
let ``Map.IsEmpty works``() =
    let xs = Map.empty<int, int>
    xs.IsEmpty |> equal true
    let ys = Map [1,1; 2,2]
    ys.IsEmpty |> equal false

[<Test>]
let ``Map.Count works``() =
    let xs = Map.empty<int, int>
    xs.Count
    |> equal 0

[<Test>]
let ``Map.add works``() =
    let xs = Map.empty |> Map.add 1 1
    xs.Count
    |> equal 1

[<Test>]
let ``Map.Add works``() =
    let xs = Map.empty.Add(1, 1)
    xs.Count
    |> equal 1

[<Test>]
let ``Map.containsKey works``() =
    let xs = Map.empty |> Map.add 1 1
    xs |> Map.containsKey 1 |> equal true
    xs |> Map.containsKey 2 |> equal false

[<Test>]
let ``Map.ContainsKey works``() =
    let xs = Map.empty |> Map.add 1 1
    xs.ContainsKey 1 |> equal true
    xs.ContainsKey 2 |> equal false

[<Test>]
let ``Map.remove works``() =
    let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
    xs.IsEmpty |> equal true

[<Test>]
let ``Map.Remove works``() =
    let xs = (Map.empty |> Map.add 1 1).Remove 1
    xs.IsEmpty |> equal true

[<Test>]
let ``Map.iter works``() =
    let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
    let total = ref 0.
    xs |> Map.iter (fun x y -> total := !total + x + y)
    !total |> equal 40.

[<Test>]
let ``Map.forAll works``() =
    let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
    xs |> Map.forall (fun x y -> x < 5.)
    |> equal true

[<Test>]
let ``Map.exists works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.exists (fun k v -> k = 2)
    |> equal true

[<Test>]
let ``Map.filter works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = xs |> Map.filter (fun x y -> x % 2 = 0)
    ys.Count |> equal 2
    ys.[4] |> equal 16.

[<Test>]
let ``Map.partition works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys, zs = xs |> Map.partition (fun x y -> x % 2 = 0)
    ys.Count + zs.Count
    |> equal 4

[<Test>]
let ``Map.fold works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.fold (fun acc k v -> v - acc) 0.
    |> equal 10.

[<Test>]
let ``Map.foldBack works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    Map.foldBack (fun k v acc -> v - acc) xs 0.
    |> equal -10.

[<Test>]
let ``Map.map works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = xs |> Map.map (fun k v -> v * 2.)
    ys.[3] |> equal 18.

[<Test>]
let ``Map.find works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.find 2
    |> equal 4.

[<Test>]
let ``Map.tryFind works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.tryFind 0
    |> Option.isNone
    |> equal true

[<Test>]
let ``Map.TryFind works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs.TryFind 3
    |> Option.isSome
    |> equal true

[<Test>]
let ``Map.tryFindKey works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    xs |> Map.tryFindKey (fun k v -> k = 3)
    |> Option.isSome
    |> equal true

[<Test>]
let ``Map.pick works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let y = xs |> Map.pick (fun k v ->
       match k with
       | 3 -> Some 10
       | _ -> None)
    equal 10 y

[<Test>]
let ``Map.tryPick works``() =
    let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
    let y = xs |> Map.tryPick (fun k v ->
       match k with
       | 3 -> Some 11
       | _ -> None)
    equal 11 y.Value

[<Test>]
let ``Map.ofList works``() =
    let xs = Map.ofList [1,1.; 2,4.; 3,9.; 4,16.]
    equal 4 xs.Count

[<Test>]
let ``Map.ofArray works``() =
    let xs = Map.ofArray [|1,1.; 2,4.; 3,9.; 4,16.|]
    equal 4 xs.Count

[<Test>]
let ``Map.ofSeq works``() =
    let xs = Map.ofSeq [1,1.; 2,4.; 3,9.; 4,16.]
    equal 4 xs.Count

[<Test>]
let ``Map.toList works``() =
    let xs = [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = Map.ofList xs
    let zs = Map.toList ys
    snd xs.[2] = snd zs.[2]
    |> equal true

[<Test>]
let ``Map.toArray works``() =
    let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
    let ys = Map.ofArray xs
    let zs = Map.toArray ys
    snd xs.[2] = snd zs.[2]
    |> equal true

[<Test>]
let ``Map.toSeq works``() =
    let xs = seq [1,1.; 2,4.; 3,9.; 4,16.]
    let ys = Map.ofSeq xs
    let zs = Map.toSeq ys
    (Seq.item 2 xs |> snd) = (Seq.item 2 zs |> snd)
    |> equal true

type R = { i: int; s: string }

[<Test>]
let ``Maps can be JSON serialized forth and back``() =
    let x = ["a", { i=1; s="1" }; "b", { i=2; s="2" } ] |> Map
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Map<string, R>> json
    (0, x2) ||> Map.fold (fun acc k v -> acc + v.i) |> equal 3
    let x2 = Fable.Core.JsInterop.ofJsonAsType json (x.GetType()) :?> Map<string, R>
    (0, x2) ||> Map.fold (fun acc k v -> acc + v.i) |> equal 3
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Map<string, R>> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, R>> json
    #endif
    (0, x2) ||> Map.fold (fun acc k v -> acc + v.i)
    |> equal 3

[<Test>]
let ``Maps serialized with Json.NET can be deserialized``() =
    // let x = ["a", { i=1; s="1" }; "b", { i=2; s="2" } ] |> Map
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"Microsoft.FSharp.Collections.FSharpMap`2[[System.String, mscorlib],[Fable.Tests.Maps+R, Fable.Tests]], FSharp.Core","a":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":1,"s":"1"},"b":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":2,"s":"2"}}"""
    #if FABLE_COMPILER
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Map<string, R>> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, R>> json
    #endif
    (0, x2) ||> Map.fold (fun acc k v -> acc + v.i)
    |> equal 3

open System.Collections.Generic

type R2 = { kv: KeyValuePair<string,int> }

[<Test>]
let ``KeyValuePair can be referenced``() = // See #503
    let r2 = { kv = new KeyValuePair<_,_>("bar",25) }
    r2.kv.Key |> equal "bar"
    r2.kv.Value |> equal 25
