module Fable.Tests.Maps

open Util.Testing

let tests =
    testList "Maps" [
        testCase "Map construction from lists works" <| fun () ->
            let xs = Map [1,1; 2,2]
            xs |> Seq.isEmpty
            |> equal false

        testCase "Map.isEmpty works" <| fun () ->
            let xs = Map []
            Map.isEmpty xs |> equal true
            let ys = Map [1,1]
            Map.isEmpty ys |> equal false

        testCase "Map.IsEmpty works" <| fun () ->
            let xs = Map.empty<int, int>
            xs.IsEmpty |> equal true
            let ys = Map [1,1; 2,2]
            ys.IsEmpty |> equal false

        testCase "Map.Count works" <| fun () ->
            let xs = Map.empty<int, int>
            xs.Count
            |> equal 0

        testCase "Map.count works" <| fun () ->
            let m1 = Map.empty
            let c1 = Map.count m1
            equal 0 c1

        testCase "Map.add works" <| fun () ->
            let xs = Map.empty |> Map.add 1 1
            xs.Count
            |> equal 1

        testCase "Map.Add works" <| fun () ->
            let xs = Map.empty.Add(1, 1)
            xs.Count
            |> equal 1

        testCase "Map.TryGetValue works" <| fun () ->
            let xs = Map.empty.Add(1, 1)
            xs.TryGetValue(1)
            |> equal (true, 1)

        testCase "Map.containsKey works" <| fun () ->
            let xs = Map.empty |> Map.add 1 1
            xs |> Map.containsKey 1 |> equal true
            xs |> Map.containsKey 2 |> equal false

        testCase "Map.ContainsKey works" <| fun () ->
            let xs = Map.empty |> Map.add 1 1
            xs.ContainsKey 1 |> equal true
            xs.ContainsKey 2 |> equal false

        testCase "Map.remove works" <| fun () ->
            let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
            xs.IsEmpty |> equal true

        testCase "Map.Remove works" <| fun () ->
            let xs = (Map.empty |> Map.add 1 1).Remove 1
            xs.IsEmpty |> equal true

        testCase "Map.iter works" <| fun () ->
            let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
            let total = ref 0.
            xs |> Map.iter (fun x y -> total := !total + x + y)
            !total |> equal 40.

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

        testCase "Map.find with option works" <| fun () ->
            let xs = Map [1,Some 1.; 2,None]
            xs |> Map.find 2
            |> Option.isNone
            |> equal true
            xs |> Map.find 1 |> (fun x -> x.Value)
            |> equal 1.

        testCase "Map.tryFind with option works" <| fun () ->
            let xs = Map [1,Some 1.; 2,None]
            xs |> Map.tryFind 2 |> (fun x -> x.Value)
            |> Option.isNone
            |> equal true
            xs |> Map.tryFind 1 |> (fun x -> x.Value.Value)
            |> equal 1.
            xs |> Map.tryFind 3
            |> equal None

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
            equal 11 y.Value

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

        // type R = { i: int; s: string }

        // testCase "Maps can be JSON serialized forth and back" <| fun () ->
        //     let x = ["a", { i=1; s="1" }; "b", { i=2; s="2" } ] |> Map
        //     #if FABLE_COMPILER
        //     let json = Fable.Core.JsInterop.toJson x
        //     let x2 = Fable.Core.JsInterop.ofJson<Map<string, R>> json
        //     (0, x2) ||> Map.fold (fun acc k v -> acc + v.i) |> equal 3
        //     let x2 = Fable.Core.JsInterop.ofJsonAsType json (x.GetType()) :?> Map<string, R>
        //     (0, x2) ||> Map.fold (fun acc k v -> acc + v.i) |> equal 3
        //     let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
        //     let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Map<string, R>> json
        //     #else
        //     let json = Newtonsoft.Json.JsonConvert.SerializeObject x
        //     let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, R>> json
        //     #endif
        //     (0, x2) ||> Map.fold (fun acc k v -> acc + v.i)
        //     |> equal 3

        // testCase "Maps serialized with Json.NET can be deserialized" <| fun () ->
        //     // let x = ["a", { i=1; s="1" }; "b", { i=2; s="2" } ] |> Map
        //     // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
        //     let json = """{"$type":"Microsoft.FSharp.Collections.FSharpMap`2[[System.String, mscorlib],[Fable.Tests.Maps+R, Fable.Tests]], FSharp.Core","a":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":1,"s":"1"},"b":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":2,"s":"2"}}"""
        //     #if FABLE_COMPILER
        //     let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Map<string, R>> json
        //     #else
        //     let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, R>> json
        //     #endif
        //     (0, x2) ||> Map.fold (fun acc k v -> acc + v.i)
        //     |> equal 3

        // open System.Collections.Generic

        // type R2 = { kv: KeyValuePair<string,int> }

        // testCase "KeyValuePair can be referenced``() = // Se" <| fun () ->
        //     let r2 = { kv = new KeyValuePair<_,_>("bar",25) }
        //     r2.kv.Key |> equal "bar"
        //     r2.kv.Value |> equal 25
    ]
