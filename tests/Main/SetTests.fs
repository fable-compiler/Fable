module Fable.Tests.Sets
open Util.Testing

let tests =
    testList "Lists" [
        testCase "set function works" <| fun () ->
            let xs = set [1]
            xs |> Set.isEmpty
            |> equal false

        testCase "Set.isEmpty works" <| fun () ->
            let xs = set []
            Set.isEmpty xs |> equal true
            let ys = set [1]
            Set.isEmpty ys |> equal false

        // testCase "Set.IsEmpty works" <| fun () ->
        //     let xs = Set.empty<int>
        //     xs.IsEmpty |> equal true
        //     let ys = set [1; 1]
        //     ys.IsEmpty |> equal false

        testCase "Set.Count works" <| fun () ->
            let xs = Set.empty |> Set.add 1
            xs.Count
            |> equal 1

        // TODO: Fix this conversation test, it's running on .Net but not Fable
        // testCase "seq.isEmpty function works on Set" <| fun () ->
        //             let xs = set [1]
        //             xs |> Seq.isEmpty
        //             |> equal false

        // testCase "Set.add works" <| fun () ->
        //     let xs = Set.empty |> Set.add 1
        //     Set.count xs
        //     |> equal 1


        // testCase "Set.Add works" <| fun () ->
        //     let xs = Set.empty.Add 1
        //     Set.count xs
        //     |> equal 1

        // testCase "Set.contains works" <| fun () ->
        //     let xs = Set.empty |> Set.add 1
        //     xs |> Set.contains 1 |> equal true
        //     xs |> Set.contains 2 |> equal false


        // testCase "Set.Contains works" <| fun () ->
        //     let xs = Set.empty |> Set.add 1
        //     xs.Contains 1 |> equal true
        //     xs.Contains 2 |> equal false


        // testCase "Set.remove works" <| fun () ->
        //     let xs = Set.empty |> Set.add 1 |> Set.remove 1
        //     xs.IsEmpty |> equal true


        // testCase "Set.Remove works" <| fun () ->
        //     let xs = (Set.empty |> Set.add 1).Remove 1
        //     xs.IsEmpty |> equal true


        // testCase "Set.singleton works" <| fun () ->
        //     let xs = Set.singleton 1
        //     xs.Count |> equal 1


        // testCase "Set.union works" <| fun () ->
        //     let xs = Set.singleton 1
        //     let ys = Set.singleton 2
        //     let zs = xs |> Set.union ys
        //     (zs.Contains 1 && zs.Contains 2)
        //     |> equal true

        // testCase "Set.union works II" <| fun () ->
        //     Set.union (set [1; 2]) (set [3; 4; 5])
        //     |> equal (set [1; 2; 3; 4; 5])


        // testCase "Set (+) works" <| fun () ->
        //     let xs = Set.singleton 1
        //     let ys = Set.singleton 2
        //     let zs = xs + ys
        //     (zs.Contains 1 && zs.Contains 2)
        //     |> equal true

        // testCase "Set.unionMany works" <| fun () ->
        //     let xs = Set.singleton 1
        //     let ys = Set.singleton 2
        //     let zs = Set.singleton 3
        //     let ks = Set.unionMany [xs; ys; zs]
        //     (ks.Contains 1 && ks.Contains 2 && ks.Contains 3)
        //     |> equal true


        // testCase "Set.intersect works" <| fun () ->
        //     let xs = set [1; 2; 3; 4]
        //     let ys = set [0; 2; 4; 5]
        //     let zs = xs |> Set.intersect ys
        //     (zs.Contains 2 && zs.Contains 4 && not(zs.Contains 1) && not(zs.Contains 5))
        //     |> equal true

        // testCase "Set.intersectMany works" <| fun () ->
        //     let xs = set [1; 2]
        //     let ys = Set.singleton 2
        //     let zs = set [2; 3]
        //     let ks = Set.intersectMany [xs; ys; zs]
        //     (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
        //     |> equal true

        // testCase "Set.iterate works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let total = ref 0.
        //     xs |> Set.iter (fun x -> total := !total + x)
        //     !total |> equal 10.

        // testCase "Set.forAll works" <| fun () ->
        //     let xs = set [1; 2; 3; 4]
        //     xs |> Set.forall (fun x -> x < 5)
        //     |> equal true

        // testCase "Set.exists works" <| fun () ->
        //     let xs = set [1; 2; 3; 4]
        //     xs |> Set.exists ((=) 2) |> equal true
        //     xs |> Set.exists ((<) 5) |> equal false

        // testCase "Set.filter works" <| fun () ->
        //     let xs = set [1; 2; 3; 4]
        //     xs |> Set.filter (fun x -> x % 2 = 0)
        //     |> Set.count |> equal 2

        // testCase "Set.partition works" <| fun () ->
        //     let xs = set [1; 2; 3; 4]
        //     let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
        //     (ys.Count + zs.Count)
        //     |> equal 4

        // testCase "Set.fold works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     xs |> Set.fold (-) 0.
        //     |> equal -10.

        // testCase "Set.foldBack works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     Set.foldBack (-) xs 0.
        //     |> equal -2.

        // testCase "Set.map works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = xs |> Set.map ((*) 2.)
        //     ys.Contains 1.
        //     |> equal false

        // testCase "Set.minElement works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     xs |> Set.minElement
        //     |> equal 1.

        // testCase "Set.MinimumElement works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     xs.MinimumElement
        //     |> equal 1.

        // testCase "Set.maxElement works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     xs |> Set.maxElement
        //     |> equal 4.

        // testCase "Set.MaximumElement works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     xs.MaximumElement
        //     |> equal 4.

        // testCase "Set.difference works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     let zs = Set.difference xs ys
        //     zs.Count |> equal 2

        // testCase "Set (-) works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     let zs = xs - ys
        //     zs.Count |> equal 2

        // testCase "Set.isSubset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     Set.isSubset ys xs
        //     |> equal true

        // testCase "Set.IsSubset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     ys.IsSubsetOf xs
        //     |> equal true

        // testCase "Set.isSuperset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     Set.isSuperset xs ys
        //     |> equal true

        // testCase "Set.IsSuperset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     xs.IsSupersetOf ys
        //     |> equal true

        // testCase "Set.isProperSubset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     Set.isProperSubset ys xs
        //     |> equal true

        // testCase "Set.IsProperSubset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     ys.IsProperSubsetOf xs
        //     |> equal true

        // testCase "Set.isProperSuperset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     Set.isProperSuperset xs ys
        //     |> equal true

        // testCase "Set.IsProperSuperset works" <| fun () ->
        //     let xs = set [1.; 2.; 3.; 4.]
        //     let ys = set [1.; 2.]
        //     xs.IsProperSupersetOf ys
        //     |> equal true

        // testCase "Set.ofList works" <| fun () ->
        //     let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
        //     xs.Count |> equal 4

        // testCase "Set.ofArray works" <| fun () ->
        //     let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
        //     xs.Count |> equal 4

        // testCase "Set.ofSeq works" <| fun () ->
        //     let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
        //     xs.Count |> equal 4

        // testCase "Set.toList works" <| fun () ->
        //     let xs = [1.; 2.; 3.; 4.]
        //     let ys = Set.ofList xs
        //     let zs = Set.toList ys
        //     xs.[2] = zs.[2]
        //     |> equal true

        // testCase "Set.toArray works" <| fun () ->
        //     let xs = [|1.; 2.; 3.; 4.|]
        //     let ys = Set.ofArray xs
        //     let zs = Set.toArray ys
        //     xs.[2] = zs.[2]
        //     |> equal true

        // testCase "Set.toSeq works" <| fun () ->
        //     let xs = seq [1.; 2.; 3.; 4.]
        //     let ys = Set.ofSeq xs
        //     let zs = Set.toSeq ys
        //     (Seq.item 2 xs) = (Seq.item 2 zs)
        //     |> equal true

        // type R = { i: int; s: string }

        // testCase "Sets can be JSON serialized forth and back" <| fun () ->
        //     let x = [{ i=1; s="1" }; { i=2; s="2" } ] |> set
        //     #if FABLE_COMPILER
        //     let json = Fable.Core.JsInterop.toJson x
        //     let x2 = Fable.Core.JsInterop.ofJson<Set<R>> json
        //     x2.IsSubsetOf x |> equal true
        //     (0, x2) ||> Set.fold (fun acc v -> acc + v.i) |> equal 3
        //     let x2 = Fable.Core.JsInterop.ofJsonAsType json (x.GetType()) :?> Set<R>
        //     x2.IsSubsetOf x |> equal true
        //     (0, x2) ||> Set.fold (fun acc v -> acc + v.i) |> equal 3
        //     let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
        //     let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Set<R>> json
        //     #else
        //     let json = Newtonsoft.Json.JsonConvert.SerializeObject x
        //     let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Set<R>> json
        //     #endif
        //     x2.IsSubsetOf x |> equal true
        //     (0, x2) ||> Set.fold (fun acc v -> acc + v.i)
        //     |> equal 3

        // testCase "Sets serialized with Json.NET can be deserialized" <| fun () ->
        //     // let x = ["a", { i=1; s="1" }; "b", { i=2; s="2" } ] |> Map
        //     // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
        //     let json = """{"$type":"Microsoft.FSharp.Collections.FSharpSet`1[[Fable.Tests.Sets+R, Fable.Tests]], FSharp.Core","$values":[{"$type":"Fable.Tests.Sets+R, Fable.Tests","i":1,"s":"1"},{"$type":"Fable.Tests.Sets+R, Fable.Tests","i":2,"s":"2"}]}"""
        //     #if FABLE_COMPILER
        //     let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Set<R>> json
        //     #else
        //     let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Set<R>> json
        //     #endif
        //     (0, x2) ||> Set.fold (fun acc v -> acc + v.i)
        //     |> equal 3
    ]
