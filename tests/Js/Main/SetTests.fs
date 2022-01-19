module Fable.Tests.Sets
open Util.Testing

let tests =
    testList "Sets" [
        testCase "set function works" <| fun () ->
            let xs = set [1]
            xs |> Set.isEmpty
            |> equal false

        testCase "Set.isEmpty works" <| fun () ->
            let xs = set []
            Set.isEmpty xs |> equal true
            let ys = set [1]
            Set.isEmpty ys |> equal false

        testCase "Set.IsEmpty works" <| fun () ->
            let xs = Set.empty<int>
            xs.IsEmpty |> equal true
            let ys = set [1; 1]
            ys.IsEmpty |> equal false

        testCase "Set.Count works" <| fun () ->
            let xs = Set.empty |> Set.add 1
            xs.Count
            |> equal 1

        testCase "Seq.isEmpty function works on Set" <| fun () ->
            let xs = set [1]
            xs |> Seq.isEmpty
            |> equal false

        testCase "Set.add works" <| fun () ->
            let xs = Set.empty |> Set.add 1
            Set.count xs
            |> equal 1


        testCase "Set.Add works" <| fun () ->
            let xs = Set.empty.Add 1
            Set.count xs
            |> equal 1

        testCase "Set.contains works" <| fun () ->
            let xs = Set.empty |> Set.add 1
            xs |> Set.contains 1 |> equal true
            xs |> Set.contains 2 |> equal false


        testCase "Set.Contains works" <| fun () ->
            let xs = Set.empty |> Set.add 1
            xs.Contains 1 |> equal true
            xs.Contains 2 |> equal false


        testCase "Set.remove works" <| fun () ->
            let xs = Set.empty |> Set.add 1 |> Set.remove 1
            xs.IsEmpty |> equal true


        testCase "Set.Remove works" <| fun () ->
            let xs = (Set.empty |> Set.add 1).Remove 1
            xs.IsEmpty |> equal true


        testCase "Set.singleton works" <| fun () ->
            let xs = Set.singleton 1
            xs.Count |> equal 1


        testCase "Set.union works" <| fun () ->
            let xs = Set.singleton 1
            let ys = Set.singleton 2
            let zs = xs |> Set.union ys
            (zs.Contains 1 && zs.Contains 2)
            |> equal true

        testCase "Set.union works II" <| fun () ->
            Set.union (set [1; 2]) (set [3; 4; 5])
            |> equal (set [5; 4; 3; 2; 1])

        // TODO: Check set equality also with non-primitives and custom-equality types

        testCase "Set (+) works" <| fun () ->
            let xs = Set.singleton 1
            let ys = Set.singleton 2
            let zs = xs + ys
            (zs.Contains 1 && zs.Contains 2)
            |> equal true

        testCase "Set.unionMany works" <| fun () ->
            let xs = Set.singleton 1
            let ys = Set.singleton 2
            let zs = Set.singleton 3
            let ks = Set.unionMany [xs; ys; zs]
            (ks.Contains 1 && ks.Contains 2 && ks.Contains 3)
            |> equal true


        testCase "Set.intersect works" <| fun () ->
            let xs = set [1; 2; 3; 4]
            let ys = set [0; 2; 4; 5]
            let zs = xs |> Set.intersect ys
            (zs.Contains 2 && zs.Contains 4 && not(zs.Contains 1) && not(zs.Contains 5))
            |> equal true

        testCase "Set.intersectMany works" <| fun () ->
            let xs = set [1; 2]
            let ys = Set.singleton 2
            let zs = set [2; 3]
            let ks = Set.intersectMany [xs; ys; zs]
            (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
            |> equal true

        testCase "Set.iterate works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let total = ref 0.
            xs |> Set.iter (fun x -> total := !total + x)
            !total |> equal 10.

        testCase "Set.forAll works" <| fun () ->
            let xs = set [1; 2; 3; 4]
            xs |> Set.forall (fun x -> x < 5)
            |> equal true

        testCase "Set.exists works" <| fun () ->
            let xs = set [1; 2; 3; 4]
            xs |> Set.exists ((=) 2) |> equal true
            xs |> Set.exists ((<) 5) |> equal false

        testCase "Set.filter works" <| fun () ->
            let xs = set [1; 2; 3; 4]
            xs |> Set.filter (fun x -> x % 2 = 0)
            |> Set.count |> equal 2

        testCase "Set.partition works" <| fun () ->
            let xs = set [1; 2; 3; 4]
            let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
            (ys.Count + zs.Count)
            |> equal 4

        testCase "Set.fold works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            xs |> Set.fold (-) 0.
            |> equal -10.

        testCase "Set.foldBack works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            Set.foldBack (-) xs 0.
            |> equal -2.

        testCase "Set.map works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = xs |> Set.map ((*) 2.)
            ys.Contains 1.
            |> equal false

        testCase "Set.minElement works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            xs |> Set.minElement
            |> equal 1.

        testCase "Set.MinimumElement works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            xs.MinimumElement
            |> equal 1.

        testCase "Set.maxElement works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            xs |> Set.maxElement
            |> equal 4.

        testCase "Set.MaximumElement works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            xs.MaximumElement
            |> equal 4.

        testCase "Set.difference works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            let zs = Set.difference xs ys
            zs.Count |> equal 2

        testCase "Set (-) works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            let zs = xs - ys
            zs.Count |> equal 2

        testCase "Set.isSubset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            Set.isSubset ys xs
            |> equal true

        testCase "Set.IsSubset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            ys.IsSubsetOf xs
            |> equal true

        testCase "Set.isSuperset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            Set.isSuperset xs ys
            |> equal true

        testCase "Set.IsSuperset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            xs.IsSupersetOf ys
            |> equal true

        testCase "Set.isProperSubset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            Set.isProperSubset ys xs
            |> equal true

        testCase "Set.IsProperSubset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            ys.IsProperSubsetOf xs
            |> equal true

        testCase "Set.isProperSuperset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            Set.isProperSuperset xs ys
            |> equal true

        testCase "Set.IsProperSuperset works" <| fun () ->
            let xs = set [1.; 2.; 3.; 4.]
            let ys = set [1.; 2.]
            xs.IsProperSupersetOf ys
            |> equal true

        testCase "Set.ofList works" <| fun () ->
            let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
            xs.Count |> equal 4

        testCase "Set.ofArray works" <| fun () ->
            let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
            xs.Count |> equal 4

        testCase "Set.ofSeq works" <| fun () ->
            let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
            xs.Count |> equal 4

        testCase "Set.toList works" <| fun () ->
            let xs = [1.; 2.; 3.; 4.]
            let ys = Set.ofList xs
            let zs = Set.toList ys
            xs.[2] = zs.[2]
            |> equal true

        testCase "Set.toArray works" <| fun () ->
            let xs = [|1.; 2.; 3.; 4.|]
            let ys = Set.ofArray xs
            let zs = Set.toArray ys
            xs.[2] = zs.[2]
            |> equal true

        testCase "Set.toSeq works" <| fun () ->
            let xs = seq [1.; 2.; 3.; 4.]
            let ys = Set.ofSeq xs
            let zs = Set.toSeq ys
            (Seq.item 2 xs) = (Seq.item 2 zs)
            |> equal true

        testCase "Comparing large sets works" <| fun _ -> // See #2203
            let largeSetA = Set.ofArray [| for i in 1 .. 100_000 -> i |]
            let largeSetB = Set.ofArray [| for i in 1 .. 100_000 -> i |]
            let largeSetC = Set.ofArray [| for i in 1 .. 100_000 -> if i = 100_000 then -1 else i |]
            let largeSetD = Set.ofArray [| for i in 1 .. 99_999 -> i |]
            largeSetA = largeSetB |> equal true
            largeSetA = largeSetC |> equal false
            largeSetD = largeSetB |> equal false
            compare largeSetA largeSetB |> equal 0
            compare largeSetB largeSetC |> equal 1
            compare largeSetC largeSetA |> equal -1
            largeSetA.IsProperSupersetOf(largeSetD) |> equal true
            largeSetC.IsSupersetOf(largeSetD) |> equal true
            largeSetC.IsProperSubsetOf(largeSetA) |> equal false
            largeSetC.IsSubsetOf(largeSetB) |> equal false
            largeSetD.IsProperSubsetOf(largeSetB) |> equal true
            largeSetD.IsSubsetOf(largeSetC) |> equal true
            largeSetB.IsProperSupersetOf(largeSetA) |> equal false
            largeSetB.IsSupersetOf(largeSetA) |> equal true
            largeSetA.IsProperSubsetOf(largeSetB) |> equal false
            largeSetA.IsSubsetOf(largeSetB) |> equal true
    ]
