module Fable.Tests.Set

open Util.Testing

[<Fact>]
let ``test set function works`` () =
    let xs = set [1]
    xs |> Set.isEmpty
    |> equal false

[<Fact>]
let ``test Set.isEmpty works`` () =
    let xs = set []
    Set.isEmpty xs |> equal true
    let ys = set [1]
    Set.isEmpty ys |> equal false

[<Fact>]
let ``test xs.IsEmpty works`` () =
    let xs = Set.empty<int>
    xs.IsEmpty |> equal true
    let ys = set [1; 1]
    ys.IsEmpty |> equal false

[<Fact>]
let ``test Set.Count works`` () =
    let xs = Set.empty |> Set.add 1
    xs.Count
    |> equal 1

[<Fact>]
let ``test Seq.isEmpty function works on Set`` () =
    let xs = set [1]
    xs |> Seq.isEmpty
    |> equal false

[<Fact>]
let ``test Set.add works`` () =
    let xs = Set.empty |> Set.add 1
    Set.count xs
    |> equal 1

[<Fact>]
let ``test xs.Add works`` () =
    let xs = Set.empty.Add 1
    Set.count xs
    |> equal 1

[<Fact>]
let ``test Set.contains works`` () =
    let xs = Set.empty |> Set.add 1
    xs |> Set.contains 1 |> equal true
    xs |> Set.contains 2 |> equal false


[<Fact>]
let ``test xs.Contains works`` () =
    let xs = Set.empty |> Set.add 1
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false


[<Fact>]
let ``test Set.remove works`` () =
    let xs = Set.empty |> Set.add 1 |> Set.remove 1
    xs.IsEmpty |> equal true


[<Fact>]
let ``test xs.Remove works`` () =
    let xs = (Set.empty |> Set.add 1).Remove 1
    xs.IsEmpty |> equal true

[<Fact>]
let ``test Set.singleton works`` () =
    let xs = Set.singleton 1
    xs.Count |> equal 1


[<Fact>]
let ``test Set.union works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs |> Set.union ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Fact>]
let ``test Set.union works II`` () =
    Set.union (set [1; 2]) (set [3; 4; 5])
    |> equal (set [5; 4; 3; 2; 1])

// TODO: Check set equality also with non-primitives and custom-equality types

[<Fact>]
let ``test Set (+) works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs + ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Fact>]
let ``test Set.unionMany works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = Set.singleton 3
    let ks = Set.unionMany [xs; ys; zs]
    (ks.Contains 1 && ks.Contains 2 && ks.Contains 3)
    |> equal true


[<Fact>]
let ``test Set.intersect works`` () =
    let xs = set [1; 2; 3; 4]
    let ys = set [0; 2; 4; 5]
    let zs = xs |> Set.intersect ys
    (zs.Contains 2 && zs.Contains 4 && not(zs.Contains 1) && not(zs.Contains 5))
    |> equal true

[<Fact>]
let ``test Set.intersectMany works`` () =
    let xs = set [1; 2]
    let ys = Set.singleton 2
    let zs = set [2; 3]
    let ks = Set.intersectMany [xs; ys; zs]
    (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
    |> equal true

[<Fact>]
let ``test Set.iterate works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Set.iter (fun x -> total.Value <- total.Value + x)
    total.Value |> equal 10.

[<Fact>]
let ``test Set.forAll works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.forall (fun x -> x < 5)
    |> equal true

[<Fact>]
let ``test Set.exists works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.exists ((=) 2) |> equal true
    xs |> Set.exists ((<) 5) |> equal false

[<Fact>]
let ``test Set.filter works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.filter (fun x -> x % 2 = 0)
    |> Set.count |> equal 2

[<Fact>]
let ``test Set.partition works`` () =
    let xs = set [1; 2; 3; 4]
    let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
    (ys.Count + zs.Count)
    |> equal 4

[<Fact>]
let ``test Set.fold works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.fold (-) 0.
    |> equal -10.

[<Fact>]
let ``test Set.foldBack works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    Set.foldBack (-) xs 0.
    |> equal -2.

[<Fact>]
let ``test Set.map works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = xs |> Set.map ((*) 2.)
    ys.Contains 1.
    |> equal false

[<Fact>]
let ``test Set.minElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.minElement
    |> equal 1.

[<Fact>]
let ``test Set.MinimumElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MinimumElement
    |> equal 1.

[<Fact>]
let ``test Set.maxElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.maxElement
    |> equal 4.

[<Fact>]
let ``test Set.MaximumElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MaximumElement
    |> equal 4.

[<Fact>]
let ``test Set.difference works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = Set.difference xs ys
    zs.Count |> equal 2

[<Fact>]
let ``test Set (-) works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = xs - ys
    zs.Count |> equal 2

[<Fact>]
let ``test Set.isSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSubset ys xs
    |> equal true

[<Fact>]
let ``test xs.IsSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsSubsetOf xs
    |> equal true

[<Fact>]
let ``test Set.isSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSuperset xs ys
    |> equal true

[<Fact>]
let ``test xs.IsSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsSupersetOf ys
    |> equal true

[<Fact>]
let ``test Set.isProperSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSubset ys xs
    |> equal true

[<Fact>]
let ``test xs.IsProperSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsProperSubsetOf xs
    |> equal true

[<Fact>]
let ``test Set.isProperSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSuperset xs ys
    |> equal true

[<Fact>]
let ``test xs.IsProperSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsProperSupersetOf ys
    |> equal true

[<Fact>]
let ``test Set.ofList works`` () =
    let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Fact>]
let ``test Set.ofArray works`` () =
    let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
    xs.Count |> equal 4

[<Fact>]
let ``test Set.ofSeq works`` () =
    let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Fact>]
let ``test Set.toList works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Set.ofList xs
    let zs = Set.toList ys
    xs.[2] = zs.[2]
    |> equal true

[<Fact>]
let ``test Set.toArray works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = Set.ofArray xs
    let zs = Set.toArray ys
    xs.[2] = zs.[2]
    |> equal true

[<Fact>]
let ``test Set.toSeq works`` () =
    let xs = seq [1.; 2.; 3.; 4.]
    let ys = Set.ofSeq xs
    let zs = Set.toSeq ys
    (Seq.item 2 xs) = (Seq.item 2 zs)
    |> equal true

[<Fact>]
let ``test Comparing large sets works`` () =
    let largeSetA = Set.ofArray [| for i in 1 .. 10_000 -> i |]
    let largeSetB = Set.ofArray [| for i in 1 .. 10_000 -> i |]
    let largeSetC = Set.ofArray [| for i in 1 .. 10_000 -> if i = 10_000 then -1 else i |]
    let largeSetD = Set.ofArray [| for i in 1 .. 9_999 -> i |]
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
