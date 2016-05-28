[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Sets
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``set function works``() =
    let xs = set [1]
    xs |> Seq.isEmpty
    |> equal false

[<Test>]
let ``Set.isEmpty works``() =
    let xs = Set.empty<int>
    xs |> Seq.isEmpty
    |> equal true

[<Test>]
let ``Set.IsEmpty works``() =
    let xs = Set.empty<int>
    xs.IsEmpty
    |> equal true

[<Test>]
let ``Set.Count works``() =
    let xs = Set.empty |> Set.add 1
    xs.Count
    |> equal 1

[<Test>]
let ``Set.add works``() =
    let xs = Set.empty |> Set.add 1
    Set.count xs
    |> equal 1
    
[<Test>]
let ``Set.Add works``() =
    let xs = Set.empty.Add 1
    Set.count xs
    |> equal 1

[<Test>]
let ``Set.contains works``() =
    let xs = Set.empty |> Set.add 1
    xs |> Set.contains 1 |> equal true
    xs |> Set.contains 2 |> equal false

[<Test>]
let ``Set.Contains works``() =
    let xs = Set.empty |> Set.add 1
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false

[<Test>]
let ``Set.remove works``() =
    let xs = Set.empty |> Set.add 1 |> Set.remove 1
    xs.IsEmpty |> equal true

[<Test>]
let ``Set.Remove works``() =
    let xs = (Set.empty |> Set.add 1).Remove 1
    xs.IsEmpty |> equal true

[<Test>]
let ``Set.singleton works``() =
    let xs = Set.singleton 1
    xs.Count |> equal 1

[<Test>]
let ``Set.union works``() =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs |> Set.union ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Test>]
let ``Set (+) works``() =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs + ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Test>]
let ``Set.unionMany works``() =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = Set.singleton 3
    let ks = Set.unionMany [xs; ys; zs]
    (ks.Contains 1 && ks.Contains 2 && ks.Contains 3)
    |> equal true

[<Test>]
let ``Set.intersect works``() =
    let xs = set [1; 2; 3; 4]
    let ys = set [0; 2; 4; 5]
    let zs = xs |> Set.intersect ys
    (zs.Contains 2 && zs.Contains 4 && not(zs.Contains 1) && not(zs.Contains 5))
    |> equal true

[<Test>]
let ``Set.intersectMany works``() =
    let xs = set [1; 2]
    let ys = Set.singleton 2
    let zs = set [2; 3]
    let ks = Set.intersectMany [xs; ys; zs] 
    (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
    |> equal true

[<Test>]
let ``Set.iterate works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Set.iter (fun x -> total := !total + x)
    !total |> equal 10.

[<Test>]
let ``Set.forAll works``() =
    let xs = set [1; 2; 3; 4]
    xs |> Set.forall (fun x -> x < 5)
    |> equal true

[<Test>]
let ``Set.exists works``() =
    let xs = set [1; 2; 3; 4]
    xs |> Set.exists ((=) 2) |> equal true
    xs |> Set.exists ((<) 5) |> equal false

[<Test>]
let ``Set.filter works``() =
    let xs = set [1; 2; 3; 4]
    xs |> Set.filter (fun x -> x % 2 = 0)
    |> Set.count |> equal 2

[<Test>]
let ``Set.partition works``() =
    let xs = set [1; 2; 3; 4]
    let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
    (ys.Count + zs.Count)
    |> equal 4

[<Test>]
let ``Set.fold works``() =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.fold (+) 0.
    |> equal 10.

[<Test>]
let ``Set.foldBack works``() =
    let xs = set [1.; 2.; 3.; 4.]
    Set.foldBack (+) xs 0.
    |> equal 10.

[<Test>]
let ``Set.map works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = xs |> Set.map ((*) 2.)
    ys.Contains 1.
    |> equal false

[<Test>]
let ``Set.minElement works``() =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.minElement
    |> equal 1.

[<Test>]
let ``Set.MinimumElement works``() =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MinimumElement
    |> equal 1.

[<Test>]
let ``Set.maxElement works``() =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.maxElement
    |> equal 4.

[<Test>]
let ``Set.MaximumElement works``() =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MaximumElement
    |> equal 4.

[<Test>]
let ``Set.difference works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = Set.difference xs ys
    zs.Count |> equal 2

[<Test>]
let ``Set (-) works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = xs - ys
    zs.Count |> equal 2

[<Test>]
let ``Set.isSubset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSubset ys xs
    |> equal true

[<Test>]
let ``Set.IsSubset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsSubsetOf xs
    |> equal true

[<Test>]
let ``Set.isSuperset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSuperset xs ys
    |> equal true

[<Test>]
let ``Set.IsSuperset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsSupersetOf ys
    |> equal true

[<Test>]
let ``Set.isProperSubset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSubset ys xs
    |> equal true

[<Test>]
let ``Set.IsProperSubset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsProperSubsetOf xs
    |> equal true

[<Test>]
let ``Set.isProperSuperset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSuperset xs ys
    |> equal true

[<Test>]
let ``Set.IsProperSuperset works``() =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsProperSupersetOf ys
    |> equal true

[<Test>]
let ``Set.ofList works``() =
    let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Test>]
let ``Set.ofArray works``() =
    let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
    xs.Count |> equal 4

[<Test>]
let ``Set.ofSeq works``() =
    let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Test>]
let ``Set.toList works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Set.ofList xs
    let zs = Set.toList ys
    xs.[2] = zs.[2]
    |> equal true
    
[<Test>]
let ``Set.toArray works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = Set.ofArray xs
    let zs = Set.toArray ys
    xs.[2] = zs.[2]
    |> equal true

[<Test>]
let ``Set.toSeq works``() =
    let xs = seq [1.; 2.; 3.; 4.]
    let ys = Set.ofSeq xs
    let zs = Set.toSeq ys
    (Seq.item 2 xs) = (Seq.item 2 zs)
    |> equal true