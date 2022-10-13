module Fable.Tests.SetTests

open Util.Testing

[<Fact>]
let ``set function works`` () =
    let xs = set [1]
    xs |> Set.isEmpty
    |> equal false

[<Fact>]
let ``Set.isEmpty works`` () =
    // let xs = set [] //TODO:
    let xs = Set.empty<int>
    Set.isEmpty xs |> equal true
    let ys = set [1]
    Set.isEmpty ys |> equal false

[<Fact>]
let ``Set.IsEmpty works`` () =
    let xs = Set.empty<int>
    xs.IsEmpty |> equal true
    let ys = set [1; 1]
    ys.IsEmpty |> equal false

// [<Fact>]
// let ``Seq.isEmpty function works on Set`` () =
//     let xs = set [1]
//     xs |> Seq.isEmpty
//     |> equal false

[<Fact>]
let ``Set equality works`` () =
    let a1 = set [1; 2; 3]
    let a2 = set [1; 2; 3]
    let a3 = set [1; 2; 4]
    let a4 = set [1; 2; 3; 4]
    a1 = a1 |> equal true
    a1 = a2 |> equal true
    a1 = a3 |> equal false
    a1 = a4 |> equal false
    a1 <> a1 |> equal false
    a1 <> a2 |> equal false
    a1 <> a3 |> equal true
    a1 <> a4 |> equal true

[<Fact>]
let ``Set.Equals works`` () =
    let a1 = set [1; 2; 3]
    let a2 = set [1; 2; 3]
    let a3 = set [1; 2; 4]
    let a4 = set [1; 2; 3; 4]
    a1.Equals(a1) |> equal true
    a1.Equals(a2) |> equal true
    a1.Equals(a3) |> equal false
    a1.Equals(a4) |> equal false

[<Fact>]
let ``Set comparison works`` () =
    let a1 = set [1; 2; 3]
    let a2 = set [1; 2; 3]
    let a3 = set [1; 2; 4]
    let a4 = set [1; 2; 3; 4]
    a1 < a1 |> equal false
    a1 < a2 |> equal false
    a1 < a3 |> equal true
    a1 < a4 |> equal true
    a1 > a1 |> equal false
    a1 > a2 |> equal false
    a1 > a3 |> equal false
    a1 > a4 |> equal false

[<Fact>]
let ``Set compare works`` () =
    let a1 = set [1; 2; 3]
    let a2 = set [1; 2; 3]
    let a3 = set [1; 2; 4]
    let a4 = set [1; 2; 3; 4]
    compare a1 a1 |> equal 0
    compare a1 a2 |> equal 0
    compare a1 a3 |> equal -1
    compare a1 a4 |> equal -1
    compare a3 a4 |> equal 1
    compare a3 a2 |> equal 1
    compare a4 a2 |> equal 1
    compare a4 a3 |> equal -1

[<Fact>]
let ``Set.Count works`` () =
    let xs = Set.empty |> Set.add 1
    xs.Count
    |> equal 1

[<Fact>]
let ``Set.add works`` () =
    let xs = Set.empty |> Set.add 1
    Set.count xs
    |> equal 1

[<Fact>]
let ``Set.Add works`` () =
    let xs = Set.empty.Add 1
    Set.count xs
    |> equal 1

[<Fact>]
let ``Set.contains works`` () =
    let xs = Set.empty |> Set.add 1
    xs |> Set.contains 1 |> equal true
    xs |> Set.contains 2 |> equal false

[<Fact>]
let ``Set.Contains works`` () =
    let xs = Set.empty |> Set.add 1
    xs.Contains 1 |> equal true
    xs.Contains 2 |> equal false

[<Fact>]
let ``Set.remove works`` () =
    let xs = Set.empty |> Set.add 1 |> Set.remove 1
    xs.IsEmpty |> equal true

[<Fact>]
let ``Set.Remove works`` () =
    let xs = (Set.empty |> Set.add 1).Remove 1
    xs.IsEmpty |> equal true

[<Fact>]
let ``Set.singleton works`` () =
    let xs = Set.singleton 1
    xs.Count |> equal 1

[<Fact>]
let ``Set.union works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs |> Set.union ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Fact>]
let ``Set.union works II`` () =
    Set.union (set [1; 2]) (set [3; 4; 5]) = (set [5; 4; 3; 2; 1])
    |> equal true

// TODO: Check set equality also with non-primitives and custom-equality types

[<Fact>]
let ``Set (+) works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = xs + ys
    (zs.Contains 1 && zs.Contains 2)
    |> equal true

[<Fact>]
let ``Set.unionMany works`` () =
    let xs = Set.singleton 1
    let ys = Set.singleton 2
    let zs = Set.singleton 3
    let ks = Set.unionMany [xs; ys; zs]
    (ks.Contains 1 && ks.Contains 2 && ks.Contains 3)
    |> equal true

[<Fact>]
let ``Set.intersect works`` () =
    let xs = set [1; 2; 3; 4]
    let ys = set [0; 2; 4; 5]
    let zs = xs |> Set.intersect ys
    (zs.Contains 2 && zs.Contains 4 && not(zs.Contains 1) && not(zs.Contains 5))
    |> equal true

[<Fact>]
let ``Set.intersectMany works`` () =
    let xs = set [1; 2]
    let ys = Set.singleton 2
    let zs = set [2; 3]
    let ks = Set.intersectMany [xs; ys; zs]
    (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
    |> equal true

[<Fact>]
let ``Set.iterate works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let mutable total = 0.
    xs |> Set.iter (fun x -> total <- total + x)
    total |> equal 10.

[<Fact>]
let ``Set.forAll works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.forall (fun x -> x < 5)
    |> equal true

[<Fact>]
let ``Set.exists works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.exists ((=) 2) |> equal true
    xs |> Set.exists ((<) 5) |> equal false

[<Fact>]
let ``Set.filter works`` () =
    let xs = set [1; 2; 3; 4]
    xs |> Set.filter (fun x -> x % 2 = 0)
    |> Set.count |> equal 2

[<Fact>]
let ``Set.partition works`` () =
    let xs = set [1; 2; 3; 4]
    let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
    (ys.Count + zs.Count)
    |> equal 4

[<Fact>]
let ``Set.fold works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.fold (-) 0.
    |> equal -10.

[<Fact>]
let ``Set.foldBack works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    Set.foldBack (-) xs 0.
    |> equal -2.

[<Fact>]
let ``Set.map works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = xs |> Set.map ((*) 2.)
    ys.Contains 1.
    |> equal false

[<Fact>]
let ``Set.minElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.minElement
    |> equal 1.

[<Fact>]
let ``Set.MinimumElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MinimumElement
    |> equal 1.

[<Fact>]
let ``Set.maxElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs |> Set.maxElement
    |> equal 4.

[<Fact>]
let ``Set.MaximumElement works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    xs.MaximumElement
    |> equal 4.

[<Fact>]
let ``Set.difference works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = Set.difference xs ys
    zs.Count |> equal 2

[<Fact>]
let ``Set (-) works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    let zs = xs - ys
    zs.Count |> equal 2

[<Fact>]
let ``Set.isSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSubset ys xs
    |> equal true

[<Fact>]
let ``Set.IsSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsSubsetOf xs
    |> equal true

[<Fact>]
let ``Set.isSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isSuperset xs ys
    |> equal true

[<Fact>]
let ``Set.IsSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsSupersetOf ys
    |> equal true

[<Fact>]
let ``Set.isProperSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSubset ys xs
    |> equal true

[<Fact>]
let ``Set.IsProperSubset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    ys.IsProperSubsetOf xs
    |> equal true

[<Fact>]
let ``Set.isProperSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    Set.isProperSuperset xs ys
    |> equal true

[<Fact>]
let ``Set.IsProperSuperset works`` () =
    let xs = set [1.; 2.; 3.; 4.]
    let ys = set [1.; 2.]
    xs.IsProperSupersetOf ys
    |> equal true

[<Fact>]
let ``Set.ofList works`` () =
    let xs = Set.ofList [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Fact>]
let ``Set.ofArray works`` () =
    let xs = Set.ofArray [|1.; 2.; 3.; 4.; 4.|]
    xs.Count |> equal 4

[<Fact>]
let ``Set.ofSeq works`` () =
    let xs = Set.ofSeq [1.; 2.; 3.; 4.; 4.]
    xs.Count |> equal 4

[<Fact>]
let ``Set.toList works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Set.ofList xs
    let zs = Set.toList ys
    xs[2] = zs[2]
    |> equal true

[<Fact>]
let ``Set.toArray works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = Set.ofArray xs
    let zs = Set.toArray ys
    xs[2] = zs[2]
    |> equal true

[<Fact>]
let ``Set.toSeq works`` () =
    let xs = seq [1.; 2.; 3.; 4.]
    let ys = Set.ofSeq xs
    let zs = Set.toSeq ys
    (Seq.item 2 xs) = (Seq.item 2 zs)
    |> equal true

[<Fact>]
let ``Comparing large sets works`` () = // See #2203
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
