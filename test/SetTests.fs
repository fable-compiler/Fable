[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Sets
open NUnit.Framework
open Fabel.Tests.Util

[<Test>]
let ``set function works``() =
   check  
      <@@ 
         let xs = set [1]
         xs |> Seq.isEmpty
      @@>

[<Test>]
let ``Set.isEmpty works``() =
   check  
      <@@ 
         let xs = Set.empty<int>
         xs |> Seq.isEmpty
      @@>

[<Test>]
let ``Set.IsEmpty works``() =
   check  
      <@@ 
         let xs = Set.empty<int>
         xs.IsEmpty
      @@>

[<Test>]
let ``Set.empty works``() =
   check  
      <@@ 
         let xs = Set.empty<int>
         ()
      @@>

[<Test>]
let ``Set.count works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1
         Set.count xs |> float
      @@>

[<Test>]
let ``Set.Count works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1
         xs.Count |> float
      @@>

[<Test>]
let ``Set.add works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1
         Set.count xs |> float
      @@>

[<Test>]
let ``Set.Add works``() =
   check  
      <@@ 
         let xs = Set.empty.Add 1
         Set.count xs |> float
      @@>

[<Test>]
let ``Set.contains works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1
         xs |> Set.contains 1
      @@>

[<Test>]
let ``Set.Contains works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1
         xs.Contains 1
      @@>

[<Test>]
let ``Set.singleton works``() =
   check  
      <@@ 
         let xs = Set.singleton 1
         float xs.Count
      @@>


[<Test>]
let ``Set.remove works``() =
   check  
      <@@ 
         let xs = Set.empty |> Set.add 1 |> Set.remove 1
         xs.IsEmpty
      @@>

[<Test>]
let ``Set.Remove works``() =
   check  
      <@@ 
         let xs = (Set.empty |> Set.add 1).Remove 1
         xs.IsEmpty
      @@>

[<Test>]
let ``Set.union works``() =
   check  
      <@@ 
         let xs = Set.singleton 1
         let ys = Set.singleton 2
         let zs = xs |> Set.union ys
         zs.Contains 1 && zs.Contains 2
      @@>

[<Test>]
let ``Set (+) works``() =
   check  
      <@@ 
         let xs = Set.singleton 1
         let ys = Set.singleton 2
         let zs = xs + ys
         zs.Contains 1 && zs.Contains 2
      @@>

[<Test>]
let ``Set.unionMany works``() =
   check  
      <@@ 
         let xs = Set.singleton 1
         let ys = Set.singleton 2
         let zs = Set.singleton 3
         let ks = Set.unionMany [xs; ys; zs]
         ks.Contains 1 && ks.Contains 2 && ks.Contains 3
      @@>

[<Test>]
let ``Set.intersect works``() =
   check  
      <@@ 
         let xs = set [1; 2]
         let ys = Set.singleton 2
         let zs = xs |> Set.intersect ys
         zs.Contains 2 && not(zs.Contains 1)
      @@>

[<Test>]
let ``Set.intersectMany works``() =
   check  
      <@@ 
         let xs = set [1; 2]
         let ys = Set.singleton 2
         let zs = set [2; 3]
         let ks = Set.intersectMany [xs; ys; zs] 
         ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3)
      @@>

[<Test>]
let ``Set.iterate works``() =
   check  
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> Set.iter (fun x -> total := !total + x)
         !total
      @@>


[<Test>]
let ``Set.forAll works``() =
   check  
      <@@ 
         let xs = set [1; 2; 3; 4]
         xs |> Set.forall (fun x -> x < 5)
      @@>

[<Test>]
let ``Set.exists works``() =
   check  
      <@@ 
         let xs = set [1; 2; 3; 4]
         xs |> Set.exists ((=) 2)
      @@>

[<Test>]
let ``Set.filter works``() =
   check  
      <@@ 
         let xs = set [1; 2; 3; 4]
         xs |> Set.filter (fun x -> x % 2 = 0)
         |> Set.count |> float
      @@>

[<Test>]
let ``Set.partition works``() =
   check  
      <@@ 
         let xs = set [1; 2; 3; 4]
         let ys, zs = xs |> Set.partition (fun x -> x % 2 = 0)
         float(ys.Count + zs.Count)
      @@> 

[<Test>]
let ``Set.fold works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         xs |> Set.fold (+) 0.
      @@>

[<Test>]
let ``Set.foldBack works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         Set.foldBack (+) xs 0.
      @@>

[<Test>]
let ``Set.map works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = xs |> Set.map ((*) 2.)
         ys.Contains 1.
      @@>

[<Test>]
let ``Set.minElement works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         xs |> Set.minElement
      @@>

[<Test>]
let ``Set.MinimumElement works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         xs.MinimumElement
      @@>

[<Test>]
let ``Set.maxElement works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         xs |> Set.maxElement
      @@>

[<Test>]
let ``Set.MaximumElement works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         xs.MaximumElement
      @@>

[<Test>]
let ``Set.difference works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         let zs = Set.difference xs ys
         float zs.Count
      @@>

[<Test>]
let ``Set (-) works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         let zs = xs - ys
         float zs.Count
      @@>

[<Test>]
let ``Set.isSubset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         Set.isSubset ys xs
      @@>


[<Test>]
let ``Set.IsSubset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         ys.IsSubsetOf xs
      @@>

[<Test>]
let ``Set.isSuperset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         Set.isSuperset xs ys
      @@>


[<Test>]
let ``Set.IsSuperset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         xs.IsSupersetOf ys
      @@>

[<Test>]
let ``Set.isProperSubset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         Set.isProperSubset ys xs
      @@>


[<Test>]
let ``Set.IsProperSubset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         ys.IsProperSubsetOf xs
      @@>

[<Test>]
let ``Set.isProperSuperset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         Set.isProperSuperset xs ys
      @@>


[<Test>]
let ``Set.IsProperSuperset works``() =
   check   
      <@@ 
         let xs = set [1.; 2.; 3.; 4.]
         let ys = set [1.; 2.]
         xs.IsProperSupersetOf ys
      @@>

[<Test>]
let ``Set.ofList works``() =
   check   
      <@@ 
         let xs = Set.ofList [1.; 2.; 3.; 4.]
         float xs.Count
      @@>

[<Test>]
let ``Set.ofArray works``() =
   check   
      <@@ 
         let xs = Set.ofArray [|1.; 2.; 3.; 4.|]
         float xs.Count
      @@>

[<Test>]
let ``Set.ofSeq works``() =
   check   
      <@@ 
         let xs = Set.ofSeq [1.; 2.; 3.; 4.]
         float xs.Count
      @@>

[<Test>]
let ``Set.toList works``() =
   check   
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = Set.ofList xs
         let zs = Set.toList ys
         xs = zs
      @@>

[<Test>]
let ``Set.toArray works``() =
   check   
      <@@ 
         let xs = [|1.; 2.; 3.; 4.|]
         let ys = Set.ofArray xs
         let zs = Set.toArray ys
         xs = zs
      @@>

[<Test>]
let ``Set.toSeq works``() =
   check   
      <@@ 
         let xs = seq [1.; 2.; 3.; 4.]
         let ys = Set.ofSeq xs
         let zs = Set.toSeq ys
         ()
      @@>