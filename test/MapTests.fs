[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Maps
open NUnit.Framework
open Fabel.Tests.Util


[<Test>]
let ``Map construction from lists works``() =
   check  
      <@@ 
         let xs = Map [1,1; 2,2]
         xs |> Seq.isEmpty
      @@>

[<Test>]
let ``Map.isEmpty works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs |> Seq.isEmpty
      @@>

[<Test>]
let ``Map.IsEmpty works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs.IsEmpty
      @@>

[<Test>]
let ``Map.empty works``() =
   check  
      <@@ 
         let xs = Map.empty<int,int>
         ()
      @@>

[<Test>]
let ``Map.Count works``() =
   check  
      <@@ 
         let xs = Map.empty<int, int>
         xs.Count |> float
      @@>

[<Test>]
let ``Map.add works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         float xs.Count
      @@>

[<Test>]
let ``Map.Add works``() =
   check  
      <@@ 
         let xs = Map.empty.Add(1, 1)
         float xs.Count
      @@>

[<Test>]
let ``Map.containsKey works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         xs |> Map.containsKey 1
      @@>

[<Test>]
let ``Map.ContainsKey works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1
         xs.ContainsKey 1
      @@>


[<Test>]
let ``Map.remove works``() =
   check  
      <@@ 
         let xs = Map.empty |> Map.add 1 1 |> Map.remove 1
         xs.IsEmpty
      @@>

[<Test>]
let ``Map.Remove works``() =
   check  
      <@@ 
         let xs = (Map.empty |> Map.add 1 1).Remove 1
         xs.IsEmpty
      @@>

[<Test>]
let ``Map.iter works``() =
   check  
      <@@ 
         let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
         let total = ref 0.
         xs |> Map.iter (fun x y -> total := !total + x + y)
         !total
      @@>


[<Test>]
let ``Map.forAll works``() =
   check  
      <@@ 
         let xs = Map [1.,1.; 2.,4.; 3.,9.; 4.,16.]
         xs |> Map.forall (fun x y -> x < 5.)
      @@>

[<Test>]
let ``Map.exists works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.exists (fun k v -> k = 2)
      @@>

[<Test>]
let ``Map.filter works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = xs |> Map.filter (fun x y -> x % 2 = 0)
         ys.Count |> float
      @@>

[<Test>]
let ``Map.partition works``() =
   check  
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys, zs = xs |> Map.partition (fun x y -> x % 2 = 0)
         float(ys.Count + zs.Count)
      @@> 

[<Test>]
let ``Map.fold works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.fold (fun acc k v -> v + acc) 0.
      @@>

[<Test>]
let ``Map.foldBack works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         Map.foldBack (fun k v acc -> v + acc) xs 0.
      @@>

[<Test>]
let ``Map.map works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = xs |> Map.map (fun k v -> v * 2.)
         ys.ContainsKey 1
      @@>

[<Test>]
let ``Map.find works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         xs |> Map.find 1
      @@>

[<Test>]
let ``Map.tryFind works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs |> Map.tryFind 0).IsNone
      @@>

[<Test>]
let ``Map.TryFind works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs.TryFind 3).IsSome
      @@>

[<Test>]
let ``Map.tryFindKey works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         (xs |> Map.tryFindKey (fun k v -> k = 3)).IsSome
      @@>

[<Test>]
let ``Map.pick works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let y = xs |> Map.pick (fun k v -> 
            match k with
            | 3 -> Some 10
            | _ -> None)
         float y
      @@>

[<Test>]
let ``Map.tryPick works``() =
   check   
      <@@ 
         let xs = Map [1,1.; 2,4.; 3,9.; 4,16.]
         let y = xs |> Map.tryPick (fun k v -> 
            match k with
            | 3 -> Some 11
            | _ -> None)
         float y.Value
      @@>

[<Test>]
let ``Map.ofList works``() =
   check   
      <@@ 
         let xs = Map.ofList [1,1.; 2,4.; 3,9.; 4,16.]
         float xs.Count
      @@>

[<Test>]
let ``Map.ofArray works``() =
   check   
      <@@ 
         let xs = Map.ofArray [|1,1.; 2,4.; 3,9.; 4,16.|]
         float xs.Count
      @@>

[<Test>]
let ``Map.ofSeq works``() =
   check   
      <@@ 
         let xs = Map.ofSeq [1,1.; 2,4.; 3,9.; 4,16.]
         float xs.Count
      @@>

[<Test>]
let ``Map.toList works``() =
   check   
      <@@ 
         let xs = [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = Map.ofList xs
         let zs = Map.toList ys
         xs = zs
      @@>

[<Test>]
let ``Map.toArray works``() =
   check   
      <@@ 
         let xs = [|1,1.; 2,4.; 3,9.; 4,16.|]
         let ys = Map.ofArray xs
         let zs = Map.toArray ys
         xs = zs
      @@>

[<Test>]
let ``Map.toSeq works``() =
   check   
      <@@ 
         let xs = seq [1,1.; 2,4.; 3,9.; 4,16.]
         let ys = Map.ofSeq xs
         let zs = Map.toSeq ys
         ()
      @@>