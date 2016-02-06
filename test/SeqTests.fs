[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Seqs
open NUnit.Framework
open Fabel.Tests.Util

let sumFirstTwo zs =
   let first = Seq.head zs
   let second = Seq.skip 1 zs |> Seq.head
   first + second

[<Test>]
let ``Seq.length works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         float (Seq.length xs)
      @@>

[<Test>]
let ``Seq.delay works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = Seq.delay (fun () -> xs :> _ seq)
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.unfold works``() =
   check  
      <@@ 
         1 |> Seq.unfold (fun x ->
            if x <= 5 then Some(x, x + 1)
            else None)
         |> Seq.length
         |> float
      @@>

[<Test>]
let ``Seq.empty works``() =
   check  
      <@@ 
         let xs = Seq.empty<int>
         xs.GetEnumerator().MoveNext()
      @@>

[<Test>]
let ``Seq.append works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [0.]
         let zs = Seq.append ys xs
         sumFirstTwo zs
      @@>

[<Test>]
let ``Seq.average works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.average xs
      @@>

[<Test>]
let ``Seq.averageBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.averageBy ((*) 2.) xs
      @@>

[<Test>]
let ``Seq.choose works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let zs = xs |> Seq.choose (fun x ->
            if x > 2. then Some x
            else None) 
         sumFirstTwo zs
      @@>

[<Test>]
let ``Seq.concat works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> Seq.concat
         sumFirstTwo ys
      @@>

[<Test>]
let ``Seq.collect works``() =
   check  
      <@@ 
         let xs = [[1.]; [2.]; [3.]; [4.]]
         let ys = xs |> Seq.collect id
         sumFirstTwo ys
      @@>

[<Test>]
let ``Seq.exists works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.exists (fun x -> x = 2.)
      @@>

[<Test>]
let ``Seq.exists2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         Seq.exists2 (fun x y -> x * y = 16.) xs ys
      @@>

[<Test>]
let ``Seq.filter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> Seq.filter (fun x -> x > 5.)
         ys |> Seq.isEmpty
      @@>

[<Test>]
let ``Seq.find works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.find ((=) 2.)
      @@>

[<Test>]
let ``Seq.findIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.findIndex ((=) 2.)
         |> float
      @@>

[<Test>]
let ``Seq.fold works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = xs |> Seq.fold (+) 0.
         total
      @@>

[<Test>]
let ``Seq.forall works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.forall (fun x -> x < 5.) xs
      @@>

[<Test>]
let ``Seq.forall2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = [1.; 2.; 3.; 4.]
         Seq.forall2 (=) xs ys
      @@>

[<Test>]
let ``Seq.head works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         Seq.head xs
      @@>

[<Test>]
let ``Seq.init works``() =
   check  
      <@@ 
         let xs = Seq.init 4 float
         sumFirstTwo xs
      @@>

[<Test>]
let ``Seq.isEmpty works``() =
   check  
      <@@ 
         let xs = [1]
         Seq.isEmpty xs
      @@>

[<Test>]
let ``Seq.iter works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> Seq.iter (fun x ->
            total := !total + x
         )
         !total
      @@>

[<Test>]
let ``Seq.iter2 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         Seq.iter2 (fun x y ->
            total := !total + x + y
         ) xs xs
         !total
      @@>

[<Test>]
let ``Seq.iteri works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let total = ref 0.
         xs |> Seq.iteri (fun i x ->
            total := !total + (float i) * x
         )
         !total
      @@>

[<Test>]
let ``Seq.map works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> Seq.map ((*) 2.)
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.map2 works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = [2.]
         let zs = Seq.map2 (*) xs ys
         zs |> Seq.head
      @@>

[<Test>]
let ``Seq.mapi works``() =
   check  
      <@@ 
         let xs = [1.]
         let ys = xs |> Seq.mapi (fun i x -> float i + x)
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.max works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.max
      @@>

[<Test>]
let ``Seq.maxBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.maxBy (fun x -> -x)
      @@>

[<Test>]
let ``Seq.min works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.min
      @@>

[<Test>]
let ``Seq.minBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.minBy (fun x -> -x)
      @@>

[<Test>]
let ``Seq.nth works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         Seq.nth 1 xs
      @@>

[<Test>]
let ``Seq.ofArray works``() =
   check  
      <@@ 
         let xs = [|1.; 2.|]
         let ys = Seq.ofArray xs
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.ofList works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = Seq.ofList xs
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.pick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.pick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
      @@>

[<Test>]
let ``Seq.reduce works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.reduce (+)
      @@>

[<Test>]
let ``Seq.scan works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         let ys = xs |> Seq.scan (+) 0.
         sumFirstTwo ys
      @@>

let ``Seq.sort works``() =
   check  
      <@@ 
         let xs = [3.; 4.; 1.; 2.]
         let ys = xs |> Seq.sort
         sumFirstTwo ys
      @@>

[<Test>]
let ``Seq.sortBy works``() =
   check  
      <@@ 
         let xs = [3.; 1.; 4.; 2.]
         let ys = xs |> Seq.sortBy (fun x -> -x)
         sumFirstTwo ys
      @@>

[<Test>]
let ``Seq.sum works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.sum
      @@>

[<Test>]
let ``Seq.sumBy works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         xs |> Seq.sumBy ((*) 2.)
      @@>

[<Test>]
let ``Seq.skip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.skip 1
         ys |> Seq.head
      @@>

[<Test>]
let ``Seq.toArray works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.toArray
         ys.[0] + ys.[1]
      @@>

[<Test>]
let ``Seq.toList works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = xs |> Seq.toList
         ys.Head + ys.Tail.Head
      @@>

[<Test>]
let ``Seq.tryFind works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> Seq.tryFind ((=) 1.)
         ys.IsSome
      @@>

[<Test>]
let ``Seq.tryFindIndex works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let ys = xs |> Seq.tryFindIndex ((=) 2.)
         ys.Value |> float
      @@>

[<Test>]
let ``Seq.tryPick works``() =
   check  
      <@@ 
         let xs = [1.; 2.]
         let r = xs |> Seq.tryPick (fun x ->
            match x with
            | 2. -> Some x
            | _ -> None)
         match r with
         | Some x -> x
         | None -> 0.
      @@>

[<Test>]
let ``Seq.zip works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = Seq.zip xs ys
         let x, y = zs |> Seq.head
         x + y
      @@>

[<Test>]
let ``Seq.zip3 works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.]
         let ys = [1.; 2.; 3.]
         let zs = [1.; 2.; 3.]
         let ks = Seq.zip3 xs ys zs
         let x, y, z = ks |> Seq.head
         x + y + z
      @@>

let ``Seq.cache works``() =
   check
      <@@
         let count = ref 0
         let xs = 
            1 |> Seq.unfold(fun i -> 
               count := !count + 1
               if i <= 10 then Some(i, i+1)
               else None)
         xs |> Seq.length |> ignore
         xs |> Seq.length |> ignore
         !count
      @@>

[<Test>]
let ``Seq.cast works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = Seq.cast<int> xs
         ys |> Seq.head |> float
      @@>

[<Test>]
let ``Seq.compareWith works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = [1; 2; 3; 4]
         let diff = Seq.compareWith (fun x y -> x - y) xs ys 
         float diff
      @@>

[<Test>]
let ``Seq.countBy works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = xs |> Seq.countBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Test>]
let ``Seq.distinct works``() =
   check  
      <@@ 
         let xs = [1; 1; 1; 2; 2; 3; 3]
         let ys = xs |> Seq.distinct
         ys |> Seq.length |> float
      @@>

[<Test>]
let ``Seq.distinctBy works``() =
   check  
      <@@ 
         let xs = [1; 1; 1; 2; 2; 3; 3]
         let ys = xs |> Seq.distinctBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Test>]
let ``Seq.exactlyOne works``() =
   check  
      <@@ 
         let xs = [1.]
         xs |> Seq.exactlyOne
      @@>

[<Test>]
let ``Seq.groupBy works``() =
   check  
      <@@ 
         let xs = [1; 2; 3; 4]
         let ys = xs |> Seq.groupBy (fun x -> x % 2)
         ys |> Seq.length |> float
      @@>

[<Test>]
let ``Seq.initInfinite works``() =
   check  
      <@@ 
         Seq.initInfinite (fun i -> 2. * float i)
         |> Seq.take 10
         |> Seq.sum
      @@>

[<Test>]
let ``Seq.last works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.last
      @@>

[<Test>]
let ``Seq.pairwise works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.pairwise
         |> Seq.map (fun (x, y) -> x * y)
         |> Seq.sum
      @@>

[<Test>]
let ``Seq.readonly works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.]
         xs |> Seq.readonly
         |> Seq.head
      @@>

[<Test>]
let ``Seq.singleton works``() =
   check  
      <@@ 
         let xs = Seq.singleton 1.
         xs |> Seq.head
      @@>

[<Test>]
let ``Seq.skipWhile works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.skipWhile (fun i -> i <= 3.)
         |> Seq.head
      @@>

[<Test>]
let ``Seq.take works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.take 2
         |> Seq.last
      @@>

[<Test>]
let ``Seq.takeWhile works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.takeWhile (fun i -> i < 3.)
         |> Seq.last
      @@>

[<Test>]
let ``Seq.truncate works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.truncate 2
         |> Seq.last
      @@>

[<Test>]
let ``Seq.where works``() =
   check  
      <@@ 
         let xs = [1.; 2.; 3.; 4.; 5.]
         xs |> Seq.where (fun i -> i <= 3.)
         |> Seq.length 
         |> float
      @@>