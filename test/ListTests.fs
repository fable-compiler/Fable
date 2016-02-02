[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Lists
open NUnit.Framework
open Fabel.Tests.Util

[<Test>]
let ``List.Length works``() =
      let xs = [1; 2; 3; 4]
      equal 4 xs.Length

[<Test>]
let ``List.IsEmpty works``() =
      let xs = [1; 2; 3; 4]
      equal false xs.IsEmpty

[<Test>]
let ``List.Head works``() =
      let xs = [1; 2; 3; 4]
      equal 1 xs.Head

[<Test>]
let ``List.Tail works``() =
      let xs = [1; 2; 3; 4]
      equal 2 xs.Tail.Head

[<Test>]
let ``List.Item works``() =
      let xs = [1; 2; 3; 4]
      equal 4 xs.[3]
      
[<Test>]
let ``List cons works``() =
      let xs = [1; 2; 3; 4]
      let ys = 3 :: xs
      let zs = List.Cons(4, xs)
      ys.Head + xs.Head
      |> equal zs.Head

[<Test>]
let ``List.empty works``() =
      let xs = 1 :: List.Empty
      let ys = 1 :: List.empty
      xs.Length + ys.Length |> equal 2

[<Test>]
let ``List.append works``() =
      let xs = [1; 2; 3; 4]
      let ys = [0]
      let zs = List.append ys xs
      zs.Head + zs.Tail.Head
      |> equal 1

[<Test>]
let ``List.average works``() =
      List.average [1.; 2.; 3.; 4.]
      |> equal 2.5

[<Test>]
let ``List.averageBy works``() =
      [1.; 2.; 3.; 4.]
      |> List.averageBy ((*) 2.)
      |> equal 5.

[<Test>]
let ``List.choose works``() =
      let xs = [1; 2; 3; 4]
      let result = xs |> List.choose (fun x ->
            if x > 2 then Some x
            else None) 
      result.Head + result.Tail.Head
      |> equal 7

[<Test>]
let ``List.collect works``() =
      let xs = [[1]; [2]; [3]; [4]]
      let ys = xs |> List.collect id
      ys.Head + ys.Tail.Head
      |> equal 3

[<Test>]
let ``List.concat works``() =
      let xs = [[1]; [2]; [3]; [4]]
      let ys = xs |> List.concat
      ys.Head  + ys.Tail.Head
      |> equal 3

[<Test>]
let ``List.exists works``() =
      let xs = [1; 2; 3; 4]
      xs |> List.exists (fun x -> x = 2)
      |> equal true

[<Test>]
let ``List.exists2 works``() =
      let xs = [1; 2; 3; 4]
      let ys = [1; 2; 3; 4]
      List.exists2 (fun x y -> x * y = 16) xs ys
      |> equal true

[<Test>]
let ``List.filter works``() =
      let xs = [1; 2; 3; 4]
      let ys = xs |> List.filter (fun x -> x > 5)
      equal ys.IsEmpty true

[<Test>]
let ``List.find works``() =
      [1; 2; 3; 4]
      |> List.find ((=) 2)
      |> equal 2

[<Test>]
let ``List.findIndex works``() =
      [1; 2; 3; 4]
      |> List.findIndex ((=) 2)
      |> equal 1

[<Test>]
let ``List.fold works``() =
      [1; 2; 3; 4]
      |> List.fold (+) 0
      |> equal 10

[<Test>]
let ``List.fold2 works``() =
      let xs = [1; 2; 3; 4]
      let ys = [1; 2; 3; 4]
      List.fold2 (fun x y z -> x + y + z) 0 xs ys
      |> equal 20

[<Test>]
let ``List.foldBack works``() =
      [1; 2; 3; 4]
      |> List.foldBack (fun x acc -> acc - x) <| 100
      |> equal 90 

[<Test>]
let ``List.foldBack2 works``() =
      let xs = [1; 2; 3; 4]
      let ys = [1; 2; 3; 4]
      List.foldBack2 (fun x y acc -> acc - y * x) xs ys 0
      |> equal -30

[<Test>]
let ``List.forall works``() =
      [1; 2; 3; 4]
      |> List.forall (fun x -> x < 5)
      |> equal true

[<Test>]
let ``List.forall2 works``() =
      let xs = [1; 2; 3; 4]
      let ys = [1; 2; 3; 4]
      List.forall2 (=) xs ys
      |> equal true

(*
[<Test>]
let ``List.head works``() =
      [1; 2; 3; 4]
      |> List.head
      |> equal 1

[<Test>]
let ``List.init works``() =
      let xs = List.init 4 float
      xs.Head + xs.Tail.Head
      |> equal 1.


[<Test>]
let ``List.isEmpty works``() =
         let xs = [1]
         List.isEmpty xs

[<Test>]
let ``List.iter works``() =
         let xs = [1; 2; 3; 4]
         let total = ref 0
         xs |> List.iter (fun x ->
            total := !total + x
         )
         !total

[<Test>]
let ``List.iter2 works``() =
         let xs = [1; 2; 3; 4]
         let total = ref 0
         List.iter2 (fun x y ->
            total := !total + x + y
         ) xs xs
         !total

[<Test>]
let ``List.iteri works``() =
         let xs = [1; 2; 3; 4]
         let total = ref 0
         xs |> List.iteri (fun i x ->
            total := !total + (float i) * x
         )
         !total

[<Test>]
let ``List.iteri2 works``() =
         let xs = [1; 2; 3; 4]
         let total = ref 0
         List.iteri2 (fun i x y ->
            total := !total + (float i) * x + (float i) * y
         ) xs xs
         !total

[<Test>]
let ``List.length works``() =
         let xs = [1; 2; 3; 4]
         List.length xs 
         |> float

[<Test>]
let ``List.map works``() =
         let xs = [1]
         let ys = xs |> List.map (fun x -> x*2)
         ys.Head

[<Test>]
let ``List.map2 works``() =
         let xs = [1]
         let ys = [2]
         let zs = List.map2 (fun x y -> x*y) xs ys
         zs.Head

[<Test>]
let ``List.map3 works``() =
         let xs = [1]
         let ys = [2]
         let zs = [3]
         let ks = List.map3 (fun x y z -> x * y * z) xs ys zs
         zs.Head

[<Test>]
let ``List.mapi works``() =
         let xs = [1]
         let ys = xs |> List.mapi (fun i x -> float i + x)
         ys.Head

[<Test>]
let ``List.mapi2 works``() =
         let xs = [1]
         let ys = [2]
         let zs = List.mapi2 (fun i x y -> float i + x * y) xs ys
         zs.Head

[<Test>]
let ``List.max works``() =
         let xs = [1; 2]
         xs |> List.max

[<Test>]
let ``List.maxBy works``() =
         let xs = [1; 2]
         xs |> List.maxBy (fun x -> -x)

[<Test>]
let ``List.min works``() =
         let xs = [1; 2]
         xs |> List.min

[<Test>]
let ``List.minBy works``() =
         let xs = [1; 2]
         xs |> List.minBy (fun x -> -x)

[<Test>]
let ``List.nth works``() =
         let xs = [1; 2]
         List.nth xs 1

[<Test>]
let ``List.ofArray works``() =
         let xs = [|1; 2|]
         let ys = List.ofArray xs
         ys.Head

[<Test>]
let ``List.ofSeq works``() =
         let xs = [|1; 2|] :> _ seq
         let ys = List.ofSeq xs
         ys.Head

[<Test>]
let ``List.partition works``() =
         let xs = [1; 2]
         let ys, zs = xs |> List.partition (fun x -> x <= 1)
         ys.Head - zs.Head

[<Test>]
let ``List.permute works``() =
         let xs = [1; 2]
         let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
         ys.Head

[<Test>]
let ``List.pick works``() =
         let xs = [1; 2]
         xs |> List.pick (fun x ->
            match x with
            | 2 -> Some x
            | _ -> None)

[<Test>]
let ``List.reduce works``() =
         let xs = [1; 2]
         xs |> List.reduce (+)

[<Test>]
let ``List.reduceBack works``() =
         let xs = [1; 2]
         xs |> List.reduceBack (+)

[<Test>]
let ``List.replicate works``() =
         let xs = List.replicate 2 1
         xs.Head + xs.Tail.Head

[<Test>]
let ``List.rev works``() =
         let xs = [1; 2]
         let ys = xs |> List.rev
         ys.Head

[<Test>]
let ``List.scan works``() =
         let xs = [1; 2; 3; 4]
         let ys = xs |> List.scan (+) 0
         ys.Head + ys.Tail.Head

[<Test>]
let ``List.scanBack works``() =
         let xs = [1; 2; 3]
         let ys = List.scanBack (+) xs 0
         ys.Head + ys.Tail.Head

[<Test>]
let ``List.sort works``() =
         let xs = [3; 4; 1; 2]
         let ys = xs |> List.sort
         ys.Head + ys.Tail.Head

[<Test>]
let ``List.sortBy works``() =
         let xs = [3; 1; 4; 2]
         let ys = xs |> List.sortBy (fun x -> -x)
         ys.Head + ys.Tail.Head

[<Test>]
let ``List.sortWith works``() =
         let xs = [3; 4; 1; 2]
         let ys = xs |> List.sortWith (fun x y -> int(x - y))
         ys.Head + ys.Tail.Head

[<Test>]
let ``List.sum works``() =
         let xs = [1; 2]
         xs |> List.sum

[<Test>]
let ``List.sumBy works``() =
         let xs = [1; 2]
         xs |> List.sumBy (fun x -> x*2)

[<Test>]
let ``List.tail works``() =
         let xs = [1; 2]
         let ys = xs |> List.tail
         ys.Head

[<Test>]
let ``List.toArray works``() =
         let xs = [1; 2]
         let ys = xs |> List.toArray
         ys.[0] + ys.[1]

[<Test>]
let ``List.toSeq works``() =
         let xs = [1; 2]
         let ys = xs |> List.toSeq
         ys.GetEnumerator().MoveNext()

[<Test>]
let ``List.tryFind works``() =
         let xs = [1; 2]
         let ys = xs |> List.tryFind ((=) 1)
         ys.IsSome

[<Test>]
let ``List.tryFindIndex works``() =
         let xs = [1; 2]
         let ys = xs |> List.tryFindIndex ((=) 2)
         ys.Value |> float

[<Test>]
let ``List.tryPick works``() =
         let xs = [1; 2]
         let r = xs |> List.tryPick (fun x ->
            match x with
            | 2 -> Some x
            | _ -> None)
         match r with
         | Some x -> x
         | None -> 0

[<Test>]
let ``List.unzip works``() =
         let xs = [1, 2]
         let ys, zs = xs |> List.unzip
         ys.Head + zs.Head

[<Test>]
let ``List.unzip3 works``() =
         let xs = [1, 2, 3]
         let ys, zs, ks = xs |> List.unzip3
         ys.Head + zs.Head + ks.Head

[<Test>]
let ``List.zip works``() =
         let xs = [1; 2; 3]
         let ys = [1; 2; 3]
         let zs = List.zip xs ys
         let x, y = zs.Head
         x + y

[<Test>]
let ``List.zip3 works``() =
         let xs = [1; 2; 3]
         let ys = [1; 2; 3]
         let zs = [1; 2; 3]
         let ks = List.zip3 xs ys zs
         let x, y, z = ks.Head
         x + y + z
*)