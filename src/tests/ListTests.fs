[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Lists
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``Pattern matching with lists works``() =
    match [] with [] -> true | _ -> false
    |> equal true
    match [1] with [] -> 0 | [x] -> 1 | x::xs -> 2
    |> equal 1
    match [1;2;3] with [] -> 0 | _::x::xs -> x | _ -> 3
    |> equal 2
    match [1.;2.;3.;4.] with [] -> 0 | [x] -> 1 | x::xs -> xs.Length
    |> equal 3
    match ["a";"b"] with [] -> 0 | ["a";"b"] -> 1 | _ -> 2
    |> equal 1

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
let ``List slice works``() =
      let xs = [1; 2; 3; 4]
      xs.[..2] |> List.sum |> equal 6
      xs.[2..] |> List.sum |> equal 7
      xs.[1..2] |> List.sum |> equal 5
      
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
      let list1 = [10.; 20.; 30.]
      let collectList = List.collect (fun x -> [for i in 1.0..3.0 -> x * i]) list1
      sumFirstSeq collectList 9 |> equal 360.

      let xs = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
      let ys = xs |> List.collect id
      sumFirstSeq ys 5
      |> equal 15.

[<Test>]
let ``List.concat works``() =
      let xs = [[1.; 2.; 3.]; [4.; 5.; 6.]; [7.; 8.; 9.]]
      let ys = xs |> List.concat
      sumFirstList ys 7
      |> equal 28.

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
let ``List.findBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.find ((>) 4.) |> equal 1.
    xs |> List.findBack ((>) 4.) |> equal 3.

[<Test>]
let ``List.findIndexBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.findIndex ((>) 4.) |> equal 0
    xs |> List.findIndexBack ((>) 4.) |> equal 2

[<Test>]
let ``List.tryFindBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> List.tryFindBack ((>) 4.) |> equal (Some 3.)

[<Test>]
let ``List.tryFindIndexBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> List.tryFindIndexBack ((>) 4.) |> equal (Some 2)      

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
let ``List.foldBack with composition works``() =
      [1; 2; 3; 4]
      |> List.foldBack (fun x acc -> acc >> (+) x) <| id <| 2
      |> equal 12

[<Test>]
let ``List.foldBack2 works``() =
      ([1; 2; 3; 4], [1; 2; 3; 4], 0)
      |||> List.foldBack2 (fun x y acc -> acc - y * x)
      |> equal -30

[<Test>]
let ``List.forall works``() =
      [1; 2; 3; 4]
      |> List.forall (fun x -> x < 5)
      |> equal true

[<Test>]
let ``List.forall2 works``() =
      ([1; 2; 3; 4], [1; 2; 3; 4])
      ||> List.forall2 (=)
      |> equal true

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
      List.isEmpty [1] |> equal false
      List.isEmpty [] |> equal true

[<Test>]
let ``List.iter works``() =
      let xs = [1; 2; 3; 4]
      let mutable total = 0
      xs |> List.iter (fun x ->
      total <- total + x)
      equal 10 total

[<Test>]
let ``List.iter2 works``() =
      let xs = [1; 2; 3; 4]
      let ys = [2; 4; 6; 8]
      let total = ref 0
      List.iter2 (fun x y ->
      total := !total + (y - x)
      ) xs ys
      equal 10 !total

[<Test>]
let ``List.iteri works``() =
      let mutable total = 0
      [1; 2; 3; 4]
      |> List.iteri (fun i x ->
            total <- total + (i * x))
      equal 20 total

[<Test>]
let ``List.iteri2 works``() =
      let mutable total = 0
      let xs = [1; 2; 3; 4]
      let ys = [2; 4; 6; 8]
      List.iteri2 (fun i x y ->
      total <- total + i * (y - x)
      ) xs ys
      equal 20 total

[<Test>]
let ``List.length works``() =
      let xs = [1; 2; 3; 4]
      List.length xs 
      |> equal 4

[<Test>]
let ``List.map works``() =
      let xs = [1;2;3]
      let ys = xs |> List.map ((*) 2)
      equal 4 ys.Tail.Head

[<Test>]
let ``List.mapi works``() =
      let xs = [1]
      let ys = xs |> List.mapi (fun i x -> i * x)
      equal 0 ys.Head
      
[<Test>]
let ``List.map2 works``() =
      let xs = [1;2]
      let ys = [2;3]
      let zs = List.map2 (fun x y -> x - y) xs ys
      equal -1 zs.Head

[<Test>]
let ``List.map3 works``() =
      let xs = [1;2;3]
      let ys = [5;4;3]
      let zs = [7;8;9]
      let ks = List.map3 (fun x y z -> z - y - x) xs ys zs
      List.sum ks
      |> equal 6

[<Test>]
let ``List.mapi2 works``() =
      let xs = [7;8;9]
      let ys = [5;4;3]
      let zs = List.mapi2 (fun i x y -> i * (x - y)) xs ys
      List.sum zs |> equal 16

[<Test>]
let ``List.max works``() =
      let xs = [1; 2]
      xs |> List.max
      |> equal 2

[<Test>]
let ``List.maxBy works``() =
      let xs = [1; 2]
      xs |> List.maxBy (fun x -> -x)
      |> equal 1

[<Test>]
let ``List.min works``() =
      let xs = [1; 2]
      xs |> List.min
      |> equal 1

[<Test>]
let ``List.minBy works``() =
      let xs = [1; 2]
      xs |> List.minBy (fun x -> -x)
      |> equal 2

[<Test>]
let ``List.item works``() =
      [1; 2] |> List.item 1 |> equal 2

[<Test>]
let ``List.ofArray works``() =
      let xs = [|1.; 2.; 3.; 4.|]
      let ys = List.ofArray xs
      sumFirstList ys 3 |> equal 6.

[<Test>]
let ``List.ofSeq works``() =
      // let xs = [|1; 2|] :> _ seq
      let ys = List.ofSeq <| seq { yield 1; yield 2 }
      ys.Head |> equal 1

[<Test>]
let ``List.partition works``() =
      let xs = [1; 2; 3; 4; 5; 6]
      let ys, zs = xs |> List.partition (fun x -> x % 2 = 0)
      List.sum zs |> equal 9
      equal 2 ys.[0]
      equal 5 zs.[2]
      
[<Test>]
let ``List.permute works``() =
      let xs = [1; 2; 3; 4; 5; 6]
      let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
      equal 4 ys.[2]
      equal 6 ys.[4]

[<Test>]
let ``List.pick works``() =
      let xs = [1; 2]
      xs |> List.pick (fun x ->
            match x with
            | 2 -> Some x
            | _ -> None)
      |> equal 2 

[<Test>]
let ``List.range works``() =
    [1..5]
    |> List.reduce (+)
    |> equal 15
    [0..2..9]
    |> List.reduce (+)
    |> equal 20

[<Test>]
let ``List.reduce works``() =
      let xs = [1; 2]
      xs |> List.reduce (+)
      |> equal 3

[<Test>]
let ``List.reduceBack works``() =
      let xs = [1; 2]
      xs |> List.reduceBack (+)
      |> equal 3 

[<Test>]
let ``List.replicate works``() =
      List.replicate 3 3
      |> List.sum |> equal 9

[<Test>]
let ``List.rev works``() =
      let xs = [1; 2]
      let ys = xs |> List.rev
      equal 2 ys.Head

[<Test>]
let ``List.scan works``() =
      let xs = [1; 2; 3; 4]
      let ys = (0, xs) ||> List.scan (fun acc x -> acc - x)
      ys.[3] + ys.[4]
      |> equal -16
      

[<Test>]
let ``List.scanBack works``() =
      let xs = [1; 2; 3]
      let ys = List.scanBack (fun x acc -> acc - x) xs 0
      ys.Head + ys.Tail.Head
      |> equal -11

[<Test>]
let ``List.sort works``() =
    let xs = [3; 4; 1; -3; 2; 10]
    xs |> List.sort |> List.take 3 |> List.sum |> equal 0
    let ys = ["a"; "c"; "B"; "d"]
    ys |> List.sort |> List.item 1 |> equal "a" 

[<Test>]
let ``List.sortDescending works``() =
    let xs = [3; 4; 1; -3; 2; 10]
    xs |> List.sortDescending |> List.take 3 |> List.sum |> equal 17
    let ys = ["a"; "c"; "B"; "d"]
    ys |> List.sortDescending |> List.item 1 |> equal "c"      

[<Test>]
let ``List.sortBy works``() =
      let xs = [3; 1; 4; 2]
      let ys = xs |> List.sortBy (fun x -> -x)
      ys.Head + ys.Tail.Head
      |> equal 7

[<Test>]
let ``List.sortWith works``() =
      let xs = [3; 4; 1; 2]
      let ys = xs |> List.sortWith (fun x y -> int(x - y))
      ys.Head + ys.Tail.Head
      |> equal 3

[<Test>]
let ``List.sum works``() =
      [1; 2] |> List.sum
      |> equal 3

[<Test>]
let ``List.sumBy works``() =
      [1; 2] |> List.sumBy (fun x -> x*2)
      |> equal 6

[<Test>]
let ``List.tail works``() =
      let xs = [1; 2]
      let ys = xs |> List.tail
      equal 1 ys.Length

[<Test>]
let ``List.toArray works``() =
      let xs = [1; 2]
      let ys = xs |> List.toArray
      ys.[0] + ys.[1]
      |> equal 3

[<Test>]
let ``List.toSeq works``() =
      [1; 2]
      |> List.toSeq
      |> Seq.tail |> Seq.head
      |> equal 2
      
[<Test>]
let ``List.tryPick works``() =
      [1; 2]
      |> List.tryPick (function
            | 2 -> Some 2
            | _ -> None)
      |> function
      | Some x -> x
      | None -> 0
      |> equal 2

[<Test>]
let ``List.tryFind works``() =
      [1; 2]
      |> List.tryFind ((=) 5)
      |> equal None

[<Test>]
let ``List.tryFindIndex works``() =
      let xs = [1; 2]
      let ys = xs |> List.tryFindIndex ((=) 2)
      ys.Value |> equal 1

[<Test>]
let ``List.unzip works``() =
      let xs = [1, 2]
      let ys, zs = xs |> List.unzip
      ys.Head + zs.Head
      |> equal 3

[<Test>]
let ``List.unzip3 works``() =
      let xs = [(1, 2, 3); (4, 5, 6)]
      let ys, zs, ks = xs |> List.unzip3
      ys.[1] + zs.[1] + ks.[1]
      |> equal 15

[<Test>]
let ``List.zip works``() =
      let xs = [1; 2; 3]
      let ys = [4; 5; 6]
      let zs = List.zip xs ys
      let x, y = zs.Tail.Head
      equal 2 x
      equal 5 y

[<Test>]
let ``List.zip3 works``() =
      let xs = [1; 2; 3]
      let ys = [4; 5; 6]
      let zs = [7; 8; 9]
      let ks = List.zip3 xs ys zs
      let x, y, z = List.last ks
      equal 3 x
      equal 6 y
      equal 9 z

[<Test>]
let ``List "snail" to append works``() =
      let xs = [1; 2; 3; 4]
      let ys = [0]
      let zs = ys @ xs
      zs.Head + zs.Tail.Head
      |> equal 1

[<Test>]
let ``List.tryItem works``() =
    let xs = [1.; 2.; 3.; 4.]
    List.tryItem 3 xs |> equal (Some 4.)
    List.tryItem 4 xs |> equal None
    List.tryItem -1 xs |> equal None

[<Test>]
let ``List.tryHead works``() =
    let xs = [1.; 2.; 3.; 4.]
    List.tryHead xs |> equal (Some 1.)
    List.tryHead [] |> equal None

[<Test>]
let ``List.last works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.last
    |> equal 4.
    
[<Test>]
let ``List.tryLast works``() =
    let xs = [1.; 2.; 3.; 4.]
    List.tryLast xs |> equal (Some 4.)
    List.tryLast [] |> equal None
