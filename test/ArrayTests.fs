[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Arrays
open NUnit.Framework
open Fabel.Tests.Util
open System.Collections.Generic

[<Test>]
let ``Array slices work``() =  
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[..2] |> Array.sum |> equal 6
    xs.[..2] <- ys
    xs |> Array.sum |> equal 39
    let xs = [| 1; 2; 3; 4; 5 |]
    xs.[4..] |> Array.sum |> equal 5
    xs.[4..] <- ys
    xs |> Array.sum |> equal 18
    let xs = [| 1; 2; 3; 4; 5 |]
    xs.[1..3] |> Array.sum |> equal 9
    xs.[1..3] <- ys
    xs |> Array.sum |> equal 30
    let xs = [|"A";"B";"C";"D"|]
    xs.[1..2] <- [|"X";"X";"X";"X"|]
    equal xs.[2] "X"
    equal xs.[3] "D"

[<Test>]
let ``Array literals work``() =  
    let x = [| 1; 2; 3; 4; 5 |]
    equal 5 x.Length

[<Test>]
let ``Array indexer getter works``() =  
    let x = [| 1.; 2.; 3.; 4.; 5. |]
    x.[2] |> equal 3.

[<Test>]
let ``Array indexer setter works``() =  
    let x = [| 1.; 2.; 3.; 4.; 5. |]
    x.[3] <- 10.
    equal 10. x.[3]

[<Test>]
let ``Array.Length works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    xs.Length |> equal 4

[<Test>]
let ``Array.zeroCreate works``() =   
    let xs = Array.zeroCreate 2
    equal 2 xs.Length
    equal 0 xs.[1]

// [<Test>]
// let ``Array.blit works``() =   
//     let xs = [|1.; 2.; 3.; 4.|]
//     let ys = Array.zeroCreate 2
//     Array.blit xs 2 ys 0 2
//     ys.[0] + ys.[1]
//     |> equal 1
// 
// [<Test>]
// let ``Array.copy works``() =   
//     let xs = [|1.; 2.; 3.; 4.|]
//     let ys = Array.copy xs
//     ys.[0] + ys.[1]
//     |> equal 1
// 
// [<Test>]
// let ``Array.sub works``() =   
//     let xs = [|1.; 2.; 3.; 4.|]
//     let ys = Array.sub xs 2 2
//     ys.[0] + ys.[1]
//     |> equal 1

// [<Test>]
// let ``Array.fill works``() =   
//     let xs = Array.zeroCreate 2
//     Array.fill xs 0 2 2.
//     xs.[0] + xs.[1]
//     |> equal 1

[<Test>]
let ``Array.empty works``() =   
    let xs = Array.empty<int>
    xs.Length |> equal 0

[<Test>]
let ``Array.append works``() =
    let xs = [|1; 2; 3; 4|]
    let ys = [|0|]
    let zs = Array.append ys xs
    zs.[0] + zs.[1]
    |> equal 1

[<Test>]
let ``Array.average works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    Array.average xs
    |> equal 2.5

[<Test>]
let ``Array.averageBy works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    Array.averageBy ((*) 2.) xs
    |> equal 5.

[<Test>]
let ``Array.choose works``() =   
    let xs = [|1L; 2L; 3L; 4L|]
    let result = xs |> Array.choose (fun x ->
       if x > 2L then Some x
       else None) 
    result.[0] + result.[1]
    |> equal 7L

[<Test>]
let ``Array.collect works``() =   
    let xs = [|[|1|]; [|2|]; [|3|]; [|4|]|]
    let ys = xs |> Array.collect id
    ys.[0] + ys.[1]
    |> equal 3

[<Test>]
let ``Array.concat works``() =   
    let xs = [|[|1.|]; [|2.|]; [|3.|]; [|4.|]|]
    let ys = xs |> Array.concat
    ys.[0] + ys.[1]
    |> equal 3.

[<Test>]
let ``Array.exists works``() =   
    let xs = [|1u; 2u; 3u; 4u|]
    xs |> Array.exists (fun x -> x = 2u)
    |> equal true

[<Test>]
let ``Array.exists2 works``() =   
    let xs = [|1UL; 2UL; 3UL; 4UL|]
    let ys = [|1UL; 2UL; 3UL; 4UL|]
    Array.exists2 (fun x y -> x * y = 16UL) xs ys
    |> equal true

[<Test>]
let ``Array.filter works``() =   
    let xs = [|1s; 2s; 3s; 4s|]
    let ys = xs |> Array.filter (fun x -> x > 2s)
    ys.Length |> equal 2

[<Test>]
let ``Array.find works``() =   
    let xs = [|1us; 2us; 3us; 4us|]
    xs |> Array.find ((=) 2us)
    |> equal 2us

[<Test>]
let ``Array.findIndex works``() =   
    let xs = [|1.f; 2.f; 3.f; 4.f|]
    xs |> Array.findIndex ((=) 2.f)
    |> equal 1

[<Test>]
let ``Array.fold works``() =   
    let xs = [|1y; 2y; 3y; 4y|]
    let total = xs |> Array.fold (+) 0y
    total |> equal 10y

[<Test>]
let ``Array.fold2 works``() =   
    let xs = [|1uy; 2uy; 3uy; 4uy|]
    let ys = [|1uy; 2uy; 3uy; 4uy|]
    let total = Array.fold2 (fun x y z -> x + y + z) 0uy xs ys
    total |> equal 20uy

[<Test>]
let ``Array.foldBack works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let total = Array.foldBack (fun x acc -> acc - x) xs 0.
    total |> equal -10.

[<Test>]
let ``Array.foldBack2 works``() =   
    let xs = [|1; 2; 3; 4|]
    let ys = [|1; 2; 3; 4|]
    let total = Array.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
    total |> equal -4

[<Test>]
let ``Array.forall works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    Array.forall ((>) 5.) xs
    |> equal true

[<Test>]
let ``Array.forall2 works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = [|1.; 2.; 3.; 5.|]
    Array.forall2 (=) xs ys
    |> equal false
    Array.forall2 (fun x y -> x <= 4. && y <= 5.) xs ys
    |> equal true

[<Test>]
let ``Array.init works``() =   
    let xs = Array.init 4 (float >> sqrt)
    xs.[0] + xs.[1]
    |> equal 1.
    (xs.[2] > 1. && xs.[3] < 2.)
    |> equal true

[<Test>]
let ``Array.isEmpty works``() =   
    Array.isEmpty [|"a"|] |> equal false
    Array.isEmpty [||] |> equal true

[<Test>]
let ``Array.iter works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let total = ref 0.
    xs |> Array.iter (fun x ->
       total := !total + x
    )
    !total |> equal 10.

[<Test>]
let ``Array.iter2 works``() =   
    let xs = [|1; 2; 3; 4|]
    let mutable total = 0
    Array.iter2 (fun x y ->
       total <- total - x - y
    ) xs xs
    total |> equal -20

[<Test>]
let ``Array.iteri works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let total = ref 0.
    xs |> Array.iteri (fun i x ->
       total := !total + (float i) * x
    )
    !total |> equal 20.

[<Test>]
let ``Array.iteri2 works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let total = ref 0.
    Array.iteri2 (fun i x y ->
       total := !total + (float i) * x + (float i) * y
    ) xs xs
    !total |> equal 40.

[<Test>]
let ``Array.length works``() =   
    let xs = [|"a"; "a"; "a"; "a"|]
    Array.length xs |> equal 4

[<Test>]
let ``Array.map works``() =   
    let xs = [|1.|]
    let ys = xs |> Array.map ((*) 2.)
    ys.[0] |> equal 2.

[<Test>]
let ``Array.map2 works``() =   
    let xs = [|1.|]
    let ys = [|2.|]
    let zs = Array.map2 (*) xs ys
    zs.[0] |> equal 2.

[<Test>]
let ``Array.mapi works``() =   
    let xs = [|1.; 2.|]
    let ys = xs |> Array.mapi (fun i x -> float i + x)
    ys.[1] |> equal 3.

[<Test>]
let ``Array.mapi2 works``() =   
    let xs = [|1.; 2.|]
    let ys = [|2.; 3.|]
    let zs = Array.mapi2 (fun i x y -> float i + x * y) xs ys
    zs.[1] |> equal 7.

[<Test>]
let ``Array.max works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.max
    |> equal 2.

[<Test>]
let ``Array.maxBy works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.maxBy (fun x -> -x)
    |> equal 1.

[<Test>]
let ``Array.min works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.min
    |> equal 1.

[<Test>]
let ``Array.minBy works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.minBy (fun x -> -x)
    |> equal 2.

[<Test>]
let ``Array.ofList works``() =   
    let xs = [1.; 2.]
    let ys = Array.ofList xs
    ys.Length |> equal 2

[<Test>]
let ``Array.ofSeq works``() =   
    let xs = seq { yield 1; yield 2 }
    let ys = Array.ofSeq xs
    ys.[0] |> equal 1

[<Test>]
let ``Array.partition works``() =   
    let xs = [|1.; 2.|]
    let ys, zs = xs |> Array.partition (fun x -> x <= 1.)
    ys.[0] - zs.[0]
    |> equal -1.

[<Test>]
let ``Array.permute works``() =   
    let xs = [|1.; 2.|]
    let ys = xs |> Array.permute (fun i -> i + 1 - 2 * (i % 2))
    ys.[0] |> equal 2.

[<Test>]
let ``Array.pick works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.pick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    |> equal 2.

[<Test>]
let ``Array.reduce works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.reduce (-)
    |> equal -8.

[<Test>]
let ``Array.reduceBack works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.reduceBack (-)
    |> equal -2.

[<Test>]
let ``Array.rev works``() =   
    let xs = [|1.; 2.|]
    let ys = xs |> Array.rev
    ys.[0] |> equal 2.

[<Test>]
let ``Array.scan works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = xs |> Array.scan (+) 0.
    ys.[2] + ys.[3]
    |> equal 9.

[<Test>]
let ``Array.scanBack works``() =   
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = Array.scanBack (-) xs 0.
    ys.[2] + ys.[3]
    |> equal 3.

[<Test>]
let ``Array.sort works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    let ys = xs |> Array.sort
    ys.[0] + ys.[1]
    |> equal 3.

[<Test>]
let ``Array.sortBy works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    let ys = xs |> Array.sortBy (fun x -> -x)
    ys.[0] + ys.[1]
    |> equal 7.

[<Test>]
let ``Array.sortWith works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    let ys = xs |> Array.sortWith (fun x y -> int(x - y))
    ys.[0] + ys.[1]
    |> equal 3.

[<Test>]
let ``Array.sortInPlace works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    Array.sortInPlace xs
    xs.[0] + xs.[1]
    |> equal 3.

[<Test>]
let ``Array.sortInPlaceBy works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    Array.sortInPlaceBy (fun x -> -x) xs
    xs.[0] + xs.[1]
    |> equal 7.

[<Test>]
let ``Array.sortInPlaceWith works``() =   
    let xs = [|3.; 4.; 1.; 2.|]
    Array.sortInPlaceWith (fun x y -> int(x - y)) xs
    xs.[0] + xs.[1]
    |> equal 3.

[<Test>]
let ``Array.sum works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.sum
    |> equal 3.

[<Test>]
let ``Array.sumBy works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.sumBy ((*) 2.)
    |> equal 6.

[<Test>]
let ``Array.toList works``() =   
    let xs = [|1.; 2.|]
    let ys = xs |> Array.toList
    ys.[0] + ys.[1]
    |> equal 3.

[<Test>]
let ``Array.toSeq works``() =   
    let xs = [|1.; 2.|]
    let ys = xs |> Array.toSeq
    ys |> Seq.head
    |> equal 1.

[<Test>]
let ``Array.tryFind works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.tryFind ((=) 1.)
    |> Option.isSome |> equal true
    xs |> Array.tryFind ((=) 3.)
    |> Option.isSome |> equal false

[<Test>]
let ``Array.tryFindIndex works``() =   
    let xs = [|1.; 2.|]
    xs |> Array.tryFindIndex ((=) 2.)
    |> equal (Some 1)
    xs |> Array.tryFindIndex ((=) 5.)
    |> equal None

[<Test>]
let ``Array.tryPick works``() =   
    let xs = [|1.; 2.|]
    let r = xs |> Array.tryPick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    match r with
    | Some x -> x
    | None -> 0.
    |> equal 2.

[<Test>]
let ``Array.unzip works``() =   
    let xs = [|1., 2.|]
    let ys, zs = xs |> Array.unzip
    ys.[0] + zs.[0]
    |> equal 3.

[<Test>]
let ``Array.unzip3 works``() =   
    let xs = [|1., 2., 3.|]
    let ys, zs, ks = xs |> Array.unzip3
    ys.[0] + zs.[0] + ks.[0]
    |> equal 6.

[<Test>]
let ``Array.zip works``() =   
    let xs = [|1.; 2.; 3.|]
    let ys = [|1.; 2.; 3.|]
    let zs = Array.zip xs ys
    let x, y = zs.[0]
    x + y |> equal 2.

[<Test>]
let ``Array.zip3 works``() =   
    let xs = [|1.; 2.; 3.|]
    let ys = [|1.; 2.; 3.|]
    let zs = [|1.; 2.; 3.|]
    let ks = Array.zip3 xs ys zs
    let x, y, z = ks.[0]
    x + y + z |> equal 3.

[<Test>]
let ``Array as IList indexer has same behaviour``() =
    let xs = [|1.; 2.; 3.|]
    let ys = xs :> _ IList
    ys.[0] <- -3.
    ys.[0] + ys.[2]
    |> equal 0.

[<Test>]
let ``Array as IList count has same behaviour``() =
    let xs = [|1.; 2.; 3.|]
    let ys = xs :> _ IList
    ys.Count |> equal 3

[<Test>]
let ``Array as IList Seq.length has same behaviour``() =
    let xs = [|1.; 2.; 3.|]
    let ys = xs :> _ IList
    ys |> Seq.length |> equal 3
