[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Arrays

open System
open NUnit.Framework
open Fable.Tests.Util
open System.Collections.Generic
open Fable.Core

// TODO
// [<Test>]
// let ``Pattern matching with arrays works``() =
//     match [||] with [||] -> true | _ -> false
//     |> equal true
//     match [|1|] with [||] -> 0 | [|x|] -> 1 | _ -> 2
//     |> equal 1
//     match [|"a";"b"|] with [||] -> 0 | [|"a";"b"|] -> 1 | _ -> 2
//     |> equal 1

type ParamArrayTest =
    static member Add([<ParamArray>] xs: int[]) = Array.sum xs

let add (xs: int[]) = ParamArrayTest.Add(xs)

[<Test>]
let ``ParamArrayAttribute works``() =  
    ParamArrayTest.Add(1, 2) |> equal 3

[<Test>]
let ``Passing an array to ParamArrayAttribute works``() =  
    ParamArrayTest.Add([|3; 2|]) |> equal 5

[<Test>]
let ``Passing an array to ParamArrayAttribute from another function works``() =  
    add [|5;-7|] |> equal -2

#if MOCHA
[<Emit("$1.constructor.name == $0")>]
let jsConstructorIs (s: string) (ar: 'T[]) = true 

[<Test>]
let ``Typed Arrays work``() =  
    let xs = [| 1; 2; 3; |]
    let ys = [| 1.; 2.; 3.; |]
    let zs = [| "This is a string" |]
    xs |> jsConstructorIs "Int32Array" |> equal true
    ys |> jsConstructorIs "Float64Array" |> equal true
    zs |> jsConstructorIs "Array" |> equal true

[<Test>]
let ``Mapping from Typed Arrays work``() =  
    [| 1; 2; 3; |]
    |> Array.map string
    |> jsConstructorIs "Int32Array"
    |> equal false

[<Test>]
let ``Mapping to Typed Arrays work``() =  
    [| "1"; "2"; "3"; |]
    |> Array.map int
    |> jsConstructorIs "Int32Array"
    |> equal true
    
    [| 1; 2; 3; |]
    |> Array.map float
    |> jsConstructorIs "Float64Array"
    |> equal true
#endif

let f (x:obj) (y:obj) (z:obj) = (string x) + (string y) + (string z)

[<Test>]
let ``Mapping from values to functions works``() =  
    let a = [| "a"; "b"; "c" |]
    let b = [| 1; 2; 3 |]
    let concaters1 = a |> Array.map (fun x y -> y + x)
    let concaters2 = a |> Array.map (fun x -> (fun y -> y + x))
    let concaters3 = a |> Array.map (fun x -> let f = (fun y -> y + x) in f)
    let concaters4 = a |> Array.map f
    let concaters5 = b |> Array.mapi f
    concaters1.[0] "x" |> equal "xa"
    concaters2.[1] "x" |> equal "xb"
    concaters3.[2] "x" |> equal "xc"
    concaters4.[0] "x" "y" |> equal "axy"
    concaters5.[1] "x" |> equal "12x"
    let f2 = f
    a |> Array.mapi f2 |> Array.item 2 <| "x" |> equal "2cx"

[<Test>]
let ``Byte arrays are not clamped by default``() =
    let ar = [|5uy|]
    ar.[0] <- ar.[0] + 255uy
    equal 4uy ar.[0]

#if MOCHA
[<Test>]
let ``Clamped byte arrays work``() =
    let ar = Util2.Helper2.CreateClampedArray()
    ar.[0] <- ar.[0] + 255uy
    equal 255uy ar.[0]
#endif

[<Test>]
let ``Array slice with upper index work``() =  
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[..2] |> Array.sum |> equal 6
    xs.[..2] <- ys
    xs |> Array.sum |> equal 39
    
[<Test>]
let ``Array slice with lower index work``() =  
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[4..] |> Array.sum |> equal 11
    xs.[4..] <- ys
    xs |> Array.sum |> equal 26
    
[<Test>]
let ``Array slice with both indices work``() =  
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[1..3] |> Array.sum |> equal 9
    xs.[1..3] <- ys
    xs |> Array.sum |> equal 36
    
[<Test>]
let ``Array slice with non-numeric arrays work``() =
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

[<Test>]
let ``Array.create works``() =   
    let xs = Array.create 2 5
    equal 2 xs.Length
    Array.sum xs |> equal 10

[<Test>]
let ``Array.blit works``() =   
    let xs = [|1..10|]
    let ys = Array.zeroCreate 20
    Array.blit xs 3 ys 5 4        // [|0; 0; 0; 0; 0; 4; 5; 6; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
    ys.[5] + ys.[6] + ys.[7] + ys.[8] |> equal 22

[<Test>]
let ``Array.copy works``() =   
    let xs = [|1; 2; 3; 4|]
    let ys = Array.copy xs
    xs.[0] <- 0                   // Ensure a deep copy
    ys |> Array.sum |> equal 10

[<Test>]
let ``Array.sub works``() =   
    let xs = [|0..99|]
    let ys = Array.sub xs 5 10    // [|5; 6; 7; 8; 9; 10; 11; 12; 13; 14|]
    ys |> Array.sum |> equal 95

[<Test>]
let ``Array.fill works``() =   
    let xs = Array.zeroCreate 4   // [|0; 0; 0; 0|]
    Array.fill xs 1 2 3           // [|0; 3; 3; 0|]
    xs |> Array.sum |> equal 6

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
let ``Array.findBack works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.find ((>) 4.) |> equal 1.
    xs |> Array.findBack ((>) 4.) |> equal 3.

[<Test>]
let ``Array.findIndexBack works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.findIndex ((>) 4.) |> equal 0
    xs |> Array.findIndexBack ((>) 4.) |> equal 2

[<Test>]
let ``Array.tryFindBack works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> Array.tryFindBack ((>) 4.) |> equal (Some 3.)

[<Test>]
let ``Array.tryFindIndexBack works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> Array.tryFindIndexBack ((>) 4.) |> equal (Some 2)    

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
let ``Array.range works``() =
    [|1..5|]
    |> Array.reduce (+)
    |> equal 15
    [|0..2..9|]
    |> Array.reduce (+)
    |> equal 20

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
    let xs = [|3; 4; 1; -3; 2; 10|]
    let ys = [|"a"; "c"; "B"; "d"|]
    xs |> Array.sort |> Array.take 3 |> Array.sum |> equal 0
    ys |> Array.sort |> Array.item 1 |> equal "a" 

[<Test>]
let ``Array.sortDescending works``() =
    let xs = [|3; 4; 1; -3; 2; 10|]
    let ys = [|"a"; "c"; "B"; "d"|]
    xs |> Array.sortDescending |> Array.take 3 |> Array.sum |> equal 17
    ys |> Array.sortDescending |> Array.item 1 |> equal "c"   

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
    let xs = [|3.; 4.; 1.; 2.; 10.|]
    Array.sortInPlace xs
    xs.[0] + xs.[1]
    |> equal 3.

[<Test>]
let ``Array.sortInPlaceBy works``() =   
    let xs = [|3.; 4.; 1.; 2.; 10.|]
    Array.sortInPlaceBy (fun x -> -x) xs
    xs.[0] + xs.[1]
    |> equal 14.

[<Test>]
let ``Array.sortInPlaceWith works``() =   
    let xs = [|3.; 4.; 1.; 2.; 10.|]
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

[<Test>]
let ``Mapping with typed arrays doesn't coerce``() =
    let data = [| 1 .. 12 |]
    let page size page data =
        data
        |> Array.skip ((page-1) * size)
        |> Array.take size
    let test1 =
        [| 1..4 |]
        |> Array.map (fun x -> page 3 x data)
    let test2 =
        [| 1..4 |]
        |> Seq.map (fun x -> page 3 x data)
        |> Array.ofSeq
    test1 |> Array.concat |> Array.sum |> equal 78
    test2 |> Array.concat |> Array.sum |> equal 78

[<Test>]
let ``Array.item works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.item 2 xs |> equal 3.

[<Test>]
let ``Array.tryItem works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.tryItem 3 xs |> equal (Some 4.)
    Array.tryItem 4 xs |> equal None
    Array.tryItem -1 xs |> equal None

[<Test>]
let ``Array.head works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.head xs |> equal 1.

[<Test>]
let ``Array.tryHead works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.tryHead xs |> equal (Some 1.)
    Array.tryHead [||] |> equal None

[<Test>]
let ``Array.last works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    xs |> Array.last
    |> equal 4.
    
[<Test>]
let ``Array.tryLast works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.tryLast xs |> equal (Some 4.)
    Array.tryLast [||] |> equal None

[<Test>]
let ``Array.tail works``() =
    let xs = [|1.; 2.; 3.; 4.|]
    Array.tail xs |> Array.length |> equal 3
