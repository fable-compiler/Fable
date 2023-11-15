module Fable.Tests.Dart.Array

open System
open Util

type ParamArrayTest =
    static member Add([<ParamArray>] xs: int[]) = Array.sum xs

let add (xs: int[]) = ParamArrayTest.Add(xs)

type ExceptFoo = { Bar:string }

let f (x:obj) (y:obj) (z:obj) = (string x) + (string y) + (string z)

let map f ar = Array.map f ar

type Point =
    { x: int; y: int }
    static member Zero = { x=0; y=0 }
    static member Neg(p: Point) = { x = -p.x; y = -p.y }
    static member (+) (p1, p2) = { x= p1.x + p2.x; y = p1.y + p2.y }

type MyNumber =
    | MyNumber of int
    static member Zero = MyNumber 0
    static member (+) (MyNumber x, MyNumber y) =
        MyNumber(x + y)
    static member DivideByInt (MyNumber x, i: int) =
        MyNumber(x / i)

type MyNumberWrapper =
    { myNumber: MyNumber }

type Things =
    { MainThing: int
      OtherThing: string }

let tests () =
    testCase "Pattern matching with arrays works" <| fun () ->
        match [||] with [||] -> true | _ -> false
        |> equal true
        match [|1|] with [||] -> 0 | [|_|] -> 1 | _ -> 2
        |> equal 1
        match [|"a";"b"|] with [|"a";"b"|] -> 1 | _ -> 2
        |> equal 1

    testCase "ParamArrayAttribute works" <| fun () ->
        ParamArrayTest.Add(1, 2) |> equal 3

    testCase "Passing an array to ParamArrayAttribute works" <| fun () ->
        ParamArrayTest.Add([|3; 2|]) |> equal 5

    testCase "Passing an array to ParamArrayAttribute from another function works" <| fun () ->
        add [|5;-7|] |> equal -2

    testCase "Can spread a complex expression" <| fun _ ->
        let sideEffect (ar: int[]) = ar.[1] <- 5
        ParamArrayTest.Add(let ar = [|1;2;3|] in sideEffect ar; ar)
        |> equal 9

    testCase "Mapping from values to functions works" <| fun () ->
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

    testCase "Mapping from typed arrays to non-numeric arrays doesn't coerce values" <| fun () -> // See #120, #171
        let xs = map string [|1;2|]
        (box xs.[0]) :? string |> equal true
        let xs2 = Array.map string [|1;2|]
        (box xs2.[1]) :? string |> equal true

    testCase "Array slice with upper index work" <| fun () ->
        let xs = [| 1; 2; 3; 4; 5; 6 |]
        let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
        xs.[..2] |> Array.sum |> equal 6
        xs.[..2] <- ys
        xs |> Array.sum |> equal 39

    testCase "Array slice with lower index work" <| fun () ->
        let xs = [| 1; 2; 3; 4; 5; 6 |]
        let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
        xs.[4..] |> Array.sum |> equal 11
        xs.[4..] <- ys
        xs |> Array.sum |> equal 26

    testCase "Array slice with both indices work" <| fun () ->
        let xs = [| 1; 2; 3; 4; 5; 6 |]
        let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
        xs.[1..3] |> Array.sum |> equal 9
        xs.[1..3] <- ys
        xs |> Array.sum |> equal 36

    testCase "Array slice with non-numeric arrays work" <| fun () ->
        let xs = [|"A";"B";"C";"D"|]
        xs.[1..2] <- [|"X";"X";"X";"X"|]
        equal xs.[2] "X"
        equal xs.[3] "D"

    testCase "Array slice works for slices of length 1 starting at 0" <| fun () -> // See #2844
        let a = [|1;2;3;4;5;6|]
        a.[0..0] <- [|999|]
        a.[3..3] <- [|999|]
        a |> equal [|999; 2; 3; 999; 5; 6|]

    testCase "Array literals work" <| fun () ->
        let x = [| 1; 2; 3; 4; 5 |]
        equal 5 x.Length

    testCase "Array indexer getter works" <| fun () ->
        let x = [| 1.; 2.; 3.; 4.; 5. |]
        x.[2] |> equal 3.

    testCase "Array indexer setter works" <| fun () ->
        let x = [| 1.; 2.; 3.; 4.; 5. |]
        x.[3] <- 10.
        equal 10. x.[3]

    testCase "Array getter works" <| fun () ->
        let x = [| 1.; 2.; 3.; 4.; 5. |]
        Array.get x 2 |> equal 3.

    testCase "Array setter works" <| fun () ->
        let x = [| 1.; 2.; 3.; 4.; 5. |]
        Array.set x 3 10.
        equal 10. x.[3]

    testCase "Array.Length works" <| fun () ->
        let xs = [| 1.; 2.; 3.; 4. |]
        xs.Length |> equal 4

    testCase "Array.ConvertAll works" <| fun () ->
        let xs = [| 1.; 2.; 3.; 4. |]
        let ys = System.Array.ConvertAll(xs, System.Converter(fun x -> int x))
        ys |> Seq.toList |> equal [1;2;3;4]

    testCase "Array.zeroCreate works with ints" <| fun () ->
        let xs = Array.zeroCreate 2
        equal 2 xs.Length
        equal 0 xs.[1]

    // See https://github.com/fable-compiler/repl/issues/96
    testCase "Array.zeroCreate works with KeyValuePair" <| fun () ->
        let a = Array.zeroCreate<System.Collections.Generic.KeyValuePair<float,bool>> 3
        equal 0. a.[1].Key
        equal false a.[2].Value

    testCase "Array.create works" <| fun () ->
        let xs = Array.create 2 5
        equal 2 xs.Length
        Array.sum xs |> equal 10

    testCase "Array.blit works" <| fun () ->
        let xs = [| 1..10 |]
        let ys = Array.zeroCreate 20
        Array.blit xs 3 ys 5 4        // [|0; 0; 0; 0; 0; 4; 5; 6; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
        ys.[5] + ys.[6] + ys.[7] + ys.[8] |> equal 22

    testCase "Array.blit works with non typed arrays" <| fun () ->
        let xs = [| 'a'..'h' |] |> Array.map string
        let ys = Array.zeroCreate 20
        Array.blit xs 3 ys 5 4
        ys.[5] + ys.[6] + ys.[7] + ys.[8] |> equal "defg"

    testCase "Array.copy works" <| fun () ->
        let xs = [| 1; 2; 3; 4 |]
        let ys = Array.copy xs
        xs.[0] <- 0                   // Ensure a deep copy
        ys |> Array.sum |> equal 10

    testCase "Array.distinct works" <| fun () ->
        let xs = [| 1; 1; 1; 2; 2; 3; 3 |]
        let ys = xs |> Array.distinct
        ys |> Array.length |> equal 3
        ys |> Array.sum |> equal 6

    testCase "Array.distinct with tuples works" <| fun () ->
        let xs = [|(1, 2); (2, 3); (1, 2)|]
        let ys = xs |> Array.distinct
        ys |> Array.length |> equal 2
        ys |> Array.sumBy fst |> equal 3

    testCase "Array.distinctBy works" <| fun () ->
        let xs = [| 4; 4; 4; 6; 6; 5; 5 |]
        let ys = xs |> Array.distinctBy (fun x -> x % 2)
        ys |> Array.length |> equal 2
        ys |> Array.head >= 4 |> equal true

    testCase "Array.distinctBy with tuples works" <| fun () ->
          let xs = [| 4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7 |]
          let ys = xs |> Array.distinctBy (fun (x,_) -> x % 2)
          ys |> Array.length |> equal 2
          ys |> Array.head |> fst >= 4 |> equal true

    testCase "Array distinctBy works on large array" <| fun () ->
        let xs = [| 0 .. 50000 |]
        let ys =
            Array.append xs xs
            |> Array.distinctBy(fun x -> x.ToString())
        ys |> equal xs

    testCase "Array.sub works" <| fun () ->
        let xs = [|0..99|]
        let ys = Array.sub xs 5 10    // [|5; 6; 7; 8; 9; 10; 11; 12; 13; 14|]
        ys |> Array.sum |> equal 95

    testCase "Array.fill works" <| fun () ->
        let xs = Array.zeroCreate 4   // [|0; 0; 0; 0|]
        Array.fill xs 1 2 3           // [|0; 3; 3; 0|]
        xs |> Array.sum |> equal 6

    testCase "Array.empty works" <| fun () ->
        let xs = Array.empty<int>
        xs.Length |> equal 0

    testCase "Array.append works" <| fun () ->
        let xs1 = [|1; 2; 3; 4|]
        let zs1 = Array.append [|0|] xs1
        zs1.[0] + zs1.[1] |> equal 1
        let xs2 = [|"a"; "b"; "c"|]
        let zs2 = Array.append [|"x";"y"|] xs2
        zs2.[1] + zs2.[3] |> equal "yb"

    testCase "Array.average works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.average xs
        |> equal 2.5

    testCase "Array.averageBy works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.averageBy (fun x -> x * 2.) xs
        |> equal 5.

    testCase "Array.average works with custom types" <| fun () ->
        [|MyNumber 1; MyNumber 2; MyNumber 3|] |> Array.average |> equal (MyNumber 2)

    testCase "Array.averageBy works with custom types" <| fun () ->
        [|{ myNumber = MyNumber 5 }; { myNumber = MyNumber 4 }; { myNumber = MyNumber 3 }|]
        |> Array.averageBy (fun x -> x.myNumber) |> equal (MyNumber 4)

    testCase "Array.choose with ints works" <| fun () ->
        let xs = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
        let result = xs |> Array.choose (fun i ->
            if i % 2 = 1 then Some i
            else None)
        result.Length |> equal 5

    testCase "Array.choose with longs works" <| fun () ->
        let xs = [|1L; 2L; 3L; 4L|]
        let result = xs |> Array.choose (fun x ->
           if x > 2L then Some x
           else None)
        result.[0] + result.[1]
        |> equal 7L

    testCase "Array.choose must construct array of output type" <| fun () -> // See #1658
        let source = [|1; 3; 5; 7|]
        let target = source |> Array.choose (fun x -> Some { MainThing=x; OtherThing="asd" })
        target.[3].MainThing |> equal 7

    testCase "Array.collect works" <| fun () ->
        let xs = [|[|1|]; [|2|]; [|3|]; [|4|]|]
        let ys = xs |> Array.collect id
        ys.[0] + ys.[1]
        |> equal 3

        let xs1 = [|[|1.; 2.|]; [|3.|]; [|4.; 5.; 6.;|]; [|7.|]|]
        let ys1 = xs1 |> Array.collect id
        ys1.[0] + ys1.[1] + ys1.[2] + ys1.[3] + ys1.[4]
        |> equal 15.

    testCase "Array.concat works" <| fun () ->
        let xs = [|[|1.|]; [|2.|]; [|3.|]; [|4.|]|]
        let ys = xs |> Array.concat
        ys.[0] + ys.[1]
        |> equal 3.

    testCase "Array.concat works with strings" <| fun () ->
        [| [| "One" |]; [| "Two" |] |]
        |> Array.concat
        |> List.ofArray
        |> equal [ "One"; "Two" ]

    testCase "Array.exists works" <| fun () ->
        let xs = [|1u; 2u; 3u; 4u|]
        xs |> Array.exists (fun x -> x = 2u)
        |> equal true

    testCase "Array.exists2 works" <| fun () ->
        let xs = [|1UL; 2UL; 3UL; 4UL|]
        let ys = [|1UL; 2UL; 3UL; 4UL|]
        Array.exists2 (fun x y -> x * y = 16UL) xs ys
        |> equal true

    testCase "Array.filter works" <| fun () ->
        let xs = [|1s; 2s; 3s; 4s|]
        let ys = xs |> Array.filter (fun x -> x > 2s)
        ys.Length |> equal 2

// TODO: Char.IsLetter
    // testCase "Array.filter with chars works" <| fun () ->
    //     let xs = [|'a'; '2'; 'b'; 'c'|]
    //     let ys = xs |> Array.filter Char.IsLetter
    //     ys.Length |> equal 3

    testCase "Array.find works" <| fun () ->
        let xs = [|1us; 2us; 3us; 4us|]
        xs |> Array.find ((=) 2us)
        |> equal 2us

    testCase "Array.findIndex works" <| fun () ->
        let xs = [|1.f; 2.f; 3.f; 4.f|]
        xs |> Array.findIndex ((=) 2.f)
        |> equal 1

    testCase "Array.findBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.find ((>) 4.) |> equal 1.
        xs |> Array.findBack ((>) 4.) |> equal 3.

    testCase "Array.findIndexBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.findIndex ((>) 4.) |> equal 0
        xs |> Array.findIndexBack ((>) 4.) |> equal 2

    testCase "Array.tryFindBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.tryFind ((>) 4.) |> equal (Some 1.)
        xs |> Array.tryFindBack ((>) 4.) |> equal (Some 3.)
        xs |> Array.tryFindBack ((=) 5.) |> equal None

    testCase "Array.tryFindIndexBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.tryFindIndex ((>) 4.) |> equal (Some 0)
        xs |> Array.tryFindIndexBack ((>) 4.) |> equal (Some 2)
        xs |> Array.tryFindIndexBack ((=) 5.) |> equal None

    testCase "Array.fold works" <| fun () ->
        let xs = [|1y; 2y; 3y; 4y|]
        let total = xs |> Array.fold (+) 0y
        total |> equal 10y

    testCase "Array.fold2 works" <| fun () ->
        let xs = [|1uy; 2uy; 3uy; 4uy|]
        let ys = [|1uy; 2uy; 3uy; 4uy|]
        let total = Array.fold2 (fun x y z -> x + y + z) 0uy xs ys
        total |> equal 20uy

    testCase "Array.foldBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let total = Array.foldBack (fun x acc -> acc - x) xs 0.
        total |> equal -10.

    testCase "Array.foldBack2 works" <| fun () ->
        let xs = [|1; 2; 3; 4|]
        let ys = [|1; 2; 3; 4|]
        let total = Array.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
        total |> equal -4

    testCase "Array.forall works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.forall ((>) 5.) xs
        |> equal true

    testCase "Array.forall2 works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let ys = [|1.; 2.; 3.; 5.|]
        Array.forall2 (=) xs ys
        |> equal false
        Array.forall2 (fun x y -> x <= 4. && y <= 5.) xs ys
        |> equal true

    testCase "Array.init works" <| fun () ->
        let xs = Array.init 4 (float >> sqrt)
        xs.[0] + xs.[1]
        |> equal 1.
        (xs.[2] > 1. && xs.[3] < 2.)
        |> equal true

    testCase "Array.isEmpty works" <| fun () ->
        Array.isEmpty [|"a"|] |> equal false
        Array.isEmpty [||] |> equal true

    testCase "Array.iter works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let total = ref 0.
        xs |> Array.iter (fun x ->
           total := !total + x
        )
        !total |> equal 10.

    testCase "Array.iter2 works" <| fun () ->
        let xs = [|1; 2; 3; 4|]
        let mutable total = 0
        Array.iter2 (fun x y ->
           total <- total - x - y
        ) xs xs
        total |> equal -20

    testCase "Array.iteri works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let total = ref 0.
        xs |> Array.iteri (fun i x ->
           total := !total + (float i) * x
        )
        !total |> equal 20.

    testCase "Array.iteri2 works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let total = ref 0.
        Array.iteri2 (fun i x y ->
           total := !total + (float i) * x + (float i) * y
        ) xs xs
        !total |> equal 40.

    testCase "Array.length works" <| fun () ->
        let xs = [|"a"; "a"; "a"; "a"|]
        Array.length xs |> equal 4

    testCase "Array.countBy works" <| fun () ->
        let xs = [|1; 2; 3; 4|]
        xs |> Array.countBy (fun x -> x % 2)
        |> Array.length |> equal 2

    testCase "Array.map works" <| fun () ->
        let xs = [|1.|]
        let ys = xs |> Array.map (fun x -> x * 2.)
        ys.[0] |> equal 2.

    testCase "Array.map doesn't execute side effects twice" <| fun () -> // See #1140
        let mutable c = 0
        let i () = c <- c + 1; c
        [| i (); i (); i () |] |> Array.map (fun x -> x + 1) |> ignore
        equal 3 c

    testCase "Array.map2 works" <| fun () ->
        let xs = [|1.|]
        let ys = [|2.|]
        let zs = Array.map2 (*) xs ys
        zs.[0] |> equal 2.

    testCase "Array.map3 works" <| fun () ->
        let value1 = [|1.|]
        let value2 = [|2.|]
        let value3 = [|3.|]
        let zs = Array.map3 (fun a b c -> a * b * c) value1 value2 value3
        zs.[0] |> equal 6.

    testCase "Array.mapi works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.mapi (fun i x -> float i + x)
        ys.[1] |> equal 3.

    testCase "Array.mapi2 works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = [|2.; 3.|]
        let zs = Array.mapi2 (fun i x y -> float i + x * y) xs ys
        zs.[1] |> equal 7.

    testCase "Array.mapFold works" <| fun () ->
        let xs = [|1y; 2y; 3y; 4y|]
        let result = xs |> Array.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
        fst result |> Array.sum |> equal 20y
        snd result |> equal 10y

    testCase "Array.mapFoldBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let result = Array.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
        fst result |> Array.sum |> equal -20.
        snd result |> equal -10.

    testCase "Array.max works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.max
        |> equal 2.

    testCase "Array.maxBy works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.maxBy (fun x -> -x)
        |> equal 1.

    testCase "Array.min works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.min
        |> equal 1.

    testCase "Array.minBy works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.minBy (fun x -> -x)
        |> equal 2.

    testCase "Array.ofList works" <| fun () ->
        let xs = [1.; 2.]
        let ys = Array.ofList xs
        ys.Length |> equal 2

    testCase "Array.ofSeq works" <| fun () ->
        let xs = seq { yield 1; yield 2 }
        let ys = Array.ofSeq xs
        ys.[0] |> equal 1

    testCase "Array.partition works" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys, zs = xs |> Array.partition (fun x -> x <= 1.)
        equal ys [| 1. |]
        equal zs [| 2.; 3. |]

    testCase "Array.permute works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.permute (fun i -> i + 1 - 2 * (i % 2))
        ys.[0] |> equal 2.

    testCase "Array.pick works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.pick (fun x ->
           match x with
           | 2. -> Some x
           | _ -> None)
        |> equal 2.

    testCase "Array.range works" <| fun () ->
        [|1..5|]
        |> Array.reduce (+)
        |> equal 15
        [|0..2..9|]
        |> Array.reduce (+)
        |> equal 20

    testCase "Array.reduce works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.reduce (-)
        |> equal -8.

    testCase "Array.reduce Array.append works" <| fun _ -> // See #2372
        let nums =
            [|
                [| 0 |]
                [| 1 |]
            |]
        Array.reduce Array.append nums |> equal [|0; 1|]

        let nums2d =
            [|
                [| [| 0 |] |]
                [| [| 1 |] |]
            |]
        Array.reduce Array.append nums2d
        |> equal [|[|0|]; [|1|]|]

        let strs =
            [|
                [| "a" |]
                [| "b" |]
            |]
        Array.reduce Array.append strs
        |> equal [|"a"; "b"|]

    testCase "Array.reduceBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.reduceBack (-)
        |> equal -2.

    testCase "Array.rev works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.rev
        xs.[0] |> equal 1. // Make sure there is no side effects
        ys.[0] |> equal 2.

    testCase "Array.scan works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let ys = xs |> Array.scan (+) 0.
        ys.[2] + ys.[3]
        |> equal 9.

    testCase "Array.scanBack works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        let ys = Array.scanBack (-) xs 0.
        ys.[2] + ys.[3]
        |> equal 3.

    testCase "Array.sort works" <| fun () ->
        let xs = [|3; 4; 1; -3; 2; 10|]
        let ys = [|"a"; "c"; "B"; "d"|]
        xs |> Array.sort |> Array.take 3 |> Array.sum |> equal 0
        ys |> Array.sort |> Array.item 1 |> equal "a"

    testCase "Array.sort with tuples works" <| fun () ->
        let xs = [|3; 1; 1; -3|]
        let ys = [|"a"; "c"; "B"; "d"|]
        (xs, ys) ||> Array.zip |> Array.sort |> Array.item 1 |> equal (1, "B")

    testCase "Array.truncate works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.; 5.|]
        xs |> Array.truncate 2
        |> Array.last
        |> equal 2.

        xs.Length |> equal 5 // Make sure there is no side effects

        // Array.truncate shouldn't throw an exception if there're not enough elements
        try xs |> Array.truncate 20 |> Array.length with _ -> -1
        |> equal 5

    testCase "Array.sortDescending works" <| fun () ->
        let xs = [|3; 4; 1; -3; 2; 10|]
        xs |> Array.sortDescending |> Array.take 3 |> Array.sum |> equal 17
        xs.[0] |> equal 3  // Make sure there is no side effects

        let ys = [|"a"; "c"; "B"; "d"|]
        ys |> Array.sortDescending |> Array.item 1 |> equal "c"

    testCase "Array.sortBy works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.|]
        let ys = xs |> Array.sortBy (fun x -> -x)
        ys.[0] + ys.[1]
        |> equal 7.

    testCase "Array.sortByDescending works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.|]
        let ys = xs |> Array.sortByDescending (fun x -> -x)
        ys.[0] + ys.[1]
        |> equal 3.

    testCase "Array.sortWith works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.|]
        let ys = xs |> Array.sortWith (fun x y -> int(x - y))
        ys.[0] + ys.[1]
        |> equal 3.

    testCase "Array.sortInPlace works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.; 10.|]
        Array.sortInPlace xs
        xs.[0] + xs.[1]
        |> equal 3.

    testCase "Array.sortInPlaceBy works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.; 10.|]
        Array.sortInPlaceBy (fun x -> -x) xs
        xs.[0] + xs.[1]
        |> equal 14.

    testCase "Array.sortInPlaceWith works" <| fun () ->
        let xs = [|3.; 4.; 1.; 2.; 10.|]
        Array.sortInPlaceWith (fun x y -> int(x - y)) xs
        xs.[0] + xs.[1]
        |> equal 3.

    testCase "Array.sum works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.sum
        |> equal 3.

    testCase "Array.sumBy works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.sumBy (fun x -> x * 2.)
        |> equal 6.

    testCase "Array.sum with non numeric types works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [|p1; p2|] |> Array.sum |> equal {x=3;y=30}

    testCase "Array.sumBy with non numeric types works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [|p1; p2|] |> Array.sumBy Point.Neg |> equal {x = -3; y = -30}

    testCase "Array.sumBy with numeric projection works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [|p1; p2|] |> Array.sumBy (fun p -> p.y) |> equal 30

    testCase "Array.sum with non numeric types works II" <| fun () ->
        [|MyNumber 1; MyNumber 2; MyNumber 3|] |> Array.sum |> equal (MyNumber 6)

    testCase "Array.sumBy with non numeric types works II" <| fun () ->
        [|{ myNumber = MyNumber 5 }; { myNumber = MyNumber 4 }; { myNumber = MyNumber 3 }|]
        |> Array.sumBy (fun x -> x.myNumber) |> equal (MyNumber 12)

    testCase "Array.toList works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.toList
        ys.[0] + ys.[1]
        |> equal 3.

    testCase "Array.toSeq works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = xs |> Array.toSeq
        ys |> Seq.head
        |> equal 1.

    testCase "Array.tryFind works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.tryFind ((=) 1.)
        |> Option.isSome |> equal true
        xs |> Array.tryFind ((=) 3.)
        |> Option.isSome |> equal false

    testCase "Array.tryFindIndex works" <| fun () ->
        let xs = [|1.; 2.|]
        xs |> Array.tryFindIndex ((=) 2.)
        |> equal (Some 1)
        xs |> Array.tryFindIndex ((=) 3.)
        |> equal None

    testCase "Array.tryPick works" <| fun () ->
        let xs = [|1.; 2.|]
        let r = xs |> Array.tryPick (fun x ->
           match x with
           | 2. -> Some x
           | _ -> None)
        match r with
        | Some x -> x
        | None -> 0.
        |> equal 2.

    testCase "Array.unzip works" <| fun () ->
        let xs = [|1., 2.|]
        let ys, zs = xs |> Array.unzip
        ys.[0] + zs.[0]
        |> equal 3.

    testCase "Array.unzip3 works" <| fun () ->
        let xs = [|1., 2., 3.|]
        let ys, zs, ks = xs |> Array.unzip3
        ys.[0] + zs.[0] + ks.[0]
        |> equal 6.

    testCase "Array.zip works" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys = [|1.; 2.; 3.|]
        let zs = Array.zip xs ys
        let x, y = zs.[0]
        x + y |> equal 2.

    testCase "Array.zip3 works" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys = [|1.; 2.; 3.|]
        let zs = [|1.; 2.; 3.|]
        let ks = Array.zip3 xs ys zs
        let x, y, z = ks.[0]
        x + y + z |> equal 3.

// TODO: IList
(*
    testCase "Array as IList indexer has same behaviour" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys = xs :> _ System.Collections.Generic.IList
        ys.[0] <- -3.
        ys.[0] + ys.[2]
        |> equal 0.

    testCase "Array as IList count has same behaviour" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys = xs :> _ System.Collections.Generic.IList
        ys.Count |> equal 3

    testCase "Array as IList Seq.length has same behaviour" <| fun () ->
        let xs = [|1.; 2.; 3.|]
        let ys = xs :> _ System.Collections.Generic.IList
        ys |> Seq.length |> equal 3
*)
    testCase "Mapping with typed arrays doesn't coerce" <| fun () ->
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

    testCase "Array.item works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.item 2 xs |> equal 3.

    testCase "Array.tryItem works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.tryItem 3 xs |> equal (Some 4.)
        Array.tryItem 4 xs |> equal None
        Array.tryItem -1 xs |> equal None

    testCase "Array.head works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.head xs |> equal 1.

    testCase "Array.tryHead works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.tryHead xs |> equal (Some 1.)
        Array.tryHead [||] |> equal None

    testCase "Array.last works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        xs |> Array.last
        |> equal 4.

    testCase "Array.tryLast works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.tryLast xs |> equal (Some 4.)
        Array.tryLast [||] |> equal None

    testCase "Array.tail works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.|]
        Array.tail xs |> Array.length |> equal 3

    testCase "Array.groupBy returns valid array" <| fun () ->
        let xs = [|1; 2|]
        let actual = Array.groupBy (fun _ -> true) xs
        let actualKey, actualGroup = actual.[0]
        let worked = actualKey && actualGroup.[0] = 1 && actualGroup.[1] = 2
        worked |> equal true

    testCase "Array.groupBy does not run into undefined exception" <| fun () ->
        let ys = [| 1;2;3 |] |> Array.groupBy (fun x -> x = 2)
        ys |> equal [| (false, [| 1;3 |]); (true, [| 2 |]) |]

    testCase "Array.groupBy maintains order" <| fun () ->
        let xs = [| 0,5; 1,5; 2,5; 3,5; 0,6; 1,6; 2,6; 3,6 |]
        let mapped = xs |> Array.take 4 |> Array.map (fun (x,y) -> x, [|x,y; x,y+1|])
        let grouped = xs |> Array.groupBy fst
        grouped |> equal mapped

    testCase "Array.except works" <| fun () ->
        Array.except [|2|] [|1; 3; 2|] |> Array.last |> equal 3
        Array.except [|2|] [|2; 4; 6|] |> Array.head |> equal 4
        Array.except [|1|] [|1; 1; 1; 1|] |> Array.isEmpty |> equal true
        Array.except [|'t'; 'e'; 's'; 't'|] [|'t'; 'e'; 's'; 't'|] |> Array.isEmpty |> equal true
        Array.except [|'t'; 'e'; 's'; 't'|] [|'t'; 't'|] |> Array.isEmpty |> equal true
        Array.except [|(1, 2)|] [|(1, 2)|] |> Array.isEmpty |> equal true
        Array.except [|Map.empty |> (fun m -> m.Add(1, 2))|] [|Map.ofList [(1, 2)]|] |> Array.isEmpty |> equal true
        Array.except [|{ Bar= "test" }|] [|{ Bar = "test" }|] |> Array.isEmpty |> equal true

    testCase "Array.[i] is undefined in Fable when i is out of range" <| fun () ->
        let xs = [|0|]
        try xs.[1] |> ignore; false with _ -> true
        |> equal true

    testCase "Array iterators from range do rewind" <| fun () ->
        let xs = [|1..5|] |> Array.toSeq
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    testCase "Array indexed works" <| fun () ->
        let xs = [|"a"; "b"; "c"|] |> Array.indexed
        xs.Length |> equal 3
        fst xs.[2] |> equal 2
        snd xs.[2] |> equal "c"

    testCase "Array.chunkBySize works" <| fun () ->
        [|1..8|] |> Array.chunkBySize 4 |> equal [| [|1..4|]; [|5..8|] |]
        [|1..10|] |> Array.chunkBySize 4 |> equal [| [|1..4|]; [|5..8|]; [|9..10|] |]

    // testCase "Array.splitAt works" <| fun () ->
    //     let ar = [|1;2;3;4|]
    //     Array.splitAt 0 ar |> equal ([||], [|1;2;3;4|])
    //     Array.splitAt 3 ar |> equal ([|1;2;3|], [|4|])
    //     Array.splitAt 4 ar |> equal ([|1;2;3;4|], [||])

    testCase "Arrays are independent from type of the elements" <| fun () ->
        let fromArrayToListAndBack a = a |> Array.toList |> List.toArray
        [|"a";"b"|] |> fromArrayToListAndBack |> equal [|"a";"b"|]
        [|1;2|] |> fromArrayToListAndBack |> equal [|1;2|]

    testCase "Arrays are independent from being binded to a name" <| fun () ->
        let intList = [1;2;3]
        intList  |> List.toArray  |> equal [|1;2;3|]
        [1;2;3]  |> List.toArray  |> equal [|1;2;3|]

    testCase "Functions on arrays should behave the same whether binded to a name or not" <| fun () ->
        let fromArrayToListAndBack a = a |> Array.toList |> List.toArray
        [|1;2|] |> Array.toList |> List.toArray  |> equal ([|1;2|] |> fromArrayToListAndBack)

    testCase "Array.skip works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.; 5.|]
        let ys = xs |> Array.skip 1
        ys |> Array.head
        |> equal 2.

    testCase "Array.skipWhile works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.; 5.|]
        xs |> Array.skipWhile (fun i -> i <= 3.)
        |> Array.head
        |> equal 4.

    testCase "Array.take works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.; 5.|]
        xs |> Array.take 2
        |> Array.last
        |> equal 2.
        // Array.take should throw an exception if there're not enough elements
        try xs |> Array.take 20 |> Array.length with _ -> -1
        |> equal -1

    testCase "Array.takeWhile works" <| fun () ->
        let xs = [|1.; 2.; 3.; 4.; 5.|]
        xs |> Array.takeWhile (fun i -> i < 3.)
        |> Array.last
        |> equal 2.

    testCase "Array.windowed works" <| fun () -> // See #1716
        let nums = [| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]
        Array.windowed 3 nums |> equal [|[|1.0; 1.5; 2.0|]; [|1.5; 2.0; 1.5|]; [|2.0; 1.5; 1.0|]; [|1.5; 1.0; 1.5|]|]
        Array.windowed 5 nums |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0 |]; [| 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
        Array.windowed 6 nums |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
        Array.windowed 7 nums |> Array.isEmpty |> equal true

    testCase "Array.allPairs works" <| fun () ->
        let xs = [|1;2;3;4|]
        let ys = [|'a';'b';'c';'d';'e';'f'|]
        Array.allPairs xs ys
        |> equal
            [|(1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f'); (2, 'a');
              (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f'); (3, 'a'); (3, 'b');
              (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f'); (4, 'a'); (4, 'b'); (4, 'c');
              (4, 'd'); (4, 'e'); (4, 'f')|]

    testCase "Casting to System.Array works" <| fun () ->
        let xs = [|1;2;3;4|] :> System.Array // Numeric array
        let ys = [|'a';'b';'c';'d';'e';'f'|] :> System.Array
        [ for i in xs do for j in ys do yield (unbox i, unbox j) ]
        |> equal
            [ (1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f'); (2, 'a');
              (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f'); (3, 'a'); (3, 'b');
              (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f'); (4, 'a'); (4, 'b'); (4, 'c');
              (4, 'd'); (4, 'e'); (4, 'f') ]

    testCase "Testing against System.Array works" <| fun () ->
        let xs = box [|1;2;3;4|]
        let ys = box [|'a';'b';'c';'d';'e';'f'|]
        let zs = box [1;2;3;4]
        xs :? System.Array |> equal true
        ys :? System.Array |> equal true
        zs :? System.Array |> equal false

    testCase "Array.Copy works with numeric arrays" <| fun () ->
        let source = [| 99 |]
        let destination = [| 1; 2; 3 |]
        Array.Copy(source, 0, destination, 0, 1)
        equal [| 99; 2; 3 |] destination

    testCase "Array.Copy works with non-numeric arrays" <| fun () ->
        let source = [| "xy"; "xx"; "xyz" |]
        let destination = [| "a"; "b"; "c" |]
        Array.Copy(source, 1, destination, 1, 2)
        equal [| "a"; "xx"; "xyz" |] destination

    testCase "Array.splitInto works" <| fun () ->
        [|1..10|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..7|]; [|8..10|] |]
        [|1..11|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..8|]; [|9..11|] |]
        [|1..12|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..8|]; [|9..12|] |]
        [|1..5|] |> Array.splitInto 4 |> equal [| [|1..2|]; [|3|]; [|4|]; [|5|] |]
        [|1..4|] |> Array.splitInto 20 |> equal [| [|1|]; [|2|]; [|3|]; [|4|] |]

    testCase "Array.exactlyOne works" <| fun () ->
            [|1.|] |> Array.exactlyOne |> equal 1.
            (try Array.exactlyOne [|1.;2.|] |> ignore; false with | _ -> true) |> equal true
            (try Array.exactlyOne [||] |> ignore; false with | _ -> true) |> equal true

    testCase "Array.tryExactlyOne works" <| fun () ->
            [|1.|] |> Array.tryExactlyOne |> equal (Some 1.)
            [|1.;2.|] |> Array.tryExactlyOne |> equal None
            [||] |> Array.tryExactlyOne |> equal None

    testCase "Array.pairwise works" <| fun () ->
        Array.pairwise<int> [||] |> equal [||]
        Array.pairwise [|1|] |> equal [||]
        Array.pairwise [|1; 2|] |> equal [|(1, 2)|]
        let xs = [|1; 2; 3; 4|]
        let xs2 = xs |> Array.pairwise
        equal [|(1, 2); (2, 3); (3, 4)|] xs2
        xs2 |> Array.map (fun (x, y) -> $"%i{x}%i{y}")
        |> String.concat ""
        |> equal "122334"

    testCase "Array.transpose works" <| fun () ->
        // integer array
        Array.transpose (seq [[|1..3|]; [|4..6|]])
        |> equal [|[|1;4|]; [|2;5|]; [|3;6|]|]
        Array.transpose [|[|1..3|]|]
        |> equal [|[|1|]; [|2|]; [|3|]|]
        Array.transpose [|[|1|]; [|2|]|]
        |> equal [|[|1..2|]|]
        // string array
        Array.transpose (seq [[|"a";"b";"c"|]; [|"d";"e";"f"|]])
        |> equal [|[|"a";"d"|]; [|"b";"e"|]; [|"c";"f"|]|]
        // empty array
        Array.transpose [| |]
        |> equal [| |]
        // array of empty arrays - m x 0 array transposes to 0 x m (i.e. empty)
        Array.transpose [| [||] |]
        |> equal [| |]
        Array.transpose [| [||]; [||] |]
        |> equal [| |]
        // jagged arrays
        throwsAnyError (fun () -> Array.transpose [| [|1; 2|]; [|3|] |])
        throwsAnyError (fun () -> Array.transpose [| [|1|]; [|2; 3|] |])

    testCase "Array.udpateAt works" <| fun () ->
        // integer list
        equal [|0; 2; 3; 4; 5|] (Array.updateAt 0 0 [|1..5|])
        equal [|1; 2; 0; 4; 5|] (Array.updateAt 2 0 [|1..5|])
        equal [|1; 2; 3; 4; 0|] (Array.updateAt 4 0 [|1..5|])

        //string list
        equal [|"0"; "2"; "3"; "4"; "5"|] (Array.updateAt 0 "0" [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "0"; "4"; "5"|] (Array.updateAt 2 "0" [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "3"; "4"; "0"|] (Array.updateAt 4 "0" [|"1"; "2"; "3"; "4"; "5"|])

        // empty list & out of bounds
        throwsAnyError (fun () -> Array.updateAt 0 0 [||] |> ignore)
        throwsAnyError (fun () -> Array.updateAt -1 0 [|1|] |> ignore)
        throwsAnyError (fun () -> Array.updateAt 2 0 [|1|] |> ignore)

    testCase "Array.insertAt works" <| fun () ->
        // integer list
        equal [|0; 1; 2; 3; 4; 5|] (Array.insertAt 0 0 [|1..5|])
        equal [|1; 2; 0; 3; 4; 5|] (Array.insertAt 2 0 [|1..5|])
        equal [|1; 2; 3; 4; 0; 5|] (Array.insertAt 4 0 [|1..5|])

        //string list
        equal [|"0"; "1"; "2"; "3"; "4"; "5"|] (Array.insertAt 0 "0" [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "0"; "3"; "4"; "5"|] (Array.insertAt 2 "0" [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "3"; "4"; "0"; "5"|] (Array.insertAt 4 "0" [|"1"; "2"; "3"; "4"; "5"|])

        // empty list & out of bounds
        equal [|0|] (Array.insertAt 0 0 [||])
        throwsAnyError (fun () -> Array.insertAt -1 0 [|1|] |> ignore)
        throwsAnyError (fun () -> Array.insertAt 2 0 [|1|] |> ignore)

    testCase "Array.insertManyAt works" <| fun () ->
        // integer list
        equal [|0; 0; 1; 2; 3; 4; 5|] (Array.insertManyAt 0 [0; 0] [|1..5|])
        equal [|1; 2; 0; 0; 3; 4; 5|] (Array.insertManyAt 2 [0; 0] [|1..5|])
        equal [|1; 2; 3; 4; 0; 0; 5|] (Array.insertManyAt 4 [0; 0] [|1..5|])

        //string list
        equal [|"0"; "0"; "1"; "2"; "3"; "4"; "5"|] (Array.insertManyAt 0 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "0"; "0"; "3"; "4"; "5"|] (Array.insertManyAt 2 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "3"; "4"; "0"; "0"; "5"|] (Array.insertManyAt 4 ["0"; "0"] [|"1"; "2"; "3"; "4"; "5"|])

        // empty list & out of bounds
        equal [|0; 0|] (Array.insertManyAt 0 [0; 0] [||])
        throwsAnyError (fun () -> Array.insertManyAt -1 [0; 0] [|1|] |> ignore)
        throwsAnyError (fun () -> Array.insertManyAt 2 [0; 0] [|1|] |> ignore)

    testCase "Array.removeAt works" <| fun () ->
        // integer list
        equal [|2; 3; 4; 5|] (Array.removeAt 0 [|1..5|])
        equal [|1; 2; 4; 5|] (Array.removeAt 2 [|1..5|])
        equal [|1; 2; 3; 4|] (Array.removeAt 4 [|1..5|])

        //string list
        equal [|"2"; "3"; "4"; "5"|] (Array.removeAt 0 [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "4"; "5"|] (Array.removeAt 2 [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "3"; "4"|] (Array.removeAt 4 [|"1"; "2"; "3"; "4"; "5"|])

        // empty list & out of bounds
        throwsAnyError (fun () -> Array.removeAt 0 [||] |> ignore)
        throwsAnyError (fun () -> Array.removeAt -1 [|1|] |> ignore)
        throwsAnyError (fun () -> Array.removeAt 2 [|1|] |> ignore)

    testCase "Array.removeManyAt works" <| fun () ->
        // integer list
        equal [|3; 4; 5|] (Array.removeManyAt 0 2 [|1..5|])
        equal [|1; 2; 5|] (Array.removeManyAt 2 2 [|1..5|])
        equal [|1; 2; 3|] (Array.removeManyAt 3 2 [|1..5|])

        //string list
        equal [|"3"; "4"; "5"|] (Array.removeManyAt 0 2 [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "5"|] (Array.removeManyAt 2 2 [|"1"; "2"; "3"; "4"; "5"|])
        equal [|"1"; "2"; "3"|] (Array.removeManyAt 3 2 [|"1"; "2"; "3"; "4"; "5"|])

        // empty list & out of bounds
        throwsAnyError (fun () -> Array.removeManyAt 0 2 [||] |> ignore)
        throwsAnyError (fun () -> Array.removeManyAt -1 2 [|1|] |> ignore)
        throwsAnyError (fun () -> Array.removeManyAt 2 2 [|1|] |> ignore)
