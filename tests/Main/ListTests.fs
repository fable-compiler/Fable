module Fable.Tests.Lists

open Util.Testing
open Fable.Tests.Util

type List(x: int) =
    member val Value = x

type ExceptFoo = { Bar:string }

let testListChoose xss =
    let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
    xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

let rec sumFirstList (zs: float list) (n: int): float =
    match n with
    | 0 -> 0.
    | 1 -> zs.Head
    | _ -> zs.Head + sumFirstList zs.Tail (n-1)

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
    { MyNumber: MyNumber }

module List =
    let filteri f (xs: 'T list) =
        let mutable i = -1
        List.filter (fun x -> i <- i + 1; f i x) xs

let tests =
  testList "Lists" [
    testCase "Some [] works" <| fun () ->
        let xs: int list option = Some []
        let ys: int list option = None
        Option.isSome xs |> equal true
        Option.isNone ys |> equal true

    testCase "List equality works" <| fun () ->
        let xs = [1;2;3]
        let ys = [1;2;3]
        let zs = [1;4;3]
        xs = ys |> equal true
        xs = zs |> equal false

    testCase "List comparison works" <| fun () ->
        let xs = [1;2;3]
        let ys = [1;2;3]
        let zs = [1;4;3]
        xs < ys |> equal false
        xs < zs |> equal true

    testCase "Pattern matching with lists works" <| fun () ->
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

    testCase "List.Length works" <| fun () ->
            let xs = [1; 2; 3; 4]
            equal 4 xs.Length
            equal 0 [].Length

    testCase "List.IsEmpty works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = []
            equal false xs.IsEmpty
            equal false [1; 2; 3; 4].IsEmpty
            equal true ys.IsEmpty
            equal true [].IsEmpty

    testCase "List.Equals works" <| fun () ->
            let xs = [1;2;3]
            xs.Equals(xs) |> equal true

    testCase "List.Head works" <| fun () ->
            let xs = [1; 2; 3; 4]
            equal 1 xs.Head

    testCase "List.Tail works" <| fun () ->
            let xs = [1; 2; 3; 4]
            equal 2 xs.Tail.Head

    testCase "List.Item works" <| fun () ->
            let xs = [1; 2; 3; 4]
            equal 4 xs.[3]

    testCase "List cons works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = 3 :: xs
            let zs = List.Cons(4, xs)
            ys.Head + xs.Head
            |> equal zs.Head

    testCase "List.cons works II" <| fun () ->
            let li = [1;2;3;4;5]
            let li2 = li.Tail
            let li3 = [8;9;11] @ li2
            let li3b = [20;16] @ li3.Tail
            let li4 = 14 :: li3b
            li4.[1] |> equal 20
            li4.[3] |> equal 9
            List.length li4 |> equal 9
            List.sum li4 |> equal 84

    testCase "List.empty works" <| fun () ->
            let xs = 1 :: List.Empty
            let ys = 1 :: List.empty
            xs.Length + ys.Length |> equal 2

    testCase "List.append works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = [0]
            let zs = List.append ys xs
            zs.Head + zs.Tail.Head
            |> equal 1

    testCase "List.append works II" <| fun () ->
            let li = [1;2;3;4;5]
            let li2 = li.Tail
            let li3 = [8;9;11] @ li2
            let li3b = [20;16] @ li3.Tail
            let li4 = li3b @ li2
            li4.[1] |> equal 16
            li4.[9] |> equal 3
            List.length li4 |> equal 12
            List.sum li4 |> equal 84

    testCase "List.append works with empty list" <| fun () ->
            let li = [{| value = 2|}; {| value = 4|};]
            let li = li @ []
            let li = [] @ li
            li
            |> Seq.map (fun x -> 20 / x.value)
            |> Seq.sum
            |> equal 15

    testCase "List.choose works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let result = xs |> List.choose (fun x ->
                if x > 2 then Some x
                else None)
            result.Head + result.Tail.Head
            |> equal 7

    testCase "List.exactlyOne works" <| fun () ->
            let xs = [1.;]
            xs |> List.exactlyOne
            |> equal 1.

            let xs2 = [1.;2.]
            (try List.exactlyOne xs2 |> ignore; false with | _ -> true) |> equal true

            let xs3 = []
            (try List.exactlyOne xs3 |> ignore; false with | _ -> true) |> equal true

    testCase "List.tryExactlyOne works" <| fun () ->
            [1.] |> List.tryExactlyOne |> equal (Some 1.)
            [1.;2.] |> List.tryExactlyOne |> equal None
            [] |> List.tryExactlyOne |> equal None

    testCase "List.exists works" <| fun () ->
            let xs = [1; 2; 3; 4]
            xs |> List.exists (fun x -> x = 2)
            |> equal true

    testCase "List.exists2 works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = [1; 2; 3; 4]
            List.exists2 (fun x y -> x * y = 16) xs ys
            |> equal true

    testCase "List.filter works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = xs |> List.filter (fun x -> x > 5)
            equal ys.IsEmpty true

    testCase "List.filter doesn't work backwards" <| fun () -> // See #1672
            let li = [1; 2; 3; 4; 5]
            li |> List.filteri (fun i _ -> i <> 1) |> equal [1; 3; 4; 5]

    testCase "List.find works" <| fun () ->
            [1; 2; 3; 4]
            |> List.find ((=) 2)
            |> equal 2

    testCase "List.findIndex works" <| fun () ->
            [1; 2; 3; 4]
            |> List.findIndex ((=) 2)
            |> equal 1

    testCase "List.fold works" <| fun () ->
            [1; 2; 3; 4]
            |> List.fold (+) 0
            |> equal 10

    testCase "List.fold2 works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = [1; 2; 3; 4]
            List.fold2 (fun x y z -> x + y + z) 0 xs ys
            |> equal 20

    testCase "List.foldBack works" <| fun () ->
            [1; 2; 3; 4]
            |> List.foldBack (fun x acc -> acc - x) <| 100
            |> equal 90

    testCase "List.foldBack with composition works" <| fun () ->
            [1; 2; 3; 4]
            |> List.foldBack (fun x acc -> acc >> (+) x) <| id <| 2
            |> equal 12

    testCase "List.forall works" <| fun () ->
            [1; 2; 3; 4]
            |> List.forall (fun x -> x < 5)
            |> equal true

    testCase "List.forall2 works" <| fun () ->
            ([1; 2; 3; 4], [1; 2; 3; 4])
            ||> List.forall2 (=)
            |> equal true

    testCase "List.head works" <| fun () ->
            [1; 2; 3; 4]
            |> List.head
            |> equal 1

    testCase "List.init works" <| fun () ->
            let xs = List.init 4 float
            xs.Head + xs.Tail.Head
            |> equal 1.

    testCase "List.isEmpty works" <| fun () ->
            List.isEmpty [1] |> equal false
            List.isEmpty [] |> equal true

    testCase "List.iter works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let mutable total = 0
            xs |> List.iter (fun x ->
            total <- total + x)
            equal 10 total

    testCase "List.iter2 works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = [2; 4; 6; 8]
            let total = ref 0
            List.iter2 (fun x y ->
            total := !total + (y - x)
            ) xs ys
            equal 10 !total

    testCase "List.iteri works" <| fun () ->
            let mutable total = 0
            [1; 2; 3; 4]
            |> List.iteri (fun i x ->
                total <- total + (i * x))
            equal 20 total

    testCase "List.iteri2 works" <| fun () ->
            let mutable total = 0
            let xs = [1; 2; 3; 4]
            let ys = [2; 4; 6; 8]
            List.iteri2 (fun i x y ->
            total <- total + i * (y - x)
            ) xs ys
            equal 20 total

    testCase "List.length works" <| fun () ->
            let xs = [1; 2; 3; 4]
            List.length xs
            |> equal 4

    testCase "List.item works" <| fun () ->
            [1; 2] |> List.item 1 |> equal 2

    testCase "List.map works" <| fun () ->
            let xs = [1;2;3]
            let ys = xs |> List.map ((*) 2)
            equal 4 ys.Tail.Head

    testCase "List.mapi works" <| fun () ->
            let xs = [1]
            let ys = xs |> List.mapi (fun i x -> i * x)
            equal 0 ys.Head

    testCase "List.map2 works" <| fun () ->
            let xs = [1;2]
            let ys = [2;3]
            let zs = List.map2 (fun x y -> x - y) xs ys
            equal -1 zs.Head

    testCase "List.ofArray works" <| fun () ->
            let xs = [|1; 2|]
            let ys = List.ofArray xs
            ys.Head |> equal 1

            let xs1 = [|1.; 2.; 3.; 4.|]
            let ys1 = List.ofArray xs1
            sumFirstList ys1 3 |> equal 6.

    testCase "List.ofSeq works" <| fun () ->
            // let xs = [|1; 2|] :> _ seq
            let ys = List.ofSeq <| seq { yield 1; yield 2 }
            ys.Head |> equal 1
            ys.Length |> equal 2

    testCase "List.pick works" <| fun () ->
            let xs = [1; 2]
            xs |> List.pick (fun x ->
                match x with
                | 2 -> Some x
                | _ -> None)
            |> equal 2

    testCase "List.reduce works" <| fun () ->
            let xs = [1; 2]
            xs |> List.reduce (+)
            |> equal 3

    testCase "List.reduceBack works" <| fun () ->
            let xs = [1; 2]
            xs |> List.reduceBack (+)
            |> equal 3

    testCase "List.replicate works" <| fun () ->
            List.replicate 3 3
            |> List.sum |> equal 9

    testCase "List.rev works" <| fun () ->
            let xs = [1; 2; 3]
            let ys = xs |> List.rev
            equal 3 ys.Head

    testCase "List.scan works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = (0, xs) ||> List.scan (fun acc x -> acc - x)
            ys.[3] + ys.[4]
            |> equal -16

    testCase "List.scanBack works" <| fun () ->
            let xs = [1; 2; 3]
            let ys = List.scanBack (fun x acc -> acc - x) xs 0
            ys.Head + ys.Tail.Head
            |> equal -11

    testCase "List.sort works" <| fun () ->
        let xs = [3; 4; 1; -3; 2; 10]
        let ys = ["a"; "c"; "B"; "d"]
        xs |> List.sort |> List.take 3 |> List.sum |> equal 0
        ys |> List.sort |> List.item 1 |> equal "a"

    testCase "List.sort with tuples works" <| fun () ->
        let xs = [3; 1; 1; -3]
        let ys = ["a"; "c"; "B"; "d"]
        (xs, ys) ||> List.zip |> List.sort |> List.item 1 |> equal (1, "B")

    testCase "List.sortBy works" <| fun () ->
        let xs = [3; 1; 4; 2]
        let ys = xs |> List.sortBy (fun x -> -x)
        ys.Head + ys.Tail.Head
        |> equal 7

    testCase "List.sortWith works" <| fun () ->
        let xs = [3; 4; 1; 2]
        let ys = xs |> List.sortWith (fun x y -> int(x - y))
        ys.Head + ys.Tail.Head
        |> equal 3

    testCase "List.sortDescending works" <| fun () ->
        let xs = [3; 4; 1; -3; 2; 10]
        xs |> List.sortDescending |> List.take 3 |> List.sum |> equal 17
        let ys = ["a"; "c"; "B"; "d"]
        ys |> List.sortDescending |> List.item 1 |> equal "c"

    testCase "List.sortByDescending works" <| fun () ->
        let xs = [3; 1; 4; 2]
        let ys = xs |> List.sortByDescending (fun x -> -x)
        ys.Head + ys.Tail.Head
        |> equal 3

    testCase "List.max works" <| fun () ->
            let xs = [1; 2]
            xs |> List.max
            |> equal 2

    testCase "List.maxBy works" <| fun () ->
            let xs = [1; 2]
            xs |> List.maxBy (fun x -> -x)
            |> equal 1

    testCase "List.min works" <| fun () ->
            let xs = [1; 2]
            xs |> List.min
            |> equal 1

    testCase "List.minBy works" <| fun () ->
            let xs = [1; 2]
            xs |> List.minBy (fun x -> -x)
            |> equal 2

    testCase "List.sum works" <| fun () ->
            [1; 2] |> List.sum
            |> equal 3

    testCase "List.sumBy works" <| fun () ->
            [1; 2] |> List.sumBy (fun x -> x*2)
            |> equal 6

    testCase "List.sum with non numeric types works" <| fun () ->
      let p1 = {x=1; y=10}
      let p2 = {x=2; y=20}
      [p1; p2] |> List.sum |> (=) {x=3;y=30} |> equal true

    testCase "List.sumBy with non numeric types works" <| fun () ->
      let p1 = {x=1; y=10}
      let p2 = {x=2; y=20}
      [p1; p2] |> List.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

    testCase "List.sumBy with numeric projection works" <| fun () ->
      let p1 = {x=1; y=10}
      let p2 = {x=2; y=20}
      [p1; p2] |> List.sumBy (fun p -> p.y) |> equal 30

    testCase "List.sum with non numeric types works II" <| fun () ->
        [MyNumber 1; MyNumber 2; MyNumber 3] |> List.sum |> equal (MyNumber 6)

    testCase "List.sumBy with non numeric types works II" <| fun () ->
        [{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }]
        |> List.sumBy (fun x -> x.MyNumber) |> equal (MyNumber 12)

    testCase "List.skip works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        let ys = xs |> List.skip 1
        ys |> List.head
        |> equal 2.

    testCase "List.skipWhile works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> List.skipWhile (fun i -> i <= 3.)
        |> List.head
        |> equal 4.

    testCase "List.take works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> List.take 2
        |> List.last
        |> equal 2.
        // List.take should throw an exception if there're not enough elements
        try xs |> List.take 20 |> List.length with _ -> -1
        |> equal -1

    testCase "List.takeWhile works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> List.takeWhile (fun i -> i < 3.)
        |> List.last
        |> equal 2.

    testCase "List.tail works" <| fun () ->
            let xs = [1; 2]
            let ys = xs |> List.tail
            equal 1 ys.Length

    testCase "List.toArray works" <| fun () ->
            let ys = List.toArray [1; 2]
            ys.[0] + ys.[1] |> equal 3
            let xs = [1; 1]
            let ys2 = List.toArray (2::xs)
            ys2.[0] + ys2.[1] + ys2.[2] |> equal 4

    testCase "List.toSeq works" <| fun () ->
            [1; 2]
            |> List.toSeq
            |> Seq.tail |> Seq.head
            |> equal 2

    testCase "List.tryPick works" <| fun () ->
            [1; 2]
            |> List.tryPick (function
                | 2 -> Some 2
                | _ -> None)
            |> function
            | Some x -> x
            | None -> 0
            |> equal 2

    testCase "List.tryFind works" <| fun () ->
            [1; 2]
            |> List.tryFind ((=) 5)
            |> equal None

    testCase "List.tryFindIndex works" <| fun () ->
            let xs = [1; 2]
            let ys = xs |> List.tryFindIndex ((=) 2)
            ys.Value |> equal 1
            xs |> List.tryFindIndex ((=) 5) |> equal None

    testCase "List.unzip works" <| fun () ->
            let xs = [1, 2]
            let ys, zs = xs |> List.unzip
            ys.Head + zs.Head
            |> equal 3

    testCase "List.unzip3 works" <| fun () ->
            let xs = [(1, 2, 3); (4, 5, 6)]
            let ys, zs, ks = xs |> List.unzip3
            ys.[1] + zs.[1] + ks.[1]
            |> equal 15

    testCase "List.zip works" <| fun () ->
            let xs = [1; 2; 3]
            let ys = [4; 5; 6]
            let zs = List.zip xs ys
            let x, y = zs.Tail.Head
            equal 2 x
            equal 5 y

    testCase "List snail to append works" <| fun () ->
            let xs = [1; 2; 3; 4]
            let ys = [0]
            let zs = ys @ xs
            zs.Head + zs.Tail.Head
            |> equal 1

    testCase "List slice works" <| fun () ->
            let xs = [1; 2; 3; 4]
            xs.[..2] |> List.sum |> equal 6
            xs.[2..] |> List.sum |> equal 7
            xs.[1..2] |> List.sum |> equal 5
            xs.[0..-1] |> List.sum |> equal 0

    testCase "List.truncate works" <| fun () ->
            [1..3] = (List.truncate 3 [1..5]) |> equal true
            [1..5] = (List.truncate 10 [1..5]) |> equal true
            [] = (List.truncate 0 [1..5]) |> equal true
            ["str1";"str2"] = (List.truncate 2 ["str1";"str2";"str3"]) |> equal true
            [] = (List.truncate 0 []) |> equal true
            [] = (List.truncate 1 []) |> equal true

    testCase "List.choose works with generic arguments" <| fun () ->
        let res = testListChoose [ Some [ "a" ] ]
        equal ["a"] res

    testCase "List.collect works" <| fun () ->
            let xs = [[1]; [2]; [3]; [4]]
            let ys = xs |> List.collect id
            ys.Head + ys.Tail.Head
            |> equal 3

            let list1 = [10.; 20.; 30.]
            let collectList = List.collect (fun x -> [for i in 1.0..3.0 -> x * i]) list1
            sumFirstList collectList 9 |> equal 360.

            let xs = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
            let ys = xs |> List.collect id
            sumFirstList ys 5
            |> equal 15.

    testCase "List.concat works" <| fun () ->
            let xs = [[1]; [2]; [3]; [4]]
            let ys = xs |> List.concat
            ys.Head  + ys.Tail.Head
            |> equal 3

            let xs1 = [[1.; 2.; 3.]; [4.; 5.; 6.]; [7.; 8.; 9.]]
            let ys1 = xs1 |> List.concat
            sumFirstList ys1 7
            |> equal 28.

    testCase "List.contains works" <| fun () ->
            let xs = [1; 2; 3; 4]
            xs |> List.contains 2 |> equal true
            xs |> List.contains 0 |> equal false

    testCase "List.contains lambda doesn't clash" <| fun () ->
            let modifyList current x =
                let contains = current |> List.contains x
                match contains with
                    | true -> current |> (List.filter (fun a -> a <> x))
                    | false -> x::current
            let l = [1;2;3;4]
            (modifyList l 1) |> List.contains 1 |> equal false
            (modifyList l 5) |> List.contains 5 |> equal true

    testCase "List.average works" <| fun () ->
            List.average [1.; 2.; 3.; 4.]
            |> equal 2.5

    testCase "List.averageBy works" <| fun () ->
            [1.; 2.; 3.; 4.]
            |> List.averageBy (fun x -> x * 2.)
            |> equal 5.

    testCase "List.average works with custom types" <| fun () ->
        [MyNumber 1; MyNumber 2; MyNumber 3] |> List.average |> equal (MyNumber 2)

    testCase "List.averageBy works with custom types" <| fun () ->
        [{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }]
        |> List.averageBy (fun x -> x.MyNumber) |> equal (MyNumber 4)

    testCase "List.distinct works" <| fun () ->
        let xs = [1; 1; 1; 2; 2; 3; 3]
        let ys = xs |> List.distinct
        ys |> List.length |> equal 3
        ys |> List.sum |> equal 6

    testCase "List.distinct with tuples works" <| fun () ->
        let xs = [(1, 2); (2, 3); (1, 2)]
        let ys = xs |> List.distinct
        ys |> List.length |> equal 2
        ys |> List.sumBy fst |> equal 3

    testCase "List.distinctBy works" <| fun () ->
        let xs = [4; 4; 4; 6; 6; 5; 5]
        let ys = xs |> List.distinctBy (fun x -> x % 2)
        ys |> List.length |> equal 2
        ys |> List.head >= 4 |> equal true

    testCase "List.distinctBy with tuples works" <| fun () ->
        let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
        let ys = xs |> List.distinctBy (fun (x,_) -> x % 2)
        ys |> List.length |> equal 2
        ys |> List.head |> fst >= 4 |> equal true

    testCase "List.findBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> List.find ((>) 4.) |> equal 1.
        xs |> List.findBack ((>) 4.) |> equal 3.

    testCase "List.findIndexBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> List.findIndex ((>) 4.) |> equal 0
        xs |> List.findIndexBack ((>) 4.) |> equal 2

    testCase "List.tryFindBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> List.tryFind ((>) 4.) |> equal (Some 1.)
        xs |> List.tryFindBack ((>) 4.) |> equal (Some 3.)
        xs |> List.tryFindBack ((=) 5.) |> equal None

    testCase "List.tryFindIndexBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> List.tryFindIndex ((>) 4.) |> equal (Some 0)
        xs |> List.tryFindIndexBack ((>) 4.) |> equal (Some 2)
        xs |> List.tryFindIndexBack ((=) 5.) |> equal None

    testCase "List.foldBack2 works" <| fun () ->
            ([1; 2; 3; 4], [1; 2; 3; 4], 0)
            |||> List.foldBack2 (fun x y acc -> acc - y * x)
            |> equal -30

    testCase "List.indexed works" <| fun () ->
        let xs = ["a"; "b"; "c"] |> List.indexed
        xs.Length |> equal 3
        fst xs.[2] |> equal 2
        snd xs.[2] |> equal "c"

    testCase "List.map3 works" <| fun () ->
            let xs = [1;2;3]
            let ys = [5;4;3]
            let zs = [7;8;9]
            let ks = List.map3 (fun x y z -> z - y - x) xs ys zs
            List.sum ks
            |> equal 6

    testCase "List.mapi2 works" <| fun () ->
            let xs = [7;8;9]
            let ys = [5;4;3]
            let zs = List.mapi2 (fun i x y -> i * (x - y)) xs ys
            List.sum zs |> equal 16

    testCase "List.mapFold works" <| fun () ->
        let xs = [1y; 2y; 3y; 4y]
        let result = xs |> List.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
        fst result |> List.sum |> equal 20y
        snd result |> equal 10y

    testCase "List.mapFoldBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let result = List.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
        fst result |> List.sum |> equal -20.
        snd result |> equal -10.

    // TODO: Runtime uncurry to arity 2
    testCase "List.mapFold works II" <| fun () -> // See #842
        let f x y = x,y
        let xs,_ = List.mapFold f "a" ["b"]
        equal "a" xs.Head

    testCase "List.mapFoldBack works II" <| fun () ->
        let f x y = x,y
        let xs,_ = List.mapFoldBack f ["a"] "b"
        equal "a" xs.Head

    testCase "List.partition works" <| fun () ->
            let xs = [1; 2; 3; 4; 5; 6]
            let ys, zs = xs |> List.partition (fun x -> x % 2 = 0)
            List.sum zs |> equal 9
            equal 2 ys.[0]
            equal 5 zs.[2]

    testCase "List.pairwise works" <| fun () ->
        List.pairwise<int> [] |> equal []
        List.pairwise [1] |> equal []
        let xs = [1; 2; 3; 4]
        let xs2 = xs |> List.pairwise
        equal [(1, 2); (2, 3); (3, 4)] xs2
        xs2 |> List.map (fun (x, y) -> sprintf "%i%i" x y)
        |> String.concat ""
        |> equal "122334"

    testCase "List.permute works" <| fun () ->
            let xs = [1; 2; 3; 4; 5; 6]
            let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
            equal 4 ys.[2]
            equal 6 ys.[4]

    testCase "List.chunkBySize works" <| fun () ->
        [1..8] |> List.chunkBySize 4 |> equal [ [1..4]; [5..8] ]
        [1..10] |> List.chunkBySize 4 |> equal [ [1..4]; [5..8]; [9..10] ]

    testCase "List.range works" <| fun () ->
        [1..5]
        |> List.reduce (+)
        |> equal 15
        [0..2..9]
        |> List.reduce (+)
        |> equal 20

    testCase "List.zip3 works" <| fun () ->
            let xs = [1; 2; 3]
            let ys = [4; 5; 6]
            let zs = [7; 8; 9]
            let ks = List.zip3 xs ys zs
            let x, y, z = List.last ks
            equal 3 x
            equal 6 y
            equal 9 z

    testCase "List.tryItem works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        List.tryItem 3 xs |> equal (Some 4.)
        List.tryItem 4 xs |> equal None
        List.tryItem -1 xs |> equal None

    testCase "List.tryHead works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        List.tryHead xs |> equal (Some 1.)
        List.tryHead [] |> equal None

    testCase "List.last works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> List.last
        |> equal 4.

    testCase "List.tryLast works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        List.tryLast xs |> equal (Some 4.)
        List.tryLast [] |> equal None

    testCase "List.countBy works" <| fun () ->
        let xs = [1; 2; 3; 4]
        xs |> List.countBy (fun x -> x % 2)
        |> List.length |> equal 2

    testCase "List.groupBy returns valid list" <| fun () ->
        let xs = [1; 2]
        let worked =
            match List.groupBy (fun _ -> true) xs with
            | [true, [1; 2]] -> true
            | _ -> false
        worked |> equal true

    testCase "List.groupBy maintains order" <| fun () ->
        let xs = [ 0,5; 1,5; 2,5; 3,5; 0,6; 1,6; 2,6; 3,6 ]
        let mapped = xs |> List.take 4 |> List.map (fun (x,y) -> x, [x,y; x,y+1])
        let grouped = xs |> List.groupBy fst
        grouped |> equal mapped

    testCase "List.unfold works" <| fun () ->
        let xs = 0. |> List.unfold (fun n -> if n < 3.0 then Some(n+1., n+1.) else None)
        let sum =
            match xs with
            | n1::n2::n3::[] -> n1 + n2 + n3
            | _ -> 0.0
        sum |> equal 6.0

    testCase "List.splitAt works" <| fun () ->
        let li = [1;2;3;4]
        List.splitAt 0 li |> equal ([], [1;2;3;4])
        List.splitAt 3 li |> equal ([1;2;3], [4])
        List.splitAt 4 li |> equal ([1;2;3;4], [])

    testCase "List.windowed works" <| fun () -> // See #1716
        let nums = [ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ]
        List.windowed 3 nums |> equal [[1.0; 1.5; 2.0]; [1.5; 2.0; 1.5]; [2.0; 1.5; 1.0]; [1.5; 1.0; 1.5]]
        List.windowed 5 nums |> equal [[ 1.0; 1.5; 2.0; 1.5; 1.0 ]; [ 1.5; 2.0; 1.5; 1.0; 1.5 ]]
        List.windowed 6 nums |> equal [[ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ]]
        List.windowed 7 nums |> List.isEmpty |> equal true

    testCase "Types with same name as imports work" <| fun () ->
            let li = [List 5]
            equal 5 li.Head.Value

    testCase "List.Item throws exception when index is out of range" <| fun () ->
        let xs = [0]
        (try (xs.Item 1) |> ignore; false with | _ -> true) |> equal true

    testCase "List.except works" <| fun () ->
        List.except [2] [1; 3; 2] |> List.last |> equal 3
        List.except [2] [2; 4; 6] |> List.head |> equal 4
        List.except [1] [1; 1; 1; 1] |> List.isEmpty |> equal true
        List.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> List.isEmpty |> equal true
        List.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> List.isEmpty |> equal true
        List.except [(1, 2)] [(1, 2)] |> List.isEmpty |> equal true
        List.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> List.isEmpty |> equal true
        List.except [{ Bar= "test" }] [{ Bar = "test" }] |> List.isEmpty |> equal true

    testCase "List iterators from range do rewind" <| fun () ->
        let xs = [1..5] |> List.toSeq
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    testCase "List comprehensions returning None work" <| fun () ->
        let spam : string option list = [for _ in 0..5 -> None]
        List.length spam |> equal 6

    testCase "Int list tail doesn't get wrapped with `| 0`" <| fun () -> // See #1447
        let revert xs =
            let rec rev acc (ls: int list) =
                match ls with
                | [] -> acc
                | h::t -> rev (h::acc) t
            rev [] xs
        let res = revert [2;3;4]
        equal 3 res.Length
        equal 4 res.Head

    testCase "List.allPairs works" <| fun () ->
        let xs = [1;2;3;4]
        let ys = ['a';'b';'c';'d';'e';'f']
        List.allPairs xs ys
        |> equal
            [(1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f'); (2, 'a');
             (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f'); (3, 'a'); (3, 'b');
             (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f'); (4, 'a'); (4, 'b'); (4, 'c');
             (4, 'd'); (4, 'e'); (4, 'f')]

    // TODO: Remove conditional compilation after upgrading to dotnet SDK with F# 4.7
    // #if FABLE_COMPILER
    testCase "Implicit yields work" <| fun () ->
        let makeList condition =
            [
                1
                2
                if condition then
                    3
            ]
        makeList true |> List.sum |> equal 6
        makeList false |> List.sum |> equal 3
    // #endif

    testCase "List.splitInto works" <| fun () ->
        [1..10] |> List.splitInto 3 |> equal [ [1..4]; [5..7]; [8..10] ]
        [1..11] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..11] ]
        [1..12] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..12] ]
        [1..5] |> List.splitInto 4 |> equal [ [1..2]; [3]; [4]; [5] ]
        [1..4] |> List.splitInto 20 |> equal [ [1]; [2]; [3]; [4] ]

    testCase "List.transpose works" <| fun () ->
        // integer list
        List.transpose (seq [[1..3]; [4..6]])
        |> equal [[1; 4]; [2; 5]; [3; 6]]
        List.transpose [[1..3]]
        |> equal [[1]; [2]; [3]]
        List.transpose [[1]; [2]]
        |> equal [[1..2]]
        // string list
        List.transpose (seq [["a";"b";"c"]; ["d";"e";"f"]])
        |> equal [["a";"d"]; ["b";"e"]; ["c";"f"]]
        // empty list
        List.transpose []
        |> equal []
        // list of empty lists - m x 0 list transposes to 0 x m (i.e. empty)
        List.transpose [[]]
        |> equal []
        List.transpose [[]; []]
        |> equal []
        // jagged lists
        throwsAnyError (fun () -> List.transpose [[1; 2]; [3]])
        throwsAnyError (fun () -> List.transpose [[1]; [2; 3]])
        throwsAnyError (fun () -> List.transpose [[]; [1; 2]; [3; 4]])
        throwsAnyError (fun () -> List.transpose [[1; 2]; []; [3; 4]])
        throwsAnyError (fun () -> List.transpose [[1; 2]; [3; 4]; []])

  ]