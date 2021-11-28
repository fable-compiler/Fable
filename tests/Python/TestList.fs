module Fable.Tests.List

open Util.Testing

let tryListChoose xss =
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

[<Fact>]
let ``test Some [] works`` () =
    let xs: int list option = Some []
    let ys: int list option = None
    Option.isSome xs |> equal true
    Option.isNone ys |> equal true

[<Fact>]
let ``test List equality works`` () =
    let xs = [1;2;3]
    let ys = [1;2;3]
    let zs = [1;4;3]
    xs = ys |> equal true
    xs = zs |> equal false

[<Fact>]
let ``test List comparison works`` () =
    let xs = [1;2;3]
    let ys = [1;2;3]
    let zs = [1;4;3]
    xs < ys |> equal false
    xs < zs |> equal true

[<Fact>]
let ``test Pattern matching with lists works`` () =
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

[<Fact>]
let ``test List.empty works`` () =
    let xs = List.empty<int>
    List.length xs
    |> equal 0

[<Fact>]
let ``test List.length works`` () =
    let xs = [1.; 2.; 3.; 4.]
    equal 4 xs.Length
    equal 0 [].Length


[<Fact>]
let ``test xs.IsEmpty works`` () =
    let xs = [1; 2; 3; 4]
    let ys = []
    equal false xs.IsEmpty
    equal false [1; 2; 3; 4].IsEmpty
    equal true ys.IsEmpty
    equal true [].IsEmpty

[<Fact>]
let ``test List.Equals works`` () =
    let xs = [1;2;3]
    xs.Equals(xs) |> equal true

[<Fact>]
let ``test xs.Head works`` () =
    let xs = [1; 2; 3; 4]
    equal 1 xs.Head

[<Fact>]
let ``test xs.Tail works`` () =
    let xs = [1; 2; 3; 4]
    equal 2 xs.Tail.Head

[<Fact>]
let ``test xs.Item works`` () =
    let xs = [1; 2; 3; 4]
    equal 4 xs.[3]

[<Fact>]
let ``test List cons works`` () =
    let xs = [1; 2; 3; 4]
    let ys = 3 :: xs
    let zs = List.Cons(4, xs)
    ys.Head + xs.Head
    |> equal zs.Head

[<Fact>]
let ``test List.cons works II`` () =
    let li = [1;2;3;4;5]
    let li2 = li.Tail
    let li3 = [8;9;11] @ li2
    let li3b = [20;16] @ li3.Tail
    let li4 = 14 :: li3b
    li4.[1] |> equal 20
    li4.[3] |> equal 9
    List.length li4 |> equal 9
    List.sum li4 |> equal 84

[<Fact>]
let ``test List.empty works II`` () =
    let xs = 1 :: List.Empty
    let ys = 1 :: List.empty
    xs.Length + ys.Length |> equal 2

[<Fact>]
let ``test List.append works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [0]
    let zs = List.append ys xs
    zs.Head + zs.Tail.Head
    |> equal 1

[<Fact>]
let ``test List.append works II`` () =
    let li = [1;2;3;4;5]
    let li2 = li.Tail
    let li3 = [8;9;11] @ li2
    let li3b = [20;16] @ li3.Tail
    let li4 = li3b @ li2
    li4.[1] |> equal 16
    li4.[9] |> equal 3
    List.length li4 |> equal 12
    List.sum li4 |> equal 84

[<Fact>]
let ``test List.append works with empty list`` () =
    let li = [{| value = 2|}; {| value = 4|};]
    let li = li @ []
    let li = [] @ li
    li
    |> Seq.map (fun x -> 20 / x.value)
    |> Seq.sum
    |> equal 15

[<Fact>]
let ``test List.choose works`` () =
    let xs = [1; 2; 3; 4]
    let result = xs |> List.choose (fun x ->
        if x > 2 then Some x
        else None)
    result.Head + result.Tail.Head
    |> equal 7

[<Fact>]
let ``test List.exactlyOne works`` () =
    let xs = [1.;]
    xs |> List.exactlyOne
    |> equal 1.

    let xs2 = [1.;2.]
    (try List.exactlyOne xs2 |> ignore; false with | _ -> true) |> equal true

    let xs3 = []
    (try List.exactlyOne xs3 |> ignore; false with | _ -> true) |> equal true

[<Fact>]
let ``test List.tryExactlyOne works`` () =
    [1.] |> List.tryExactlyOne |> equal (Some 1.)
    [1.;2.] |> List.tryExactlyOne |> equal None
    [] |> List.tryExactlyOne |> equal None

[<Fact>]
let ``test List.exists works`` () =
    let xs = [1; 2; 3; 4]
    xs |> List.exists (fun x -> x = 2)
    |> equal true

[<Fact>]
let ``test List.exists2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    List.exists2 (fun x y -> x * y = 16) xs ys
    |> equal true

[<Fact>]
let ``test List.filter works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.filter (fun x -> x > 5)
    equal ys.IsEmpty true

// [<Fact>]
// let ``test List.filter doesn't work backwards`` () =
//     let li = [1; 2; 3; 4; 5]
//     li |> List.filteri (fun i _ -> i <> 1) |> equal [1; 3; 4; 5]

[<Fact>]
let ``test List.find works`` () =
    [1; 2; 3; 4]
    |> List.find ((=) 2)
    |> equal 2

[<Fact>]
let ``test List.findIndex works`` () =
    [1; 2; 3; 4]
    |> List.findIndex ((=) 2)
    |> equal 1

[<Fact>]
let ``test List.fold works`` () =
    [1; 2; 3; 4]
    |> List.fold (+) 0
    |> equal 10

[<Fact>]
let ``test List.fold2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    List.fold2 (fun x y z -> x + y + z) 0 xs ys
    |> equal 20

// FIXME: need to handle reduce from right in native.fs
// [<Fact>]
// let ``test List.foldBack works`` () =
//     [1; 2; 3; 4]
//     |> List.foldBack (fun x acc -> acc - x) <| 100
//     |> equal 90

// [<Fact>]
// let ``test List.foldBack with composition works`` () =
//     [1; 2; 3; 4]
//     |> List.foldBack (fun x acc -> acc >> (+) x) <| id <| 2
//     |> equal 12

// [<Fact>]
let ``test List.forall works`` () =
    [1; 2; 3; 4]
    |> List.forall (fun x -> x < 5)
    |> equal true

[<Fact>]
let ``test List.forall2 works`` () =
    ([1; 2; 3; 4], [1; 2; 3; 4])
    ||> List.forall2 (=)
    |> equal true

[<Fact>]
let ``test List.head works`` () =
    [1; 2; 3; 4]
    |> List.head
    |> equal 1

[<Fact>]
let ``test List.init works`` () =
    let xs = List.init 4 float
    xs.Head + xs.Tail.Head
    |> equal 1.

[<Fact>]
let ``test List.isEmpty works`` () =
    List.isEmpty [1] |> equal false
    List.isEmpty [] |> equal true

[<Fact>]
let ``test List.iter works`` () =
    let xs = [1; 2; 3; 4]
    let mutable total = 0
    xs |> List.iter (fun x ->
    total <- total + x)
    equal 10 total

[<Fact>]
let ``test List.iter2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [2; 4; 6; 8]
    let total = ref 0
    List.iter2 (fun x y ->
    total.Value <- total.Value + (y - x)
    ) xs ys
    equal 10 total.Value

[<Fact>]
let ``test List.iteri works`` () =
    let mutable total = 0
    [1; 2; 3; 4]
    |> List.iteri (fun i x ->
        total <- total + (i * x))
    equal 20 total

[<Fact>]
let ``test List.iteri2 works`` () =
    let mutable total = 0
    let xs = [1; 2; 3; 4]
    let ys = [2; 4; 6; 8]
    List.iteri2 (fun i x y ->
    total <- total + i * (y - x)
    ) xs ys
    equal 20 total

[<Fact>]
let ``test List.length works II`` () =
    let xs = [1; 2; 3; 4]
    List.length xs
    |> equal 4


[<Fact>]
let ``test List.item works`` () =
    [1; 2] |> List.item 1 |> equal 2

[<Fact>]
let``test "List.map works`` () =
    let xs = [1;2;3]
    let ys = xs |> List.map ((*) 2)
    equal 4 ys.Tail.Head

[<Fact>]
let ``test List.mapi works`` () =
    let xs = [1]
    let ys = xs |> List.mapi (fun i x -> i * x)
    equal 0 ys.Head

[<Fact>]
let ``test List.map2 works`` () =
    let xs = [1;2]
    let ys = [2;3]
    let zs = List.map2 (fun x y -> x - y) xs ys
    equal -1 zs.Head

[<Fact>]
let ``test List.ofArray works`` () =
    let xs = [|1; 2|]
    let ys = List.ofArray xs
    ys.Head |> equal 1

    let xs1 = [|1.; 2.; 3.; 4.|]
    let ys1 = List.ofArray xs1
    sumFirstList ys1 3 |> equal 6.

[<Fact>]
let ``test List.ofSeq works`` () =
    // let xs = [|1; 2|] :> _ seq
    let ys = List.ofSeq <| seq { yield 1; yield 2 }
    ys.Head |> equal 1
    ys.Length |> equal 2

[<Fact>]
let ``test List.pick works`` () =
    let xs = [1; 2]
    xs |> List.pick (fun x ->
        match x with
        | 2 -> Some x
        | _ -> None)
    |> equal 2

[<Fact>]
let ``test List.reduce works`` () =
    let xs = [1; 2]
    xs |> List.reduce (+)
    |> equal 3

// [<Fact>]
// let ``test List.reduceBack works`` () =
//         let xs = [1; 2]
//         xs |> List.reduceBack (+)
//         |> equal 3

[<Fact>]
let ``test List.replicate works`` () =
    List.replicate 3 3
    |> List.sum |> equal 9

[<Fact>]
let ``test List.rev works`` () =
    let xs = [1; 2; 3]
    let ys = xs |> List.rev
    equal 3 ys.Head

[<Fact>]
let ``test List.scan works`` () =
    let xs = [1; 2; 3; 4]
    let ys = (0, xs) ||> List.scan (fun acc x -> acc - x)
    ys.[3] + ys.[4]
    |> equal -16

[<Fact>]
let ``test List.scanBack works`` () =
    let xs = [1; 2; 3]
    let ys = List.scanBack (fun x acc -> acc - x) xs 0
    ys.Head + ys.Tail.Head
    |> equal -11

[<Fact>]
let ``test List.sort works`` () =
    let xs = [3; 4; 1; -3; 2; 10]
    let ys = ["a"; "c"; "B"; "d"]
    xs |> List.sort |> List.take 3 |> List.sum |> equal 0
    ys |> List.sort |> List.item 1 |> equal "a"

[<Fact>]
let ``test List.sort with tuples works`` () =
    let xs = [3; 1; 1; -3]
    let ys = ["a"; "c"; "B"; "d"]
    (xs, ys) ||> List.zip |> List.sort |> List.item 1 |> equal (1, "B")

// TODO: Python sort cannot take arguments
// [<Fact>]
// let ``test List.sortBy works`` () =
//     let xs = [3; 1; 4; 2]
//     let ys = xs |> List.sortBy (fun x -> -x)
//     ys.Head + ys.Tail.Head
//     |> equal 7

[<Fact>]
let ``test List.sortWith works`` () =
    let xs = [3; 4; 1; 2]
    let ys = xs |> List.sortWith (fun x y -> int(x - y))
    ys.Head + ys.Tail.Head
    |> equal 3

// FIXME:
// [<Fact>]
// let ``test List.sortDescending works`` () =
//     let xs = [3; 4; 1; -3; 2; 10]
//     xs |> List.sortDescending |> List.take 3 |> List.sum |> equal 17
//     let ys = ["a"; "c"; "B"; "d"]
//     ys |> List.sortDescending |> List.item 1 |> equal "c"

[<Fact>]
let ``test List.sortByDescending works`` () =
    let xs = [3; 1; 4; 2]
    let ys = xs |> List.sortByDescending (fun x -> -x)
    ys.Head + ys.Tail.Head
    |> equal 3

[<Fact>]
let ``test List.max works`` () =
    let xs = [1; 2]
    xs |> List.max
    |> equal 2

[<Fact>]
let ``test List.maxBy works`` () =
    let xs = [1; 2]
    xs |> List.maxBy (fun x -> -x)
    |> equal 1

[<Fact>]
let ``test List.min works`` () =
    let xs = [1; 2]
    xs |> List.min
    |> equal 1

[<Fact>]
let ``test List.minBy works`` () =
    let xs = [1; 2]
    xs |> List.minBy (fun x -> -x)
    |> equal 2

[<Fact>]
let ``test List.sum works`` () =
    [1; 2] |> List.sum
    |> equal 3

[<Fact>]
let ``test List.sumBy works`` () =
    [1; 2] |> List.sumBy (fun x -> x*2)
    |> equal 6

[<Fact>]
let ``test List.sum with non numeric types works`` () =
  let p1 = {x=1; y=10}
  let p2 = {x=2; y=20}
  [p1; p2] |> List.sum |> (=) {x=3;y=30} |> equal true

[<Fact>]
let ``test List.sumBy with non numeric types works`` () =
  let p1 = {x=1; y=10}
  let p2 = {x=2; y=20}
  [p1; p2] |> List.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

[<Fact>]
let ``test List.sumBy with numeric projection works`` () =
  let p1 = {x=1; y=10}
  let p2 = {x=2; y=20}
  [p1; p2] |> List.sumBy (fun p -> p.y) |> equal 30

[<Fact>]
let ``test List.sum with non numeric types works II`` () =
    [MyNumber 1; MyNumber 2; MyNumber 3] |> List.sum |> equal (MyNumber 6)

[<Fact>]
let ``test List.sumBy with non numeric types works II`` () =
    [{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }]
    |> List.sumBy (fun x -> x.MyNumber) |> equal (MyNumber 12)

[<Fact>]
let ``test List.skip works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    let ys = xs |> List.skip 1
    ys |> List.head
    |> equal 2.

[<Fact>]
let ``test List.skipWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.skipWhile (fun i -> i <= 3.)
    |> List.head
    |> equal 4.

[<Fact>]
let ``test List.take works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.take 2
    |> List.last
    |> equal 2.
    // List.take should throw an exception if there're not enough elements
    try xs |> List.take 20 |> List.length with _ -> -1
    |> equal -1

[<Fact>]
let ``test List.takeWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.takeWhile (fun i -> i < 3.)
    |> List.last
    |> equal 2.

[<Fact>]
let ``test List.tail works`` () =
    let xs = [1; 2]
    let ys = xs |> List.tail
    equal 1 ys.Length

[<Fact>]
let ``test List.toArray works`` () =
    let ys = List.toArray [1; 2]
    ys.[0] + ys.[1] |> equal 3
    let xs = [1; 1]
    let ys2 = List.toArray (2::xs)
    ys2.[0] + ys2.[1] + ys2.[2] |> equal 4

[<Fact>]
let ``test List.toSeq works`` () =
    [1; 2]
    |> List.toSeq
    |> Seq.tail |> Seq.head
    |> equal 2

[<Fact>]
let ``test List.tryPick works`` () =
    [1; 2]
    |> List.tryPick (function
        | 2 -> Some 2
        | _ -> None)
    |> function
    | Some x -> x
    | None -> 0
    |> equal 2

[<Fact>]
let ``test List.tryFind works`` () =
    [1; 2]
    |> List.tryFind ((=) 5)
    |> equal None

[<Fact>]
let ``test List.tryFindIndex works`` () =
    let xs = [1; 2]
    let ys = xs |> List.tryFindIndex ((=) 2)
    ys.Value |> equal 1
    xs |> List.tryFindIndex ((=) 5) |> equal None

// FIXME: AttributeError: 'list' object has no attribute 'reduceRight'
// [<Fact>]
// let ``test List.unzip works`` () =
//     let xs = [1, 2]
//     let ys, zs = xs |> List.unzip
//     ys.Head + zs.Head
//     |> equal 3

// FIXME: AttributeError: 'list' object has no attribute 'reduceRight'
// [<Fact>]
// let ``test List.unzip3 works`` () =
//     let xs = [(1, 2, 3); (4, 5, 6)]
//     let ys, zs, ks = xs |> List.unzip3
//     ys.[1] + zs.[1] + ks.[1]
//     |> equal 15

[<Fact>]
let ``test List.zip works`` () =
    let xs = [1; 2; 3]
    let ys = [4; 5; 6]
    let zs = List.zip xs ys
    let x, y = zs.Tail.Head
    equal 2 x
    equal 5 y

[<Fact>]
let ``test List snail to append works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [0]
    let zs = ys @ xs
    zs.Head + zs.Tail.Head
    |> equal 1

[<Fact>]
let ``test List slice works`` () =
    let xs = [1; 2; 3; 4]
    xs.[..2] |> List.sum |> equal 6
    xs.[2..] |> List.sum |> equal 7
    xs.[1..2] |> List.sum |> equal 5
    xs.[0..-1] |> List.sum |> equal 0

[<Fact>]
let ``test List.truncate works`` () =
    [1..3] = (List.truncate 3 [1..5]) |> equal true
    [1..5] = (List.truncate 10 [1..5]) |> equal true
    [] = (List.truncate 0 [1..5]) |> equal true
    ["str1";"str2"] = (List.truncate 2 ["str1";"str2";"str3"]) |> equal true
    [] = (List.truncate 0 []) |> equal true
    [] = (List.truncate 1 []) |> equal true

[<Fact>]
let ``test List.choose works with generic arguments`` () =
    let res = tryListChoose [ Some [ "a" ] ]
    equal ["a"] res

[<Fact>]
let ``test List.collect works`` () =
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

// FIXME: AttributeError: 'list' object has no attribute 'forEach'
//[<Fact>]
// let ``test List.concat works`` () =
//     let xs = [[1]; [2]; [3]; [4]]
//     let ys = xs |> List.concat
//     ys.Head  + ys.Tail.Head
//     |> equal 3

//     let xs1 = [[1.; 2.; 3.]; [4.; 5.; 6.]; [7.; 8.; 9.]]
//     let ys1 = xs1 |> List.concat
//     sumFirstList ys1 7
//     |> equal 28.

[<Fact>]
let ``test List.contains works`` () =
    let xs = [1; 2; 3; 4]
    xs |> List.contains 2 |> equal true
    xs |> List.contains 0 |> equal false

[<Fact>]
let ``test List.contains lambda doesn't clash`` () =
    let modifyList current x =
        let contains = current |> List.contains x
        match contains with
            | true -> current |> (List.filter (fun a -> a <> x))
            | false -> x::current
    let l = [1;2;3;4]
    (modifyList l 1) |> List.contains 1 |> equal false
    (modifyList l 5) |> List.contains 5 |> equal true

[<Fact>]
let ``test List.average works`` () =
    List.average [1.; 2.; 3.; 4.]
    |> equal 2.5

[<Fact>]
let ``test List.averageBy works`` () =
    [1.; 2.; 3.; 4.]
    |> List.averageBy (fun x -> x * 2.)
    |> equal 5.


[<Fact>]
let ``test List.singleton works`` () =
    let xs = List.singleton 42
    xs
    |> equal [42]


[<Fact>]
let ``test List.collect works II`` () =
    let xs = ["a"; "fable"; "bar" ]
    xs
    |> List.collect (fun a -> [a.Length])
    |> equal [1; 5; 3]
