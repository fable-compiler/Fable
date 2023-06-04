module Fable.Tests.ListTests

open Util.Testing

type List(x: int) =
    member val Value = x

type ExceptFoo = { Bar:string }

// let testListChoose xss =
//     let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
//     xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

let rec sumFirstList (zs: float list) (n: int): float =
    match n with
    | 0 -> 0.
    | 1 -> zs.Head
    | _ -> zs.Head + sumFirstList zs.Tail (n-1)

type Point =
    { x: int; y: int }
    static member Zero = { x=0; y=0 }
    static member Neg(p: Point) = { x = -p.x; y = -p.y }
    static member (+) (p1, p2) = { x = p1.x + p2.x; y = p1.y + p2.y }

// type MyNumber =
//     | MyNumber of int
//     static member Zero = MyNumber 0
//     static member (+) (MyNumber x, MyNumber y) =
//         MyNumber(x + y)
//     static member DivideByInt (MyNumber x, i: int) =
//         MyNumber(x / i)

// type MyNumberWrapper =
//     { MyNumber: MyNumber }

[<Fact>]
let ``List.Empty works`` () =
    let xs: int list = []
    List.isEmpty xs |> equal true
    let xs = List.empty<int>
    List.isEmpty xs |> equal true

[<Fact>]
let ``List.empty generic works`` () =
    let xs = []
    List.isEmpty xs |> equal true

[<Fact>]
let ``List.empty works`` () =
    let xs = 1 :: List.Empty
    let ys = 1 :: List.empty
    xs.Length + ys.Length |> equal 2

[<Fact>]
let ``List.Head works`` () =
    let xs = [1; 2; 3; 4]
    equal 1 xs.Head

[<Fact>]
let ``List.Tail works`` () =
    let xs = [1; 2; 3; 4]
    equal 2 xs.Tail.Head

[<Fact>]
let ``List.Cons works`` () =
    let xs = 1::2::3::[]
    List.head xs |> equal 1
    let xs = List.tail xs
    List.head xs |> equal 2
    let xs = List.tail xs
    List.head xs |> equal 3
    let xs = List.tail xs
    List.isEmpty xs |> equal true

[<Fact>]
let ``List.Cons works II`` () =
    let xs = [1; 2; 3; 4]
    let ys = 3 :: xs
    let zs = List.Cons(4, xs)
    ys.Head + xs.Head
    |> equal zs.Head

[<Fact>]
let ``List.Cons works III`` () =
    let li = [1;2;3;4;5]
    let li2 = li.Tail
    let li3 = [8;9;11] @ li2
    let li3b = [20;16] @ li3.Tail
    let li4 = 14 :: li3b
    li4[1] |> equal 20
    li4[3] |> equal 9
    List.length li4 |> equal 9
    List.sum li4 |> equal 84

[<Fact>]
let ``List.Length works`` () =
    let xs = [1; 2; 3; 4]
    let ys: int list = []
    equal 4 xs.Length
    equal 0 ys.Length

[<Fact>]
let ``List.IsEmpty works`` () =
    let xs = [1; 2; 3; 4]
    let ys: int list = []
    let zs = List.empty<int>
    equal false xs.IsEmpty
    equal true ys.IsEmpty
    equal true zs.IsEmpty

[<Fact>]
let ``List equality works`` () =
    let a1 = [1; 2; 3]
    let a2 = [1; 2; 3]
    let a3 = [1; 2; 4]
    let a4 = [1; 2; 3; 4]
    a1 = a1 |> equal true
    a1 = a2 |> equal true
    a1 = a3 |> equal false
    a1 = a4 |> equal false
    a1 <> a1 |> equal false
    a1 <> a2 |> equal false
    a1 <> a3 |> equal true
    a1 <> a4 |> equal true

[<Fact>]
let ``List.Equals works`` () =
    let a1 = [1; 2; 3]
    let a2 = [1; 2; 3]
    let a3 = [1; 2; 4]
    let a4 = [1; 2; 3; 4]
    a1.Equals(a1) |> equal true
    a1.Equals(a2) |> equal true
    a1.Equals(a3) |> equal false
    a1.Equals(a4) |> equal false

[<Fact>]
let ``List comparison works`` () =
    let a1 = [1; 2; 3]
    let a2 = [1; 2; 3]
    let a3 = [1; 2; 4]
    let a4 = [1; 2; 3; 4]
    a1 < a1 |> equal false
    a1 < a2 |> equal false
    a1 < a3 |> equal true
    a1 < a4 |> equal true
    a1 > a1 |> equal false
    a1 > a2 |> equal false
    a1 > a3 |> equal false
    a1 > a4 |> equal false

[<Fact>]
let ``List compare works`` () =
    let a1 = [1; 2; 3]
    let a2 = [1; 2; 3]
    let a3 = [1; 2; 4]
    let a4 = [1; 2; 3; 4]
    compare a1 a1 |> equal 0
    compare a1 a2 |> equal 0
    compare a1 a3 |> equal -1
    compare a1 a4 |> equal -1
    compare a3 a4 |> equal 1
    compare a3 a2 |> equal 1
    compare a4 a2 |> equal 1
    compare a4 a3 |> equal -1

[<Fact>]
let ``List.compareWith works`` () =
    let a1 = [1; 2; 3]
    let a2 = [1; 2; 3]
    let a3 = [1; 2; 4]
    let a4 = [1; 2; 3; 4]
    List.compareWith compare a1 a1 |> equal 0
    List.compareWith compare a1 a2 |> equal 0
    List.compareWith compare a1 a3 |> equal -1
    List.compareWith compare a1 a4 |> equal -1
    List.compareWith compare a3 a4 |> equal 1
    List.compareWith compare a3 a2 |> equal 1
    List.compareWith compare a4 a2 |> equal 1
    List.compareWith compare a4 a3 |> equal -1

[<Fact>]
let ``List.Item works`` () =
    let xs = [1; 2; 3; 4]
    equal 4 xs[3]

[<Fact>]
let ``List.item works`` () =
    let xs = [1; 2]
    xs |> List.item 1 |> equal 2

[<Fact>]
let ``List.length works`` () =
    let xs = 1::2::3::[]
    List.length xs |> equal 3
    let xs = [1; 2; 3; 4]
    List.length xs |> equal 4

[<Fact>]
let ``List option works`` () =
    let xs: int list option = Some []
    let ys: int list option = None
    Option.isSome xs |> equal true
    Option.isNone ys |> equal true

[<Fact>]
let ``Pattern matching with lists works`` () =
    match []: int list with [] -> true | _ -> false
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
let ``List.append works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [0]
    let zs = List.append ys xs
    zs.Head + zs.Tail.Head
    |> equal 1

[<Fact>]
let ``List.append works II`` () =
    let li = [1;2;3;4;5]
    let li2 = li.Tail
    let li3 = [8;9;11] @ li2
    let li3b = [20;16] @ li3.Tail
    let li4 = li3b @ li2
    li4[1] |> equal 16
    li4[9] |> equal 3
    List.length li4 |> equal 12
    List.sum li4 |> equal 84

[<Fact>]
let ``List.append works with empty list`` () =
    let li = [{| value = 2|}; {| value = 4|}]
    let li = li @ []
    let li = [] @ li
    li
    |> List.map (fun x -> 20 / x.value)
    |> List.sum
    |> equal 15

[<Fact>]
let ``List.choose works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.choose (fun x ->
        if x > 2 then Some x
        else None)
    ys |> List.toArray |> equal [|3; 4|]

[<Fact>]
let ``List.exactlyOne works`` () =
    let xs = [1.]
    xs |> List.exactlyOne
    |> equal 1.

[<Fact>]
let ``List.exactlyOne works II`` () =
    let xs2 = [1.; 2.]
    throwsAnyError (fun () -> List.exactlyOne xs2 |> ignore)
    let xs3: float list = []
    throwsAnyError (fun () -> List.exactlyOne xs3 |> ignore)

[<Fact>]
let ``List.tryExactlyOne works`` () =
    [1.] |> List.tryExactlyOne |> equal (Some 1.)
    [1.; 2.] |> List.tryExactlyOne |> equal None
    let xs: float list = []
    xs |> List.tryExactlyOne |> equal None

[<Fact>]
let ``List.exists works`` () =
    let xs = [1; 2; 3; 4]
    xs |> List.exists (fun x -> x = 2)
    |> equal true

[<Fact>]
let ``List.exists2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    List.exists2 (fun x y -> x * y = 16) xs ys
    |> equal true

[<Fact>]
let ``List.filter works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.filter (fun x -> x % 2 = 0)
    ys |> List.toArray |> equal [|2; 4|]

[<Fact>]
let ``List.filter doesn't work backwards`` () = // See #1672
    let mutable i = -1
    let xs = [1; 2; 3; 4; 5]
    let ys = xs |> List.filter (fun x -> i <- i + 1; i <> 1)
    ys |> List.toArray |> equal [|1; 3; 4; 5|]

[<Fact>]
let ``List.where works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.where (fun x -> x % 2 = 1)
    ys |> List.toArray |> equal [|1; 3|]

[<Fact>]
let ``List.find works`` () =
    [1; 2; 3; 4]
    |> List.find ((=) 2)
    |> equal 2

[<Fact>]
let ``List.findIndex works`` () =
    [1; 2; 3; 4]
    |> List.findIndex ((=) 2)
    |> equal 1

[<Fact>]
let ``List.fold works`` () =
    [1; 2; 3; 4]
    |> List.fold (+) 0
    |> equal 10

[<Fact>]
let ``List.fold2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    let total = List.fold2 (fun x y z -> x + y + z) 0 xs ys
    total |> equal 20

[<Fact>]
let ``List.foldBack works`` () =
    let xs = [1; 2; 3; 4]
    let total = List.foldBack (fun x acc -> acc - x) xs 0
    total |> equal -10

[<Fact>]
let ``List.foldBack2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    let total = List.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
    total |> equal -4

[<Fact>]
let ``List.foldBack with composition works`` () =
    [1; 2; 3; 4]
    |> List.foldBack (fun x acc -> acc >> (+) x) <| id <| 2
    |> equal 12

[<Fact>]
let ``List.forall works`` () =
    [1; 2; 3; 4]
    |> List.forall (fun x -> x < 5)
    |> equal true

[<Fact>]
let ``List.forall2 works`` () =
    ([1; 2; 3; 4], [1; 2; 3; 4])
    ||> List.forall2 (=)
    |> equal true

[<Fact>]
let ``List.head works`` () =
    [1; 2; 3; 4]
    |> List.head
    |> equal 1

[<Fact>]
let ``List.init works`` () =
    let xs = List.init 4 float
    xs |> List.toArray |> equal [|0.; 1.; 2.; 3.|]

[<Fact>]
let ``List.isEmpty works`` () =
    List.isEmpty [1] |> equal false
    List.isEmpty<int> [] |> equal true

[<Fact>]
let ``List.iter works`` () =
    let xs = [1; 2; 3; 4]
    let mutable total = 0
    xs |> List.iter (fun x ->
        total <- total + x)
    total |> equal 10

[<Fact>]
let ``List.iter2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [2; 4; 6; 8]
    let mutable total = 0
    List.iter2 (fun x y ->
        total <- total + (y - x)
        ) xs ys
    total |> equal 10

[<Fact>]
let ``List.iteri works`` () =
    let xs = [1; 2; 3; 4]
    let mutable total = 0
    xs |> List.iteri (fun i x ->
        total <- total + (i * x))
    total |> equal 20

[<Fact>]
let ``List.iteri2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [2; 4; 6; 8]
    let mutable total = 0
    List.iteri2 (fun i x y ->
        total <- total + i * (y - x)
        ) xs ys
    total |> equal 20

[<Fact>]
let ``List.map works`` () =
    let xs = [1;2;3]
    let ys = xs |> List.map ((*) 2)
    ys |> List.toArray |> equal [|2; 4; 6|]

[<Fact>]
let ``List.mapi works`` () =
    let xs = [1; 2; 3]
    let ys = xs |> List.mapi (fun i x -> i * x)
    ys |> List.toArray |> equal [|0; 2; 6|]

[<Fact>]
let ``List.map2 works`` () =
    let xs = [1;2]
    let ys = [2;3]
    let zs = List.map2 (fun x y -> y - x) xs ys
    zs |> List.toArray |> equal [|1; 1|]

[<Fact>]
let ``List.ofArray works`` () =
    let xs = [|1; 2|]
    let ys = List.ofArray xs
    ys.Head |> equal 1
    let xs1 = [|1.; 2.; 3.; 4.|]
    let ys1 = List.ofArray xs1
    sumFirstList ys1 3 |> equal 6.

[<Fact>]
let ``List.ofSeq works`` () =
    // let xs = [|1; 2|] :> _ seq
    let ys = List.ofSeq <| seq { yield 1; yield 2 }
    ys.Head |> equal 1
    ys.Length |> equal 2

[<Fact>]
let ``List.pick works`` () =
    let xs = [1; 2]
    xs |> List.pick (fun x ->
        match x with
        | 2 -> Some x
        | _ -> None)
    |> equal 2

[<Fact>]
let ``List.reduce works`` () =
    let xs = [1; 2]
    xs |> List.reduce (+)
    |> equal 3

[<Fact>]
let ``List.reduceBack works`` () =
    let xs = [1; 2]
    xs |> List.reduceBack (+)
    |> equal 3

[<Fact>]
let ``List.replicate works`` () =
    List.replicate 3 4
    |> List.toArray |> equal [|4;4;4|]

[<Fact>]
let ``List.rev works`` () =
    let xs = [1; 2; 3]
    let ys = xs |> List.rev
    ys.Head |> equal 3
    ys.Length |> equal 3

[<Fact>]
let ``List.scan works`` () =
    let xs = [1; 2; 3; 4]
    let ys = (0, xs) ||> List.scan (fun acc x -> acc - x)
    ys[3] + ys[4]
    |> equal -16

[<Fact>]
let ``List.scanBack works`` () =
    let xs = [1; 2; 3]
    let ys = List.scanBack (fun x acc -> acc - x) xs 0
    ys.Head + ys.Tail.Head
    |> equal -11

[<Fact>]
let ``List.sort works`` () =
    let xs = [3; 4; 1; -3; 2; 10]
    let ys = ["a"; "c"; "B"; "d"]
    xs |> List.sort |> List.take 3 |> List.sum |> equal 0
    ys |> List.sort |> List.item 1 |> equal "a"

[<Fact>]
let ``List.sort with tuples works`` () =
    let xs = [3; 1; 1; -3]
    let ys = ["a"; "c"; "B"; "d"]
    (xs, ys) ||> List.zip |> List.sort |> List.item 1 |> equal (1, "B")

[<Fact>]
let ``List.sortBy works`` () =
    let xs = [3; 1; 4; 2]
    let ys = xs |> List.sortBy (fun x -> -x)
    ys.Head + ys.Tail.Head
    |> equal 7

[<Fact>]
let ``List.sortWith works`` () =
    let xs = [3; 4; 1; 2]
    let ys = xs |> List.sortWith (fun x y -> int(x - y))
    ys |> List.toArray |> equal [|1; 2; 3; 4|]

[<Fact>]
let ``List.sortDescending works`` () =
    let xs = [3; 4; 1; -3; 2; 10]
    xs |> List.sortDescending |> List.take 3 |> List.sum |> equal 17
    let ys = ["a"; "c"; "B"; "d"]
    ys |> List.sortDescending |> List.item 1 |> equal "c"

[<Fact>]
let ``List.sortByDescending works`` () =
    let xs = [3; 1; 4; 2]
    let ys = xs |> List.sortByDescending (fun x -> -x)
    ys.Head + ys.Tail.Head
    |> equal 3

[<Fact>]
let ``List.max works`` () =
    let xs = [1; 2]
    xs |> List.max
    |> equal 2

[<Fact>]
let ``List.maxBy works`` () =
    let xs = [1; 2]
    xs |> List.maxBy (fun x -> -x)
    |> equal 1

[<Fact>]
let ``List.min works`` () =
    let xs = [1; 2]
    xs |> List.min
    |> equal 1

[<Fact>]
let ``List.minBy works`` () =
    let xs = [1; 2]
    xs |> List.minBy (fun x -> -x)
    |> equal 2

[<Fact>]
let ``List.sum works`` () =
    [1; 2] |> List.sum
    |> equal 3

[<Fact>]
let ``List.sumBy works`` () =
    [1; 2] |> List.sumBy (fun x -> x*2)
    |> equal 6

// [<Fact>]
// let ``List.sum with non numeric types works`` () =
//     let p1 = {x=1; y=10}
//     let p2 = {x=2; y=20}
//     [p1; p2] |> List.sum |> (=) {x=3;y=30} |> equal true

// [<Fact>]
// let ``List.sumBy with non numeric types works`` () =
//     let p1 = {x=1; y=10}
//     let p2 = {x=2; y=20}
//     [p1; p2] |> List.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

[<Fact>]
let ``List.sumBy with numeric projection works`` () =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> List.sumBy (fun p -> p.y) |> equal 30

// [<Fact>]
// let ``List.sum with non numeric types works II`` () =
//     [MyNumber 1; MyNumber 2; MyNumber 3]
//     |> List.sum
//     |> equal (MyNumber 6)

// [<Fact>]
// let ``List.sumBy with non numeric types works II`` () =
//     [{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }]
//     |> List.sumBy (fun x -> x.MyNumber)
//     |> equal (MyNumber 12)

[<Fact>]
let ``List.skip works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.skip 2
    |> List.toArray |> equal [|3.; 4.; 5.|]

[<Fact>]
let ``List.skipWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.skipWhile (fun i -> i < 3.)
    |> List.toArray |> equal [|3.; 4.; 5.|]

[<Fact>]
let ``List.take works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.take 2
    |> List.toArray |> equal [|1.; 2.|]

[<Fact>]
let ``List.take works II`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    // List.take should throw an exception if there're not enough elements
    throwsAnyError (fun () -> xs |> List.take 20 |> List.length |> ignore)

[<Fact>]
let ``List.takeWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> List.takeWhile (fun i -> i < 3.)
    |> List.toArray |> equal [|1.; 2.|]

[<Fact>]
let ``List.tail works`` () =
    let xs = [1; 2; 3]
    xs |> List.tail
    |> List.toArray |> equal [|2; 3|]

[<Fact>]
let ``List.toArray works`` () =
    let xs = 1::2::3::[]
    xs |> List.toArray |> equal [|1;2;3|]

[<Fact>]
let ``List.toArray works II`` () =
    let ys = List.toArray [1; 2]
    ys[0] + ys[1] |> equal 3
    let xs = [1; 1]
    let ys2 = List.toArray (2::xs)
    ys2[0] + ys2[1] + ys2[2] |> equal 4

[<Fact>]
let ``List.toSeq works`` () =
    [1; 2]
    |> List.toSeq
    |> Seq.tail |> Seq.head
    |> equal 2

[<Fact>]
let ``List.tryPick works`` () =
    [1; 2]
    |> List.tryPick (function
        | 2 -> Some 2
        | _ -> None)
    |> function
    | Some x -> x
    | None -> 0
    |> equal 2

[<Fact>]
let ``List.tryFind works`` () =
    [1; 2]
    |> List.tryFind ((=) 5)
    |> equal None

[<Fact>]
let ``List.tryFindIndex works`` () =
    let xs = [1; 2]
    let ys = xs |> List.tryFindIndex ((=) 2)
    ys.Value |> equal 1
    xs |> List.tryFindIndex ((=) 5) |> equal None

[<Fact>]
let ``List.unzip works`` () =
    let xs = [1, 2]
    let ys, zs = xs |> List.unzip
    ys.Head + zs.Head
    |> equal 3

[<Fact>]
let ``List.unzip3 works`` () =
    let xs = [(1, 2, 3); (4, 5, 6)]
    let ys, zs, ks = xs |> List.unzip3
    ys[1] + zs[1] + ks[1]
    |> equal 15

[<Fact>]
let ``List.zip works`` () =
    let xs = [1; 2; 3]
    let ys = [4; 5; 6]
    let zs = List.zip xs ys
    let x, y = zs.Tail.Head
    equal 2 x
    equal 5 y

[<Fact>]
let ``List.zip3 works`` () =
    let xs = [1; 2; 3]
    let ys = [4; 5; 6]
    let zs = [7; 8; 9]
    let ks = List.zip3 xs ys zs
    let x, y, z = List.last ks
    equal 3 x
    equal 6 y
    equal 9 z

[<Fact>]
let ``List snail to append works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [0]
    let zs = ys @ xs
    zs |> List.toArray |> equal [|0; 1; 2; 3; 4|]

[<Fact>]
let ``List slice works`` () =
    let xs = [1; 2; 3; 4]
    xs[..2] |> List.toArray |> equal [|1; 2; 3|]
    xs[2..] |> List.toArray |> equal [|3; 4|]
    xs[1..2] |> List.toArray |> equal [|2; 3|]
    xs[0..-1] |> List.toArray |> equal [||]
    xs[-50..50] |> List.toArray |> equal [|1; 2; 3; 4|]

[<Fact>]
let ``List.truncate works`` () =
    let xs = [1; 2; 3; 4; 5]
    let ys = List.truncate 3 xs
    ys |> List.toArray |> equal [|1;2;3|]

// [<Fact>]
// let ``List.truncate works II`` () =
//     [1..3] = (List.truncate 3 [1..5]) |> equal true
//     [1..5] = (List.truncate 10 [1..5]) |> equal true
//     [] = (List.truncate 0 [1..5]) |> equal true
//     ["str1";"str2"] = (List.truncate 2 ["str1";"str2";"str3"]) |> equal true
//     [] = (List.truncate 0 []) |> equal true
//     [] = (List.truncate 1 []) |> equal true

// [<Fact>]
// let ``List.choose works with generic arguments`` () =
//     let res = testListChoose [ Some [ "a" ] ]
//     equal ["a"] res

[<Fact>]
let ``List.collect works`` () =
    let xs = [[1]; [2]; [3]; [4]]
    let ys = xs |> List.collect id
    ys.Head + ys.Tail.Head |> equal 3
    let list1 = [10.; 20.; 30.]
    let collectList = List.collect (fun x -> [x*1.; x*2.; x*3.]) list1
    sumFirstList collectList 9 |> equal 360.
    let xs = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
    let ys = xs |> List.collect id
    sumFirstList ys 5 |> equal 15.

[<Fact>]
let ``List.concat works`` () =
    let xs = [[1]; [2]; [3]; [4]]
    let ys = xs |> List.concat
    ys.Head  + ys.Tail.Head |> equal 3
    let xs1 = [[1.; 2.; 3.]; [4.; 5.; 6.]; [7.; 8.; 9.]]
    let ys1 = xs1 |> List.concat
    sumFirstList ys1 7 |> equal 28.

[<Fact>]
let ``List.contains works`` () =
    let xs = [1; 2; 3; 4]
    xs |> List.contains 2 |> equal true
    xs |> List.contains 0 |> equal false

// [<Fact>]
// let ``List.contains lambda doesn't clash`` () =
//     let modifyList current x =
//         let contains = current |> List.contains x
//         match contains with
//             | true -> current |> (List.filter (fun a -> a <> x))
//             | false -> x::current
//     let l = [1;2;3;4]
//     (modifyList l 1) |> List.contains 1 |> equal false
//     (modifyList l 5) |> List.contains 5 |> equal true

[<Fact>]
let ``List.average works`` () =
    List.average [1.; 2.; 3.; 4.]
    |> equal 2.5

[<Fact>]
let ``List.averageBy works`` () =
    [1.; 2.; 3.; 4.]
    |> List.averageBy (fun x -> x * 2.)
    |> equal 5.

// [<Fact>]
// let ``List.average works with custom types`` () =
//     [MyNumber 1; MyNumber 2; MyNumber 3] |> List.average |> equal (MyNumber 2)

// [<Fact>]
// let ``List.averageBy works with custom types`` () =
//     [{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }]
//     |> List.averageBy (fun x -> x.MyNumber) |> equal (MyNumber 4)

[<Fact>]
let ``List.distinct works`` () =
    let xs = [1; 1; 1; 2; 2; 3; 3]
    let ys = xs |> List.distinct
    ys |> List.length |> equal 3
    ys |> List.sum |> equal 6

[<Fact>]
let ``List.distinct with tuples works`` () =
    let xs = [(1, 2); (2, 3); (1, 2)]
    let ys = xs |> List.distinct
    ys |> List.length |> equal 2
    ys |> List.sumBy fst |> equal 3

[<Fact>]
let ``List.distinctBy works`` () =
    let xs = [4; 4; 4; 6; 6; 5; 5]
    let ys = xs |> List.distinctBy (fun x -> x % 2)
    ys |> List.length |> equal 2
    ys |> List.head >= 4 |> equal true

[<Fact>]
let ``List.distinctBy with tuples works`` () =
    let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
    let ys = xs |> List.distinctBy (fun (x,_) -> x % 2)
    ys |> List.length |> equal 2
    ys |> List.head |> fst >= 4 |> equal true

[<Fact>]
let ``List.findBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.find ((>) 4.) |> equal 1.
    xs |> List.findBack ((>) 4.) |> equal 3.

[<Fact>]
let ``List.findIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.findIndex ((>) 4.) |> equal 0
    xs |> List.findIndexBack ((>) 4.) |> equal 2

[<Fact>]
let ``List.tryFindBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> List.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> List.tryFindBack ((=) 5.) |> equal None

[<Fact>]
let ``List.tryFindIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> List.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> List.tryFindIndexBack ((=) 5.) |> equal None

[<Fact>]
let ``List.indexed works`` () =
    let xs = ["a"; "b"; "c"] |> List.indexed
    xs.Length |> equal 3
    fst xs[2] |> equal 2
    snd xs[2] |> equal "c"

[<Fact>]
let ``List.map3 works`` () =
    let xs = [1;2;3]
    let ys = [5;4;3]
    let zs = [7;8;9]
    let ks = List.map3 (fun x y z -> z - y - x) xs ys zs
    ks |> List.toArray |> equal [|1; 2; 3|]

[<Fact>]
let ``List.mapi2 works`` () =
    let xs = [7;8;9]
    let ys = [5;4;3]
    let zs = List.mapi2 (fun i x y -> i * (x - y)) xs ys
    zs |> List.toArray |> equal [|0; 4; 12|]

[<Fact>]
let ``List.mapFold works`` () =
    let xs = [1y; 2y; 3y; 4y]
    let res = xs |> List.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
    fst res |> List.toArray |> equal [|2y; 4y; 6y; 8y|]
    snd res |> equal 10y

[<Fact>]
let ``List.mapFoldBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let res = List.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
    fst res |> List.toArray |> equal [|-2.; -4.; -6.; -8.|]
    snd res |> equal -10.

[<Fact>]
let ``List.mapFold works II`` () = // See #842
    let f x y = x,y
    let xs, _ = List.mapFold f "a" ["b"]
    equal "a" xs.Head

[<Fact>]
let ``List.mapFoldBack works II`` () =
    let f x y = x,y
    let xs, _ = List.mapFoldBack f ["a"] "b"
    equal "a" xs.Head

[<Fact>]
let ``List.partition works`` () =
    let xs = [1; 2; 3; 4; 5]
    let ys, zs = xs |> List.partition (fun x -> x % 2 = 0)
    ys |> List.toArray |> equal [| 2; 4 |]
    zs |> List.toArray |> equal [| 1; 3; 5 |]

[<Fact>]
let ``List.pairwise works`` () =
    ([]: int list) |> List.pairwise |> List.isEmpty |> equal true
    [1] |> List.pairwise |> List.isEmpty |> equal true
    [1; 2] |> Seq.pairwise |> Seq.head |> equal (1, 2)
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.pairwise
    ys |> List.toArray |> equal [|(1, 2); (2, 3); (3, 4)|]

[<Fact>]
let ``List.permute works`` () =
    let xs = [1; 2; 3; 4; 5; 6]
    let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
    ys |> List.toArray |> equal [| 2; 1; 4; 3; 6; 5 |]

[<Fact>]
let ``List.chunkBySize works`` () =
    [1..8] |> List.chunkBySize 4 |> equal [ [1..4]; [5..8] ]
    [1..10] |> List.chunkBySize 4 |> equal [ [1..4]; [5..8]; [9..10] ]

[<Fact>]
let ``List.range works`` () =
    [1..5]
    |> List.reduce (+)
    |> equal 15
    [0..2..9]
    |> List.reduce (+)
    |> equal 20

[<Fact>]
let ``List.tryItem works`` () =
    let xs = [1.; 2.; 3.; 4.]
    List.tryItem 3 xs |> equal (Some 4.)
    List.tryItem 4 xs |> equal None
    List.tryItem -1 xs |> equal None

[<Fact>]
let ``List.tryHead works`` () =
    let xs = [1.; 2.; 3.; 4.]
    List.tryHead xs |> equal (Some 1.)
    List.tryHead<float> [] |> equal None

[<Fact>]
let ``List.last works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.last |> equal 4.

[<Fact>]
let ``List.tryLast works`` () =
    let xs = [1.; 2.; 3.; 4.]
    List.tryLast xs |> equal (Some 4.)
    List.tryLast<float> [] |> equal None

[<Fact>]
let ``List.countBy works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> List.countBy (fun x -> x % 2)
    ys |> List.toArray |> equal [|(1, 2); (0, 2)|]

[<Fact>]
let ``List.groupBy returns valid list`` () =
    let xs = [1; 2]
    let worked =
        match List.groupBy (fun _ -> true) xs with
        | [true, [1; 2]] -> true
        | _ -> false
    worked |> equal true

[<Fact>]
let ``List.groupBy maintains order`` () =
    let xs = [ 0,5; 1,5; 2,5; 3,5; 0,6; 1,6; 2,6; 3,6 ]
    let mapped = xs |> List.take 4 |> List.map (fun (x,y) -> x, [x,y; x,y+1])
    let grouped = xs |> List.groupBy fst
    grouped |> equal mapped

[<Fact>]
let ``List.unfold works`` () =
    let xs = 0. |> List.unfold (fun n -> if n < 3.0 then Some(n+1., n+1.) else None)
    let sum =
        match xs with
        | n1::n2::n3::[] -> n1 + n2 + n3
        | _ -> 0.0
    sum |> equal 6.0

[<Fact>]
let ``List.splitAt works`` () =
    let li = [1;2;3;4]
    List.splitAt 0 li |> equal ([], [1;2;3;4])
    List.splitAt 3 li |> equal ([1;2;3], [4])
    List.splitAt 4 li |> equal ([1;2;3;4], [])

[<Fact>]
let ``List.windowed works`` () = // See #1716
    let nums = [ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ]
    List.windowed 3 nums |> equal [[1.0; 1.5; 2.0]; [1.5; 2.0; 1.5]; [2.0; 1.5; 1.0]; [1.5; 1.0; 1.5]]
    List.windowed 5 nums |> equal [[ 1.0; 1.5; 2.0; 1.5; 1.0 ]; [ 1.5; 2.0; 1.5; 1.0; 1.5 ]]
    List.windowed 6 nums |> equal [[ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ]]
    List.windowed 7 nums |> List.isEmpty |> equal true

[<Fact>]
let ``Types with same name as imports work`` () =
    let li = [List 5]
    equal 5 li.Head.Value

[<Fact>]
let ``List.Item throws exception when index is out of range`` () =
    let xs = [0]
    throwsAnyError (fun () -> xs.Item 1 |> ignore)

[<Fact>]
let ``List.except works`` () =
    List.except [2] [1; 3; 2] |> List.last |> equal 3
    List.except [2] [2; 4; 6] |> List.head |> equal 4
    List.except [1] [1; 1; 1; 1] |> List.isEmpty |> equal true
    List.except [49] [7; 49] |> List.last |> equal 7
    List.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> List.isEmpty |> equal true
    List.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> List.isEmpty |> equal true
    List.except [(1, 2)] [(1, 2)] |> List.isEmpty |> equal true
    List.except [{ Bar= "test" }] [{ Bar = "test" }] |> List.isEmpty |> equal true
    // List.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> List.isEmpty |> equal true

[<Fact>]
let ``List iterators from range do rewind`` () =
    let xs = [1..5] |> List.toSeq
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

// [<Fact>]
// let ``List comprehensions returning None work`` () =
//     let spam : string option list = [for _ in 0..5 -> None]
//     List.length spam |> equal 6

[<Fact>]
let ``Int list tail doesn't get wrapped with | 0`` () = // See #1447
    let revert xs =
        let rec rev acc (ls: int list) =
            match ls with
            | [] -> acc
            | h::t -> rev (h::acc) t
        rev [] xs
    let res = revert [2;3;4]
    equal 3 res.Length
    equal 4 res.Head

[<Fact>]
let ``List.allPairs works`` () =
    let xs = [1;2;3;4]
    let ys = ['a';'b';'c';'d';'e';'f']
    List.allPairs xs ys
    |> equal
        [(1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f'); (2, 'a');
            (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f'); (3, 'a'); (3, 'b');
            (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f'); (4, 'a'); (4, 'b'); (4, 'c');
            (4, 'd'); (4, 'e'); (4, 'f')]

[<Fact>]
let ``Implicit yields work`` () =
    let makeList condition =
        [
            1
            2
            if condition then
                3
        ]
    makeList true |> List.sum |> equal 6
    makeList false |> List.sum |> equal 3

[<Fact>]
let ``List.splitInto works`` () =
    [1..10] |> List.splitInto 3 |> equal [ [1..4]; [5..7]; [8..10] ]
    [1..11] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..11] ]
    [1..12] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..12] ]
    [1..5] |> List.splitInto 4 |> equal [ [1..2]; [3]; [4]; [5] ]
    [1..4] |> List.splitInto 20 |> equal [ [1]; [2]; [3]; [4] ]

[<Fact>]
let ``List.transpose works`` () =
    // integer list
    List.transpose (seq [[1..3]; [4..6]]) |> equal [[1; 4]; [2; 5]; [3; 6]]
    List.transpose [[1..3]] |> equal [[1]; [2]; [3]]
    List.transpose [[1]; [2]] |> equal [[1..2]]
    // string list
    List.transpose (seq [["a";"b";"c"]; ["d";"e";"f"]])
    |> equal [["a";"d"]; ["b";"e"]; ["c";"f"]]
    // empty list
    List.transpose<int> [] |> equal []
    // list of empty lists - m x 0 list transposes to 0 x m (i.e. empty)
    List.transpose<int> [[]] |> equal []
    List.transpose<int> [[]; []] |> equal []
    // jagged lists throw on transpose
    throwsAnyError (fun () -> List.transpose [[1; 2]; [3]])
    throwsAnyError (fun () -> List.transpose [[1]; [2; 3]])
    throwsAnyError (fun () -> List.transpose [[]; [1; 2]; [3; 4]])
    throwsAnyError (fun () -> List.transpose [[1; 2]; []; [3; 4]])
    throwsAnyError (fun () -> List.transpose [[1; 2]; [3; 4]; []])

[<Fact>]
let ``List.updateAt works`` () =
    // integer list
    equal [0; 2; 3; 4; 5] (List.updateAt 0 0 [1..5])
    equal [1; 2; 0; 4; 5] (List.updateAt 2 0 [1..5])
    equal [1; 2; 3; 4; 0] (List.updateAt 4 0 [1..5])

    //string list
    equal ["0"; "2"; "3"; "4"; "5"] (List.updateAt 0 "0" ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "0"; "4"; "5"] (List.updateAt 2 "0" ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "3"; "4"; "0"] (List.updateAt 4 "0" ["1"; "2"; "3"; "4"; "5"])

    // empty list & out of bounds
    throwsAnyError (fun () -> List.updateAt<int> 0 0 [] |> ignore)
    throwsAnyError (fun () -> List.updateAt -1 0 [1] |> ignore)
    throwsAnyError (fun () -> List.updateAt 2 0 [1] |> ignore)

[<Fact>]
let ``List.insertAt works`` () =
    // integer list
    equal [0; 1; 2; 3; 4; 5] (List.insertAt 0 0 [1..5])
    equal [1; 2; 0; 3; 4; 5] (List.insertAt 2 0 [1..5])
    equal [1; 2; 3; 4; 0; 5] (List.insertAt 4 0 [1..5])

    //string list
    equal ["0"; "1"; "2"; "3"; "4"; "5"] (List.insertAt 0 "0" ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "0"; "3"; "4"; "5"] (List.insertAt 2 "0" ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "3"; "4"; "0"; "5"] (List.insertAt 4 "0" ["1"; "2"; "3"; "4"; "5"])

    // empty list & out of bounds
    equal [0] (List.insertAt 0 0 [])
    throwsAnyError (fun () -> List.insertAt -1 0 [1] |> ignore)
    throwsAnyError (fun () -> List.insertAt 2 0 [1] |> ignore)

[<Fact>]
let ``List.insertManyAt works`` () =
    // integer list
    equal [0; 0; 1; 2; 3; 4; 5] (List.insertManyAt 0 [0; 0] [1..5])
    equal [1; 2; 0; 0; 3; 4; 5] (List.insertManyAt 2 [0; 0] [1..5])
    equal [1; 2; 3; 4; 0; 0; 5] (List.insertManyAt 4 [0; 0] [1..5])

    //string list
    equal ["0"; "0"; "1"; "2"; "3"; "4"; "5"] (List.insertManyAt 0 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "0"; "0"; "3"; "4"; "5"] (List.insertManyAt 2 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "3"; "4"; "0"; "0"; "5"] (List.insertManyAt 4 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"])

    // empty list & out of bounds
    equal [0; 0] (List.insertManyAt 0 [0; 0] [])
    throwsAnyError (fun () -> List.insertManyAt -1 [0; 0] [1] |> ignore)
    throwsAnyError (fun () -> List.insertManyAt 2 [0; 0] [1] |> ignore)

[<Fact>]
let ``List.removeAt works`` () =
    // integer list
    equal [2; 3; 4; 5] (List.removeAt 0 [1..5])
    equal [1; 2; 4; 5] (List.removeAt 2 [1..5])
    equal [1; 2; 3; 4] (List.removeAt 4 [1..5])

    //string list
    equal ["2"; "3"; "4"; "5"] (List.removeAt 0 ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "4"; "5"] (List.removeAt 2 ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "3"; "4"] (List.removeAt 4 ["1"; "2"; "3"; "4"; "5"])

    // empty list & out of bounds
    throwsAnyError (fun () -> List.removeAt<int> 0 [] |> ignore)
    throwsAnyError (fun () -> List.removeAt -1 [1] |> ignore)
    throwsAnyError (fun () -> List.removeAt 2 [1] |> ignore)

[<Fact>]
let ``List.removeManyAt works`` () =
    // integer list
    equal [3; 4; 5] (List.removeManyAt 0 2 [1..5])
    equal [1; 2; 5] (List.removeManyAt 2 2 [1..5])
    equal [1; 2; 3] (List.removeManyAt 3 2 [1..5])

    //string list
    equal ["3"; "4"; "5"] (List.removeManyAt 0 2 ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "5"] (List.removeManyAt 2 2 ["1"; "2"; "3"; "4"; "5"])
    equal ["1"; "2"; "3"] (List.removeManyAt 3 2 ["1"; "2"; "3"; "4"; "5"])

    // empty list & out of bounds
    throwsAnyError (fun () -> List.removeManyAt<int> 0 2 [] |> ignore)
    throwsAnyError (fun () -> List.removeManyAt -1 2 [1] |> ignore)
    throwsAnyError (fun () -> List.removeManyAt 2 2 [1] |> ignore)
