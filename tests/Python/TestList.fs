module Fable.Tests.List

open Util.Testing

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
let ``test List.IsEmpty works`` () =
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
let ``test List.Head works`` () =
    let xs = [1; 2; 3; 4]
    equal 1 xs.Head

[<Fact>]
let ``test List.Tail works`` () =
    let xs = [1; 2; 3; 4]
    equal 2 xs.Tail.Head

[<Fact>]
let ``test List.Item works`` () =
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
    total := !total + (y - x)
    ) xs ys
    equal 10 !total

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
let ``test List.map works`` () =
    let xs = [1; 2; 3; 4]
    xs
    |> List.map string
    |> equal ["1"; "2"; "3"; "4"]


[<Fact>]
let ``test List.singleton works`` () =
    let xs = List.singleton 42
    xs
    |> equal [42]


[<Fact>]
let ``test List.collect works`` () =
    let xs = ["a"; "fable"; "bar" ]
    xs
    |> List.collect (fun a -> [a.Length])
    |> equal [1; 5; 3]
