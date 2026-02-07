module Fable.Tests.List

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test List literal works`` () =
    let xs = [1; 2; 3]
    xs.Length |> equal 3

[<Fact>]
let ``test Empty list works`` () =
    let xs: int list = []
    xs.Length |> equal 0

[<Fact>]
let ``test List cons works`` () =
    let xs = 1 :: [2; 3]
    xs.Length |> equal 3

[<Fact>]
let ``test List.head works`` () =
    let xs = [1; 2; 3]
    xs.Head |> equal 1

[<Fact>]
let ``test List.tail works`` () =
    let xs = [1; 2; 3]
    xs.Tail.Head |> equal 2

[<Fact>]
let ``test List equality works`` () =
    let xs = [1; 2; 3]
    let ys = [1; 2; 3]
    let zs = [1; 4; 3]
    xs = ys |> equal true
    xs = zs |> equal false

[<Fact>]
let ``test Pattern matching with lists works`` () =
    match [] with [] -> true | _ -> false
    |> equal true
    match [1] with [] -> 0 | [x] -> 1 | x :: xs -> 2
    |> equal 1
    match [1; 2; 3] with [] -> 0 | _ :: x :: xs -> x | _ -> 3
    |> equal 2

[<Fact>]
let ``test List.isEmpty works`` () =
    List.isEmpty [1] |> equal false
    List.isEmpty<int> [] |> equal true

[<Fact>]
let ``test List.length works`` () =
    List.length [1; 2; 3] |> equal 3
    List.length [] |> equal 0

[<Fact>]
let ``test List.map works`` () =
    [1; 2; 3]
    |> List.map (fun x -> x * 2)
    |> equal [2; 4; 6]

[<Fact>]
let ``test List.filter works`` () =
    [1; 2; 3; 4; 5]
    |> List.filter (fun x -> x % 2 = 0)
    |> equal [2; 4]

[<Fact>]
let ``test List.fold works`` () =
    [1; 2; 3; 4]
    |> List.fold (fun acc x -> acc + x) 0
    |> equal 10

[<Fact>]
let ``test List.rev works`` () =
    [1; 2; 3] |> List.rev |> equal [3; 2; 1]

[<Fact>]
let ``test List.append works`` () =
    List.append [1; 2] [3; 4] |> equal [1; 2; 3; 4]

[<Fact>]
let ``test List.sum works`` () =
    [1; 2; 3; 4] |> List.sum |> equal 10

[<Fact>]
let ``test List.contains works`` () =
    [1; 2; 3] |> List.contains 2 |> equal true
    [1; 2; 3] |> List.contains 5 |> equal false

[<Fact>]
let ``test List.exists works`` () =
    [1; 2; 3; 4] |> List.exists (fun x -> x > 3) |> equal true
    [1; 2; 3; 4] |> List.exists (fun x -> x > 10) |> equal false

[<Fact>]
let ``test List.forall works`` () =
    [1; 2; 3] |> List.forall (fun x -> x > 0) |> equal true
    [1; 2; 3] |> List.forall (fun x -> x > 2) |> equal false

[<Fact>]
let ``test List.find works`` () =
    [1; 2; 3; 4] |> List.find (fun x -> x > 2) |> equal 3

[<Fact>]
let ``test List.tryFind works`` () =
    [1; 2; 3] |> List.tryFind (fun x -> x > 2) |> equal (Some 3)
    [1; 2; 3] |> List.tryFind (fun x -> x > 10) |> equal None

[<Fact>]
let ``test List.choose works`` () =
    [1; 2; 3; 4; 5]
    |> List.choose (fun x -> if x % 2 = 0 then Some (x * 10) else None)
    |> equal [20; 40]

[<Fact>]
let ``test List.collect works`` () =
    [1; 2; 3]
    |> List.collect (fun x -> [x; x * 10])
    |> equal [1; 10; 2; 20; 3; 30]

[<Fact>]
let ``test List.sort works`` () =
    [3; 1; 4; 1; 5] |> List.sort |> equal [1; 1; 3; 4; 5]

[<Fact>]
let ``test List.sortBy works`` () =
    [3; -1; 2; -4] |> List.sortBy (fun x -> abs x) |> equal [-1; 2; 3; -4]

[<Fact>]
let ``test List.sortDescending works`` () =
    [1; 3; 2] |> List.sortDescending |> equal [3; 2; 1]

[<Fact>]
let ``test List.partition works`` () =
    let (yes, no) = [1; 2; 3; 4; 5] |> List.partition (fun x -> x % 2 = 0)
    yes |> equal [2; 4]
    no |> equal [1; 3; 5]

[<Fact>]
let ``test List.zip works`` () =
    let result = List.zip [1; 2; 3] ["a"; "b"; "c"]
    List.length result |> equal 3
    fst (List.head result) |> equal 1
    snd (List.head result) |> equal "a"

[<Fact>]
let ``test List.unzip works`` () =
    let (a, b) = List.unzip [(1, "a"); (2, "b"); (3, "c")]
    a |> equal [1; 2; 3]
    b |> equal ["a"; "b"; "c"]

[<Fact>]
let ``test List.min works`` () =
    [3; 1; 4; 1; 5] |> List.min |> equal 1

[<Fact>]
let ``test List.max works`` () =
    [3; 1; 4; 1; 5] |> List.max |> equal 5

[<Fact>]
let ``test List.reduce works`` () =
    [1; 2; 3; 4] |> List.reduce (fun a b -> a + b) |> equal 10

[<Fact>]
let ``test List.concat works`` () =
    List.concat [[1; 2]; [3; 4]; [5]] |> equal [1; 2; 3; 4; 5]

[<Fact>]
let ``test List.singleton works`` () =
    List.singleton 42 |> equal [42]

[<Fact>]
let ``test List.foldBack works`` () =
    [1; 2; 3] |> List.foldBack (fun x acc -> x :: acc) <| [] |> equal [1; 2; 3]

[<Fact>]
let ``test List.indexed works`` () =
    let result = ["a"; "b"; "c"] |> List.indexed
    List.length result |> equal 3
    fst (List.head result) |> equal 0
    snd (List.head result) |> equal "a"
