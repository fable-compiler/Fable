module Fable.Tests.List

open Fable.Tests.Util
open Util.Testing

#if !FABLE_COMPILER_BEAM // TODO: needs fable-library-beam (list module)
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
#endif

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

#if !FABLE_COMPILER_BEAM // TODO: needs fable-library-beam (list module)
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
#endif
