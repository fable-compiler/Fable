module Fable.Tests.Seq

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Seq.map works`` () =
    [1; 2; 3]
    |> Seq.map (fun x -> x * 2)
    |> Seq.toList
    |> equal [2; 4; 6]

[<Fact>]
let ``test Seq.filter works`` () =
    [1; 2; 3; 4; 5]
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.toList
    |> equal [2; 4]

[<Fact>]
let ``test Seq.fold works`` () =
    [1; 2; 3; 4]
    |> Seq.fold (fun acc x -> acc + x) 0
    |> equal 10

[<Fact>]
let ``test Seq.reduce works`` () =
    [1; 2; 3; 4]
    |> Seq.reduce (fun a b -> a + b)
    |> equal 10

[<Fact>]
let ``test Seq.head works`` () =
    [1; 2; 3] |> Seq.head |> equal 1

[<Fact>]
let ``test Seq.last works`` () =
    [1; 2; 3] |> Seq.last |> equal 3

[<Fact>]
let ``test Seq.length works`` () =
    [1; 2; 3] |> Seq.length |> equal 3

[<Fact>]
let ``test Seq.isEmpty works`` () =
    Seq.isEmpty [1] |> equal false
    Seq.isEmpty<int> [] |> equal true

[<Fact>]
let ``test Seq.append works`` () =
    Seq.append [1; 2] [3; 4]
    |> Seq.toList
    |> equal [1; 2; 3; 4]

[<Fact>]
let ``test Seq.concat works`` () =
    Seq.concat [[1; 2]; [3; 4]; [5]]
    |> Seq.toList
    |> equal [1; 2; 3; 4; 5]

[<Fact>]
let ``test Seq.collect works`` () =
    [1; 2; 3]
    |> Seq.collect (fun x -> [x; x * 10])
    |> Seq.toList
    |> equal [1; 10; 2; 20; 3; 30]

[<Fact>]
let ``test Seq.toList works`` () =
    [1; 2; 3] |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.toArray works`` () =
    let result = [1; 2; 3] |> Seq.toArray
    result.Length |> equal 3
    result.[0] |> equal 1

[<Fact>]
let ``test Seq.ofList works`` () =
    [1; 2; 3] |> Seq.ofList |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.exists works`` () =
    [1; 2; 3; 4] |> Seq.exists (fun x -> x > 3) |> equal true
    [1; 2; 3; 4] |> Seq.exists (fun x -> x > 10) |> equal false

[<Fact>]
let ``test Seq.forall works`` () =
    [1; 2; 3] |> Seq.forall (fun x -> x > 0) |> equal true
    [1; 2; 3] |> Seq.forall (fun x -> x > 2) |> equal false

[<Fact>]
let ``test Seq.contains works`` () =
    [1; 2; 3] |> Seq.contains 2 |> equal true
    [1; 2; 3] |> Seq.contains 5 |> equal false

[<Fact>]
let ``test Seq.find works`` () =
    [1; 2; 3; 4] |> Seq.find (fun x -> x > 2) |> equal 3

[<Fact>]
let ``test Seq.tryFind works`` () =
    [1; 2; 3] |> Seq.tryFind (fun x -> x > 2) |> equal (Some 3)
    [1; 2; 3] |> Seq.tryFind (fun x -> x > 10) |> equal None

[<Fact>]
let ``test Seq.choose works`` () =
    [1; 2; 3; 4; 5]
    |> Seq.choose (fun x -> if x % 2 = 0 then Some (x * 10) else None)
    |> Seq.toList
    |> equal [20; 40]

[<Fact>]
let ``test Seq.sort works`` () =
    [3; 1; 4; 1; 5] |> Seq.sort |> Seq.toList |> equal [1; 1; 3; 4; 5]

[<Fact>]
let ``test Seq.sortBy works`` () =
    [3; -1; 2; -4] |> Seq.sortBy (fun x -> abs x) |> Seq.toList |> equal [-1; 2; 3; -4]

[<Fact>]
let ``test Seq.sortDescending works`` () =
    [1; 3; 2] |> Seq.sortDescending |> Seq.toList |> equal [3; 2; 1]

[<Fact>]
let ``test Seq.take works`` () =
    [1; 2; 3; 4; 5] |> Seq.take 3 |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.skip works`` () =
    [1; 2; 3; 4; 5] |> Seq.skip 2 |> Seq.toList |> equal [3; 4; 5]

[<Fact>]
let ``test Seq.truncate works`` () =
    [1; 2; 3; 4; 5] |> Seq.truncate 3 |> Seq.toList |> equal [1; 2; 3]
    [1; 2] |> Seq.truncate 5 |> Seq.toList |> equal [1; 2]

[<Fact>]
let ``test Seq.sum works`` () =
    [1; 2; 3; 4] |> Seq.sum |> equal 10

[<Fact>]
let ``test Seq.sumBy works`` () =
    [1; 2; 3] |> Seq.sumBy (fun x -> x * 2) |> equal 12

[<Fact>]
let ``test Seq.min works`` () =
    [3; 1; 4; 1; 5] |> Seq.min |> equal 1

[<Fact>]
let ``test Seq.max works`` () =
    [3; 1; 4; 1; 5] |> Seq.max |> equal 5

[<Fact>]
let ``test Seq.indexed works`` () =
    let result = ["a"; "b"; "c"] |> Seq.indexed |> Seq.toList
    List.length result |> equal 3
    fst (List.head result) |> equal 0
    snd (List.head result) |> equal "a"

[<Fact>]
let ``test Seq.zip works`` () =
    let result = Seq.zip [1; 2; 3] ["a"; "b"; "c"] |> Seq.toList
    List.length result |> equal 3
    fst (List.head result) |> equal 1
    snd (List.head result) |> equal "a"

[<Fact>]
let ``test Seq.pairwise works`` () =
    let result = [1; 2; 3; 4] |> Seq.pairwise |> Seq.toList
    List.length result |> equal 3
    fst (List.head result) |> equal 1
    snd (List.head result) |> equal 2

[<Fact>]
let ``test Seq.delay and seq expression works`` () =
    let xs = seq { yield 1; yield 2; yield 3 }
    xs |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.singleton works`` () =
    Seq.singleton 42 |> Seq.toList |> equal [42]

[<Fact>]
let ``test Seq.unfold works`` () =
    let result =
        Seq.unfold (fun state ->
            if state > 5 then None
            else Some (state, state + 1)
        ) 1
        |> Seq.toList
    result |> equal [1; 2; 3; 4; 5]

[<Fact>]
let ``test Seq.init works`` () =
    Seq.init 5 (fun i -> i * 2) |> Seq.toList |> equal [0; 2; 4; 6; 8]

[<Fact>]
let ``test Seq.rev works`` () =
    [1; 2; 3] |> Seq.rev |> Seq.toList |> equal [3; 2; 1]

[<Fact>]
let ``test Seq.distinct works`` () =
    [1; 2; 3; 2; 1; 4] |> Seq.distinct |> Seq.toList |> equal [1; 2; 3; 4]

[<Fact>]
let ``test Seq.distinctBy works`` () =
    [1; -1; 2; -2; 3] |> Seq.distinctBy abs |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.except works`` () =
    [1; 2; 3; 4; 5] |> Seq.except [2; 4] |> Seq.toList |> equal [1; 3; 5]

[<Fact>]
let ``test Seq.takeWhile works`` () =
    [1; 2; 3; 4; 1] |> Seq.takeWhile (fun x -> x < 4) |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.skipWhile works`` () =
    [1; 2; 3; 4; 1] |> Seq.skipWhile (fun x -> x < 3) |> Seq.toList |> equal [3; 4; 1]

[<Fact>]
let ``test Seq.foldBack works`` () =
    [1; 2; 3] |> Seq.foldBack (fun x acc -> x :: acc) <| [] |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.iterate works`` () =
    let mutable sum = 0
    [1; 2; 3] |> Seq.iter (fun x -> sum <- sum + x)
    sum |> equal 6

[<Fact>]
let ``test Seq.mapIndexed works`` () =
    ["a"; "b"; "c"]
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.toList
    |> List.length
    |> equal 3

[<Fact>]
let ``test Seq.partition works`` () =
    let (yes, no) = [1; 2; 3; 4; 5] |> Seq.toList |> List.partition (fun x -> x % 2 = 0)
    yes |> equal [2; 4]
    no |> equal [1; 3; 5]
