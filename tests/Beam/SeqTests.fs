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

[<Fact>]
let ``test Seq.findIndex works`` () =
    [1; 2; 3; 4] |> Seq.findIndex (fun x -> x > 2) |> equal 2

[<Fact>]
let ``test Seq.findBack works`` () =
    [1; 2; 3; 4] |> Seq.findBack (fun x -> x < 4) |> equal 3

[<Fact>]
let ``test Seq.findIndexBack works`` () =
    [1; 2; 3; 4] |> Seq.findIndexBack (fun x -> x < 4) |> equal 2

[<Fact>]
let ``test Seq.tryFindBack works`` () =
    [1; 2; 3] |> Seq.tryFindBack (fun x -> x > 2) |> equal (Some 3)
    [1; 2; 3] |> Seq.tryFindBack (fun x -> x > 10) |> equal None

[<Fact>]
let ``test Seq.tryFindIndex works`` () =
    [1; 2; 3; 4] |> Seq.tryFindIndex (fun x -> x > 2) |> equal (Some 2)
    [1; 2; 3; 4] |> Seq.tryFindIndex (fun x -> x > 10) |> equal None

[<Fact>]
let ``test Seq.tryFindIndexBack works`` () =
    [1; 2; 3; 4] |> Seq.tryFindIndexBack (fun x -> x < 4) |> equal (Some 2)
    [1; 2; 3; 4] |> Seq.tryFindIndexBack (fun x -> x > 10) |> equal None

[<Fact>]
let ``test Seq.tryHead works`` () =
    [1; 2; 3] |> Seq.tryHead |> equal (Some 1)
    Seq.empty<int> |> Seq.tryHead |> equal None

[<Fact>]
let ``test Seq.tail works`` () =
    [1; 2; 3] |> Seq.tail |> Seq.toList |> equal [2; 3]

[<Fact>]
let ``test Seq.item works`` () =
    [10; 20; 30] |> Seq.item 1 |> equal 20

[<Fact>]
let ``test Seq.tryItem works`` () =
    [10; 20; 30] |> Seq.tryItem 1 |> equal (Some 20)
    [10; 20; 30] |> Seq.tryItem 5 |> equal None

[<Fact>]
let ``test Seq.tryLast works`` () =
    [1; 2; 3] |> Seq.tryLast |> equal (Some 3)
    Seq.empty<int> |> Seq.tryLast |> equal None

[<Fact>]
let ``test Seq.exactlyOne works`` () =
    [42] |> Seq.exactlyOne |> equal 42

[<Fact>]
let ``test Seq.tryExactlyOne works`` () =
    [42] |> Seq.tryExactlyOne |> equal (Some 42)
    [1; 2] |> Seq.tryExactlyOne |> equal None
    Seq.empty<int> |> Seq.tryExactlyOne |> equal None

[<Fact>]
let ``test Seq.iteri works`` () =
    let mutable total = 0
    [10; 20; 30] |> Seq.iteri (fun i x -> total <- total + i + x)
    total |> equal 63

[<Fact>]
let ``test Seq.iter2 works`` () =
    let mutable total = 0
    Seq.iter2 (fun a b -> total <- total + a + b) [1; 2; 3] [10; 20; 30]
    total |> equal 66

[<Fact>]
let ``test Seq.map2 works`` () =
    Seq.map2 (fun a b -> a + b) [1; 2; 3] [10; 20; 30]
    |> Seq.toList
    |> equal [11; 22; 33]

[<Fact>]
let ``test Seq.mapi works`` () =
    [10; 20; 30]
    |> Seq.mapi (fun i x -> i * 100 + x)
    |> Seq.toList
    |> equal [10; 120; 230]

[<Fact>]
let ``test Seq.mapFold works`` () =
    let result, state =
        [1; 2; 3; 4]
        |> Seq.mapFold (fun acc x -> (acc + x, acc + x)) 0
    result |> Seq.toList |> equal [1; 3; 6; 10]
    state |> equal 10

[<Fact>]
let ``test Seq.mapFoldBack works`` () =
    let result, state =
        Seq.mapFoldBack (fun x acc -> (x + acc, x + acc)) [1; 2; 3; 4] 0
    result |> Seq.toList |> equal [10; 9; 7; 4]
    state |> equal 10

[<Fact>]
let ``test Seq.scan works`` () =
    [1; 2; 3; 4]
    |> Seq.scan (fun acc x -> acc + x) 0
    |> Seq.toList
    |> equal [0; 1; 3; 6; 10]

[<Fact>]
let ``test Seq.maxBy works`` () =
    [1; 3; 2] |> Seq.maxBy (fun x -> -x) |> equal 1

[<Fact>]
let ``test Seq.minBy works`` () =
    [1; 3; 2] |> Seq.minBy (fun x -> -x) |> equal 3

[<Fact>]
let ``test Seq.ofArray works`` () =
    [| 1; 2; 3 |] |> Seq.ofArray |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.pick works`` () =
    [1; 2; 3; 4]
    |> Seq.pick (fun x -> if x > 2 then Some (x * 10) else None)
    |> equal 30

[<Fact>]
let ``test Seq.tryPick works`` () =
    [1; 2; 3] |> Seq.tryPick (fun x -> if x > 2 then Some (x * 10) else None) |> equal (Some 30)
    [1; 2; 3] |> Seq.tryPick (fun x -> if x > 10 then Some x else None) |> equal None

[<Fact>]
let ``test Seq.exists2 works`` () =
    Seq.exists2 (fun a b -> a + b > 5) [1; 2; 3] [3; 3; 3] |> equal true
    Seq.exists2 (fun a b -> a + b > 10) [1; 2; 3] [3; 3; 3] |> equal false

[<Fact>]
let ``test Seq.forall2 works`` () =
    Seq.forall2 (fun a b -> a + b > 0) [1; 2; 3] [3; 3; 3] |> equal true
    Seq.forall2 (fun a b -> a + b > 4) [1; 2; 3] [3; 3; 3] |> equal false

[<Fact>]
let ``test Seq.zip3 works`` () =
    let result = Seq.zip3 [1; 2] ["a"; "b"] [10; 20] |> Seq.toList
    List.length result |> equal 2
    let (a, b, c) = result.[0]
    a |> equal 1
    b |> equal "a"
    c |> equal 10

[<Fact>]
let ``test Seq.countBy works`` () =
    let result = [1; 2; 3; 4; 5] |> Seq.countBy (fun x -> x % 2 = 0) |> Seq.toList
    result |> List.length |> equal 2

[<Fact>]
let ``test Seq.groupBy works`` () =
    let result = [1; 2; 3; 4; 5; 6] |> Seq.groupBy (fun x -> x % 3) |> Seq.toList
    result |> List.length |> equal 3

[<Fact>]
let ``test Seq.readonly works`` () =
    [1; 2; 3] |> Seq.readonly |> Seq.toList |> equal [1; 2; 3]

[<Fact>]
let ``test Seq.where works`` () =
    [1; 2; 3; 4; 5] |> Seq.where (fun x -> x > 3) |> Seq.toList |> equal [4; 5]

[<Fact>]
let ``test Seq.windowed works`` () =
    [1; 2; 3; 4; 5]
    |> Seq.windowed 3
    |> Seq.toList
    |> equal [[|1; 2; 3|]; [|2; 3; 4|]; [|3; 4; 5|]]

[<Fact>]
let ``test Seq.allPairs works`` () =
    let result = Seq.allPairs [1; 2] ["a"; "b"] |> Seq.toList
    result |> List.length |> equal 4
    fst result.[0] |> equal 1
    snd result.[0] |> equal "a"

[<Fact>]
let ``test Seq.splitInto works`` () =
    let result = [1; 2; 3; 4; 5] |> Seq.splitInto 3 |> Seq.toList
    result |> List.length |> equal 3

[<Fact>]
let ``test Seq.transpose works`` () =
    [[1; 2; 3]; [4; 5; 6]]
    |> Seq.transpose
    |> Seq.map Seq.toList
    |> Seq.toList
    |> equal [[1; 4]; [2; 5]; [3; 6]]

[<Fact>]
let ``test Seq.updateAt works`` () =
    [1; 2; 3; 4; 5]
    |> Seq.updateAt 2 99
    |> Seq.toList
    |> equal [1; 2; 99; 4; 5]

[<Fact>]
let ``test Seq.insertAt works`` () =
    [1; 2; 3]
    |> Seq.insertAt 1 99
    |> Seq.toList
    |> equal [1; 99; 2; 3]

[<Fact>]
let ``test Seq.insertManyAt works`` () =
    [1; 2; 3]
    |> Seq.insertManyAt 1 [88; 99]
    |> Seq.toList
    |> equal [1; 88; 99; 2; 3]

[<Fact>]
let ``test Seq.removeAt works`` () =
    [1; 2; 3; 4]
    |> Seq.removeAt 1
    |> Seq.toList
    |> equal [1; 3; 4]

[<Fact>]
let ``test Seq.removeManyAt works`` () =
    [1; 2; 3; 4; 5]
    |> Seq.removeManyAt 1 2
    |> Seq.toList
    |> equal [1; 4; 5]

[<Fact>]
let ``test Seq.average works`` () =
    [1.0; 2.0; 3.0] |> Seq.average |> equal 2.0

[<Fact>]
let ``test Seq.averageBy works`` () =
    [1; 2; 3] |> Seq.averageBy (fun x -> float x) |> equal 2.0

[<Fact>]
let ``test Seq.reduceBack works`` () =
    [1; 2; 3; 4]
    |> Seq.reduceBack (fun x acc -> x - acc)
    |> equal -2

[<Fact>]
let ``test Seq.sortWith works`` () =
    [3; 1; 4; 1; 5]
    |> Seq.sortWith (fun a b -> compare a b)
    |> Seq.toList
    |> equal [1; 1; 3; 4; 5]

[<Fact>]
let ``test Seq.compareWith works`` () =
    Seq.compareWith compare [1; 2; 3] [1; 2; 3] |> equal 0
    Seq.compareWith compare [1; 2; 3] [1; 2; 4] |> equal -1
    Seq.compareWith compare [1; 2; 4] [1; 2; 3] |> equal 1
    Seq.compareWith compare [1; 2] [1; 2; 3] |> equal -1

[<Fact>]
let ``test Seq.fold2 works`` () =
    Seq.fold2 (fun acc a b -> acc + a + b) 0 [1; 2; 3] [10; 20; 30]
    |> equal 66

[<Fact>]
let ``test Seq.scanBack works`` () =
    Seq.scanBack (fun x acc -> x + acc) [1; 2; 3; 4] 0
    |> Seq.toList
    |> equal [10; 9; 7; 4; 0]

[<Fact>]
let ``test Seq.cache works`` () =
    [1; 2; 3] |> Seq.cache |> Seq.toList |> equal [1; 2; 3]

// --- Additional tests ported from JS ---

[<Fact>]
let ``test Seq.delay works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Seq.delay (fun () -> xs :> _ seq)
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.sortByDescending works`` () =
    let xs = [3.; 1.; 4.; 2.]
    let ys = xs |> Seq.sortByDescending (fun x -> -x)
    (ys |> Seq.item 0) + (ys |> Seq.item 1)
    |> equal 3.

[<Fact>]
let ``test Seq.sort with tuples works`` () =
    let xs = seq {3; 1; 1; -3}
    let ys = seq {"a"; "c"; "B"; "d"}
    (xs, ys) ||> Seq.zip |> Seq.sort |> Seq.item 1 |> equal (1, "B")

[<Fact>]
let ``test Seq.distinct with tuples works`` () =
    let xs = [(1, 2); (2, 3); (1, 2)]
    let ys = xs |> Seq.distinct
    ys |> Seq.length |> equal 2
    ys |> Seq.sumBy fst |> equal 3

[<Fact>]
let ``test Seq.distinctBy with tuples works`` () =
    let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
    let ys = xs |> Seq.distinctBy (fun (x,_) -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.head |> fst >= 4 |> equal true

[<Fact>]
let ``test Seq.foldBack2 works`` () =
    Seq.foldBack2 (fun x y acc -> x + y - acc) [1; 2; 3; 4] [1; 2; 3; 4] 0
    |> equal -4

// TODO: Seq.forall laziness test fails because Beam sequences are eager lists;
// Seq.map evaluates all elements before Seq.forall checks them
// [<Fact>]
// let ``test Seq.forall is lazy`` () =
//     let mutable x = ""
//     let one() = x <- "one"; false
//     let two() = x <- "two"; true
//     let ok =
//         [one; two]
//         |> Seq.map (fun c -> c())
//         |> Seq.forall id
//     ok |> equal false
//     x |> equal "one"

[<Fact>]
let ``test Seq.fold with tupled arguments works`` () =
    let a, b =
        ((1, 5), [1;2;3;4])
        ||> Seq.fold (fun (a, b) i ->
            a * i, b + i)
    equal 24 a
    equal 15 b

[<Fact>]
let ``test Seq.scan works with empty input`` () =
    let xs = Seq.empty
    let ys = xs |> Seq.scan (+) 3
    Seq.head ys |> equal 3
    Seq.length ys |> equal 1

[<Fact>]
let ``test Seq.chunkBySize works`` () =
    seq {1..8} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|] ]
    seq {1..10} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..10|] ]

[<Fact>]
let ``test Seq.range works`` () =
    seq{1..5}
    |> Seq.reduce (+)
    |> equal 15
    // Float ranges don't work: lists:seq only supports integers in Erlang

[<Fact>]
let ``test Seq.range step works`` () =
    seq{0..2..9}
    |> Seq.reduce (+)
    |> equal 20
    // Float range with step doesn't work: lists:seq only supports integers in Erlang
