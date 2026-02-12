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

// --- New tests ---

[<Fact>]
let ``test List.iter works`` () =
    let mutable total = 0
    [1; 2; 3] |> List.iter (fun x -> total <- total + x)
    equal 6 total

[<Fact>]
let ``test List.mapi works`` () =
    ["a"; "b"; "c"]
    |> List.mapi (fun i x -> (i, x))
    |> equal [(0, "a"); (1, "b"); (2, "c")]

[<Fact>]
let ``test List.sumBy works`` () =
    [1; 2; 3; 4] |> List.sumBy (fun x -> x * x) |> equal 30

[<Fact>]
let ``test List.sortWith works`` () =
    [3; 1; 2] |> List.sortWith (fun a b -> compare a b) |> equal [1; 2; 3]

[<Fact>]
let ``test List.sortByDescending works`` () =
    [1; 3; 2] |> List.sortByDescending id |> equal [3; 2; 1]

[<Fact>]
let ``test List.minBy works`` () =
    ["ab"; "a"; "abc"] |> List.minBy (fun s -> s.Length) |> equal "a"

[<Fact>]
let ``test List.maxBy works`` () =
    ["ab"; "a"; "abc"] |> List.maxBy (fun s -> s.Length) |> equal "abc"

[<Fact>]
let ``test List.init works`` () =
    List.init 5 (fun i -> i * 2) |> equal [0; 2; 4; 6; 8]

[<Fact>]
let ``test List.item works`` () =
    List.item 1 [10; 20; 30] |> equal 20

[<Fact>]
let ``test List.replicate works`` () =
    List.replicate 3 "a" |> equal ["a"; "a"; "a"]

[<Fact>]
let ``test List.skip works`` () =
    [1; 2; 3; 4; 5] |> List.skip 2 |> equal [3; 4; 5]

[<Fact>]
let ``test List.take works`` () =
    [1; 2; 3; 4; 5] |> List.take 3 |> equal [1; 2; 3]

[<Fact>]
let ``test List.skipWhile works`` () =
    [1; 2; 3; 4; 1] |> List.skipWhile (fun x -> x < 3) |> equal [3; 4; 1]

[<Fact>]
let ``test List.takeWhile works`` () =
    [1; 2; 3; 4; 1] |> List.takeWhile (fun x -> x < 3) |> equal [1; 2]

[<Fact>]
let ``test List.truncate works`` () =
    [1; 2; 3; 4; 5] |> List.truncate 3 |> equal [1; 2; 3]

[<Fact>]
let ``test List.last works`` () =
    [1; 2; 3] |> List.last |> equal 3

[<Fact>]
let ``test List.findIndex works`` () =
    [1; 2; 3; 4] |> List.findIndex (fun x -> x > 2) |> equal 2

[<Fact>]
let ``test List.tryFindIndex works`` () =
    [1; 2; 3] |> List.tryFindIndex (fun x -> x > 2) |> equal (Some 2)
    [1; 2; 3] |> List.tryFindIndex (fun x -> x > 10) |> equal None

[<Fact>]
let ``test List.tryHead works`` () =
    List.tryHead [1; 2; 3] |> equal (Some 1)
    List.tryHead<int> [] |> equal None

[<Fact>]
let ``test List.tryLast works`` () =
    List.tryLast [1; 2; 3] |> equal (Some 3)
    List.tryLast<int> [] |> equal None

[<Fact>]
let ``test List.tryItem works`` () =
    List.tryItem 1 [10; 20; 30] |> equal (Some 20)
    List.tryItem 5 [10; 20; 30] |> equal None

[<Fact>]
let ``test List.exactlyOne works`` () =
    List.exactlyOne [42] |> equal 42

[<Fact>]
let ``test List.tryExactlyOne works`` () =
    List.tryExactlyOne [42] |> equal (Some 42)
    List.tryExactlyOne [1; 2] |> equal None
    List.tryExactlyOne<int> [] |> equal None

[<Fact>]
let ``test List.distinct works`` () =
    [1; 2; 3; 1; 2] |> List.distinct |> equal [1; 2; 3]

[<Fact>]
let ``test List.distinctBy works`` () =
    [1; -1; 2; -2; 3] |> List.distinctBy abs |> equal [1; 2; 3]

[<Fact>]
let ``test List.pairwise works`` () =
    [1; 2; 3; 4] |> List.pairwise |> equal [(1, 2); (2, 3); (3, 4)]

[<Fact>]
let ``test List.exists2 works`` () =
    List.exists2 (fun a b -> a + b = 5) [1; 2; 3] [4; 3; 2] |> equal true
    List.exists2 (fun a b -> a + b = 10) [1; 2; 3] [4; 3; 2] |> equal false

[<Fact>]
let ``test List.forall2 works`` () =
    List.forall2 (fun a b -> a < b) [1; 2; 3] [4; 5; 6] |> equal true
    List.forall2 (fun a b -> a < b) [1; 2; 3] [4; 1; 6] |> equal false

[<Fact>]
let ``test List.map2 works`` () =
    List.map2 (fun a b -> a + b) [1; 2; 3] [10; 20; 30] |> equal [11; 22; 33]

[<Fact>]
let ``test List.map3 works`` () =
    List.map3 (fun a b c -> a + b + c) [1; 2] [10; 20] [100; 200] |> equal [111; 222]

[<Fact>]
let ``test List.scan works`` () =
    [1; 2; 3; 4] |> List.scan (+) 0 |> equal [0; 1; 3; 6; 10]

[<Fact>]
let ``test List.scanBack works`` () =
    List.scanBack (+) [1; 2; 3; 4] 0 |> equal [10; 9; 7; 4; 0]

[<Fact>]
let ``test List.reduceBack works`` () =
    [1; 2; 3; 4] |> List.reduceBack (fun a b -> a + b) |> equal 10

[<Fact>]
let ``test List.ofArray works`` () =
    List.ofArray [|1; 2; 3|] |> equal [1; 2; 3]

[<Fact>]
let ``test List.toArray works`` () =
    List.toArray [1; 2; 3] |> equal [|1; 2; 3|]

[<Fact>]
let ``test List.pick works`` () =
    [1; 2; 3] |> List.pick (fun x -> if x > 2 then Some (x * 10) else None) |> equal 30

[<Fact>]
let ``test List.tryPick works`` () =
    [1; 2; 3] |> List.tryPick (fun x -> if x > 2 then Some (x * 10) else None) |> equal (Some 30)
    [1; 2; 3] |> List.tryPick (fun x -> if x > 10 then Some x else None) |> equal None

[<Fact>]
let ``test List.unfold works`` () =
    List.unfold (fun x -> if x < 5 then Some(x, x + 1) else None) 0 |> equal [0; 1; 2; 3; 4]

[<Fact>]
let ``test List.splitAt works`` () =
    let (a, b) = List.splitAt 2 [1; 2; 3; 4; 5]
    a |> equal [1; 2]
    b |> equal [3; 4; 5]

[<Fact>]
let ``test List.chunkBySize works`` () =
    [1; 2; 3; 4; 5] |> List.chunkBySize 2 |> equal [[1; 2]; [3; 4]; [5]]

[<Fact>]
let ``test List.windowed works`` () =
    [1; 2; 3; 4; 5] |> List.windowed 3 |> equal [[1; 2; 3]; [2; 3; 4]; [3; 4; 5]]

[<Fact>]
let ``test List.except works`` () =
    [1; 2; 3; 4; 5] |> List.except [2; 4] |> equal [1; 3; 5]

[<Fact>]
let ``test List.allPairs works`` () =
    List.allPairs [1; 2] ["a"; "b"] |> equal [(1, "a"); (1, "b"); (2, "a"); (2, "b")]

[<Fact>]
let ``test List.zip3 works`` () =
    let (a, b, c) = List.zip3 [1; 2] ["a"; "b"] [true; false] |> List.head
    a |> equal 1
    b |> equal "a"
    c |> equal true

[<Fact>]
let ``test List.unzip3 works`` () =
    let (a, b, c) = List.unzip3 [(1, "a", true); (2, "b", false)]
    a |> equal [1; 2]
    b |> equal ["a"; "b"]
    c |> equal [true; false]

[<Fact>]
let ``test List.average works`` () =
    [1.0; 2.0; 3.0; 4.0] |> List.average |> equal 2.5

[<Fact>]
let ``test List.averageBy works`` () =
    [1; 2; 3; 4] |> List.averageBy float |> equal 2.5

[<Fact>]
let ``test List.permute works`` () =
    [1; 2; 3] |> List.permute (fun i -> (i + 1) % 3) |> equal [3; 1; 2]

[<Fact>]
let ``test List.foldBack with composition works`` () =
    [1; 2; 3; 4]
    |> List.foldBack (fun x acc -> acc + string x) <| ""
    |> equal "4321"

[<Fact>]
let ``test List comparison works`` () =
    compare [1; 2; 3] [1; 2; 3] |> equal 0
    compare [1; 2; 3] [1; 2; 4] < 0 |> equal true
    compare [1; 2; 4] [1; 2; 3] > 0 |> equal true

[<Fact>]
let ``test List.mapFold works`` () =
    let (mapped, finalState) =
        (10, [1; 2; 3]) ||> List.mapFold (fun acc x -> (acc + x, acc + x))
    mapped |> equal [11; 13; 16]
    finalState |> equal 16

[<Fact>]
let ``test List.mapFoldBack works`` () =
    let (mapped, finalState) =
        List.mapFoldBack (fun x acc -> (x + acc, x + acc)) [1; 2; 3] 10
    mapped |> equal [16; 15; 13]
    finalState |> equal 16

[<Fact>]
let ``test List iter2 works`` () =
    let mutable total = 0
    List.iter2 (fun a b -> total <- total + a + b) [1; 2; 3] [10; 20; 30]
    equal 66 total

[<Fact>]
let ``test List iteri works`` () =
    let mutable total = 0
    List.iteri (fun i x -> total <- total + i + x) [10; 20; 30]
    equal 63 total

[<Fact>]
let ``test List.sort with tuples works`` () =
    [(3, "c"); (1, "a"); (2, "b")] |> List.sort |> equal [(1, "a"); (2, "b"); (3, "c")]

[<Fact>]
let ``test xs.IsEmpty works`` () =
    let xs: int list = []
    let ys = [1]
    xs.IsEmpty |> equal true
    ys.IsEmpty |> equal false

[<Fact>]
let ``test xs.Item works`` () =
    let xs = [10; 20; 30]
    xs.[1] |> equal 20

[<Fact>]
let ``test List snail to append works`` () =
    let xs = [1; 2] @ [3; 4]
    xs |> equal [1; 2; 3; 4]

[<Fact>]
let ``test List range works`` () =
    [1..5] |> equal [1; 2; 3; 4; 5]
    [1..2..9] |> equal [1; 3; 5; 7; 9]

// --- Additional tests ported from JS ---

[<Fact>]
let ``test List.fold2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    List.fold2 (fun x y z -> x + y + z) 0 xs ys
    |> equal 20

[<Fact>]
let ``test List.foldBack2 works`` () =
    ([1; 2; 3; 4], [1; 2; 3; 4], 0)
    |||> List.foldBack2 (fun x y acc -> acc - y * x)
    |> equal -30

[<Fact>]
let ``test List.findBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.find ((>) 4.) |> equal 1.
    xs |> List.findBack ((>) 4.) |> equal 3.

[<Fact>]
let ``test List.findIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.findIndex ((>) 4.) |> equal 0
    xs |> List.findIndexBack ((>) 4.) |> equal 2

[<Fact>]
let ``test List.tryFindBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> List.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> List.tryFindBack ((=) 5.) |> equal None

[<Fact>]
let ``test List.tryFindIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> List.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> List.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> List.tryFindIndexBack ((=) 5.) |> equal None

[<Fact>]
let ``test List.mapi2 works`` () =
    let xs = [7;8;9]
    let ys = [5;4;3]
    let zs = List.mapi2 (fun i x y -> i * (x - y)) xs ys
    List.sum zs |> equal 16

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
let ``test List.countBy works`` () =
    let xs = [1; 2; 3; 4]
    xs |> List.countBy (fun x -> x % 2)
    |> List.length |> equal 2

[<Fact>]
let ``test List.groupBy returns valid list`` () =
    let xs = [1; 2]
    let worked =
        match List.groupBy (fun _ -> true) xs with
        | [true, [1; 2]] -> true
        | _ -> false
    worked |> equal true

[<Fact>]
let ``test List.distinct with tuples works`` () =
    let xs = [(1, 2); (2, 3); (1, 2)]
    let ys = xs |> List.distinct
    ys |> List.length |> equal 2
    ys |> List.sumBy fst |> equal 3

[<Fact>]
let ``test List.distinctBy with tuples works`` () =
    let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
    let ys = xs |> List.distinctBy (fun (x,_) -> x % 2)
    ys |> List.length |> equal 2
    ys |> List.head |> fst >= 4 |> equal true

[<Fact>]
let ``test List.insertAt works`` () =
    equal [0; 1; 2; 3; 4; 5] (List.insertAt 0 0 [1..5])
    equal [1; 2; 0; 3; 4; 5] (List.insertAt 2 0 [1..5])
    equal [1; 2; 3; 4; 0; 5] (List.insertAt 4 0 [1..5])
    equal [0] (List.insertAt 0 0 [])

[<Fact>]
let ``test List.insertManyAt works`` () =
    equal [0; 0; 1; 2; 3; 4; 5] (List.insertManyAt 0 [0; 0] [1..5])
    equal [1; 2; 0; 0; 3; 4; 5] (List.insertManyAt 2 [0; 0] [1..5])
    equal [1; 2; 3; 4; 0; 0; 5] (List.insertManyAt 4 [0; 0] [1..5])
    equal [0; 0] (List.insertManyAt 0 [0; 0] [])

[<Fact>]
let ``test List.removeAt works`` () =
    equal [2; 3; 4; 5] (List.removeAt 0 [1..5])
    equal [1; 2; 4; 5] (List.removeAt 2 [1..5])
    equal [1; 2; 3; 4] (List.removeAt 4 [1..5])

[<Fact>]
let ``test List.removeManyAt works`` () =
    equal [3; 4; 5] (List.removeManyAt 0 2 [1..5])
    equal [1; 2; 5] (List.removeManyAt 2 2 [1..5])
    equal [1; 2; 3] (List.removeManyAt 3 2 [1..5])

[<Fact>]
let ``test List.updateAt works`` () =
    equal [0; 2; 3; 4; 5] (List.updateAt 0 0 [1..5])
    equal [1; 2; 0; 4; 5] (List.updateAt 2 0 [1..5])
    equal [1; 2; 3; 4; 0] (List.updateAt 4 0 [1..5])

[<Fact>]
let ``test List.splitInto works`` () =
    [1..10] |> List.splitInto 3 |> equal [ [1..4]; [5..7]; [8..10] ]
    [1..11] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..11] ]
    [1..12] |> List.splitInto 3 |> equal [ [1..4]; [5..8]; [9..12] ]
    [1..5] |> List.splitInto 4 |> equal [ [1..2]; [3]; [4]; [5] ]
    [1..4] |> List.splitInto 20 |> equal [ [1]; [2]; [3]; [4] ]

[<Fact>]
let ``test List.transpose works`` () =
    List.transpose [[1..3]; [4..6]]
    |> equal [[1; 4]; [2; 5]; [3; 6]]
    List.transpose [[1..3]]
    |> equal [[1]; [2]; [3]]
    List.transpose [[1]; [2]]
    |> equal [[1..2]]
    List.transpose []
    |> equal []
    List.transpose [[]]
    |> equal []

[<Fact>]
let ``test List.compareWith works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 5]
    let zs = [1; 2; 3; 3]
    List.compareWith compare xs xs |> equal 0
    List.compareWith compare xs ys |> equal -1
    List.compareWith compare xs zs |> equal 1

[<Fact>]
let ``test List.empty works`` () =
    let xs = List.empty<int>
    List.length xs
    |> equal 0

[<Fact>]
let ``test xs.Head works`` () =
    let xs = [1; 2; 3; 4]
    equal 1 xs.Head

[<Fact>]
let ``test xs.Tail works`` () =
    let xs = [1; 2; 3; 4]
    equal 2 xs.Tail.Head

[<Fact>]
let ``test List cons works II`` () =
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
let ``test Some [] works`` () =
    let xs: int list option = Some []
    let ys: int list option = None
    Option.isSome xs |> equal true
    Option.isNone ys |> equal true

[<Fact>]
let ``test List.Equals works`` () =
    let xs = [1;2;3]
    xs.Equals(xs) |> equal true

[<Fact>]
let ``test Implicit yields work`` () =
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
let ``test List comprehensions returning None work`` () =
    let spam : string option list = [for _ in 0..5 -> None]
    List.length spam |> equal 6

[<Fact>]
let ``test List.collect works II`` () =
    let xs = ["a"; "fable"; "bar" ]
    xs
    |> List.collect (fun a -> [a.Length])
    |> equal [1; 5; 3]

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
let ``test List.Item throws exception when index is out of range`` () =
    let xs = [0]
    (try (xs.Item 1) |> ignore; false with | _ -> true) |> equal true

// TODO: List.GetSlice not yet supported by Fable Beam
// [<Fact>]
// let ``test List slice works`` () =
//     let xs = [1; 2; 3; 4]
//     xs.[..2] |> List.sum |> equal 6
//     xs.[2..] |> List.sum |> equal 7
//     xs.[1..2] |> List.sum |> equal 5

[<Fact>]
let ``test List.groupBy maintains order`` () =
    let xs = [ 0,5; 1,5; 2,5; 3,5; 0,6; 1,6; 2,6; 3,6 ]
    let mapped = xs |> List.take 4 |> List.map (fun (x,y) -> x, [x,y; x,y+1])
    let grouped = xs |> List.groupBy fst
    grouped |> equal mapped

// --- More tests ported from Python ---

[<Fact>]
let ``test List.empty works II`` () =
    let xs = 1 :: List.Empty
    let ys = 1 :: List.empty
    xs.Length + ys.Length |> equal 2

[<Fact>]
let ``test List.ofSeq works`` () =
    let ys = List.ofSeq <| seq { yield 1; yield 2 }
    ys.Head |> equal 1
    ys.Length |> equal 2

[<Fact>]
let ``test List.length works II`` () =
    let xs = [1; 2; 3; 4]
    List.length xs |> equal 4

[<Fact>]
let ``test List.toSeq works`` () =
    [1; 2]
    |> List.toSeq
    |> Seq.tail |> Seq.head
    |> equal 2

[<Fact>]
let ``test List iterators from range do rewind`` () =
    let xs = [1..5] |> List.toSeq
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

[<Fact>]
let ``test Int list tail doesn't get wrapped with `| 0` ``  () =
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
let ``test List.mapFold works II`` () =
    let f x y = x,y
    let xs,_ = List.mapFold f "a" ["b"]
    equal "a" xs.Head

[<Fact>]
let ``test List.mapFoldBack works II`` () =
    let f x y = x,y
    let xs,_ = List.mapFoldBack f ["a"] "b"
    equal "a" xs.Head
