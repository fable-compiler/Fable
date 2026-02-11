module Fable.Tests.ArrayTest

open Fable.Tests.Util
open Util.Testing

type Animal = Duck of int | Dog of int

// --- Literals, Indexing, Length ---

[<Fact>]
let ``test Array literal works`` () =
    let arr = [| 1; 2; 3 |]
    arr.Length |> equal 3

[<Fact>]
let ``test Array literals work`` () =
    let x = [| 1; 2; 3; 4; 5 |]
    equal 5 x.Length

[<Fact>]
let ``test Array indexing works`` () =
    let arr = [| 10; 20; 30 |]
    arr.[0] |> equal 10
    arr.[1] |> equal 20
    arr.[2] |> equal 30

[<Fact>]
let ``test Array indexer getter works`` () =
    let x = [| 1.; 2.; 3.; 4.; 5. |]
    x.[2] |> equal 3.

[<Fact>]
let ``test Array getter works`` () =
    let x = [| 1.; 2.; 3.; 4.; 5. |]
    Array.get x 2 |> equal 3.

[<Fact>]
let ``test xs.Length works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs.Length |> equal 4

[<Fact>]
let ``test Array.length works`` () =
    Array.length [| 1; 2; 3; 4; 5 |] |> equal 5
    let xs = [| "a"; "a"; "a"; "a" |]
    Array.length xs |> equal 4

[<Fact>]
let ``test Empty array works`` () =
    let arr: int array = [||]
    Array.length arr |> equal 0
    Array.isEmpty arr |> equal true

[<Fact>]
let ``test Array.empty works`` () =
    let xs = Array.empty<int>
    xs.Length |> equal 0

[<Fact>]
let ``test Array.item works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.item 2 xs |> equal 3.

[<Fact>]
let ``test Array.tryItem works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.tryItem 3 xs |> equal (Some 4.)
    Array.tryItem 4 xs |> equal None
    Array.tryItem -1 xs |> equal None

// --- Pattern matching ---

// TODO: Array pattern matching generates util:default_of() null checks
// [<Fact>]
// let ``test Pattern matching with arrays works`` () =
//     match [||] with [||] -> true | _ -> false
//     |> equal true
//     match [|1|] with [||] -> 0 | [|x|] -> 1 | _ -> 2
//     |> equal 1
//     match [|"a";"b"|] with [|"a";"b"|] -> 1 | _ -> 2
//     |> equal 1

// --- Creation ---

[<Fact>]
let ``test Array.zeroCreate works`` () =
    let xs = Array.zeroCreate 2
    equal 2 xs.Length
    equal 0 xs.[1]

[<Fact>]
let ``test Array.create works`` () =
    let xs = Array.create 2 5
    equal 2 xs.Length
    Array.sum xs |> equal 10

[<Fact>]
let ``test Array.init works`` () =
    let xs = Array.init 4 (float >> sqrt)
    xs.[0] + xs.[1]
    |> equal 1.
    (xs.[2] > 1. && xs.[3] < 2.)
    |> equal true

[<Fact>]
let ``test Array.copy works`` () =
    let xs = [| 1; 2; 3; 4 |]
    let ys = Array.copy xs
    ys |> Array.sum |> equal 10

// --- Map, Filter, Collect ---

[<Fact>]
let ``test Array.map works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.map (fun x -> x * 2) arr
    result.[0] |> equal 2
    result.[1] |> equal 4
    result.[2] |> equal 6

[<Fact>]
let ``test Array.map2 works`` () =
    let xs = [| 1. |]
    let ys = [| 2. |]
    let zs = Array.map2 (*) xs ys
    zs.[0] |> equal 2.

[<Fact>]
let ``test Array.map3 works`` () =
    let value1 = [| 1. |]
    let value2 = [| 2. |]
    let value3 = [| 3. |]
    let zs = Array.map3 (fun a b c -> a * b * c) value1 value2 value3
    zs.[0] |> equal 6.

[<Fact>]
let ``test Array.mapi works`` () =
    let xs = [| 1.; 2. |]
    let ys = xs |> Array.mapi (fun i x -> float i + x)
    ys.[1] |> equal 3.

[<Fact>]
let ``test Array.mapi2 works`` () =
    let xs = [| 1.; 2. |]
    let ys = [| 2.; 3. |]
    let zs = Array.mapi2 (fun i x y -> float i + x * y) xs ys
    zs.[1] |> equal 7.

[<Fact>]
let ``test Array.filter works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    let result = Array.filter (fun x -> x > 3) arr
    Array.length result |> equal 2

[<Fact>]
let ``test Array.collect works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.collect (fun x -> [| x; x * 10 |]) arr
    Array.length result |> equal 6
    result.[1] |> equal 10

[<Fact>]
let ``test Array.collect works 2`` () =
    let xs = [|[|1|]; [|2|]; [|3|]; [|4|]|]
    let ys = xs |> Array.collect id
    ys.[0] + ys.[1]
    |> equal 3

// --- Fold, Reduce, Scan ---

[<Fact>]
let ``test Array.fold works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    Array.fold (fun acc x -> acc + x) 0 arr |> equal 15

[<Fact>]
let ``test Array.foldBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    let total = Array.foldBack (fun x acc -> acc - x) xs 0.
    total |> equal -10.

[<Fact>]
let ``test Array.reduce works`` () =
    [| 1; 2; 3; 4; 5 |] |> Array.reduce (+) |> equal 15

[<Fact>]
let ``test Array.reduce with subtraction works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.reduce (-)
    |> equal -8.

[<Fact>]
let ``test Array.reduce Array.append works`` () =
    let nums =
        [|
            [| 0 |]
            [| 1 |]
        |]
    Array.reduce Array.append nums |> equal [|0; 1|]

[<Fact>]
let ``test Array.reduceBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.reduceBack (-)
    |> equal -2.

[<Fact>]
let ``test Array.scan works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    let ys = xs |> Array.scan (+) 0.
    ys.[2] + ys.[3]
    |> equal 9.

[<Fact>]
let ``test Array.scanBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    let ys = Array.scanBack (-) xs 0.
    ys.[2] + ys.[3]
    |> equal 3.

[<Fact>]
let ``test Array.mapFold works`` () =
    let xs = [| 1y; 2y; 3y; 4y |]
    let result = xs |> Array.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
    fst result |> Array.sum |> equal 20y
    snd result |> equal 10y

[<Fact>]
let ``test Array.mapFoldBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    let result = Array.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
    fst result |> Array.sum |> equal -20.
    snd result |> equal -10.

// --- Sum, Average, Min, Max ---

[<Fact>]
let ``test Array.sum works`` () =
    [| 1; 2; 3; 4; 5 |] |> Array.sum |> equal 15

[<Fact>]
let ``test Array.sum with floats works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.sum
    |> equal 3.

[<Fact>]
let ``test Array.sumBy works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.sumBy (fun x -> x * 2.)
    |> equal 6.

[<Fact>]
let ``test Array.average works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.average xs
    |> equal 2.5

[<Fact>]
let ``test Array.averageBy works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.averageBy (fun x -> x * 2.) xs
    |> equal 5.

[<Fact>]
let ``test Array.min works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.min
    |> equal 1.

[<Fact>]
let ``test Array.minBy works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.minBy (fun x -> -x)
    |> equal 2.

[<Fact>]
let ``test Array.max works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.max
    |> equal 2.

[<Fact>]
let ``test Array.maxBy works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.maxBy (fun x -> -x)
    |> equal 1.

// --- Search ---

[<Fact>]
let ``test Array.isEmpty works`` () =
    Array.isEmpty [||] |> equal true
    Array.isEmpty [| 1 |] |> equal false

[<Fact>]
let ``test Array.exists works`` () =
    Array.exists (fun x -> x > 3) [| 1; 2; 3; 4 |] |> equal true
    Array.exists (fun x -> x > 10) [| 1; 2; 3; 4 |] |> equal false

[<Fact>]
let ``test Array.forall works`` () =
    Array.forall (fun x -> x > 0) [| 1; 2; 3 |] |> equal true
    Array.forall (fun x -> x > 2) [| 1; 2; 3 |] |> equal false

[<Fact>]
let ``test Array.contains works`` () =
    Array.contains 3 [| 1; 2; 3; 4 |] |> equal true
    Array.contains 5 [| 1; 2; 3; 4 |] |> equal false

[<Fact>]
let ``test Array.find works`` () =
    let xs = [| 1; 2; 3; 4 |]
    xs |> Array.find ((=) 2)
    |> equal 2

[<Fact>]
let ``test Array.findIndex works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.findIndex ((=) 2.)
    |> equal 1

[<Fact>]
let ``test Array.findBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.find ((>) 4.) |> equal 1.
    xs |> Array.findBack ((>) 4.) |> equal 3.

[<Fact>]
let ``test Array.findIndexBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.findIndex ((>) 4.) |> equal 0
    xs |> Array.findIndexBack ((>) 4.) |> equal 2

[<Fact>]
let ``test Array.tryFind works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.tryFind ((=) 1.)
    |> Option.isSome |> equal true
    xs |> Array.tryFind ((=) 3.)
    |> Option.isSome |> equal false

[<Fact>]
let ``test Array.tryFindIndex works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.tryFindIndex ((=) 2.)
    |> equal (Some 1)
    xs |> Array.tryFindIndex ((=) 3.)
    |> equal None

[<Fact>]
let ``test Array.tryFindBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> Array.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> Array.tryFindBack ((=) 5.) |> equal None

[<Fact>]
let ``test Array.tryFindIndexBack works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    xs |> Array.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> Array.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> Array.tryFindIndexBack ((=) 5.) |> equal None

[<Fact>]
let ``test Array.pick works`` () =
    let xs = [| 1.; 2. |]
    xs |> Array.pick (fun x ->
        match x with
        | 2. -> Some x
        | _ -> None)
    |> equal 2.

[<Fact>]
let ``test Array.tryPick works`` () =
    let xs = [| 1.; 2. |]
    let r = xs |> Array.tryPick (fun x ->
        match x with
        | 2. -> Some x
        | _ -> None)
    match r with
    | Some x -> x
    | None -> 0.
    |> equal 2.

// --- Sort ---

[<Fact>]
let ``test Array.sort works`` () =
    let arr = [| 3; 1; 4; 1; 5 |]
    let sorted = Array.sort arr
    sorted.[0] |> equal 1
    sorted.[4] |> equal 5

[<Fact>]
let ``test Array.sort with tuples works`` () =
    let xs = [|3; 1; 1; -3|]
    let ys = [|"a"; "c"; "B"; "d"|]
    (xs, ys) ||> Array.zip |> Array.sort |> Array.item 1 |> equal (1, "B")

[<Fact>]
let ``test Array.sortBy works`` () =
    let xs = [| 3.; 4.; 1.; 2. |]
    let ys = xs |> Array.sortBy (fun x -> -x)
    ys.[0] + ys.[1]
    |> equal 7.

[<Fact>]
let ``test Array.sortWith works`` () =
    let xs = [| 3.; 4.; 1.; 2. |]
    let ys = xs |> Array.sortWith (fun x y -> int(x - y))
    ys.[0] + ys.[1]
    |> equal 3.

[<Fact>]
let ``test Array.sortDescending works`` () =
    let xs = [| 3; 4; 1; -3; 2; 10 |]
    xs |> Array.sortDescending |> Array.take 3 |> Array.sum |> equal 17

// --- Reverse, Append, Concat ---

[<Fact>]
let ``test Array.reverse works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.rev arr
    result.[0] |> equal 3
    result.[1] |> equal 2
    result.[2] |> equal 1

[<Fact>]
let ``test Array.append works`` () =
    let result = Array.append [| 1; 2 |] [| 3; 4 |]
    Array.length result |> equal 4
    result.[2] |> equal 3

[<Fact>]
let ``test Array.append with strings works`` () =
    let xs1 = [| 1; 2; 3; 4 |]
    let zs1 = Array.append [| 0 |] xs1
    zs1.[0] + zs1.[1] |> equal 1
    let xs2 = [| "a"; "b"; "c" |]
    let zs2 = Array.append [| "x"; "y" |] xs2
    zs2.[1] + zs2.[3] |> equal "yb"

[<Fact>]
let ``test Array.concat works`` () =
    let xs = [|[| 1. |]; [| 2. |]; [| 3. |]; [| 4. |]|]
    let ys = xs |> Array.concat
    ys.[0] + ys.[1]
    |> equal 3.

[<Fact>]
let ``test Array.concat works with strings`` () =
    [| [| "One" |]; [| "Two" |] |]
    |> Array.concat
    |> List.ofArray
    |> equal [ "One"; "Two" ]

// --- Head, Last, Tail, TryHead, TryLast ---

[<Fact>]
let ``test Array.head works`` () =
    Array.head [| 10; 20; 30 |] |> equal 10

[<Fact>]
let ``test Array.tryHead works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.tryHead xs |> equal (Some 1.)
    Array.tryHead [||] |> equal None

[<Fact>]
let ``test Array.last works`` () =
    Array.last [| 10; 20; 30 |] |> equal 30

[<Fact>]
let ``test Array.tryLast works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.tryLast xs |> equal (Some 4.)
    Array.tryLast [||] |> equal None

[<Fact>]
let ``test Array.tail works`` () =
    let xs = [| 1.; 2.; 3.; 4. |]
    Array.tail xs |> Array.length |> equal 3

// --- Choose, Partition ---

[<Fact>]
let ``test Array.choose with ints works`` () =
    let xs = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
    let result = xs |> Array.choose (fun i ->
        if i % 2 = 1 then Some i
        else None)
    result.Length |> equal 5

[<Fact>]
let ``test Array.choose with longs works`` () =
    let xs = [| 1L; 2L; 3L; 4L |]
    let result = xs |> Array.choose (fun x ->
        if x > 2L then Some x
        else None)
    result.[0] + result.[1]
    |> equal 7L

[<Fact>]
let ``test Array.partition works`` () =
    let xs = [| 1.; 2.; 3. |]
    let ys, zs = xs |> Array.partition (fun x -> x <= 1.)
    equal ys [| 1. |]
    equal zs [| 2.; 3. |]

// --- Distinct ---

[<Fact>]
let ``test Array.distinct works`` () =
    let xs = [| 1; 1; 1; 2; 2; 3; 3 |]
    let ys = xs |> Array.distinct
    ys |> Array.length |> equal 3
    ys |> Array.sum |> equal 6

// --- Take, Skip, Truncate ---

[<Fact>]
let ``test Array.skip works`` () =
    let xs = [| 1; 2; 3; 4; 5 |]
    xs |> Array.skip 2 |> equal [| 3; 4; 5 |]

[<Fact>]
let ``test Array.skipWhile works`` () =
    let xs = [| 1; 2; 3; 4; 5 |]
    xs |> Array.skipWhile (fun x -> x < 3) |> equal [| 3; 4; 5 |]
    xs |> Array.skipWhile (fun x -> x < 1) |> equal [| 1; 2; 3; 4; 5 |]
    xs |> Array.skipWhile (fun x -> x < 6) |> equal [||]
    [||] |> Array.skipWhile (fun x -> x < 3) |> equal [||]

[<Fact>]
let ``test Array.take works`` () =
    let xs = [| 1; 2; 3; 4; 5 |]
    xs |> Array.take 3 |> equal [| 1; 2; 3 |]

[<Fact>]
let ``test Array.takeWhile works`` () =
    let xs = [| 1; 2; 3; 4; 5 |]
    xs |> Array.takeWhile (fun x -> x < 3) |> equal [| 1; 2 |]
    xs |> Array.takeWhile (fun x -> x < 1) |> equal [||]
    xs |> Array.takeWhile (fun x -> x < 6) |> equal [| 1; 2; 3; 4; 5 |]
    [||] |> Array.takeWhile (fun x -> x < 3) |> equal [||]

[<Fact>]
let ``test Array.truncate works`` () =
    let xs = [| 1.; 2.; 3.; 4.; 5. |]
    xs |> Array.truncate 2
    |> Array.last
    |> equal 2.
    xs.Length |> equal 5

// --- Zip, Unzip ---

[<Fact>]
let ``test Array.zip works`` () =
    let xs = [| 1.; 2.; 3. |]
    let ys = [| 1.; 2.; 3. |]
    let zs = Array.zip xs ys
    let x, y = zs.[0]
    x + y |> equal 2.

[<Fact>]
let ``test Array.unzip works`` () =
    let xs = [| 1., 2. |]
    let ys, zs = xs |> Array.unzip
    ys.[0] + zs.[0]
    |> equal 3.

// --- ToList, OfList, OfSeq, ToSeq ---

[<Fact>]
let ``test Array.ofList works`` () =
    let xs = [1.; 2.]
    let ys = Array.ofList xs
    ys.Length |> equal 2

[<Fact>]
let ``test Array.ofSeq works`` () =
    let xs = seq { yield 1; yield 2 }
    let ys = Array.ofSeq xs
    ys.[0] |> equal 1

[<Fact>]
let ``test Array.toList works`` () =
    let xs = [| 1.; 2. |]
    let ys = xs |> Array.toList
    ys.[0] + ys.[1]
    |> equal 3.

[<Fact>]
let ``test Array.toSeq works`` () =
    let xs = [| 1.; 2. |]
    let ys = xs |> Array.toSeq
    ys |> Seq.head
    |> equal 1.

// --- Range ---

[<Fact>]
let ``test Array.range works`` () =
    [| 1..5 |]
    |> Array.reduce (+)
    |> equal 15
    [| 0..2..9 |]
    |> Array.reduce (+)
    |> equal 20

// --- Permute ---

[<Fact>]
let ``test Array.permute works`` () =
    let xs = [| 1.; 2. |]
    let ys = xs |> Array.permute (fun i -> i + 1 - 2 * (i % 2))
    ys.[0] |> equal 2.

// --- CountBy, GroupBy ---

[<Fact>]
let ``test Array.countBy works`` () =
    let xs = [| 1; 2; 3; 4 |]
    xs |> Array.countBy (fun x -> x % 2)
    |> Array.length |> equal 2

[<Fact>]
let ``test Array.groupBy returns valid array`` () =
    let xs = [| 1; 2 |]
    let actual = Array.groupBy (fun _ -> true) xs
    let actualKey, actualGroup = actual.[0]
    let worked = actualKey && actualGroup.[0] = 1 && actualGroup.[1] = 2
    worked |> equal true

// --- Windowed, Pairwise ---

[<Fact>]
let ``test Array.windowed works`` () =
    let nums = [| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]
    Array.windowed 3 nums |> equal [|[|1.0; 1.5; 2.0|]; [|1.5; 2.0; 1.5|]; [|2.0; 1.5; 1.0|]; [|1.5; 1.0; 1.5|]|]
    Array.windowed 5 nums |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0 |]; [| 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
    Array.windowed 6 nums |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
    Array.windowed 7 nums |> Array.isEmpty |> equal true

[<Fact>]
let ``test Array.pairwise works`` () =
    Array.pairwise<int> [||] |> equal [||]
    Array.pairwise [|1|] |> equal [||]
    Array.pairwise [|1; 2|] |> equal [|(1, 2)|]
    let xs = [| 1; 2; 3; 4 |]
    let xs2 = xs |> Array.pairwise
    equal [|(1, 2); (2, 3); (3, 4)|] xs2

// --- SplitInto, Transpose ---

[<Fact>]
let ``test Array.splitInto works`` () =
    [|1..10|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..7|]; [|8..10|] |]
    [|1..11|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..8|]; [|9..11|] |]
    [|1..12|] |> Array.splitInto 3 |> equal [| [|1..4|]; [|5..8|]; [|9..12|] |]
    [|1..5|] |> Array.splitInto 4 |> equal [| [|1..2|]; [|3|]; [|4|]; [|5|] |]
    [|1..4|] |> Array.splitInto 20 |> equal [| [|1|]; [|2|]; [|3|]; [|4|] |]

[<Fact>]
let ``test Array.transpose works`` () =
    Array.transpose (seq [[|1..3|]; [|4..6|]])
    |> equal [|[|1;4|]; [|2;5|]; [|3;6|]|]
    Array.transpose [|[|1..3|]|]
    |> equal [|[|1|]; [|2|]; [|3|]|]
    Array.transpose [|[|1|]; [|2|]|]
    |> equal [|[|1..2|]|]
    Array.transpose [| |]
    |> equal [| |]
    Array.transpose [| [||] |]
    |> equal [| |]

// --- UpdateAt, InsertAt, RemoveAt ---

[<Fact>]
let ``test Array.updateAt works`` () =
    equal [|0; 2; 3; 4; 5|] (Array.updateAt 0 0 [|1..5|])
    equal [|1; 2; 0; 4; 5|] (Array.updateAt 2 0 [|1..5|])
    equal [|1; 2; 3; 4; 0|] (Array.updateAt 4 0 [|1..5|])
    equal [|"0"; "2"; "3"; "4"; "5"|] (Array.updateAt 0 "0" [|"1"; "2"; "3"; "4"; "5"|])

[<Fact>]
let ``test Array.insertAt works`` () =
    equal [|0; 1; 2; 3; 4; 5|] (Array.insertAt 0 0 [|1..5|])
    equal [|1; 2; 0; 3; 4; 5|] (Array.insertAt 2 0 [|1..5|])
    equal [|1; 2; 3; 4; 0; 5|] (Array.insertAt 4 0 [|1..5|])
    equal [|0|] (Array.insertAt 0 0 [||])

[<Fact>]
let ``test Array.insertManyAt works`` () =
    equal [|0; 0; 1; 2; 3; 4; 5|] (Array.insertManyAt 0 [0; 0] [|1..5|])
    equal [|1; 2; 0; 0; 3; 4; 5|] (Array.insertManyAt 2 [0; 0] [|1..5|])
    equal [|1; 2; 3; 4; 0; 0; 5|] (Array.insertManyAt 4 [0; 0] [|1..5|])
    equal [|0; 0|] (Array.insertManyAt 0 [0; 0] [||])

[<Fact>]
let ``test Array.removeAt works`` () =
    equal [|2; 3; 4; 5|] (Array.removeAt 0 [|1..5|])
    equal [|1; 2; 4; 5|] (Array.removeAt 2 [|1..5|])
    equal [|1; 2; 3; 4|] (Array.removeAt 4 [|1..5|])

[<Fact>]
let ``test Array.removeManyAt works`` () =
    equal [|3; 4; 5|] (Array.removeManyAt 0 2 [|1..5|])
    equal [|1; 2; 5|] (Array.removeManyAt 2 2 [|1..5|])
    equal [|1; 2; 3|] (Array.removeManyAt 3 2 [|1..5|])

// --- CompareWith ---

[<Fact>]
let ``test Array.compareWith works`` () =
    let a = [|1;3|]
    let b = [|1;2;3|]
    let c3 = Array.compareWith compare a b
    equal c3 1

// --- System.Array ---

// TODO: System.Array.IndexOf with union types requires equality dispatch
// [<Fact>]
// let ``test System.Array.IndexOf works with non-primitive types`` () =
//     let myArray = [|Duck 5|]
//     System.Array.IndexOf(myArray, Duck 3) |> equal -1
//     System.Array.IndexOf(myArray, Dog 5) |> equal -1
//     System.Array.IndexOf(myArray, Duck 5) |> equal 0

// --- Additional tests ported from JS ---

[<Fact>]
let ``test Array.sub works`` () =
    let xs = [|0..99|]
    let ys = Array.sub xs 5 10
    ys |> Array.sum |> equal 95

// TODO: Array.fill requires mutable array support
// [<Fact>]
// let ``test Array.fill works`` () =
//     let xs = Array.zeroCreate 4
//     Array.fill xs 1 2 3
//     xs |> Array.sum |> equal 6

// TODO: Array.blit requires mutable array support
// [<Fact>]
// let ``test Array.blit works`` () =
//     let xs = [| 1..10 |]
//     let ys = Array.zeroCreate 20
//     Array.blit xs 3 ys 5 4
//     ys.[5] + ys.[6] + ys.[7] + ys.[8] |> equal 22

[<Fact>]
let ``test Array.exists2 works`` () =
    let xs = [|1; 2; 3; 4|]
    let ys = [|1; 2; 3; 4|]
    Array.exists2 (fun x y -> x * y = 16) xs ys
    |> equal true

[<Fact>]
let ``test Array.forall2 works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let ys = [|1.; 2.; 3.; 5.|]
    Array.forall2 (=) xs ys
    |> equal false
    Array.forall2 (fun x y -> x <= 4. && y <= 5.) xs ys
    |> equal true

[<Fact>]
let ``test Array.fold2 works`` () =
    let xs = [|1; 2; 3; 4|]
    let ys = [|1; 2; 3; 4|]
    let total = Array.fold2 (fun x y z -> x + y + z) 0 xs ys
    total |> equal 20

[<Fact>]
let ``test Array.foldBack2 works`` () =
    let xs = [|1; 2; 3; 4|]
    let ys = [|1; 2; 3; 4|]
    let total = Array.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
    total |> equal -4

[<Fact>]
let ``test Array.iter works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let mutable total = 0.
    xs |> Array.iter (fun x -> total <- total + x)
    total |> equal 10.

[<Fact>]
let ``test Array.iter2 works`` () =
    let xs = [|1; 2; 3; 4|]
    let mutable total = 0
    Array.iter2 (fun x y -> total <- total - x - y) xs xs
    total |> equal -20

[<Fact>]
let ``test Array.iteri works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let mutable total = 0.
    xs |> Array.iteri (fun i x -> total <- total + (float i) * x)
    total |> equal 20.

[<Fact>]
let ``test Array.iteri2 works`` () =
    let xs = [|1.; 2.; 3.; 4.|]
    let mutable total = 0.
    Array.iteri2 (fun i x y -> total <- total + (float i) * x + (float i) * y) xs xs
    total |> equal 40.

// TODO: Array.sortInPlace requires mutable array support (arrays are immutable lists in Beam)
// [<Fact>]
// let ``test Array.sortInPlace works`` () =
//     let xs = [|3.; 4.; 1.; 2.; 10.|]
//     Array.sortInPlace xs
//     xs.[0] + xs.[1] |> equal 3.

// TODO: Array.sortInPlaceBy requires mutable array support
// [<Fact>]
// let ``test Array.sortInPlaceBy works`` () =
//     let xs = [|3.; 4.; 1.; 2.; 10.|]
//     Array.sortInPlaceBy (fun x -> -x) xs
//     xs.[0] + xs.[1] |> equal 14.

// TODO: Array.sortInPlaceWith requires mutable array support
// [<Fact>]
// let ``test Array.sortInPlaceWith works`` () =
//     let xs = [|3.; 4.; 1.; 2.; 10.|]
//     Array.sortInPlaceWith (fun x y -> int(x - y)) xs
//     xs.[0] + xs.[1] |> equal 3.

[<Fact>]
let ``test Array.sortByDescending works`` () =
    let xs = [|3.; 4.; 1.; 2.|]
    let ys = xs |> Array.sortByDescending (fun x -> -x)
    ys.[0] + ys.[1] |> equal 3.

[<Fact>]
let ``test Array.exactlyOne works`` () =
    [|1.|] |> Array.exactlyOne |> equal 1.
    (try Array.exactlyOne [|1.;2.|] |> ignore; false with | _ -> true) |> equal true
    (try Array.exactlyOne [||] |> ignore; false with | _ -> true) |> equal true

[<Fact>]
let ``test Array.tryExactlyOne works`` () =
    [|1.|] |> Array.tryExactlyOne |> equal (Some 1.)
    [|1.;2.|] |> Array.tryExactlyOne |> equal None
    [||] |> Array.tryExactlyOne |> equal None

[<Fact>]
let ``test Array.groupBy works`` () =
    let xs = [|1; 2|]
    let actual = Array.groupBy (fun _ -> true) xs
    let actualKey, actualGroup = actual.[0]
    let worked = actualKey && actualGroup.[0] = 1 && actualGroup.[1] = 2
    worked |> equal true

[<Fact>]
let ``test Array.except works`` () =
    Array.except [|2|] [|1; 3; 2|] |> Array.last |> equal 3
    Array.except [|2|] [|2; 4; 6|] |> Array.head |> equal 4
    Array.except [|1|] [|1; 1; 1; 1|] |> Array.isEmpty |> equal true
    Array.except [|'t'; 'e'; 's'; 't'|] [|'t'; 'e'; 's'; 't'|] |> Array.isEmpty |> equal true
    Array.except [|(1, 2)|] [|(1, 2)|] |> Array.isEmpty |> equal true

[<Fact>]
let ``test Array.chunkBySize works`` () =
    [|1..8|] |> Array.chunkBySize 4 |> equal [| [|1..4|]; [|5..8|] |]
    [|1..10|] |> Array.chunkBySize 4 |> equal [| [|1..4|]; [|5..8|]; [|9..10|] |]

[<Fact>]
let ``test Array.splitAt works`` () =
    let ar = [|1;2;3;4|]
    Array.splitAt 0 ar |> equal ([||], [|1;2;3;4|])
    Array.splitAt 3 ar |> equal ([|1;2;3|], [|4|])
    Array.splitAt 4 ar |> equal ([|1;2;3;4|], [||])

[<Fact>]
let ``test Array.allPairs works`` () =
    let xs = [|1;2|]
    let ys = [|'a';'b'|]
    Array.allPairs xs ys
    |> equal [|(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')|]

[<Fact>]
let ``test Array.distinct with tuples works`` () =
    let xs = [|(1, 2); (2, 3); (1, 2)|]
    let ys = xs |> Array.distinct
    ys |> Array.length |> equal 2
    ys |> Array.sumBy fst |> equal 3

[<Fact>]
let ``test Array.distinctBy with tuples works`` () =
    let xs = [| 4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7 |]
    let ys = xs |> Array.distinctBy (fun (x,_) -> x % 2)
    ys |> Array.length |> equal 2

[<Fact>]
let ``test Array.indexed works`` () =
    let xs = [|"a"; "b"; "c"|] |> Array.indexed
    xs.Length |> equal 3
    fst xs.[2] |> equal 2
    snd xs.[2] |> equal "c"

[<Fact>]
let ``test Array.zip3 works`` () =
    let xs = [|1.; 2.; 3.|]
    let ys = [|1.; 2.; 3.|]
    let zs = [|1.; 2.; 3.|]
    let ks = Array.zip3 xs ys zs
    let x, y, z = ks.[0]
    x + y + z |> equal 3.

[<Fact>]
let ``test Array.unzip3 works`` () =
    let xs = [|1., 2., 3.|]
    let ys, zs, ks = xs |> Array.unzip3
    ys.[0] + zs.[0] + ks.[0] |> equal 6.
