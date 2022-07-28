module Fable.Tests.Seqs

open Util.Testing

let sumFirstTwo (zs: seq<float>) =
    let second = Seq.skip 1 zs |> Seq.head
    let first = Seq.head zs
    first + second

let rec sumFirstSeq (zs: seq<float>) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> Seq.head zs
   | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

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
let ``test Seq.empty works`` () =
    let xs = Seq.empty<int>
    Seq.length xs
    |> equal 0

[<Fact>]
let ``test Seq.length works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.length xs
    |> equal 4

[<Fact>]
let ``test Seq.map works`` () =
    let xs = [1; 2; 3; 4]
    xs
    |> Seq.map string
    |> List.ofSeq
    |> equal ["1"; "2"; "3"; "4"]


[<Fact>]
let ``test Seq.singleton works`` () =
    let xs = Seq.singleton 42
    xs
    |> List.ofSeq
    |> equal [42]

[<Fact>]
let ``test Seq.collect works`` () =
    let xs = ["a"; "fable"; "bar" ]
    xs
    |> Seq.ofList
    |> Seq.collect (fun a -> [a.Length])
    |> List.ofSeq
    |> equal [1; 5; 3]

[<Fact>]
let ``test Seq.collect works II"`` () =
    let xs = [[1.]; [2.]; [3.]; [4.]]
    let ys = xs |> Seq.collect id
    sumFirstTwo ys
    |> equal 3.

    let xs1 = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
    let ys1 = xs1 |> Seq.collect id
    sumFirstSeq ys1 5
    |> equal 15.

[<Fact>]
let ``test Seq.collect works with Options`` () =
    let xss = [[Some 1; Some 2]; [None; Some 3]]
    Seq.collect id xss
    |> Seq.sumBy (function
        | Some n -> n
        | None -> 0
    )
    |> equal 6

    seq {
        for xs in xss do
            for x in xs do
                x
    }
    |> Seq.length
    |> equal 4

[<Fact>]
let ``test Seq.chunkBySize works`` () =
    seq {1..8} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|] ]
    seq {1..10} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..10|] ]

[<Fact>]
let ``test Seq.exists works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.exists (fun x -> x = 2.)
    |> equal true

[<Fact>]
let ``test Seq.exists2 works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [1.; 2.; 3.; 4.]
    Seq.exists2 (fun x y -> x * y = 16.) xs ys
    |> equal true

[<Fact>]
let ``test Seq.filter works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = xs |> Seq.filter (fun x -> x > 5.)
    ys |> Seq.isEmpty
    |> equal true

[<Fact>]
let ``test Seq.find works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.find ((=) 2.)
    |> equal 2.

[<Fact>]
let ``test Seq.findIndex works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.findIndex ((=) 2.)

[<Fact>]
let ``test Seq.findBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.find ((>) 4.) |> equal 1.
    xs |> Seq.findBack ((>) 4.) |> equal 3.

[<Fact>]
let ``test Seq.findBack with option works`` () =
    let xs = [Some 1.; None]
    xs |> Seq.find Option.isSome |> equal (Some(1.))
    xs |> Seq.find Option.isSome |> (fun v -> v.Value) |> equal 1.
    xs |> Seq.findBack Option.isNone |> equal None

[<Fact>]
let ``test Seq.findIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.findIndex ((>) 4.) |> equal 0
    xs |> Seq.findIndexBack ((>) 4.) |> equal 2

[<Fact>]
let ``test Seq.tryFindBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> Seq.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> Seq.tryFindBack ((=) 5.) |> equal None

[<Fact>]
let ``test Seq.tryFindBack with option works`` () =
    let xs = [Some 1.; None]
    xs |> Seq.tryFind Option.isSome |> equal (Some(Some 1.))
    xs |> Seq.tryFind Option.isSome |> (fun v -> v.Value.Value) |> equal 1.
    xs |> Seq.tryFindBack Option.isNone |> equal (Some None)
    xs |> Seq.tryFindBack Option.isNone |> (fun v -> v.Value) |>  equal None
    xs |> Seq.tryFindBack (fun v -> match v with Some v -> v = 2. | _ -> false) |> equal None

[<Fact>]
let ``test Seq.tryFindIndexBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> Seq.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> Seq.tryFindIndexBack ((=) 5.) |> equal None

[<Fact>]
let ``test Seq.fold works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let total = xs |> Seq.fold (+) 0.
    total |> equal 10.

[<Fact>]
let ``test Seq.fold with tupled arguments works`` () =
    let a, b =
        ((1, 5), [1;2;3;4])
        ||> Seq.fold (fun (a, b) i ->
            a * i, b + i)
    equal 24 a
    equal 15 b

[<Fact>]
let ``test Seq.forall works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.forall (fun x -> x < 5.) xs
    |> equal true

[<Fact>]
let ``test Seq.forall2 works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [1.; 2.; 3.; 4.]
    Seq.forall2 (=) xs ys
    |> equal true

[<Fact>]
let ``test Seq.forall is lazy`` () =
    let mutable x = ""
    let one() = x <- "one"; false
    let two() = x <- "two"; true
    let ok =
        [one; two]
        |> Seq.map (fun c -> c())
        |> Seq.forall id
    ok |> equal false
    x |> equal "one"

[<Fact>]
let ``test Seq.head works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.head xs
    |> equal 1.

[<Fact>]
let ``test Seq.head with option works`` () =
    let xs = [Some 1.; None]
    Seq.head xs |> equal (Some 1.)
    Seq.head xs |> (fun v -> v.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.head xs |> equal None

[<Fact>]
let ``test Seq.tryHead works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryHead xs |> equal (Some 1.)
    Seq.tryHead [] |> equal None

[<Fact>]
let ``test Seq.tryHead with option works`` () =
    let xs = [Some 1.; None]
    Seq.tryHead xs |> equal (Some(Some 1.))
    Seq.tryHead xs |> (fun v -> v.Value.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.tryHead xs |> equal (Some(None))
    Seq.tryHead xs |> (fun v -> v.Value) |> equal None

[<Fact>]
let ``test Seq.tail works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tail xs |> Seq.length |> equal 3

[<Fact>]
let ``test Seq.init works`` () =
    let xs = Seq.init 4 float
    sumFirstTwo xs
    |> equal 1.

[<Fact>]
let ``test Seq.isEmpty works`` () =
    let xs = [1]
    Seq.isEmpty xs |> equal false
    Seq.isEmpty [] |> equal true

[<Fact>]
let ``test Seq.iter works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Seq.iter (fun x ->
       total.Value <- total.Value + x
    )
    total.Value |> equal 10.

[<Fact>]
let ``test Seq.iter2 works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    Seq.iter2 (fun x y ->
       total.Value <- total.Value + x + y
    ) xs xs
    total.Value |> equal 20.

[<Fact>]
let ``test Seq.iteri works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Seq.iteri (fun i x ->
       total.Value <- total.Value + (float i) * x
    )
    total.Value |> equal 20.

[<Fact>]
let ``test Seq.map works II`` () =
    let xs = [1.]
    let ys = xs |> Seq.map ((*) 2.)
    ys |> Seq.head
    |> equal 2.

[<Fact>]
let ``test Seq.map2 works`` () =
    let xs = [1.]
    let ys = [2.]
    let zs = Seq.map2 (*) xs ys
    zs |> Seq.head
    |> equal 2.

[<Fact>]
let ``test Seq.mapi works`` () =
    let xs = [1.]
    let ys = xs |> Seq.mapi (fun i x -> float i + x)
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.indexed works`` () =
    let xs = seq { "a"; "b"; "c" } |> Seq.indexed
    let x = xs |> Seq.tail |> Seq.head
    fst x |> equal 1
    snd x |> equal "b"

[<Fact>]
let ``test Seq.mapFold works`` () =
    let xs = [1y; 2y; 3y; 4y]
    let result = xs |> Seq.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
    fst result |> Seq.sum |> equal 20y
    snd result |> equal 10y

[<Fact>]
let ``test Seq.mapFoldBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let result = Seq.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
    fst result |> Seq.sum |> equal -20.
    snd result |> equal -10.

[<Fact>]
let ``test Seq.max works`` () =
    let xs = [1.; 2.]
    xs |> Seq.max
    |> equal 2.

[<Fact>]
let ``test Seq.maxBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.maxBy (fun x -> -x)
    |> equal 1.

[<Fact>]
let ``test Seq.min works`` () =
    let xs = [1.; 2.]
    xs |> Seq.min
    |> equal 1.

[<Fact>]
let ``test Seq.minBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.minBy (fun x -> -x)
    |> equal 2.

[<Fact>]
let ``test Seq.max with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.max |> equal p2

[<Fact>]
let ``test Seq.maxBy with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.maxBy Point.Neg |> equal p1

[<Fact>]
let ``test Seq.min with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.min |> equal p1

[<Fact>]
let ``test Seq.minBy with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.minBy Point.Neg |> equal p2

[<Fact>]
let ``test Seq.maxBy with numeric projection works`` () =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.maxBy (fun p -> p.y) |> equal p1

[<Fact>]
let ``test Seq.minBy with numeric projection works`` () =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.minBy (fun p -> p.y) |> equal p2

[<Fact>]
let ``test Seq.sum works`` () =
    let xs = [1.; 2.]
    xs |> Seq.sum
    |> equal 3.

[<Fact>]
let ``test Seq.sumBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.sumBy ((*) 2.)
    |> equal 6.

[<Fact>]
let ``test Seq.sum with non numeric types works`` () =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sum |> (=) {x=3;y=30} |> equal true

[<Fact>]
let ``test Seq.sumBy with non numeric types works`` () =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

[<Fact>]
let ``test Seq.sumBy with numeric projection works`` () =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sumBy (fun p -> p.y) |> equal 30

[<Fact>]
let ``test Seq.sum with non numeric types works II`` () =
    seq {MyNumber 1; MyNumber 2; MyNumber 3}
    |> Seq.sum |> equal (MyNumber 6)

[<Fact>]
let ``test Seq.sumBy with non numeric types works II`` () =
    seq {{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }}
    |> Seq.sumBy (fun x -> x.MyNumber) |> equal (MyNumber 12)

[<Fact>]
let ``test Seq.item works`` () =
    let xs = [1.; 2.]
    Seq.item 1 xs
    |> equal 2.

[<Fact>]
let ``test Seq.tryItem works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryItem 3 xs |> equal (Some 4.)
    Seq.tryItem 4 xs |> equal None
    Seq.tryItem -1 xs |> equal None

[<Fact>]
let ``test Seq.ofArray works`` () =
    let xs = [|1.; 2.|]
    let ys = Seq.ofArray xs
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.ofList works`` () =
    let xs = [1.; 2.]
    let ys = Seq.ofList xs
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.pick works`` () =
    let xs = [1.; 2.]
    xs |> Seq.pick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    |> equal 2.

[<Fact>]
let ``test Seq.range works`` () =
    seq{1..5}
    |> Seq.reduce (+)
    |> equal 15

    seq{1. .. 5.}
    |> Seq.reduce (+)
    |> equal 15.

[<Fact>]
let ``test Seq.range step works`` () =
    seq{0..2..9}
    |> Seq.reduce (+)
    |> equal 20

    seq{0. .. 2. .. 9.}
    |> Seq.reduce (+)
    |> equal 20.

    seq{9 .. -2 .. 0}
    |> Seq.reduce (+)
    |> equal 25

[<Fact>]
let ``test Seq.range works with chars`` () =
    seq{'a' .. 'f'}
    |> Seq.toArray
    |> System.String
    |> equal "abcdef"

    seq{'z' .. 'a'}
    |> Seq.length
    |> equal 0

[<Fact>]
let ``test Seq.range works with long`` () =
    seq{1L..5L}
    |> Seq.reduce (+)
    |> equal 15L

    seq{1UL .. 5UL}
    |> Seq.reduce (+)
    |> equal 15UL

[<Fact>]
let ``test Seq.range step works with long`` () =
    seq{0L..2L..9L}
    |> Seq.reduce (+)
    |> equal 20L

    seq{0UL..2UL..9UL}
    |> Seq.reduce (+)
    |> equal 20UL

[<Fact>]
let ``test Seq.range works with decimal`` () =
    seq{1M .. 50M}
    |> Seq.reduce (+)
    |> equal 1275M

[<Fact>]
let ``test Seq.range step works with decimal`` () =
    seq {-3M .. -0.4359698987M .. -50M}
    |> Seq.reduce (+)
    |> equal -2843.0340746886M

[<Fact>]
let ``test Seq.range works with bigint`` () =
    seq{1I..2000I}
    |> Seq.reduce (+)
    |> equal 2001000I

[<Fact>]
let ``test Seq.range step works with bigint`` () =
    seq {1I .. 10000000000000I .. 20000000000000000I}
    |> Seq.reduce (+)
    |> equal 19990000000000002000I

[<Fact>]
let ``test Seq.reduce works`` () =
    let xs = [1.; 2.]
    xs |> Seq.reduce (+)
    |> equal 3.

[<Fact>]
let ``test Seq.scan works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = xs |> Seq.scan (+) 0.
    sumFirstTwo ys |> equal 1.

[<Fact>]
let ``test Seq.scan works with empty input`` () =
    let xs = Seq.empty
    let ys = xs |> Seq.scan (+) 3
    Seq.head ys |> equal 3
    Seq.length ys |> equal 1

[<Fact>]
let ``test Seq.sort works`` () =
    let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
    let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
    xs |> Seq.sort |> Seq.take 3 |> Seq.sum |> equal 0.
    ys |> Seq.sort |> Seq.item 1 |> equal "a"

[<Fact>]
let ``test Seq.sort with tuples works`` () =
    let xs = seq {3; 1; 1; -3}
    let ys = seq {"a"; "c"; "B"; "d"}
    (xs, ys) ||> Seq.zip |> Seq.sort |> Seq.item 1 |> equal (1, "B")

// FIXME:
//[<Fact>]
// let ``test Seq.sortDescending works`` () =
//     let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
//     xs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> equal 17.
//     let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
//     ys |> Seq.sortDescending |> Seq.item 1 |> equal "c"

// FIXME:
// [<Fact>]
// let ``test Seq.sortBy works`` () =
//     let xs = [3.; 1.; 4.; 2.]
//     let ys = xs |> Seq.sortBy (fun x -> -x)
//     sumFirstTwo ys
//     |> equal 7.

[<Fact>]
let ``test Seq.sortByDescending works`` () =
    let xs = [3.; 1.; 4.; 2.]
    let ys = xs |> Seq.sortByDescending (fun x -> -x)
    sumFirstTwo ys
    |> equal 3.

[<Fact>]
let ``test Seq.skip works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.skip 1
    ys |> Seq.head
    |> equal 2.

[<Fact>]
let ``test Seq.skip fails when there're not enough elements`` () =
    let error, xs = ref false, [|1;2;3;4;5|]
    try
        Seq.skip 5 xs |> Seq.length |> equal 0
    with _ -> error.Value <- true
    equal false error.Value
    try
        Seq.skip 6 xs |> Seq.length |> equal 0
    with _ -> error.Value <- true
    equal true error.Value

[<Fact>]
let ``test Seq.toArray works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toArray
    ys.[0] + ys.[1]
    |> equal 3.

[<Fact>]
let ``test Seq.toList works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toList
    ys.Head + ys.Tail.Head
    |> equal 3.

[<Fact>]
let ``test Seq.tryFind works`` () =
    [1.; 2.]
    |> Seq.tryFind ((=) 1.)
    |> Option.isSome
    |> equal true
    [1.; 2.] |> Seq.tryFind ((=) 5.) |> equal None

[<Fact>]
let ``test Seq.tryFindIndex works`` () =
    [1.; 2.]
    |> Seq.tryFindIndex ((=) 2.)
    |> Option.get
    |> equal 1
    [1.; 2.] |> Seq.tryFindIndex ((=) 5.) |> equal None

[<Fact>]
let ``test Seq.tryPick works`` () =
    let xs = [1.; 2.]
    let r = xs |> Seq.tryPick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    match r with
    | Some x -> x
    | None -> 0.
    |> equal 2.

[<Fact>]
let ``test Seq.zip works`` () =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = Seq.zip xs ys
    let x, y = zs |> Seq.head
    x + y
    |> equal 2.

[<Fact>]
let ``test Seq.zip3 works`` () =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = [1.; 2.; 3.]
    let ks = Seq.zip3 xs ys zs
    let x, y, z = ks |> Seq.head
    x + y + z
    |> equal 3.

[<Fact>]
let ``test Seq.cache works`` () =
     let count = ref 0
     let xs =
        1 |> Seq.unfold(fun i ->
           count.Value <- count.Value + 1
           if i <= 10 then Some(i, i+1)
           else None)
           |> Seq.cache
     xs |> Seq.length |> ignore
     xs |> Seq.length |> ignore
     count.Value
     |> equal 11

[<Fact>]
let ``test Seq.cast works`` () =
    let xs = [box 1; box 2; box 3]
    let ys = Seq.cast<int> xs
    ys |> Seq.head |> equal 1

[<Fact>]
let ``test Seq.compareWith works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 5]
    let zs = [1; 2; 3; 3]
    Seq.compareWith (-) xs xs |> equal 0
    Seq.compareWith (-) xs ys |> equal -1
    Seq.compareWith (-) xs zs |> equal 1

[<Fact>]
let ``test Seq.countBy works`` () =
     let xs = [1; 2; 3; 4]
     let ys = xs |> Seq.countBy (fun x -> x % 2)
     ys |> Seq.length |> equal 2

[<Fact>]
let ``test Seq.distinct works`` () =
    let xs = [1; 1; 1; 2; 2; 3; 3]
    let ys = xs |> Seq.distinct
    ys |> Seq.length |> equal 3
    ys |> Seq.sum |> equal 6

[<Fact>]
let ``test Seq.distinct with tuples works`` () =
    let xs = [(1, 2); (2, 3); (1, 2)]
    let ys = xs |> Seq.distinct
    ys |> Seq.length |> equal 2
    ys |> Seq.sumBy fst |> equal 3

[<Fact>]
let ``test Seq.distinctBy works`` () =
    let xs = [4; 4; 4; 6; 6; 5; 5]
    let ys = xs |> Seq.distinctBy (fun x -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.head >= 4 |> equal true

[<Fact>]
let ``test Seq.distinctBy with tuples works`` () =
    let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
    let ys = xs |> Seq.distinctBy (fun (x,_) -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.head |> fst >= 4 |> equal true

[<Fact>]
let ``test Seq.distinct works on infinite sequences`` () =
    let rec numbersFrom n =
      seq { yield n; yield n; yield! numbersFrom (n + 1) }
    let xs =
        numbersFrom 1
        |> Seq.distinct
        |> Seq.take 5
    xs |> Seq.toList |> equal [1; 2; 3; 4; 5]

[<Fact>]
let ``test Seq.distinctBy works on infinite sequences`` () =
    let rec numbersFrom n =
      seq { yield n; yield n; yield! numbersFrom (n + 1) }
    let xs =
        numbersFrom 1
        |> Seq.distinctBy (fun x -> x / 5)
        |> Seq.take 5
    xs |> Seq.toList |> equal [1; 5; 10; 15; 20]

// [<Fact>]
// let ``test Seq.groupBy works`` () =
//     let xs = [1; 2; 3; 4]
//     let ys = xs |> Seq.groupBy (fun x -> x % 2)
//     ys |> Seq.length |> equal 2
//     ys |> Seq.iter (fun (k,v) ->
//         v |> Seq.exists (fun x -> x % 2 <> k) |> equal false)
