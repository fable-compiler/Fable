module Fable.Tests.Seqs

open Util
open Util.Testing

let sumFirstTwo (zs: seq<float>) =
    let second = Seq.skip 1 zs |> Seq.head
    let first = Seq.head zs
    first + second

let seqChoose xss =
    let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
    xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

let rec sumFirstSeq (zs: seq<float>) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> Seq.head zs
   | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

type DummyUnion = Number of int
type ExceptFoo = { Bar:string }

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
let ``test Can test untyped enumerables`` () =
    let sumEnumerable (enumerable: obj) =
        let mutable sum = 0
        match enumerable with
        | :? System.Collections.IEnumerable as items ->
            for item in items do
                sum <- sum + (item :?> int)
        | _ -> sum <- -1
        sum
    let xs = box [|1; 2|]
    let ys = box [3; 4]
    let zs = box 1
    sumEnumerable xs |> equal 3
    sumEnumerable ys |> equal 7
    sumEnumerable zs |> equal -1

[<Fact>]
let ``test Seq.length works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.length xs
    |> equal 4

[<Fact>]
let ``test Seq.delay works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Seq.delay (fun () -> xs :> _ seq)
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.unfold works`` () =
    1 |> Seq.unfold (fun x ->
       if x <= 5 then Some(x, x + 1)
       else None)
    |> Seq.length
    |> equal 5

[<Fact>]
let ``test Seq.empty works`` () =
    let xs = Seq.empty<int>
    Seq.length xs
    |> equal 0

[<Fact>]
let ``test Seq.append works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [0.]
    let zs = Seq.append ys xs
    sumFirstTwo zs
    |> equal 1.

[<Fact>]
let ``test Seq.average for empty sequence`` () =
    let xs = Seq.empty<float>
    (try Seq.average xs |> ignore; false with | _ -> true) |> equal true

[<Fact>]
let ``test Seq.averageBy for empty sequence`` () =
    let xs = Seq.empty<float>
    (try Seq.averageBy ((*) 2.) xs |> ignore; false with | _ -> true) |> equal true

[<Fact>]
let ``test Seq.average works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.average xs
    |> equal 2.5

[<Fact>]
let ``test Seq.averageBy works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.averageBy ((*) 2.) xs
    |> equal 5.

[<Fact>]
let ``test Seq.average works with custom types`` () =
    seq {MyNumber 1; MyNumber 2; MyNumber 3}
    |> Seq.average |> equal (MyNumber 2)

[<Fact>]
let ``test Seq.averageBy works with custom types`` () =
    seq {{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }}
    |> Seq.averageBy (fun x -> x.MyNumber) |> equal (MyNumber 4)

[<Fact>]
let ``test Seq.choose works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let zs = xs |> Seq.choose (fun x ->
       if x > 2. then Some x
       else None)
    sumFirstTwo zs
    |> equal 7.

[<Fact>]
let ``test Seq.choose works with generic arguments`` () =
    let res = seqChoose [ Some [ 5 ] ]
    equal [ 5 ] res

[<Fact>]
let ``test Seq.concat works`` () =
    let xs = [[1.]; [2.]; [3.]; [4.]]
    let ys = xs |> Seq.concat
    sumFirstTwo ys
    |> equal 3.

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
    xs |> Seq.findIndex ((=) 2.) |> equal 1

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
let ``test Seq.map works`` () =
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

[<Fact>]
let ``test Seq.groupBy works`` () =
     let xs = [1; 2; 3; 4]
     let ys = xs |> Seq.groupBy (fun x -> x % 2)
     ys |> Seq.length |> equal 2
     ys |> Seq.iter (fun (k,v) ->
         v |> Seq.exists (fun x -> x % 2 <> k) |> equal false)

[<Fact>]
let ``test Seq.groupBy with structural equality works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.groupBy (fun x -> Number (x % 2))
    ys |> Seq.length |> equal 2

[<Fact>]
let ``test Seq.exactlyOne works`` () =
    let xs = [1.]
    xs |> Seq.exactlyOne
    |> equal 1.

[<Fact>]
let ``test Seq.tryExactlyOne works`` () =
        seq {1.} |> Seq.tryExactlyOne |> equal (Some 1.)
        seq {1.; 2.} |> Seq.tryExactlyOne |> equal None
        Seq.empty<float> |> Seq.tryExactlyOne |> equal None

[<Fact>]
let ``test Seq.initInfinite works`` () =
    Seq.initInfinite (fun i -> 2. * float i)
    |> Seq.take 10
    |> Seq.sum
    |> equal 90.

[<Fact>]
let ``test Seq.last works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.last
    |> equal 4.

[<Fact>]
let ``test Seq.tryLast works`` () =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryLast xs |> equal (Some 4.)
    Seq.tryLast [] |> equal None

[<Fact>]
let ``test Seq.pairwise works`` () =
    let xs = [1; 2; 3; 4]
    xs |> Seq.pairwise
    |> Seq.map (fun (x, y) -> sprintf "%i%i" x y)
    |> String.concat ""
    |> equal "122334"

[<Fact>]
let ``test Seq.pairwise works with empty input`` () = // See #1941
    ([||] : int array) |> Seq.pairwise |> Seq.toArray |> equal [||]
    [1] |> Seq.pairwise |> Seq.toList |> equal []
    [1; 2] |> Seq.pairwise |> Seq.toList |> equal [(1, 2)]

[<Fact>]
let ``test Seq.readonly works`` () =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.readonly
    |> Seq.head
    |> equal 1.

[<Fact>]
let ``test Seq.singleton works`` () =
    let xs = Seq.singleton 1.
    Seq.head xs |> equal 1.
    Seq.head xs |> equal 1.

[<Fact>]
let ``test Seq.singleton works with None`` () =
    let xs : int option seq = Seq.singleton None
    xs
    |> Seq.length
    |> equal 1

[<Fact>]
let ``test Seq.skipWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.skipWhile (fun i -> i <= 3.)
    |> Seq.head
    |> equal 4.

[<Fact>]
let ``test Seq.take works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.take 2
    |> Seq.last
    |> equal 2.
    // Seq.take should throw an exception if there're not enough elements
    try xs |> Seq.take 20 |> Seq.length with _ -> -1
    |> equal -1

[<Fact>]
let ``test Seq.takeWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.takeWhile (fun i -> i < 3.)
    |> Seq.last
    |> equal 2.

[<Fact>]
let ``test Seq.truncate works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.truncate 2
    |> Seq.last
    |> equal 2.
    // Seq.truncate shouldn't throw an exception if there're not enough elements
    try xs |> Seq.truncate 20 |> Seq.length with _ -> -1
    |> equal 5

[<Fact>]
let ``test Seq.where works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.where (fun i -> i <= 3.)
    |> Seq.length
    |> equal 3

[<Fact>]
let ``test Seq.except works`` () =
    Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
    Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4
    Seq.except [1] [1; 1; 1; 1] |> Seq.isEmpty |> equal true
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> Seq.isEmpty |> equal true
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> Seq.isEmpty |> equal true
    Seq.except [(1, 2)] [(1, 2)] |> Seq.isEmpty |> equal true
    Seq.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> Seq.isEmpty |> equal true
    Seq.except [|49|] [|7; 49|] |> Seq.last|> equal 7
    Seq.except [{ Bar= "test" }] [{ Bar = "test" }] |> Seq.isEmpty |> equal true

[<Fact>]
let ``test Seq.item throws exception when index is out of range`` () =
    let xs = [0]
    (try Seq.item 1 xs |> ignore; false with | _ -> true) |> equal true

[<Fact>]
let ``test Seq iterators from range do rewind`` () =
    let xs = seq {for i=1 to 5 do i}
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    let xs = seq {1..5}
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    let xs = seq {'A'..'F'}
    xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"
    xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"

[<Fact>]
let ``test Seq.filter doesn't blow the stack with long sequences`` () = // See #459
  let max = 1000000
  let a = [| for i in 1 .. max -> 0  |] // init with 0
  let b = a |> Seq.filter( fun x -> x > 10) |> Seq.toArray
  equal 0 b.Length

[<Fact>]
let ``test Seq.windowed works`` () = // See #1716
    let nums = [ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ] :> _ seq
    Seq.windowed 3 nums |> Seq.toArray |> equal [|[|1.0; 1.5; 2.0|]; [|1.5; 2.0; 1.5|]; [|2.0; 1.5; 1.0|]; [|1.5; 1.0; 1.5|]|]
    Seq.windowed 5 nums |> Seq.toArray |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0 |]; [| 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
    Seq.windowed 6 nums |> Seq.toArray |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
    Seq.windowed 7 nums |> Seq.isEmpty |> equal true

[<Fact>]
let ``test Seq.allPairs works`` () =
    let mutable accX = 0
    let mutable accY = 0
    let xs = seq { accX <- accX + 1; for i = 1 to 4 do i }
    let ys = seq { accY <- accY + 1; for i = 'a' to 'f' do i }
    let res = Seq.allPairs xs ys
    let res1 = List.ofSeq res
    let res2 = List.ofSeq res
    let expected =
        [(1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f');
         (2, 'a'); (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f');
         (3, 'a'); (3, 'b'); (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f');
         (4, 'a'); (4, 'b'); (4, 'c'); (4, 'd'); (4, 'e'); (4, 'f')]
    accX |> equal 2
    accY |> equal 1
    equal expected res1
    equal expected res2

[<Fact>]
let ``test Seq.splitInto works`` () =
    seq {1..10} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..7|]; [|8..10|] ]
    seq {1..11} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..11|] ]
    seq {1..12} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..12|] ]
    seq {1..5} |> Seq.splitInto 4 |> Seq.toList |> equal [ [|1..2|]; [|3|]; [|4|]; [|5|] ]
    seq {1..4} |> Seq.splitInto 20 |> Seq.toList |> equal [ [|1|]; [|2|]; [|3|]; [|4|] ]

[<Fact>]
let ``test Seq.transpose works`` () =
    let seqEqual (expected: seq<'T seq>) (actual: seq<'T seq>) =
        (actual |> Seq.map Seq.toArray |> Seq.toArray)
        |> equal (expected |> Seq.map Seq.toArray |> Seq.toArray)
    // integer seq
    Seq.transpose (seq [seq {1..3}; seq {4..6}])
    |> seqEqual [seq [1; 4]; seq [2; 5]; seq [3; 6]]
    Seq.transpose (seq [seq {1..3}])
    |> seqEqual [seq [1]; seq [2]; seq [3]]
    Seq.transpose (seq [seq [1]; seq [2]])
    |> seqEqual [seq {1..2}]
    // string seq
    Seq.transpose (seq [seq ["a";"b";"c"]; seq ["d";"e";"f"]])
    |> seqEqual [seq ["a";"d"]; seq ["b";"e"]; seq ["c";"f"]]
    // empty seq
    Seq.transpose Seq.empty
    |> seqEqual Seq.empty
    // seq of empty seqs - m x 0 seq transposes to 0 x m (i.e. empty)
    Seq.transpose (seq [Seq.empty])
    |> seqEqual Seq.empty
    Seq.transpose (seq [Seq.empty; Seq.empty])
    |> seqEqual Seq.empty
    // sequences of lists
    Seq.transpose [["a";"b"]; ["c";"d"]]
    |> seqEqual [seq ["a";"c"]; seq ["b";"d"]]
    Seq.transpose (seq { ["a";"b"]; ["c";"d"] })
    |> seqEqual [seq ["a";"c"]; seq ["b";"d"]]

[<Fact>]
let ``test Seq.udpateAt works`` () =
    // integer list
    equal [0; 2; 3; 4; 5] (Seq.updateAt 0 0 [1..5] |> Seq.toList)
    equal [1; 2; 0; 4; 5] (Seq.updateAt 2 0 [1..5] |> Seq.toList)
    equal [1; 2; 3; 4; 0] (Seq.updateAt 4 0 [1..5] |> Seq.toList)

    //string list
    equal ["0"; "2"; "3"; "4"; "5"] (Seq.updateAt 0 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "0"; "4"; "5"] (Seq.updateAt 2 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"; "4"; "0"] (Seq.updateAt 4 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    throwsAnyError (fun () -> Seq.updateAt 0 0 [] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.updateAt -1 0 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.updateAt 2 0 [1] |> Seq.toList |> ignore)

[<Fact>]
let ``test Seq.insertAt works`` () =
    // integer list
    equal [0; 1; 2; 3; 4; 5] (Seq.insertAt 0 0 [1..5] |> Seq.toList)
    equal [1; 2; 0; 3; 4; 5] (Seq.insertAt 2 0 [1..5] |> Seq.toList)
    equal [1; 2; 3; 4; 0; 5] (Seq.insertAt 4 0 [1..5] |> Seq.toList)

    //string list
    equal ["0"; "1"; "2"; "3"; "4"; "5"] (Seq.insertAt 0 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "0"; "3"; "4"; "5"] (Seq.insertAt 2 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"; "4"; "0"; "5"] (Seq.insertAt 4 "0" ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    equal [0] (Seq.insertAt 0 0 [] |> Seq.toList)
    throwsAnyError (fun () -> Seq.insertAt -1 0 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.insertAt 2 0 [1] |> Seq.toList |> ignore)

[<Fact>]
let ``test Seq.insertManyAt works`` () =
    // integer list
    equal [0; 0; 1; 2; 3; 4; 5] (Seq.insertManyAt 0 [0; 0] [1..5] |> Seq.toList)
    equal [1; 2; 0; 0; 3; 4; 5] (Seq.insertManyAt 2 [0; 0] [1..5] |> Seq.toList)
    equal [1; 2; 3; 4; 0; 0; 5] (Seq.insertManyAt 4 [0; 0] [1..5] |> Seq.toList)

    //string list
    equal ["0"; "0"; "1"; "2"; "3"; "4"; "5"] (Seq.insertManyAt 0 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "0"; "0"; "3"; "4"; "5"] (Seq.insertManyAt 2 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"; "4"; "0"; "0"; "5"] (Seq.insertManyAt 4 ["0"; "0"] ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    equal [0; 0] (Seq.insertManyAt 0 [0; 0] [] |> Seq.toList)
    throwsAnyError (fun () -> Seq.insertManyAt -1 [0; 0] [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.insertManyAt 2 [0; 0] [1] |> Seq.toList |> ignore)

[<Fact>]
let ``test Seq.removeAt works`` () =
    // integer list
    equal [2; 3; 4; 5] (Seq.removeAt 0 [1..5] |> Seq.toList)
    equal [1; 2; 4; 5] (Seq.removeAt 2 [1..5] |> Seq.toList)
    equal [1; 2; 3; 4] (Seq.removeAt 4 [1..5] |> Seq.toList)

    //string list
    equal ["2"; "3"; "4"; "5"] (Seq.removeAt 0 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "4"; "5"] (Seq.removeAt 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"; "4"] (Seq.removeAt 4 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    throwsAnyError (fun () -> Seq.removeAt 0 [] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeAt -1 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeAt 2 [1] |> Seq.toList |> ignore)

[<Fact>]
let ``test Seq.removeManyAt works`` () =
    // integer list
    equal [3; 4; 5] (Seq.removeManyAt 0 2 [1..5] |> Seq.toList)
    equal [1; 2; 5] (Seq.removeManyAt 2 2 [1..5] |> Seq.toList)
    equal [1; 2; 3] (Seq.removeManyAt 3 2 [1..5] |> Seq.toList)

    //string list
    equal ["3"; "4"; "5"] (Seq.removeManyAt 0 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "5"] (Seq.removeManyAt 2 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"] (Seq.removeManyAt 3 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    throwsAnyError (fun () -> Seq.removeManyAt 0 2 [] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeManyAt -1 2 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeManyAt 2 2 [1] |> Seq.toList |> ignore)
