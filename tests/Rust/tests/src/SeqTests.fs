module Fable.Tests.SeqTests

open Util.Testing

let sumFirstTwo (xs: seq<float>) =
    let e = xs.GetEnumerator()
    let first = if e.MoveNext() then e.Current else nan
    let second = if e.MoveNext() then e.Current else nan
    first + second

// let rec sumFirstSeq (zs: seq<float>) (n: int): float =
//    match n with
//    | 0 -> 0.
//    | 1 -> Seq.head zs
//    | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

// let testSeqChoose xss =
//     let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
//     xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

type DummyUnion = Number of int
type ExceptFoo = { Bar:string }

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

// [<Fact>]
// let ``Can test untyped enumerables`` () =
//     let sumEnumerable (enumerable: obj) =
//         let mutable sum = 0
//         match enumerable with
//         | :? System.Collections.IEnumerable as items ->
//             for item in items do
//                 sum <- sum + (item :?> int)
//         | _ -> sum <- -1
//         sum
//     let xs = box [|1; 2|]
//     let ys = box [3; 4]
//     let zs = box 1
//     sumEnumerable xs |> equal 3
//     sumEnumerable ys |> equal 7
//     sumEnumerable zs |> equal -1

[<Fact>]
let ``Seq.length with arrays works`` () =
    let xs = [|1; 2; 3; 4|]
    Seq.length xs |> equal 4

[<Fact>]
let ``Seq.length with lists works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.length xs |> equal 4

[<Fact>]
let ``Seq.length with seq works`` () =
    let xs = seq { 1; 2; 3; 4 }
    Seq.length xs |> equal 4

[<Fact>]
let ``Seq.delay works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = Seq.delay (fun () -> xs :> _ seq)
    Seq.length ys |> equal 4

[<Fact>]
let ``Seq.unfold works`` () =
    1 |> Seq.unfold (fun x ->
        if x <= 5 then Some(x, x + 1)
        else None)
    |> Seq.length
    |> equal 5

[<Fact>]
let ``Seq.empty works`` () =
    let xs = Seq.empty<int>
    Seq.length xs |> equal 0

// [<Fact>]
// let ``Seq equality works`` () =
//     let a1 = seq {1; 2; 3}
//     let a2 = seq {1; 2; 3}
//     let a3 = seq {1; 2; 4}
//     let a4 = seq {1; 2; 3; 4}
//     a1 = a1 |> equal true
//     // a1 = a2 |> equal false //TODO: reference equality
//     a1 = a3 |> equal false
//     a1 = a4 |> equal false
//     a1 <> a1 |> equal false
//     // a1 <> a2 |> equal true //TODO: reference inequality
//     a1 <> a3 |> equal true
//     a1 <> a4 |> equal true

// [<Fact>]
// let ``Seq.Equals works`` () =
//     let a1 = seq {1; 2; 3}
//     let a2 = seq {1; 2; 3}
//     let a3 = seq {1; 2; 4}
//     let a4 = seq {1; 2; 3; 4}
//     a1.Equals(a1) |> equal true
//     // a1.Equals(a2) |> equal false //TODO: reference equality
//     a1.Equals(a3) |> equal false
//     a1.Equals(a4) |> equal false

[<Fact>]
let ``Seq.compareWith works`` () =
    let a1 = seq {1; 2; 3}
    let a2 = seq {1; 2; 3}
    let a3 = seq {1; 2; 4}
    let a4 = seq {1; 2; 3; 4}
    Seq.compareWith compare a1 a1 |> equal 0
    Seq.compareWith compare a1 a2 |> equal 0
    Seq.compareWith compare a1 a3 |> equal -1
    Seq.compareWith compare a1 a4 |> equal -1
    Seq.compareWith compare a3 a4 |> equal 1
    Seq.compareWith compare a3 a2 |> equal 1
    Seq.compareWith compare a4 a2 |> equal 1
    Seq.compareWith compare a4 a3 |> equal -1

[<Fact>]
let ``Seq.append works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = [5.; 6.; 7.]
    let zs = Seq.append xs ys
    Seq.length zs |> equal 7
    sumFirstTwo zs |> equal 3.

// [<Fact>]
// let ``Seq.average for empty sequence`` () =
//     let xs = Seq.empty<float>
//     throwsAnyError (fun () -> Seq.average xs |> ignore)

// [<Fact>]
// let ``Seq.averageBy for empty sequence`` () =
//     let xs = Seq.empty<float>
//     throwsAnyError (fun () -> Seq.averageBy ((*) 2.) xs |> ignore)

[<Fact>]
let ``Seq.average works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.average xs
    |> equal 2.5

[<Fact>]
let ``Seq.averageBy works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.averageBy ((*) 2.) xs
    |> equal 5.

// [<Fact>]
// let ``Seq.average works with custom types`` () =
//     seq {MyNumber 1; MyNumber 2; MyNumber 3}
//     |> Seq.average |> equal (MyNumber 2)

// [<Fact>]
// let ``Seq.averageBy works with custom types`` () =
//     seq {{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }}
//     |> Seq.averageBy (fun x -> x.MyNumber) |> equal (MyNumber 4)

[<Fact>]
let ``Seq.choose works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let zs = xs |> Seq.choose (fun x ->
        if x > 2. then Some x
        else None)
    sumFirstTwo zs
    |> equal 7.

// [<Fact>]
// let ``Seq.choose works with generic arguments`` () =
//     let res = testSeqChoose [ Some [ 5 ] ]
//     equal [ 5 ] res

[<Fact>]
let ``Seq.concat works`` () =
    let xs = [[1.] :> seq<_>; [2.; 3.; 4.] :> seq<_>]
    let ys = xs |> Seq.concat
    Seq.length ys |> equal 4
    sumFirstTwo ys |> equal 3.

[<Fact>]
let ``Seq.concat works II`` () =
    let xs = [| seq {1.}; seq {2.; 3.; 4.} |]
    let ys = xs |> Seq.concat
    Seq.length ys |> equal 4
    sumFirstTwo ys |> equal 3.

// [<Fact>]
// let ``Seq.concat works III`` () =
//     //TODO: make it work without casting inner items to seq
//     let xs = [[1.]; [2.]; [3.]; [4.]]
//     let ys = xs |> Seq.concat
//     Seq.length ys |> equal 4
//     sumFirstTwo ys |> equal 3.

[<Fact>]
let ``Seq.collect works`` () =
    let xs = [[1.]; [2.]; [3.]; [4.]]
    let ys = xs |> List.map List.toSeq |> Seq.collect id
    sumFirstTwo ys |> equal 3.
    let xs1 = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
    let ys1 = xs1 |> List.map List.toSeq |> Seq.collect id
    ys1 |> Seq.toArray |> equal [|1.;2.;3.;4.;5.;6.;7.|]

[<Fact>]
let ``Seq.collect works with Options`` () =
    let xss = [[Some 1; Some 2]; [None; Some 3]]
    xss |> List.map List.toSeq
    |> Seq.collect id
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
let ``Seq.chunkBySize works`` () =
    seq {1..8} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|] ]
    seq {1..10} |> Seq.chunkBySize 4 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..10|] ]

[<Fact>]
let ``Seq.contains works`` () =
    let xs = seq {1; 2; 3; 4}
    xs |> Seq.contains 2 |> equal true
    xs |> Seq.contains 0 |> equal false

[<Fact>]
let ``Seq.exists works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.exists (fun x -> x = 2.)
    |> equal true

[<Fact>]
let ``Seq.exists2 works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = seq {1.; 2.; 3.; 4.}
    Seq.exists2 (fun x y -> x * y = 16.) xs ys
    |> equal true

[<Fact>]
let ``Seq.filter works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = xs |> Seq.filter (fun x -> x > 5.)
    ys |> Seq.isEmpty
    |> equal true

[<Fact>]
let ``Seq.find works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.find ((=) 2.)
    |> equal 2.

[<Fact>]
let ``Seq.findIndex works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.findIndex ((=) 2.)
    |> equal 1

[<Fact>]
let ``Seq.findBack works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.find ((>) 4.) |> equal 1.
    xs |> Seq.findBack ((>) 4.) |> equal 3.

[<Fact>]
let ``Seq.findBack with option works`` () =
    let xs = [Some 1.; None]
    xs |> Seq.find Option.isSome |> equal (Some(1.))
    xs |> Seq.find Option.isSome |> (fun v -> v.Value) |> equal 1.
    xs |> Seq.findBack Option.isNone |> equal None

[<Fact>]
let ``Seq.findIndexBack works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.findIndex ((>) 4.) |> equal 0
    xs |> Seq.findIndexBack ((>) 4.) |> equal 2

[<Fact>]
let ``Seq.tryFindBack works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> Seq.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> Seq.tryFindBack ((=) 5.) |> equal None

[<Fact>]
let ``Seq.tryFindBack with option works`` () =
    let xs = [Some 1.; None]
    xs |> Seq.tryFind Option.isSome |> equal (Some(Some 1.))
    xs |> Seq.tryFind Option.isSome |> (fun v -> v.Value.Value) |> equal 1.
    xs |> Seq.tryFindBack Option.isNone |> equal (Some None)
    xs |> Seq.tryFindBack Option.isNone |> (fun v -> v.Value) |>  equal None
    xs |> Seq.tryFindBack (fun v -> match v with Some v -> v = 2. | _ -> false) |> equal None

[<Fact>]
let ``Seq.tryFindIndexBack works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> Seq.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> Seq.tryFindIndexBack ((=) 5.) |> equal None

[<Fact>]
let ``Seq.fold works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let total = xs |> Seq.fold (+) 0.
    total |> equal 10.

[<Fact>]
let ``Seq.fold2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    let total = Seq.fold2 (fun x y z -> x + y + z) 0 xs ys
    total |> equal 20

[<Fact>]
let ``Seq.fold with tupled arguments works`` () =
    let a, b =
        ((1, 5), [1;2;3;4])
        ||> Seq.fold (fun (a, b) i ->
            a * i, b + i)
    equal 24 a
    equal 15 b

[<Fact>]
let ``Seq.foldBack works`` () =
    let xs = [1; 2; 3; 4]
    let total = Seq.foldBack (fun x acc -> acc - x) xs 0
    total |> equal -10

[<Fact>]
let ``Seq.foldBack2 works`` () =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 4]
    let total = Seq.foldBack2 (fun x y acc -> x + y - acc) xs ys 0
    total |> equal -4

[<Fact>]
let ``Seq.forall works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.forall (fun x -> x < 5.) xs
    |> equal true

[<Fact>]
let ``Seq.forall2 works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = [1.; 2.; 3.; 4.]
    Seq.forall2 (=) xs ys
    |> equal true

// [<Fact>]
// let ``Seq.forall is lazy`` () = // See #1715
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
let ``Seq.head works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.head xs |> equal 1.

[<Fact>]
let ``Seq.head with option works`` () =
    let xs = [Some 1.; None]
    Seq.head xs |> equal (Some 1.)
    Seq.head xs |> (fun v -> v.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.head xs |> equal None

[<Fact>]
let ``Seq.tryHead works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.tryHead xs |> equal (Some 1.)
    // Seq.tryHead [] |> equal None // TODO: untyped
    Seq.tryHead<float> [] |> equal None

[<Fact>]
let ``Seq.tryHead with option works`` () =
    let xs = [Some 1.; None]
    Seq.tryHead xs |> equal (Some(Some 1.))
    Seq.tryHead xs |> (fun v -> v.Value.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.tryHead xs |> equal (Some(None))
    Seq.tryHead xs |> (fun v -> v.Value) |> equal None

[<Fact>]
let ``Seq.tail works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = Seq.tail xs
    Seq.length ys |> equal 3
    Seq.head ys |> equal 2.

[<Fact>]
let ``Seq.init works`` () =
    let xs = Seq.init 4 float
    sumFirstTwo xs
    |> equal 1.

[<Fact>]
let ``Seq.isEmpty works`` () =
    let xs = [1]
    Seq.isEmpty xs |> equal false
    // Seq.isEmpty [] |> equal true //TODO: untyped
    Seq.isEmpty<int> [] |> equal true

[<Fact>]
let ``Seq.iter works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let mutable total = 0.
    xs |> Seq.iter (fun x ->
        total <- total + x
    )
    total |> equal 10.

[<Fact>]
let ``Seq.iter2 works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let mutable total = 0.
    Seq.iter2 (fun x y ->
        total <- total + x + y
    ) xs xs
    total |> equal 20.

[<Fact>]
let ``Seq.iteri works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let mutable total = 0.
    xs |> Seq.iteri (fun i x ->
        total <- total + (float i) * x
    )
    total |> equal 20.

[<Fact>]
let ``Seq.map works`` () =
    let xs = [1.]
    let ys = xs |> Seq.map ((+) 2.)
    ys |> Seq.head |> equal 3.

[<Fact>]
let ``Seq.map2 works`` () =
    let xs = [1.]
    let ys = [2.]
    let zs = Seq.map2 (+) xs ys
    zs |> Seq.head |> equal 3.

[<Fact>]
let ``Seq.map3 works`` () =
    let xs = [1.]
    let ys = [2.]
    let zs = [3.]
    let res = Seq.map3 (fun x y z -> x + y + z) xs ys zs
    res |> Seq.head |> equal 6.

[<Fact>]
let ``Seq.mapi works`` () =
    let xs = [1.]
    let ys = Seq.mapi (fun i x -> float i + x) xs
    ys |> Seq.head |> equal 1.

[<Fact>]
let ``Seq.mapi2 works`` () =
    let xs = [1.]
    let ys = [2.]
    let zs = Seq.mapi2 (fun i x y -> float i + x + y) xs ys
    zs |> Seq.length |> equal 1
    zs |> Seq.head |> equal 3.

[<Fact>]
let ``Seq.indexed works`` () =
    let xs = seq { "a"; "b"; "c" } |> Seq.indexed
    let x = xs |> Seq.tail |> Seq.head
    fst x |> equal 1
    snd x |> equal "b"

[<Fact>]
let ``Seq.mapFold works`` () =
    let xs = [1y; 2y; 3y; 4y]
    let res = xs |> Seq.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
    fst res |> Seq.toArray |> equal [|2y; 4y; 6y; 8y|]
    snd res |> equal 10y

[<Fact>]
let ``Seq.mapFoldBack works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let res = Seq.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
    fst res |> Seq.toArray |> equal [|-2.; -4.; -6.; -8.|]
    snd res |> equal -10.

[<Fact>]
let ``Seq.max works`` () =
    let xs = [1.; 2.]
    xs |> Seq.max
    |> equal 2.

[<Fact>]
let ``Seq.maxBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.maxBy (fun x -> -x)
    |> equal 1.

[<Fact>]
let ``Seq.min works`` () =
    let xs = [1.; 2.]
    xs |> Seq.min
    |> equal 1.

[<Fact>]
let ``Seq.minBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.minBy (fun x -> -x)
    |> equal 2.

[<Fact>]
let ``Seq.max with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.max |> equal p2

[<Fact>]
let ``Seq.maxBy with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.maxBy Point.Neg |> equal p1

[<Fact>]
let ``Seq.min with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.min |> equal p1

[<Fact>]
let ``Seq.minBy with non numeric types works`` () =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.minBy Point.Neg |> equal p2

[<Fact>]
let ``Seq.maxBy with numeric projection works`` () =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.maxBy (fun p -> p.y) |> equal p1

[<Fact>]
let ``Seq.minBy with numeric projection works`` () =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.minBy (fun p -> p.y) |> equal p2

[<Fact>]
let ``Seq.sum works`` () =
    let xs = [1.; 2.]
    xs |> Seq.sum
    |> equal 3.

[<Fact>]
let ``Seq.sumBy works`` () =
    let xs = [1.; 2.]
    xs |> Seq.sumBy ((*) 2.)
    |> equal 6.

// [<Fact>]
// let ``Seq.sum with non numeric types works`` () =
//     let p1 = {x=1; y=10}
//     let p2 = {x=2; y=20}
//     [p1; p2] |> Seq.sum |> (=) {x=3;y=30} |> equal true

// [<Fact>]
// let ``Seq.sumBy with non numeric types works`` () =
//     let p1 = {x=1; y=10}
//     let p2 = {x=2; y=20}
//     [p1; p2] |> Seq.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

[<Fact>]
let ``Seq.sumBy with numeric projection works`` () =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sumBy (fun p -> p.y) |> equal 30

// [<Fact>]
// let ``Seq.sum with non numeric types works II`` () =
//     seq {MyNumber 1; MyNumber 2; MyNumber 3}
//     |> Seq.sum |> equal (MyNumber 6)

// [<Fact>]
// let ``Seq.sumBy with non numeric types works II`` () =
//     seq {{ MyNumber = MyNumber 5 }; { MyNumber = MyNumber 4 }; { MyNumber = MyNumber 3 }}
//     |> Seq.sumBy (fun x -> x.MyNumber) |> equal (MyNumber 12)

[<Fact>]
let ``Seq.item works`` () =
    let xs = [1.; 2.]
    Seq.item 1 xs
    |> equal 2.

[<Fact>]
let ``Seq.tryItem works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    Seq.tryItem 3 xs |> equal (Some 4.)
    Seq.tryItem 4 xs |> equal None
    Seq.tryItem -1 xs |> equal None

[<Fact>]
let ``Seq.ofArray works`` () =
    let xs = [|1.; 2.|]
    let ys = Seq.ofArray xs
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``Seq.ofList works`` () =
    let xs = [1.; 2.]
    let ys = Seq.ofList xs
    ys |> Seq.head
    |> equal 1.

[<Fact>]
let ``Seq.pick works`` () =
    let xs = [1.; 2.]
    xs |> Seq.pick (fun x ->
        match x with
        | 2. -> Some x
        | _ -> None)
    |> equal 2.

[<Fact>]
let ``Seq.range works`` () =
    seq {1..5}
    |> Seq.reduce (+)
    |> equal 15

    seq {1. .. 5.}
    |> Seq.reduce (+)
    |> equal 15.

[<Fact>]
let ``Seq.range step works`` () =
    seq {0..2..9}
    |> Seq.reduce (+)
    |> equal 20
    seq {0. .. 2. .. 9.}
    |> Seq.reduce (+)
    |> equal 20.
    seq {9 .. -2 .. 0}
    |> Seq.reduce (+)
    |> equal 25

[<Fact>]
let ``Seq.range works with chars`` () =
    seq {'a' .. 'f'}
    |> Seq.toArray
    |> equal [|'a';'b';'c';'d';'e';'f'|]
    seq {'z' .. 'a'}
    |> Seq.length
    |> equal 0

[<Fact>]
let ``Seq.range works with long`` () =
    seq {1L..5L}
    |> Seq.reduce (+)
    |> equal 15L
    seq {1UL .. 5UL}
    |> Seq.reduce (+)
    |> equal 15UL

[<Fact>]
let ``Seq.range step works with long`` () =
    seq {0L..2L..9L}
    |> Seq.reduce (+)
    |> equal 20L
    seq {0UL..2UL..9UL}
    |> Seq.reduce (+)
    |> equal 20UL

[<Fact>]
let ``Seq.range works with decimal`` () =
    seq {1M .. 50M}
    |> Seq.reduce (+)
    |> equal 1275M

[<Fact>]
let ``Seq.range step works with decimal`` () =
    seq {-3M .. -0.4359698987M .. -50M}
    |> Seq.reduce (+)
    |> equal -2843.0340746886M

[<Fact>]
let ``Seq.range works with bigint`` () =
    seq {1I..2000I}
    |> Seq.reduce (+)
    |> equal 2001000I

[<Fact>]
let ``Seq.range step works with bigint`` () =
    seq {1I .. 10000000000000I .. 20000000000000000I}
    |> Seq.reduce (+)
    |> equal 19990000000000002000I

[<Fact>]
let ``Seq.reduce works`` () =
    let xs = [1.; 2.]
    xs |> Seq.reduce (+)
    |> equal 3.

[<Fact>]
let ``Seq.reduceBack works`` () =
    let xs = [1.; 2.]
    xs |> Seq.reduceBack (+)
    |> equal 3.

[<Fact>]
let ``Seq.scan works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = xs |> Seq.scan (+) 0.
    sumFirstTwo ys |> equal 1.

[<Fact>]
let ``Seq.scan works with empty input`` () =
    let xs = Seq.empty<int>
    let ys = xs |> Seq.scan (+) 3
    Seq.head ys |> equal 3
    Seq.length ys |> equal 1

[<Fact>]
let ``Seq.sort works`` () =
    let xs = [3; 4; 1; -3; 2; 10] |> List.toSeq
    let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
    xs |> Seq.sort |> Seq.toArray |> equal [|-3; 1; 2; 3; 4; 10|]
    ys |> Seq.sort |> Seq.toArray |> equal [|"B"; "a"; "c"; "d"|]

[<Fact>]
let ``Seq.sort with tuples works`` () =
    let xs = seq {3; 1; 1; -3}
    let ys = seq {"a"; "c"; "B"; "d"}
    (xs, ys) ||> Seq.zip |> Seq.sort |> Seq.item 1 |> equal (1, "B")

[<Fact>]
let ``Seq.sortDescending works`` () =
    let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
    xs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> equal 17.
    let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
    ys |> Seq.sortDescending |> Seq.item 1 |> equal "c"

[<Fact>]
let ``Seq.sortBy works`` () =
    let xs = [3.; 1.; 4.; 2.]
    let ys = xs |> Seq.sortBy (fun x -> -x)
    sumFirstTwo ys
    |> equal 7.

[<Fact>]
let ``Seq.sortByDescending works`` () =
    let xs = [3.; 1.; 4.; 2.]
    let ys = xs |> Seq.sortByDescending (fun x -> -x)
    sumFirstTwo ys
    |> equal 3.

[<Fact>]
let ``Seq.sortWith works`` () =
    let xs = [3; 4; 1; 2]
    let ys = xs |> Seq.sortWith (fun x y -> int(x - y))
    ys |> Seq.toArray |> equal [|1; 2; 3; 4|]

[<Fact>]
let ``Seq.skip works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.skip 1
    Seq.length ys |> equal 2
    Seq.head ys |> equal 2.

[<Fact>]
let ``Seq.skip fails when there're not enough elements`` () =
    let mutable error = false
    let xs = [|1;2;3;4;5|]
    doesntThrow (fun () ->
        Seq.skip 5 xs |> Seq.length |> equal 0
    )
    throwsAnyError (fun () ->
        Seq.skip 6 xs |> Seq.length |> equal 0
    )

[<Fact>]
let ``Seq.toArray works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toArray
    ys.Length |> equal 3
    ys[0] + ys[1] |> equal 3.

[<Fact>]
let ``Seq.toArray works II`` () =
    let xs = [|1.; 2.; 3.|]
    let ys = xs |> Seq.toArray
    xs[0] <- 2.
    ys.Length |> equal 3
    ys[0] + ys[1] |> equal 3.

[<Fact>]
let ``Seq.toList works`` () =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toList
    ys |> List.length |> equal 3
    ys.Head + ys.Tail.Head |> equal 3.

[<Fact>]
let ``Seq.tryFind works`` () =
    [1.; 2.]
    |> Seq.tryFind ((=) 1.)
    |> Option.isSome
    |> equal true
    [1.; 2.] |> Seq.tryFind ((=) 5.) |> equal None

[<Fact>]
let ``Seq.tryFindIndex works`` () =
    [1.; 2.]
    |> Seq.tryFindIndex ((=) 2.)
    |> Option.get
    |> equal 1
    [1.; 2.] |> Seq.tryFindIndex ((=) 5.) |> equal None

[<Fact>]
let ``Seq.tryPick works`` () =
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
let ``Seq.zip works`` () =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = Seq.zip xs ys
    let x, y = zs |> Seq.head
    x + y
    |> equal 2.

[<Fact>]
let ``Seq.zip3 works`` () =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = [1.; 2.; 3.]
    let ks = Seq.zip3 xs ys zs
    let x, y, z = ks |> Seq.head
    x + y + z
    |> equal 3.

[<Fact>]
let ``Seq.cache works`` () =
    let mutable count = 0
    let xs =
        1 |> Seq.unfold (fun i ->
            count <- count + 1
            if i <= 10 then Some(i, i + 1)
            else None)
        |> Seq.cache
    xs |> Seq.length |> ignore
    xs |> Seq.length |> ignore
    count |> equal 11

[<Fact>]
let ``Seq.cache works for infinite sequences`` () =
    let answers =
        Seq.initInfinite (fun _ -> 42)
        |> Seq.cache
    let n = 10
    let xs = answers |> Seq.truncate n |> Seq.toList
    let ys = answers |> Seq.truncate n |> Seq.toList
    xs |> equal ys

[<Fact>]
let ``Seq.cache works when enumerating partially`` () =
    let xs = seq { 1; 1; 99 }
    let rec loop xs ts =
        match Seq.tryHead xs with
        | Some x ->
            if x < 10
            then loop (Seq.tail xs) (x :: ts)
            else Some x
        | None ->
            None
    loop (Seq.cache xs) [] |> equal (Some 99)
    loop xs [] |> equal (Some 99)

// [<Fact>]
// let ``Seq.cast works`` () =
//     let xs = [box 1; box 2; box 3]
//     let ys = Seq.cast<int> xs
//     ys |> Seq.head |> equal 1

[<Fact>]
let ``Seq.countBy works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.countBy (fun x -> x % 2)
    ys |> Seq.toArray |> equal [|(1, 2); (0, 2)|]

[<Fact>]
let ``Seq.distinct works`` () =
    let xs = [1; 1; 1; 2; 2; 3; 3]
    let ys = xs |> Seq.distinct
    ys |> Seq.length |> equal 3
    ys |> Seq.sum |> equal 6

[<Fact>]
let ``Seq.distinct with tuples works`` () =
    let xs = [(1, 2); (2, 3); (1, 2)]
    let ys = xs |> Seq.distinct
    ys |> Seq.length |> equal 2
    ys |> Seq.sumBy fst |> equal 3

[<Fact>]
let ``Seq.distinctBy works`` () =
    let xs = [4; 4; 4; 6; 6; 5; 5]
    let ys = xs |> Seq.distinctBy (fun x -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.head >= 4 |> equal true

[<Fact>]
let ``Seq.distinctBy with tuples works`` () =
    let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
    let ys = xs |> Seq.distinctBy (fun (x,_) -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.head |> fst >= 4 |> equal true

[<Fact>]
let ``Seq.distinct works on infinite sequences`` () =
    let rec numbersFrom n =
        seq { yield n; yield n; yield! numbersFrom (n + 1) }
    let xs =
        numbersFrom 1
        |> Seq.distinct
        |> Seq.take 5
    xs |> Seq.toList |> equal [1; 2; 3; 4; 5]

[<Fact>]
let ``Seq.distinctBy works on infinite sequences`` () =
    let rec numbersFrom n =
        seq { yield n; yield n; yield! numbersFrom (n + 1) }
    let xs =
        numbersFrom 1
        |> Seq.distinctBy (fun x -> x / 5)
        |> Seq.take 5
    xs |> Seq.toList |> equal [1; 5; 10; 15; 20]

[<Fact>]
let ``Seq.groupBy works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.groupBy (fun x -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.iter (fun (k,v) ->
        v |> Seq.exists (fun x -> x % 2 <> k) |> equal false)

[<Fact>]
let ``Seq.groupBy with structural equality works`` () =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.groupBy (fun x -> Number (x % 2))
    ys |> Seq.length |> equal 2

[<Fact>]
let ``Seq.exactlyOne works`` () =
    let xs = [1.]
    xs |> Seq.exactlyOne
    |> equal 1.

[<Fact>]
let ``Seq.tryExactlyOne works`` () =
    seq {1.} |> Seq.tryExactlyOne |> equal (Some 1.)
    seq {1.; 2.} |> Seq.tryExactlyOne |> equal None
    Seq.empty<float> |> Seq.tryExactlyOne |> equal None

[<Fact>]
let ``Seq.initInfinite works`` () =
    Seq.initInfinite (fun i -> 2. * float i)
    |> Seq.take 10
    |> Seq.sum
    |> equal 90.

[<Fact>]
let ``Seq.last works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    xs |> Seq.last
    |> equal 4.

[<Fact>]
let ``Seq.tryLast works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let empty: float list = []
    xs |> Seq.tryLast |> equal (Some 4.)
    empty |> Seq.tryLast |> equal None

[<Fact>]
let ``Seq.pairwise works`` () =
    Seq.pairwise<int> [] |> Seq.length |> equal 0
    Seq.pairwise [1] |> Seq.length |> equal 0
    Seq.pairwise [1; 2] |> Seq.head |> equal (1, 2)
    let xs = seq {1; 2; 3; 4}
    let ys = xs |> Seq.pairwise
    ys |> Seq.toArray |> equal [|(1, 2); (2, 3); (3, 4)|]

[<Fact>]
let ``Seq.permute works`` () =
    let xs = [1; 2; 3; 4; 5; 6]
    let ys = xs |> Seq.permute (fun i -> i + 1 - 2 * (i % 2))
    ys |> Seq.toArray |> equal [| 2; 1; 4; 3; 6; 5 |]

[<Fact>]
let ``Seq.readonly works`` () =
    let xs = seq {1.; 2.; 3.; 4.}
    let ys = xs |> Seq.readonly
    ys |> Seq.length |> equal 4
    ys |> Seq.head |> equal 1.

[<Fact>]
let ``Seq.replicate works`` () =
    let xs = Seq.replicate 3 4
    xs |> Seq.toArray |> equal [|4;4;4|]

[<Fact>]
let ``Seq.rev works`` () =
    let xs = [1; 2; 3]
    let ys = xs |> Seq.rev
    ys |> Seq.toArray |> equal [|3;2;1|]

[<Fact>]
let ``Seq.singleton works`` () =
    let xs = Seq.singleton 1.
    Seq.head xs |> equal 1.
    Seq.head xs |> equal 1.

[<Fact>]
let ``Seq.singleton works with None`` () =
    let xs: int option seq = Seq.singleton None
    xs
    |> Seq.length
    |> equal 1

[<Fact>]
let ``Seq.skipWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.skipWhile (fun i -> i <= 3.)
    |> Seq.head
    |> equal 4.

[<Fact>]
let ``Seq.take works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    let ys = xs |> Seq.take 2
    ys |> Seq.toArray |> equal [|1.; 2.|]

[<Fact>]
let ``Seq.take works II`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    // Seq.take should throw an exception if there're not enough elements
    throwsAnyError (fun () -> xs |> Seq.take 20 |> Seq.length |> ignore)

[<Fact>]
let ``Seq.takeWhile works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    let ys = xs |> Seq.takeWhile (fun i -> i < 4.)
    ys |> Seq.toArray |> equal [|1.; 2.; 3.|]

[<Fact>]
let ``Seq.truncate works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.truncate 2
    |> Seq.last
    |> equal 2.
    // Seq.truncate shouldn't throw an exception if there're not enough elements
    try xs |> Seq.truncate 20 |> Seq.length with _ -> -1
    |> equal 5

[<Fact>]
let ``Seq.where works`` () =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.where (fun i -> i <= 3.)
    |> Seq.length
    |> equal 3

[<Fact>]
let ``Seq.except works`` () =
    Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
    Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4
    Seq.except [1] [1; 1; 1; 1] |> Seq.isEmpty |> equal true
    Seq.except [|49|] [|7; 49|] |> Seq.last |> equal 7
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> Seq.isEmpty |> equal true
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> Seq.isEmpty |> equal true
    Seq.except [(1, 2)] [(1, 2)] |> Seq.isEmpty |> equal true
    Seq.except [{ Bar= "test" }] [{ Bar = "test" }] |> Seq.isEmpty |> equal true
    // Seq.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> Seq.isEmpty |> equal true

[<Fact>]
let ``Seq.item throws exception when index is out of range`` () =
    let xs = [0]
    throwsAnyError (fun () -> Seq.item 1 xs |> ignore)

[<Fact>]
let ``Seq iterators from range do rewind`` () =
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
let ``Seq.filter doesn't blow the stack with long sequences`` () = // See #459
    let max = 1000000
    let a = [| for i in 1 .. max -> 0 |] // init with 0
    let b = a |> Seq.filter (fun x -> x > 10) |> Seq.toArray
    equal 0 b.Length

[<Fact>]
let ``Seq.windowed works`` () = // See #1716
    let nums = [| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |] :> _ seq
    Seq.windowed 3 nums |> Seq.toArray
    |> equal [| [|1.0; 1.5; 2.0|]; [|1.5; 2.0; 1.5|]; [|2.0; 1.5; 1.0|]; [|1.5; 1.0; 1.5|] |]
    Seq.windowed 5 nums |> Seq.toArray
    |> equal [| [| 1.0; 1.5; 2.0; 1.5; 1.0 |]; [| 1.5; 2.0; 1.5; 1.0; 1.5 |] |]
    Seq.windowed 6 nums |> Seq.toArray
    |> equal [| [| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |] |]
    Seq.windowed 7 nums |> Seq.isEmpty |> equal true

[<Fact>]
let ``Seq.allPairs works`` () =
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
let ``Seq.splitInto works`` () =
    seq {1..10} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..7|]; [|8..10|] ]
    seq {1..11} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..11|] ]
    seq {1..12} |> Seq.splitInto 3 |> Seq.toList |> equal [ [|1..4|]; [|5..8|]; [|9..12|] ]
    seq {1..5} |> Seq.splitInto 4 |> Seq.toList |> equal [ [|1..2|]; [|3|]; [|4|]; [|5|] ]
    seq {1..4} |> Seq.splitInto 20 |> Seq.toList |> equal [ [|1|]; [|2|]; [|3|]; [|4|] ]

[<Fact>]
let ``Seq.transpose works`` () =
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

let ``Seq.transpose with empty seqs works`` () =
    // empty seq of seqs
    Seq.transpose Seq.empty<int seq>
    |> seqEqual Seq.empty
    // seq of empty seqs - m x 0 seq transposes to 0 x m (i.e. empty)
    Seq.transpose (seq [Seq.empty<int>])
    |> seqEqual Seq.empty
    Seq.transpose (seq [Seq.empty<int>; Seq.empty<int>])
    |> seqEqual Seq.empty

// let ``Seq.transpose with empty seqs works`` () =
//     // empty seq of seqs
//     Seq.transpose Seq.empty
//     |> seqEqual Seq.empty
//     // seq of empty seqs - m x 0 seq transposes to 0 x m (i.e. empty)
//     Seq.transpose (seq [Seq.empty])
//     |> seqEqual Seq.empty
//     Seq.transpose (seq [Seq.empty; Seq.empty])
//     |> seqEqual Seq.empty

// [<Fact>]
// let ``Seq.transpose with arrays works`` () =
//     Seq.transpose [| [|"a";"b"|]; [|"c";"d"|] |]
//     |> seqEqual [| [|"a";"c"|]; [|"b";"d"|] |]
//     Seq.transpose (seq { [|"a";"b"|]; [|"c";"d"|] })
//     |> seqEqual [| seq [|"a";"c"|]; seq [|"b";"d"|] |]

// [<Fact>]
// let ``Seq.transpose with lists works`` () =
//     Seq.transpose [["a";"b"]; ["c";"d"]]
//     |> seqEqual [["a";"c"]; ["b";"d"]]
//     Seq.transpose (seq { ["a";"b"]; ["c";"d"] })
//     |> seqEqual [seq ["a";"c"]; seq ["b";"d"]]

[<Fact>]
let ``Seq.updateAt works`` () =
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
let ``Seq.insertAt works`` () =
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
let ``Seq.insertManyAt works`` () =
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
let ``Seq.removeAt works`` () =
    // integer list
    equal [2; 3; 4; 5] (Seq.removeAt 0 [1..5] |> Seq.toList)
    equal [1; 2; 4; 5] (Seq.removeAt 2 [1..5] |> Seq.toList)
    equal [1; 2; 3; 4] (Seq.removeAt 4 [1..5] |> Seq.toList)

    //string list
    equal ["2"; "3"; "4"; "5"] (Seq.removeAt 0 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "4"; "5"] (Seq.removeAt 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"; "4"] (Seq.removeAt 4 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    throwsAnyError (fun () -> Seq.removeAt<int> 0 [] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeAt -1 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeAt 2 [1] |> Seq.toList |> ignore)

[<Fact>]
let ``Seq.removeManyAt works`` () =
    // integer list
    equal [3; 4; 5] (Seq.removeManyAt 0 2 [1..5] |> Seq.toList)
    equal [1; 2; 5] (Seq.removeManyAt 2 2 [1..5] |> Seq.toList)
    equal [1; 2; 3] (Seq.removeManyAt 3 2 [1..5] |> Seq.toList)

    //string list
    equal ["3"; "4"; "5"] (Seq.removeManyAt 0 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "5"] (Seq.removeManyAt 2 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)
    equal ["1"; "2"; "3"] (Seq.removeManyAt 3 2 ["1"; "2"; "3"; "4"; "5"] |> Seq.toList)

    // empty list & out of bounds
    throwsAnyError (fun () -> Seq.removeManyAt<int> 0 2 [] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeManyAt -1 2 [1] |> Seq.toList |> ignore)
    throwsAnyError (fun () -> Seq.removeManyAt 2 2 [1] |> Seq.toList |> ignore)
