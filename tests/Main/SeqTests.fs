[<Util.Testing.TestFixture>]
module Fable.Tests.Seqs
open Util.Testing
open Fable.Tests.Util

let sumFirstTwo (zs: seq<float>) =
   let first = Seq.head zs
   let second = Seq.skip 1 zs |> Seq.head
   first + second

[<Test>]
let ``Seq.length works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.length xs
    |> equal 4

[<Test>]
let ``Seq.delay works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = Seq.delay (fun () -> xs :> _ seq)
    ys |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.unfold works``() =
    1 |> Seq.unfold (fun x ->
       if x <= 5 then Some(x, x + 1)
       else None)
    |> Seq.length
    |> equal 5

[<Test>]
let ``Seq.empty works``() =
    let xs = Seq.empty<int>
    Seq.length xs
    |> equal 0

[<Test>]
let ``Seq.append works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [0.]
    let zs = Seq.append ys xs
    sumFirstTwo zs
    |> equal 1.

[<Test>]
let ``Seq.average works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.average xs
    |> equal 2.5

[<Test>]
let ``Seq.averageBy works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.averageBy ((*) 2.) xs
    |> equal 5.

[<Test>]
let ``Seq.choose works``() =
    let xs = [1.; 2.; 3.; 4.]
    let zs = xs |> Seq.choose (fun x ->
       if x > 2. then Some x
       else None)
    sumFirstTwo zs
    |> equal 7.

let testSeqChoose xss =
    let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
    xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

[<Test>]
let ``Seq.choose works with generic arguments``() =
    let res = testSeqChoose  [ Some [  5  ] ]
    equal [ 5 ] res

[<Test>]
let ``Seq.concat works``() =
    let xs = [[1.]; [2.]; [3.]; [4.]]
    let ys = xs |> Seq.concat
    sumFirstTwo ys
    |> equal 3.


[<Test>]
let ``Seq.chunkBySize works``() =
    let xs = [1;2;3]
    Seq.chunkBySize 1 xs // [[1]; [2]; [3]]
    |> Seq.length
    |> equal 3

    Seq.chunkBySize 2 xs // [[1;2]; [3]]
    |> Seq.head
    |> Seq.sum
    |> equal 3

    Seq.chunkBySize 10 xs // [[1;2;3]]
    |> Seq.head
    |> Seq.sum
    |> equal 6

[<Test>]
let ``Seq.collect works``() =
    let xs = [[1.]; [2.]; [3.]; [4.]]
    let ys = xs |> Seq.collect id
    sumFirstTwo ys
    |> equal 3.

    let xs1 = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
    let ys1 = xs1 |> Seq.collect id
    sumFirstSeq ys1 5
    |> equal 15.

[<Test>]
let ``Seq.collect works with Options``() =
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
                yield x
    }
    |> Seq.length
    |> equal 4



[<Test>]
let ``Seq.exists works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.exists (fun x -> x = 2.)
    |> equal true

[<Test>]
let ``Seq.exists2 works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [1.; 2.; 3.; 4.]
    Seq.exists2 (fun x y -> x * y = 16.) xs ys
    |> equal true

[<Test>]
let ``Seq.filter works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = xs |> Seq.filter (fun x -> x > 5.)
    ys |> Seq.isEmpty
    |> equal true

[<Test>]
let ``Seq.find works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.find ((=) 2.)
    |> equal 2.

[<Test>]
let ``Seq.findIndex works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.findIndex ((=) 2.)
    |> equal 1

[<Test>]
let ``Seq.findBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.find ((>) 4.) |> equal 1.
    xs |> Seq.findBack ((>) 4.) |> equal 3.

[<Test>]
let ``Seq.findBack with option works``() =
    let xs = [Some 1.; None]
    xs |> Seq.find Option.isSome |> equal (Some(1.))
    xs |> Seq.find Option.isSome |> (fun v -> v.Value) |> equal 1.
    xs |> Seq.findBack Option.isNone |> equal None

[<Test>]
let ``Seq.findIndexBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.findIndex ((>) 4.) |> equal 0
    xs |> Seq.findIndexBack ((>) 4.) |> equal 2

[<Test>]
let ``Seq.tryFindBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.tryFind ((>) 4.) |> equal (Some 1.)
    xs |> Seq.tryFindBack ((>) 4.) |> equal (Some 3.)
    xs |> Seq.tryFindBack ((=) 5.) |> equal None


[<Test>]
let ``Seq.tryFindBack with option works``() =
    let xs = [Some 1.; None]
    xs |> Seq.tryFind Option.isSome |> equal (Some(Some 1.))
    xs |> Seq.tryFind Option.isSome |> (fun v -> v.Value.Value) |> equal 1.
    xs |> Seq.tryFindBack Option.isNone |> equal (Some None)
    xs |> Seq.tryFindBack Option.isNone |> (fun v -> v.Value) |>  equal None
    xs |> Seq.tryFindBack (fun v -> match v with Some v -> v = 2. | _ -> false) |> equal None

[<Test>]
let ``Seq.tryFindIndexBack works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.tryFindIndex ((>) 4.) |> equal (Some 0)
    xs |> Seq.tryFindIndexBack ((>) 4.) |> equal (Some 2)
    xs |> Seq.tryFindIndexBack ((=) 5.) |> equal None

[<Test>]
let ``Seq.fold works``() =
    let xs = [1.; 2.; 3.; 4.]
    let total = xs |> Seq.fold (+) 0.
    total |> equal 10.

[<Test>]
let ``Seq.fold with tupled arguments works``() =
    let a, b =
        ((1, 5), [1;2;3;4])
        ||> Seq.fold (fun (a, b) i ->
            a * i, b + i)
    equal 24 a
    equal 15 b

[<Test>]
let ``Seq.forall works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.forall (fun x -> x < 5.) xs
    |> equal true

[<Test>]
let ``Seq.forall2 works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = [1.; 2.; 3.; 4.]
    Seq.forall2 (=) xs ys
    |> equal true

[<Test>]
let ``Seq.head works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.head xs
    |> equal 1.

[<Test>]
let ``Seq.head with option works``() =
    let xs = [Some 1.; None]
    Seq.head xs |> equal (Some 1.)
    Seq.head xs |> (fun v -> v.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.head xs |> equal None

[<Test>]
let ``Seq.tryHead works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryHead xs |> equal (Some 1.)
    Seq.tryHead [] |> equal None

[<Test>]
let ``Seq.tryHead with option works``() =
    let xs = [Some 1.; None]
    Seq.tryHead xs |> equal (Some(Some 1.))
    Seq.tryHead xs |> (fun v -> v.Value.Value) |> equal 1.
    let xs = [None; Some 1.]
    Seq.tryHead xs |> equal (Some(None))
    Seq.tryHead xs |> (fun v -> v.Value) |> equal None

[<Test>]
let ``Seq.tail works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tail xs |> Seq.length |> equal 3

[<Test>]
let ``Seq.init works``() =
    let xs = Seq.init 4 float
    sumFirstTwo xs
    |> equal 1.

[<Test>]
let ``Seq.isEmpty works``() =
    let xs = [1]
    Seq.isEmpty xs
    |> equal false

[<Test>]
let ``Seq.iter works``() =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Seq.iter (fun x ->
       total := !total + x
    )
    !total |> equal 10.

[<Test>]
let ``Seq.iter2 works``() =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    Seq.iter2 (fun x y ->
       total := !total + x + y
    ) xs xs
    !total |> equal 20.

[<Test>]
let ``Seq.iteri works``() =
    let xs = [1.; 2.; 3.; 4.]
    let total = ref 0.
    xs |> Seq.iteri (fun i x ->
       total := !total + (float i) * x
    )
    !total |> equal 20.

[<Test>]
let ``Seq.map works``() =
    let xs = [1.]
    let ys = xs |> Seq.map ((*) 2.)
    ys |> Seq.head
    |> equal 2.

[<Test>]
let ``Seq.map2 works``() =
    let xs = [1.]
    let ys = [2.]
    let zs = Seq.map2 (*) xs ys
    zs |> Seq.head
    |> equal 2.

[<Test>]
let ``Seq.mapi works``() =
    let xs = [1.]
    let ys = xs |> Seq.mapi (fun i x -> float i + x)
    ys |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.indexed works`` () =
    let xs = seq { yield "a"; yield "b"; yield "c" } |> Seq.indexed
    let x = xs |> Seq.tail |> Seq.head
    fst x |> equal 1
    snd x |> equal "b"

[<Test>]
let ``Seq.mapFold works`` () =
    let xs = [1y; 2y; 3y; 4y]
    let result = xs |> Seq.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
    fst result |> Seq.sum |> equal 20y
    snd result |> equal 10y

[<Test>]
let ``Seq.mapFoldBack works`` () =
    let xs = [1.; 2.; 3.; 4.]
    let result = Seq.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
    fst result |> Seq.sum |> equal -20.
    snd result |> equal -10.

[<Test>]
let ``Seq.max works``() =
    let xs = [1.; 2.]
    xs |> Seq.max
    |> equal 2.

[<Test>]
let ``Seq.maxBy works``() =
    let xs = [1.; 2.]
    xs |> Seq.maxBy (fun x -> -x)
    |> equal 1.

[<Test>]
let ``Seq.min works``() =
    let xs = [1.; 2.]
    xs |> Seq.min
    |> equal 1.

[<Test>]
let ``Seq.minBy works``() =
    let xs = [1.; 2.]
    xs |> Seq.minBy (fun x -> -x)
    |> equal 2.

type Point =
    { x: int; y: int }
    static member Zero = { x=0; y=0 }
    static member Neg(p: Point) = { x = -p.x; y = -p.y }
    static member (+) (p1, p2) = { x= p1.x + p2.x; y = p1.y + p2.y }

let ``Seq.max with non numeric types works``() =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.max |> equal p2

[<Test>]
let ``Seq.maxBy with non numeric types works``() =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.maxBy Point.Neg |> equal p1

[<Test>]
let ``Seq.min with non numeric types works``() =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.min |> equal p1

[<Test>]
let ``Seq.minBy with non numeric types works``() =
    let p1 = {x=1; y=1}
    let p2 = {x=2; y=2}
    [p1; p2] |> Seq.minBy Point.Neg |> equal p2

[<Test>]
let ``Seq.maxBy with numeric projection works``() =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.maxBy (fun p -> p.y) |> equal p1

[<Test>]
let ``Seq.minBy with numeric projection works``() =
    let p1 = {x=1; y=2}
    let p2 = {x=2; y=1}
    [p1; p2] |> Seq.minBy (fun p -> p.y) |> equal p2

[<Test>]
let ``Seq.item works``() =
    let xs = [1.; 2.]
    Seq.item 1 xs
    |> equal 2.

[<Test>]
let ``Seq.tryItem works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryItem 3 xs |> equal (Some 4.)
    Seq.tryItem 4 xs |> equal None
    Seq.tryItem -1 xs |> equal None

[<Test>]
let ``Seq.ofArray works``() =
    let xs = [|1.; 2.|]
    let ys = Seq.ofArray xs
    ys |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.ofList works``() =
    let xs = [1.; 2.]
    let ys = Seq.ofList xs
    ys |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.pick works``() =
    let xs = [1.; 2.]
    xs |> Seq.pick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    |> equal 2.

[<Test>]
let ``Seq.range works``() =
    seq{1..5}
    |> Seq.reduce (+)
    |> equal 15

    seq{0..2..9}
    |> Seq.reduce (+)
    |> equal 20

    seq{1. .. 5.}
    |> Seq.reduce (+)
    |> equal 15.

    seq{0. .. 2. .. 9.}
    |> Seq.reduce (+)
    |> equal 20.

    seq{9 .. -2 .. 0}
    |> Seq.reduce (+)
    |> equal 25

    seq{'a' .. 'f'}
    |> Seq.toArray
    |> System.String
    |> equal "abcdef"

    seq{'z' .. 'a'}
    |> Seq.length
    |> equal 0

[<Test>]
let ``Seq.reduce works``() =
    let xs = [1.; 2.]
    xs |> Seq.reduce (+)
    |> equal 3.

[<Test>]
let ``Seq.scan works``() =
    let xs = [1.; 2.; 3.; 4.]
    let ys = xs |> Seq.scan (+) 0.
    sumFirstTwo ys
    |> equal 1.

[<Test>]
let ``Seq.sort works``() =
    let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
    xs |> Seq.sort |> Seq.take 3 |> Seq.sum |> equal 0.
    let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
    ys |> Seq.sort |> Seq.item 1 |> equal "a"

[<Test>]
let ``Seq.sortDescending works``() =
    let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
    xs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> equal 17.
    let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
    ys |> Seq.sortDescending |> Seq.item 1 |> equal "c"

[<Test>]
let ``Seq.sortBy works``() =
    let xs = [3.; 1.; 4.; 2.]
    let ys = xs |> Seq.sortBy (fun x -> -x)
    sumFirstTwo ys
    |> equal 7.

[<Test>]
let ``Seq.sum works``() =
    let xs = [1.; 2.]
    xs |> Seq.sum
    |> equal 3.

[<Test>]
let ``Seq.sumBy works``() =
    let xs = [1.; 2.]
    xs |> Seq.sumBy ((*) 2.)
    |> equal 6.

[<Test>]
let ``Seq.sum with non numeric types works``() =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sum |> (=) {x=3;y=30} |> equal true

[<Test>]
let ``Seq.sumBy with non numeric types works``() =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

[<Test>]
let ``Seq.sumBy with numeric projection works``() =
    let p1 = {x=1; y=10}
    let p2 = {x=2; y=20}
    [p1; p2] |> Seq.sumBy (fun p -> p.y) |> equal 30

[<Test>]
let ``Seq.skip works``() =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.skip 1
    ys |> Seq.head
    |> equal 2.

[<Test>]
let ``Seq.skip fails when there're not enough elements``() =
    let error, xs = ref false, [|1;2;3;4;5|]
    try
        Seq.skip 5 xs |> Seq.length |> equal 0
    with _ -> error := true
    equal false !error
    try
        Seq.skip 6 xs |> Seq.length |> equal 0
    with _ -> error := true
    equal true !error

[<Test>]
let ``Seq.toArray works``() =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toArray
    ys.[0] + ys.[1]
    |> equal 3.

[<Test>]
let ``Seq.toList works``() =
    let xs = [1.; 2.; 3.]
    let ys = xs |> Seq.toList
    ys.Head + ys.Tail.Head
    |> equal 3.

[<Test>]
let ``Seq.tryFind works``() =
    [1.; 2.]
    |> Seq.tryFind ((=) 1.)
    |> Option.isSome
    |> equal true
    [1.; 2.] |> Seq.tryFind ((=) 5.) |> equal None

[<Test>]
let ``Seq.tryFindIndex works``() =
    [1.; 2.]
    |> Seq.tryFindIndex ((=) 2.)
    |> Option.get
    |> equal 1
    [1.; 2.] |> Seq.tryFindIndex ((=) 5.) |> equal None

[<Test>]
let ``Seq.tryPick works``() =
    let xs = [1.; 2.]
    let r = xs |> Seq.tryPick (fun x ->
       match x with
       | 2. -> Some x
       | _ -> None)
    match r with
    | Some x -> x
    | None -> 0.
    |> equal 2.

[<Test>]
let ``Seq.zip works``() =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = Seq.zip xs ys
    let x, y = zs |> Seq.head
    x + y
    |> equal 2.

[<Test>]
let ``Seq.zip3 works``() =
    let xs = [1.; 2.; 3.]
    let ys = [1.; 2.; 3.]
    let zs = [1.; 2.; 3.]
    let ks = Seq.zip3 xs ys zs
    let x, y, z = ks |> Seq.head
    x + y + z
    |> equal 3.

let ``Seq.cache works``() =
     let count = ref 0
     let xs =
        1 |> Seq.unfold(fun i ->
           count := !count + 1
           if i <= 10 then Some(i, i+1)
           else None)
           |> Seq.cache
     xs |> Seq.length |> ignore
     xs |> Seq.length |> ignore
     !count
     |> equal 11

[<Test>]
let ``Seq.cast works``() =
    let xs = [box 1; box 2; box 3]
    let ys = Seq.cast<int> xs
    ys |> Seq.head |> equal 1

[<Test>]
let ``Seq.compareWith works``() =
    let xs = [1; 2; 3; 4]
    let ys = [1; 2; 3; 5]
    let zs = [1; 2; 3; 3]
    Seq.compareWith (fun x y -> x - y) xs xs
    |> equal 0
    Seq.compareWith (fun x y -> x - y) xs ys
    |> equal -1
    Seq.compareWith (fun x y -> x - y) xs zs
    |> equal 1

[<Test>]
let ``Seq.countBy works``() =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.countBy (fun x -> x % 2)
    ys |> Seq.length
    |> equal 2

[<Test>]
let ``Seq.distinct works``() =
    let xs = [1; 1; 1; 2; 2; 3; 3]
    let ys = xs |> Seq.distinct
    ys |> Seq.length
    |> equal 3

[<Test>]
let ``Seq.distinctBy works``() =
    [1; 1; 1; 2; 2; 3; 3]
    |> Seq.distinctBy (fun x -> x % 2)
    |> Seq.length
    |> equal 2

[<Test>]
let ``Seq.exactlyOne works``() =
    let xs = [1.]
    xs |> Seq.exactlyOne
    |> equal 1.

[<Test>]
let ``Seq.groupBy works``() =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.groupBy (fun x -> x % 2)
    ys |> Seq.length |> equal 2
    ys |> Seq.item 0 |> snd |> Seq.item 1 |> equal 3
    ys |> Seq.item 1 |> snd |> Seq.item 0 |> equal 2

type DummyUnion = Number of int

[<Test>]
let ``Seq.groupBy with structural equality works``() =
    let xs = [1; 2; 3; 4]
    let ys = xs |> Seq.groupBy (fun x -> Number (x % 2))
    ys |> Seq.length
    |> equal 2

[<Test>]
let ``Seq.initInfinite works``() =
    Seq.initInfinite (fun i -> 2. * float i)
    |> Seq.take 10
    |> Seq.sum
    |> equal 90.

[<Test>]
let ``Seq.last works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.last
    |> equal 4.

[<Test>]
let ``Seq.tryLast works``() =
    let xs = [1.; 2.; 3.; 4.]
    Seq.tryLast xs |> equal (Some 4.)
    Seq.tryLast [] |> equal None

[<Test>]
let ``Seq.pairwise works``() =
    let xs = [1; 2; 3; 4]
    xs |> Seq.pairwise
    |> Seq.map (fun (x, y) -> sprintf "%i%i" x y)
    |> String.concat ""
    |> equal "122334"

[<Test>]
let ``Seq.readonly works``() =
    let xs = [1.; 2.; 3.; 4.]
    xs |> Seq.readonly
    |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.singleton works``() =
    Seq.singleton 1.
    |> Seq.head
    |> equal 1.

[<Test>]
let ``Seq.skipWhile works``() =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.skipWhile (fun i -> i <= 3.)
    |> Seq.head
    |> equal 4.

[<Test>]
let ``Seq.take works``() =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.take 2
    |> Seq.last
    |> equal 2.
    // Seq.take should throw an exception if there're not enough elements
    try xs |> Seq.take 20 |> Seq.length with _ -> -1
    |> equal -1

[<Test>]
let ``Seq.takeWhile works``() =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.takeWhile (fun i -> i < 3.)
    |> Seq.last
    |> equal 2.

[<Test>]
let ``Seq.truncate works``() =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.truncate 2
    |> Seq.last
    |> equal 2.
    // Seq.truncate shouldn't throw an exception if there're not enough elements
    try xs |> Seq.truncate 20 |> Seq.length with _ -> -1
    |> equal 5

[<Test>]
let ``Seq.where works``() =
    let xs = [1.; 2.; 3.; 4.; 5.]
    xs |> Seq.where (fun i -> i <= 3.)
    |> Seq.length
    |> equal 3

type ExceptFoo = { Bar:string }
[<Test>]
let ``Seq.except works``() =
    Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
    Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4
    Seq.except [1] [1; 1; 1; 1] |> Seq.isEmpty |> equal true
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> Seq.isEmpty |> equal true
    Seq.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> Seq.isEmpty |> equal true
    Seq.except [(1, 2)] [(1, 2)] |> Seq.isEmpty |> equal true
    Seq.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> Seq.isEmpty |> equal true
    Seq.except [|49|] [|7; 49|] |> Seq.last|> equal 7
    Seq.except [{ Bar= "test" }] [{ Bar = "test" }] |> Seq.isEmpty |> equal true

[<Test>]
let ``Seq.item throws exception when index is out of range`` () =
    let xs = [0]
    (try Seq.item 1 xs |> ignore; false with | _ -> true) |> equal true

[<Test>]
let ``Seq iterators from range do rewind`` () =
    let xs = seq {for i=1 to 5 do yield i}
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    let xs = seq {1..5}
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
    xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

    let xs = seq {'A'..'F'}
    xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"
    xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"

[<Test>]
let ``Seq.filter doesn't blow the stack with long sequences`` () = // See #459
  let max = 1000000
  let a = [| for i in 1 .. max -> 0  |] // init with 0
  let b = a |> Seq.filter( fun x -> x > 10) |> Seq.toArray
  equal 0 b.Length
