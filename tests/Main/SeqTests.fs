module Fable.Tests.Seqs

open Util.Testing
open Fable.Tests.Util

let sumFirstTwo (zs: seq<float>) =
   let first = Seq.head zs
   let second = Seq.skip 1 zs |> Seq.head
   first + second

let testSeqChoose xss =
    let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
    xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

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

let tests =
  testList "Seqs" [
    testCase "Can test untyped enumerables" <| fun () ->
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

    testCase "Seq.length works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.length xs
        |> equal 4

    testCase "Seq.delay works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = Seq.delay (fun () -> xs :> _ seq)
        ys |> Seq.head
        |> equal 1.

    testCase "Seq.unfold works" <| fun () ->
        1 |> Seq.unfold (fun x ->
           if x <= 5 then Some(x, x + 1)
           else None)
        |> Seq.length
        |> equal 5

    testCase "Seq.empty works" <| fun () ->
        let xs = Seq.empty<int>
        Seq.length xs
        |> equal 0

    testCase "Seq.append works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = [0.]
        let zs = Seq.append ys xs
        sumFirstTwo zs
        |> equal 1.

    testCase "Seq.average works" <| fun () ->
        let xs = seq {yield 1.; yield 2.; yield 3.; yield 4.}
        Seq.average xs
        |> equal 2.5

    testCase "Seq.averageBy works" <| fun () ->
        let xs = seq {yield 1.; yield 2.; yield 3.; yield 4.}
        Seq.averageBy ((*) 2.) xs
        |> equal 5.

    testCase "Seq.average works with custom types" <| fun () ->
        seq {yield MyNumber 1; yield MyNumber 2; yield MyNumber 3}
        |> Seq.average |> equal (MyNumber 2)

    testCase "Seq.averageBy works with custom types" <| fun () ->
        seq {yield { MyNumber = MyNumber 5 }; yield { MyNumber = MyNumber 4 }; yield { MyNumber = MyNumber 3 }}
        |> Seq.averageBy (fun x -> x.MyNumber) |> equal (MyNumber 4)

    testCase "Seq.choose works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let zs = xs |> Seq.choose (fun x ->
           if x > 2. then Some x
           else None)
        sumFirstTwo zs
        |> equal 7.

    testCase "Seq.choose works with generic arguments" <| fun () ->
        let res = testSeqChoose  [ Some [  5  ] ]
        equal [ 5 ] res

    testCase "Seq.concat works" <| fun () ->
        let xs = [[1.]; [2.]; [3.]; [4.]]
        let ys = xs |> Seq.concat
        sumFirstTwo ys
        |> equal 3.

    testCase "Seq.collect works" <| fun () ->
        let xs = [[1.]; [2.]; [3.]; [4.]]
        let ys = xs |> Seq.collect id
        sumFirstTwo ys
        |> equal 3.

        let xs1 = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
        let ys1 = xs1 |> Seq.collect id
        sumFirstSeq ys1 5
        |> equal 15.

    testCase "Seq.collect works with Options" <| fun () ->
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

    testCase "Seq.chunkBySize works" <| fun () ->
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

    testCase "Seq.exists works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.exists (fun x -> x = 2.)
        |> equal true

    testCase "Seq.exists2 works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = [1.; 2.; 3.; 4.]
        Seq.exists2 (fun x y -> x * y = 16.) xs ys
        |> equal true

    testCase "Seq.filter works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = xs |> Seq.filter (fun x -> x > 5.)
        ys |> Seq.isEmpty
        |> equal true

    testCase "Seq.find works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.find ((=) 2.)
        |> equal 2.

    testCase "Seq.findIndex works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.findIndex ((=) 2.)
        |> equal 1

    testCase "Seq.findBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.find ((>) 4.) |> equal 1.
        xs |> Seq.findBack ((>) 4.) |> equal 3.

    testCase "Seq.findBack with option works" <| fun () ->
        let xs = [Some 1.; None]
        xs |> Seq.find Option.isSome |> equal (Some(1.))
        xs |> Seq.find Option.isSome |> (fun v -> v.Value) |> equal 1.
        xs |> Seq.findBack Option.isNone |> equal None

    testCase "Seq.findIndexBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.findIndex ((>) 4.) |> equal 0
        xs |> Seq.findIndexBack ((>) 4.) |> equal 2

    testCase "Seq.tryFindBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.tryFind ((>) 4.) |> equal (Some 1.)
        xs |> Seq.tryFindBack ((>) 4.) |> equal (Some 3.)
        xs |> Seq.tryFindBack ((=) 5.) |> equal None

    testCase "Seq.tryFindBack with option works" <| fun () ->
        let xs = [Some 1.; None]
        xs |> Seq.tryFind Option.isSome |> equal (Some(Some 1.))
        xs |> Seq.tryFind Option.isSome |> (fun v -> v.Value.Value) |> equal 1.
        xs |> Seq.tryFindBack Option.isNone |> equal (Some None)
        xs |> Seq.tryFindBack Option.isNone |> (fun v -> v.Value) |>  equal None
        xs |> Seq.tryFindBack (fun v -> match v with Some v -> v = 2. | _ -> false) |> equal None

    testCase "Seq.tryFindIndexBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.tryFindIndex ((>) 4.) |> equal (Some 0)
        xs |> Seq.tryFindIndexBack ((>) 4.) |> equal (Some 2)
        xs |> Seq.tryFindIndexBack ((=) 5.) |> equal None

    testCase "Seq.fold works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let total = xs |> Seq.fold (+) 0.
        total |> equal 10.

    testCase "Seq.fold with tupled arguments works" <| fun () ->
        let a, b =
            ((1, 5), [1;2;3;4])
            ||> Seq.fold (fun (a, b) i ->
                a * i, b + i)
        equal 24 a
        equal 15 b

    testCase "Seq.forall works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.forall (fun x -> x < 5.) xs
        |> equal true

    testCase "Seq.forall2 works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = [1.; 2.; 3.; 4.]
        Seq.forall2 (=) xs ys
        |> equal true

    testCase "Seq.forall is lazy" <| fun () -> // See #1715
        let mutable x = ""
        let one() = x <- "one"; false
        let two() = x <- "two"; true
        let ok =
            [one; two]
            |> Seq.map (fun c -> c())
            |> Seq.forall id
        ok |> equal false
        x |> equal "one"

    testCase "Seq.head works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.head xs
        |> equal 1.

    testCase "Seq.head with option works" <| fun () ->
        let xs = [Some 1.; None]
        Seq.head xs |> equal (Some 1.)
        Seq.head xs |> (fun v -> v.Value) |> equal 1.
        let xs = [None; Some 1.]
        Seq.head xs |> equal None

    testCase "Seq.tryHead works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.tryHead xs |> equal (Some 1.)
        Seq.tryHead [] |> equal None

    testCase "Seq.tryHead with option works" <| fun () ->
        let xs = [Some 1.; None]
        Seq.tryHead xs |> equal (Some(Some 1.))
        Seq.tryHead xs |> (fun v -> v.Value.Value) |> equal 1.
        let xs = [None; Some 1.]
        Seq.tryHead xs |> equal (Some(None))
        Seq.tryHead xs |> (fun v -> v.Value) |> equal None

    testCase "Seq.tail works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.tail xs |> Seq.length |> equal 3

    testCase "Seq.init works" <| fun () ->
        let xs = Seq.init 4 float
        sumFirstTwo xs
        |> equal 1.

    testCase "Seq.isEmpty works" <| fun () ->
        let xs = [1]
        Seq.isEmpty xs
        |> equal false

    testCase "Seq.iter works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let total = ref 0.
        xs |> Seq.iter (fun x ->
           total := !total + x
        )
        !total |> equal 10.

    testCase "Seq.iter2 works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let total = ref 0.
        Seq.iter2 (fun x y ->
           total := !total + x + y
        ) xs xs
        !total |> equal 20.

    testCase "Seq.iteri works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let total = ref 0.
        xs |> Seq.iteri (fun i x ->
           total := !total + (float i) * x
        )
        !total |> equal 20.

    testCase "Seq.map works" <| fun () ->
        let xs = [1.]
        let ys = xs |> Seq.map ((*) 2.)
        ys |> Seq.head
        |> equal 2.

    testCase "Seq.map2 works" <| fun () ->
        let xs = [1.]
        let ys = [2.]
        let zs = Seq.map2 (*) xs ys
        zs |> Seq.head
        |> equal 2.

    testCase "Seq.mapi works" <| fun () ->
        let xs = [1.]
        let ys = xs |> Seq.mapi (fun i x -> float i + x)
        ys |> Seq.head
        |> equal 1.

    testCase "Seq.indexed works" <| fun () ->
        let xs = seq { yield "a"; yield "b"; yield "c" } |> Seq.indexed
        let x = xs |> Seq.tail |> Seq.head
        fst x |> equal 1
        snd x |> equal "b"

    testCase "Seq.mapFold works" <| fun () ->
        let xs = [1y; 2y; 3y; 4y]
        let result = xs |> Seq.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
        fst result |> Seq.sum |> equal 20y
        snd result |> equal 10y

    testCase "Seq.mapFoldBack works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let result = Seq.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
        fst result |> Seq.sum |> equal -20.
        snd result |> equal -10.

    testCase "Seq.max works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.max
        |> equal 2.

    testCase "Seq.maxBy works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.maxBy (fun x -> -x)
        |> equal 1.

    testCase "Seq.min works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.min
        |> equal 1.

    testCase "Seq.minBy works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.minBy (fun x -> -x)
        |> equal 2.

    testCase "Seq.max with non numeric types works" <| fun () ->
        let p1 = {x=1; y=1}
        let p2 = {x=2; y=2}
        [p1; p2] |> Seq.max |> equal p2

    testCase "Seq.maxBy with non numeric types works" <| fun () ->
        let p1 = {x=1; y=1}
        let p2 = {x=2; y=2}
        [p1; p2] |> Seq.maxBy Point.Neg |> equal p1

    testCase "Seq.min with non numeric types works" <| fun () ->
        let p1 = {x=1; y=1}
        let p2 = {x=2; y=2}
        [p1; p2] |> Seq.min |> equal p1

    testCase "Seq.minBy with non numeric types works" <| fun () ->
        let p1 = {x=1; y=1}
        let p2 = {x=2; y=2}
        [p1; p2] |> Seq.minBy Point.Neg |> equal p2

    testCase "Seq.maxBy with numeric projection works" <| fun () ->
        let p1 = {x=1; y=2}
        let p2 = {x=2; y=1}
        [p1; p2] |> Seq.maxBy (fun p -> p.y) |> equal p1

    testCase "Seq.minBy with numeric projection works" <| fun () ->
        let p1 = {x=1; y=2}
        let p2 = {x=2; y=1}
        [p1; p2] |> Seq.minBy (fun p -> p.y) |> equal p2

    testCase "Seq.sum works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.sum
        |> equal 3.

    testCase "Seq.sumBy works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.sumBy ((*) 2.)
        |> equal 6.

    testCase "Seq.sum with non numeric types works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [p1; p2] |> Seq.sum |> (=) {x=3;y=30} |> equal true

    testCase "Seq.sumBy with non numeric types works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [p1; p2] |> Seq.sumBy Point.Neg |> (=) {x = -3; y = -30} |> equal true

    testCase "Seq.sumBy with numeric projection works" <| fun () ->
        let p1 = {x=1; y=10}
        let p2 = {x=2; y=20}
        [p1; p2] |> Seq.sumBy (fun p -> p.y) |> equal 30

    testCase "Seq.sum with non numeric types works II" <| fun () ->
        seq {yield MyNumber 1; yield MyNumber 2; yield MyNumber 3}
        |> Seq.sum |> equal (MyNumber 6)

    testCase "Seq.sumBy with non numeric types works II" <| fun () ->
        seq {yield { MyNumber = MyNumber 5 }; yield { MyNumber = MyNumber 4 }; yield { MyNumber = MyNumber 3 }}
        |> Seq.sumBy (fun x -> x.MyNumber) |> equal (MyNumber 12)

    testCase "Seq.item works" <| fun () ->
        let xs = [1.; 2.]
        Seq.item 1 xs
        |> equal 2.

    testCase "Seq.tryItem works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.tryItem 3 xs |> equal (Some 4.)
        Seq.tryItem 4 xs |> equal None
        Seq.tryItem -1 xs |> equal None

    testCase "Seq.ofArray works" <| fun () ->
        let xs = [|1.; 2.|]
        let ys = Seq.ofArray xs
        ys |> Seq.head
        |> equal 1.

    testCase "Seq.ofList works" <| fun () ->
        let xs = [1.; 2.]
        let ys = Seq.ofList xs
        ys |> Seq.head
        |> equal 1.

    testCase "Seq.pick works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.pick (fun x ->
           match x with
           | 2. -> Some x
           | _ -> None)
        |> equal 2.

    testCase "Seq.range works" <| fun () ->
        seq{1..5}
        |> Seq.reduce (+)
        |> equal 15

        seq{1. .. 5.}
        |> Seq.reduce (+)
        |> equal 15.

    testCase "Seq.range step works" <| fun () ->
        seq{0..2..9}
        |> Seq.reduce (+)
        |> equal 20

        seq{0. .. 2. .. 9.}
        |> Seq.reduce (+)
        |> equal 20.

        seq{9 .. -2 .. 0}
        |> Seq.reduce (+)
        |> equal 25

    testCase "Seq.range works with chars" <| fun () ->
        seq{'a' .. 'f'}
        |> Seq.toArray
        |> System.String
        |> equal "abcdef"

        seq{'z' .. 'a'}
        |> Seq.length
        |> equal 0

    testCase "Seq.range works with long" <| fun () ->
        seq{1L..5L}
        |> Seq.reduce (+)
        |> equal 15L

        seq{1UL .. 5UL}
        |> Seq.reduce (+)
        |> equal 15UL

    testCase "Seq.range step works with long" <| fun () ->
        seq{0L..2L..9L}
        |> Seq.reduce (+)
        |> equal 20L

        seq{0UL..2UL..9UL}
        |> Seq.reduce (+)
        |> equal 20UL

    testCase "Seq.range works with decimal" <| fun () ->
        seq{1M .. 50M}
        |> Seq.reduce (+)
        |> equal 1275M

    testCase "Seq.range step works with decimal" <| fun () ->
        seq {-3M .. -0.4359698987M .. -50M}
        |> Seq.reduce (+)
        |> equal -2843.0340746886M

    testCase "Seq.range works with bigint" <| fun () ->
        seq{1I..2000I}
        |> Seq.reduce (+)
        |> equal 2001000I

    testCase "Seq.range step works with bigint" <| fun () ->
        seq {1I .. 10000000000000I .. 20000000000000000I}
        |> Seq.reduce (+)
        |> equal 19990000000000002000I

    testCase "Seq.reduce works" <| fun () ->
        let xs = [1.; 2.]
        xs |> Seq.reduce (+)
        |> equal 3.

    testCase "Seq.scan works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        let ys = xs |> Seq.scan (+) 0.
        sumFirstTwo ys
        |> equal 1.

    testCase "Seq.sort works" <| fun () ->
        let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
        xs |> Seq.sort |> Seq.take 3 |> Seq.sum |> equal 0.
        let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
        ys |> Seq.sort |> Seq.item 1 |> equal "a"

    testCase "Seq.sortDescending works" <| fun () ->
        let xs = [3.; 4.; 1.; -3.; 2.; 10.] |> List.toSeq
        xs |> Seq.sortDescending |> Seq.take 3 |> Seq.sum |> equal 17.
        let ys = ["a"; "c"; "B"; "d"] |> List.toSeq
        ys |> Seq.sortDescending |> Seq.item 1 |> equal "c"

    testCase "Seq.sortBy works" <| fun () ->
        let xs = [3.; 1.; 4.; 2.]
        let ys = xs |> Seq.sortBy (fun x -> -x)
        sumFirstTwo ys
        |> equal 7.

    testCase "Seq.skip works" <| fun () ->
        let xs = [1.; 2.; 3.]
        let ys = xs |> Seq.skip 1
        ys |> Seq.head
        |> equal 2.

    testCase "Seq.skip fails when there're not enough elements" <| fun () ->
        let error, xs = ref false, [|1;2;3;4;5|]
        try
            Seq.skip 5 xs |> Seq.length |> equal 0
        with _ -> error := true
        equal false !error
        try
            Seq.skip 6 xs |> Seq.length |> equal 0
        with _ -> error := true
        equal true !error

    testCase "Seq.toArray works" <| fun () ->
        let xs = [1.; 2.; 3.]
        let ys = xs |> Seq.toArray
        ys.[0] + ys.[1]
        |> equal 3.

    testCase "Seq.toList works" <| fun () ->
        let xs = [1.; 2.; 3.]
        let ys = xs |> Seq.toList
        ys.Head + ys.Tail.Head
        |> equal 3.

    testCase "Seq.tryFind works" <| fun () ->
        [1.; 2.]
        |> Seq.tryFind ((=) 1.)
        |> Option.isSome
        |> equal true
        [1.; 2.] |> Seq.tryFind ((=) 5.) |> equal None

    testCase "Seq.tryFindIndex works" <| fun () ->
        [1.; 2.]
        |> Seq.tryFindIndex ((=) 2.)
        |> Option.get
        |> equal 1
        [1.; 2.] |> Seq.tryFindIndex ((=) 5.) |> equal None

    testCase "Seq.tryPick works" <| fun () ->
        let xs = [1.; 2.]
        let r = xs |> Seq.tryPick (fun x ->
           match x with
           | 2. -> Some x
           | _ -> None)
        match r with
        | Some x -> x
        | None -> 0.
        |> equal 2.

    testCase "Seq.zip works" <| fun () ->
        let xs = [1.; 2.; 3.]
        let ys = [1.; 2.; 3.]
        let zs = Seq.zip xs ys
        let x, y = zs |> Seq.head
        x + y
        |> equal 2.

    testCase "Seq.zip3 works" <| fun () ->
        let xs = [1.; 2.; 3.]
        let ys = [1.; 2.; 3.]
        let zs = [1.; 2.; 3.]
        let ks = Seq.zip3 xs ys zs
        let x, y, z = ks |> Seq.head
        x + y + z
        |> equal 3.

    testCase "Seq.cache works" <| fun () ->
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

    testCase "Seq.cast works" <| fun () ->
        let xs = [box 1; box 2; box 3]
        let ys = Seq.cast<int> xs
        ys |> Seq.head |> equal 1

    testCase "Seq.compareWith works" <| fun () ->
        let xs = [1; 2; 3; 4]
        let ys = [1; 2; 3; 5]
        let zs = [1; 2; 3; 3]
        Seq.compareWith (-) xs xs |> equal 0
        Seq.compareWith (-) xs ys |> equal -1
        Seq.compareWith (-) xs zs |> equal 1

    testCase "Seq.countBy works" <| fun () ->
        let xs = [1; 2; 3; 4]
        let ys = xs |> Seq.countBy (fun x -> x % 2)
        ys |> Seq.length |> equal 2

    testCase "Seq.distinct works" <| fun () ->
        let xs = [1; 1; 1; 2; 2; 3; 3]
        let ys = xs |> Seq.distinct
        ys |> Seq.length |> equal 3
        ys |> Seq.sum |> equal 6

    testCase "Seq.distinct with tuples works" <| fun () ->
        let xs = [(1, 2); (2, 3); (1, 2)]
        let ys = xs |> Seq.distinct
        ys |> Seq.length |> equal 2
        ys |> Seq.sumBy fst |> equal 3

    testCase "Seq.distinctBy works" <| fun () ->
        let xs = [4; 4; 4; 6; 6; 5; 5]
        let ys = xs |> Seq.distinctBy (fun x -> x % 2)
        ys |> Seq.length |> equal 2
        ys |> Seq.head >= 4 |> equal true

    testCase "Seq.distinctBy with tuples works" <| fun () ->
        let xs = [4,1; 4,2; 4,3; 6,4; 6,5; 5,6; 5,7]
        let ys = xs |> Seq.distinctBy (fun (x,_) -> x % 2)
        ys |> Seq.length |> equal 2
        ys |> Seq.head |> fst >= 4 |> equal true

    testCase "Seq.distinct works on infinite sequences" <| fun () ->
        let rec numbersFrom n =
          seq { yield n; yield n; yield! numbersFrom (n + 1) }
        let xs =
            numbersFrom 1
            |> Seq.distinct
            |> Seq.take 5
        xs |> Seq.toList |> equal [1; 2; 3; 4; 5]

    testCase "Seq.distinctBy works on infinite sequences" <| fun () ->
        let rec numbersFrom n =
          seq { yield n; yield n; yield! numbersFrom (n + 1) }
        let xs =
            numbersFrom 1
            |> Seq.distinctBy (fun x -> x / 5)
            |> Seq.take 5
        xs |> Seq.toList |> equal [1; 5; 10; 15; 20]

    testCase "Seq.groupBy works" <| fun () ->
        let xs = [1; 2; 3; 4]
        let ys = xs |> Seq.groupBy (fun x -> x % 2)
        ys |> Seq.length |> equal 2
        ys |> Seq.iter (fun (k,v) ->
            v |> Seq.exists (fun x -> x % 2 <> k) |> equal false)

    testCase "Seq.groupBy with structural equality works" <| fun () ->
        let xs = [1; 2; 3; 4]
        let ys = xs |> Seq.groupBy (fun x -> Number (x % 2))
        ys |> Seq.length |> equal 2

    testCase "Seq.exactlyOne works" <| fun () ->
        let xs = [1.]
        xs |> Seq.exactlyOne
        |> equal 1.

    testCase "Seq.tryExactlyOne works" <| fun () ->
            seq {yield 1.} |> Seq.tryExactlyOne |> equal (Some 1.)
            seq {yield 1.; yield 2.} |> Seq.tryExactlyOne |> equal None
            Seq.empty<float> |> Seq.tryExactlyOne |> equal None

    testCase "Seq.initInfinite works" <| fun () ->
        Seq.initInfinite (fun i -> 2. * float i)
        |> Seq.take 10
        |> Seq.sum
        |> equal 90.

    testCase "Seq.last works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.last
        |> equal 4.

    testCase "Seq.tryLast works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        Seq.tryLast xs |> equal (Some 4.)
        Seq.tryLast [] |> equal None

    testCase "Seq.pairwise works" <| fun () ->
        let xs = [1; 2; 3; 4]
        xs |> Seq.pairwise
        |> Seq.map (fun (x, y) -> sprintf "%i%i" x y)
        |> String.concat ""
        |> equal "122334"

    testCase "Seq.readonly works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.]
        xs |> Seq.readonly
        |> Seq.head
        |> equal 1.

    testCase "Seq.singleton works" <| fun () ->
        let xs = Seq.singleton 1.
        Seq.head xs |> equal 1.
        Seq.head xs |> equal 1.

    testCase "Seq.singleton works with None" <| fun () ->
        let xs : int option seq = Seq.singleton None
        xs
        |> Seq.length
        |> equal 1

    testCase "Seq.skipWhile works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> Seq.skipWhile (fun i -> i <= 3.)
        |> Seq.head
        |> equal 4.

    testCase "Seq.take works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> Seq.take 2
        |> Seq.last
        |> equal 2.
        // Seq.take should throw an exception if there're not enough elements
        try xs |> Seq.take 20 |> Seq.length with _ -> -1
        |> equal -1

    testCase "Seq.takeWhile works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> Seq.takeWhile (fun i -> i < 3.)
        |> Seq.last
        |> equal 2.

    testCase "Seq.truncate works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> Seq.truncate 2
        |> Seq.last
        |> equal 2.
        // Seq.truncate shouldn't throw an exception if there're not enough elements
        try xs |> Seq.truncate 20 |> Seq.length with _ -> -1
        |> equal 5

    testCase "Seq.where works" <| fun () ->
        let xs = [1.; 2.; 3.; 4.; 5.]
        xs |> Seq.where (fun i -> i <= 3.)
        |> Seq.length
        |> equal 3

    testCase "Seq.except works" <| fun () ->
        Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
        Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4
        Seq.except [1] [1; 1; 1; 1] |> Seq.isEmpty |> equal true
        Seq.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> Seq.isEmpty |> equal true
        Seq.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> Seq.isEmpty |> equal true
        Seq.except [(1, 2)] [(1, 2)] |> Seq.isEmpty |> equal true
        Seq.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> Seq.isEmpty |> equal true
        Seq.except [|49|] [|7; 49|] |> Seq.last|> equal 7
        Seq.except [{ Bar= "test" }] [{ Bar = "test" }] |> Seq.isEmpty |> equal true

    testCase "Seq.item throws exception when index is out of range" <| fun () ->
        let xs = [0]
        (try Seq.item 1 xs |> ignore; false with | _ -> true) |> equal true

    testCase "Seq iterators from range do rewind" <| fun () ->
        let xs = seq {for i=1 to 5 do yield i}
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

        let xs = seq {1..5}
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"
        xs |> Seq.map string |> String.concat "," |> equal "1,2,3,4,5"

        let xs = seq {'A'..'F'}
        xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"
        xs |> Seq.map string |> String.concat "," |> equal "A,B,C,D,E,F"

    testCase "Seq.filter doesn't blow the stack with long sequences" <| fun () -> // See #459
      let max = 1000000
      let a = [| for i in 1 .. max -> 0  |] // init with 0
      let b = a |> Seq.filter( fun x -> x > 10) |> Seq.toArray
      equal 0 b.Length

    testCase "Seq.windowed works" <| fun () -> // See #1716
        let nums = [ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ] :> _ seq
        Seq.windowed 3 nums |> Seq.toArray |> equal [|[|1.0; 1.5; 2.0|]; [|1.5; 2.0; 1.5|]; [|2.0; 1.5; 1.0|]; [|1.5; 1.0; 1.5|]|]
        Seq.windowed 5 nums |> Seq.toArray |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0 |]; [| 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
        Seq.windowed 6 nums |> Seq.toArray |> equal [|[| 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 |]|]
        Seq.windowed 7 nums |> Seq.isEmpty |> equal true

    testCase "Seq.allPairs works" <| fun () ->
        let mutable accX = 0
        let mutable accY = 0
        let xs = seq { accX <- accX + 1; for i = 1 to 4 do yield i }
        let ys = seq { accY <- accY + 1; for i = 'a' to 'f' do yield i }
        let res = Seq.allPairs xs ys
        let res1 = List.ofSeq res
        let res2 = List.ofSeq res
        let expected =
            [(1, 'a'); (1, 'b'); (1, 'c'); (1, 'd'); (1, 'e'); (1, 'f'); (2, 'a');
             (2, 'b'); (2, 'c'); (2, 'd'); (2, 'e'); (2, 'f'); (3, 'a'); (3, 'b');
             (3, 'c'); (3, 'd'); (3, 'e'); (3, 'f'); (4, 'a'); (4, 'b'); (4, 'c');
             (4, 'd'); (4, 'e'); (4, 'f')]
        accX |> equal 2
        accY |> equal 1
        equal expected res1
        equal expected res2
  ]