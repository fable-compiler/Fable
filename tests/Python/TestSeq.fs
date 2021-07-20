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
