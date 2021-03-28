module Fable.Tests.Seqs

open Util.Testing


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
