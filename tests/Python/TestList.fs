module Fable.Tests.List

open Util.Testing


[<Fact>]
let ``test List.empty works`` () =
    let xs = List.empty<int>
    List.length xs
    |> equal 0

[<Fact>]
let ``test List.length works`` () =
    let xs = [1.; 2.; 3.; 4.]
    List.length xs
    |> equal 4

[<Fact>]
let ``test List.map works`` () =
    let xs = [1; 2; 3; 4]
    xs
    |> List.map string
    |> equal ["1"; "2"; "3"; "4"]


[<Fact>]
let ``test List.singleton works`` () =
    let xs = List.singleton 42
    xs
    |> equal [42]


[<Fact>]
let ``test List.collect works`` () =
    let xs = ["a"; "fable"; "bar" ]
    xs
    |> List.collect (fun a -> [a.Length])
    |> equal [1; 5; 3]
