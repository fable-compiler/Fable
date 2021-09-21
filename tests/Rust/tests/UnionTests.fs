module Fable.Tests.Union

open Util.Testing

type MyUnion =
    | Case1 of int
    | Case2 of int
    | Case3 of int

[<Fact>]
let ``Union case matching works`` () =
    let x = Case1 5
    let res =
        match x with
        | Case1 a -> a
        | Case2 b -> b
        | Case3 c -> c
    res |> equal 5

[<Fact>]
let ``Union case equality works`` () =
    Case1 5 = Case1 5 |> equal true
    Case1 5 = Case2 5 |> equal false
    Case3 2 = Case3 3 |> equal false
    Case2 1 = Case2 1 |> equal true
    Case3 1 = Case3 1 |> equal true
