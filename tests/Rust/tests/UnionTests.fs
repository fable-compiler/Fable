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
