module Fable.Tests.Array

open Util.Testing

[<Fact>]
let testCreateArray () =
    let arr: int[] = [||]
    arr |> equal [||]

[<Fact>]
let testCreateArray2 () =
    let arr = [|1;2;3|]
    arr |> equal [|1;2;3|]

[<Fact>]
let testGetElement () =
    let arr = [|1;2;3|]
    arr.[0] |> equal 1
    arr.[1] |> equal 2
    arr.[2] |> equal 3

[<Fact>]
let testSetElement () =
    let arr = [|1;0;3|]
    arr.[1] <- 2
    arr |> equal [|1;2;3|]
