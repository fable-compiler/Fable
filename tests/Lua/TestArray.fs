module Fable.Tests.Array

open System
open Util.Testing

[<Fact>]
let testCreateArray () =
    let arr = [||]
    arr |> equal [||]

[<Fact>]
let testCreateArray2 () =
    let arr = [|1;2;3|]
    arr |> equal [|1;2;3|]

[<Fact>]
let testDeref1 () =
    let arr = [|1;2;3|]
    arr.[0] |> equal 1
    arr.[1] |> equal 2
    arr.[2] |> equal 3

// [<Fact>]
// let testDestructure () =
//     let x = [|1;2|]
//     let res =
//         match x with
//         | [|a; b|] ->
//             Some (a, b)
//         | _ -> None
//     res |> equal (Some(1, 2))