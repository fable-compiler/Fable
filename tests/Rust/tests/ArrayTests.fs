module Fable.Tests.ArrayTests

open Util.Testing

[<Fact>]
let ``Array expressions works`` () =
    let a1: int[] = [||]
    a1 |> equal [||]
    a1.Length |> equal 0
    let a2 = [|1;2;3|]
    a2 |> equal [|1;2;3|]
    a2.Length |> equal 3

[<Fact>]
let ``Array equality works`` () =
    let a1 = [|1;2;3|]
    let a2 = [|1;2;3|]
    let a3 = [|1;2;1|]
    let a4 = [|1;2|]
    a1 = a1 |> equal true
    a1 = a2 |> equal true
    a1 = a3 |> equal false
    a1 = a4 |> equal false

[<Fact>]
let ``Array get element works`` () =
    let arr = [|1;2;3|]
    arr.[0] |> equal 1
    arr.[1] |> equal 2
    arr.[2] |> equal 3

[<Fact>]
let ``Array set element works`` () =
    let arr = [|1;2;3|]
    arr.[1] <- arr.[2] + 1
    arr |> equal [|1;4;3|]

let inc_elem2 (a: _[]) i =
    a.[i] <- a.[i] + 1
    a

[<Fact>]
let ``Array pass by reference works`` () =
    let inc_elem (a: _[]) i =
        a.[i] <- a.[i] + 1
        a
    let arr = [|1;2;3|]
    inc_elem arr 2 |> equal [|1;2;4|]
    arr |> equal [|1;2;4|]
    let _ = inc_elem arr 1
    arr |> equal [|1;3;4|]
    let _ = inc_elem2 arr 0
    arr |> equal [|2;3;4|]

// [<Fact>]
// let ``Array.create works`` () =
//     let arr = 2 |> Array.create 3
//     arr |> equal [|2;2;2|]

// [<Fact>]
// let ``Array.rev works`` () =
//     let arr = [|1;2;3|] |> Array.rev
//     arr |> equal [|3;2;1|]

// [<Fact>]
// let ``Array.copy works`` () =
//     let xs = [|1;2;3|]
//     let ys = xs |> Array.copy
//     xs.[1] <- xs.[1] + 1
//     ys |> equal [|1;2;3|]
