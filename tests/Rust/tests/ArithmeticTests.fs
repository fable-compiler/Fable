module Fable.Tests.Arithmetic

open Util.Testing

[<Fact>]
let testTwoPlusTwo () =
    2 + 2 |> equal 4

[<Fact>]
let testMinus () =
    7 - 2 |> equal 5

[<Fact>]
let testMultiply () =
    3 * 2 |> equal 6

[<Fact>]
let testDivide () =
    10 / 2 |> equal 5

[<Fact>]
let testFloatAdd () =
    3.141 + 2.85 |> equal 5.991

let private addFn a b = a + b

[<Fact>]
let testAddThroughTrivialFn () =
    addFn 2 2 |> equal 4

[<Fact>]
let testLocalsWithFcalls () =
    let a = addFn 1 0
    let b = 2
    let c = addFn 3 0
    a + b + c |> equal 6

[<Fact>]
let testLocalFunction () =
    let locAdd1 a =
        addFn 1 a
    locAdd1 2 |> equal 3

[<Fact>]
let testInlineLambda () =
    1 |> fun x -> x + 1 |> fun x -> x - 3 |> equal (-1)

// let add42 = addFn 42
// [<Fact>]
// let testPartialApply () =
//     add42 3 |> equal 45
