module Fable.Tests.ControlFlowTests

open Util.Testing

[<Fact>]
let testIfElse () =
    let r =
        if true then 4 else 6
    r |> equal 4

[<Fact>]
let testIfElse2 () =
    let r =
        if false then 4 else 6
    r |> equal 6

let bfn x (a: int) (b: int) =
    if x then a else b

[<Fact>]
let testIfElseFn1 () =
    bfn true 1 2 |> equal 1

[<Fact>]
let testIfElseFn2 () =
    bfn false 3 4 |> equal 4

[<Fact>]
let testIfElseIf () =
    let a x =
        if x = 1 then
            1
        else if x = 2 then
            2
        else 3
    a 1 |> equal 1
    a 2 |> equal 2
    a 3 |> equal 3

[<Fact>]
let testCurriedApply () =
    let mul a b = a * b
    let mul2 x = mul 2 x // todo let mul2 = mul 2 // partial apply calls util.PartialApply which is not yet working (module loading)
    mul 3 2 |> equal 6
    mul2 4 |> equal 8

[<Fact>]
let testForEach1 () =
    let mutable a = 42
    for i in 0..5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let testForEach2 () =
    let mutable a = 42
    for i = 0 to 5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let testForEach3 () =
    let mutable a = 42
    for i = 5 downto 0 do
        a <- i + a
    a |> equal 57
