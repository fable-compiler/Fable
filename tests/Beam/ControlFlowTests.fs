module Fable.Tests.ControlFlow

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test if_then_else works`` () =
    let r = if true then 4 else 6
    r |> equal 4
    let r = if false then 4 else 6
    r |> equal 6

let bfn x (a: int) (b: int) =
    if x then a else b

[<Fact>]
let ``test if_then_else works II`` () =
    bfn true 1 2 |> equal 1
    bfn false 3 4 |> equal 4

[<Fact>]
let ``test Nested if_then_else works`` () =
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
let ``test Curried apply works`` () =
    let mul a b = a * b
    let mul2 x = mul 2 x
    mul 3 2 |> equal 6
    mul2 4 |> equal 8

[<Fact>]
let ``test for_in loop works`` () =
    let mutable a = 42
    for i in 0..5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``test for_to loop works`` () =
    let mutable a = 42
    for i = 0 to 5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``test for_downto loop works`` () =
    let mutable a = 42
    for i = 5 downto 0 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``test while loop works`` () =
    let mutable i = 0
    let mutable total = 0
    while i < 10 do
        i <- i + 1
        total <- total + i
    total |> equal 55
