module Fable.Tests.TestControlFlow

open System
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

let bfn x a b=
    if x then a else b

[<Fact>]
let testIfElseFn1 () =
    bfn true 1 2 |> equal 1
[<Fact>]
let testIfElseFn2 () =
    bfn false 3 4 |> equal 4