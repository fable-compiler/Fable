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
let testForEach1 () =
    let mutable a = 42
    for i in 0..5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let testWhile1 () =
    let mutable a = 1
    while a < 3 do
        a <- a + 1
    a |> equal 3

[<Fact>]
let testExHappy() =
    let a =
        try
            3
        with ex ->
            4
    a |> equal 3

[<Fact>]
let testExThrow() =
    let a =
        try
            failwith "boom"
            3
        with ex ->
            4
    a |> equal 4


[<Fact>]
let testSimpleFnParam() =
    let add a b = a + b
    let fn addFn a b = addFn a b
    fn (add) 3 2 |> equal 5

[<Fact>]
let testPartialApply() =
    let add a b = a + b
    let fn addFn a b = addFn a b
    fn (add) 3 2 |> equal 5
    let fnAdd4 = fn (add) 4
    fnAdd4 5 |> equal 9
    fnAdd4 2 |> equal 6