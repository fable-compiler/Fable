module Fable.Tests.CustomOperators

open System
open Util.Testing

let (+) (x: int) (y: int) = x * y

let (-) (x: int) (y: int) = x / y

let (||||) x y = x + y

let inline (>>) x y = x * y * 2

[<Fact>]
let ``test Custom operators work`` () =
    5 + 5 |> equal 25
    10 - 2 |> equal 5
    2 |||| 2 |> equal 4

[<Fact>]
let ``test Inline custom operators work`` () =
    5 >> 5 |> equal 50
