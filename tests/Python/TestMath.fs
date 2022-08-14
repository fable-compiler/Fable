module Fable.Tests.Math

open System
open Util.Testing


[<Fact>]
let ``test power works`` () =
    let x = 10.0 ** 2.
    x |> equal 100.0

[<Fact>]
let ``test extremas work`` () =
    0.0 / 0.0 |> Double.IsNaN |> equal true
    0.0 / (-0.0) |> Double.IsNaN |> equal true
    1.0 / infinity |> equal 0
    1.0 / (-infinity) |> equal 0
    1.0 / 0.0 |> Double.IsInfinity |> equal true

    1.0 / (-0.0)
    |> Double.IsNegativeInfinity
    |> equal true

    -1.0 / (-0.0)
    |> Double.IsNegativeInfinity
    |> equal false

    -infinity < infinity |> equal true
    (-0.0) < 0.0 |> equal false
