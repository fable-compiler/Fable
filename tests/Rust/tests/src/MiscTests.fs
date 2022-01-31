module Fable.Tests.MiscTests

open Util.Testing

let inc1 (x: byref<int>) =
    x <- x + 1

let inc2 x =
    x + 1

let mx = 3
let mutable my = 4

[<Fact>]
let ``byref works`` () =
    let mutable x = 5
    inc1 &x
    let y = inc2 x
    x |> equal 6
    y |> equal 7

[<Fact>]
let ``Module let bindings work`` () =
    my <- my + 1
    let z = mx + my
    z |> equal 8
