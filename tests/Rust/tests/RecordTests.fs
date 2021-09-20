module Fable.Tests.Record

open Util.Testing

type MyRecord = {
    a: int
    b: string
    c: float
}

[<Fact>]
let ``Record fields works`` () =
    let x = { a=1; b="2"; c=3.0 }
    x.a |> equal 1
    x.b |> equal "2"
    x.c |> equal 3.0

[<Fact>]
let ``Record structural equality works WIP`` () =
    let x = { a=1; b="2"; c=3.0 }
    let y = { a=1; b="2"; c=3.0 }
    let z = { a=3; b="4"; c=5.0 }

    (x = y) |> equal true
    //todo - the below tests immediately break because they hit the borrow checker move roles - need to look at .clone() for more complex
    // (y = z) |> equal false
    // (x = z) |> equal false
