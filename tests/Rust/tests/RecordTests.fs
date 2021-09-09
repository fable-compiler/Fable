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
