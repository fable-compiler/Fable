module Fable.Tests.Math

open Util.Testing


[<Fact>]
let ``test power works`` () =
    let x = 10.0 ** 2.
    x
    |> equal 100.0
