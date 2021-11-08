module Fable.Tests.UnitTests

open Util.Testing

[<Measure>] type m
[<Measure>] type s

[<Fact>]
let ``Units should work`` () =
    let a = 4<m>
    let b = 2<s>

    let c = a / b
    c |> equal (2<m/s>)
