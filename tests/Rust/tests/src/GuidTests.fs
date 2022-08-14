module Fable.Tests.GuidTests

open Util.Testing
open System

[<Fact>]
let ``Guid constructors works`` () =
    let g = new Guid()
    g.ToString() |> equal "00000000-0000-0000-0000-000000000000"
    let g2 = new Guid("a3cc8928-7148-43e2-a863-a3954aea02df")
    g2.ToString() |> equal "a3cc8928-7148-43e2-a863-a3954aea02df"

[<Fact>]
let ``Guid.Empty works`` () =
    let g = Guid.Empty
    g.ToString() |> equal "00000000-0000-0000-0000-000000000000"

[<Fact>]
let ``Guid.NewGuid creates different guids`` () =
    let g = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g = g2 |> equal false

[<Fact>]
let ``Guid.Parse works`` () =
    let s = "f46d24f0-183e-4fca-9f47-e382afdfde8b"
    let g = Guid.Parse(s)
    g.ToString() |> equal s

[<Fact>]
let ``Guid equality works`` () =
    let s = "f46d24f0-183e-4fca-9f47-e382afdfde8b"
    let g = Guid.Parse(s)
    let g2 = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    let g3 = Guid.Parse(s)
    g = g |> equal true
    g = g2 |> equal false
    g = g3 |> equal true

[<Fact>]
let ``Guid.ToString works`` () =
    let g = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    g.ToString() |> equal "a3cc8928-7148-43e2-a863-a3954aea02df"
