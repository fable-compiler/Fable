module Fable.Tests.GuidTests

open Fable.Core
open Util.Testing

open System

// Something wrong with this - keeps redirecting onto string even though the .ctor has been substituted
// [<Fact>]
// let ``Should create simple guid`` () =
//     let g = new Guid()
//     equal g g

// Todo - not yet implemented. Should we use rand crate?
// [<Fact>]
// let ``Should create different guids when using NewGuid`` () =
//     let g = Guid.NewGuid()
//     let g2 = Guid.NewGuid()
//     notEqual g g2

[<Fact>]
let ``Should parse guids`` () =
    let s = "f46d24f0-183e-4fca-9f47-e382afdfde8b"
    let g = Guid.Parse(s)
    let g2 = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    let g3 = Guid.Parse(s)
    notEqual g g2
    equal g g3

[<Fact>]
let ``Guid equality should work`` () =
    let s = "f46d24f0-183e-4fca-9f47-e382afdfde8b"
    let g = Guid.Parse(s)
    let g2 = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    let g3 = Guid.Parse(s)
    let gEqg3 = g = g3
    let gEqg2 = g = g2
    let gEqSelf = g = g

    equal true gEqg3
    equal false gEqg2
    equal true gEqSelf

// Something wron with Empty too - not redirecting!
// [<Fact>]
// let ``Guid empty should work`` () =
//     let g = Guid.Empty
//     let g2 = Guid.Parse("00000000-0000-0000-0000-000000000000")
//     equal g g2

[<Fact>]
let ``Guid ToString should work`` () =
    let g = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    g.ToString() |> equal "a3cc8928-7148-43e2-a863-a3954aea02df"