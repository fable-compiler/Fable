module Fable.Tests.ClassTests

open Util.Testing

type CTest(x: int, y: int) =
    let a = x + y
    // print "%i %i" x y
    //member this.A = a
    // member this.B = 1
    member this.Add m = a + m

[<Fact>]
let ``Class instance comparisons work`` () = //this is inconsistent with .net since structural equality is not the default with classes
    let a = CTest(1, 2)
    let b = CTest(3, 4)
    // let r = a.Add(1)
    // r |> equal 4
    a |> equal a
    a |> notEqual b