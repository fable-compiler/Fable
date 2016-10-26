[<Util.Testing.TestFixture>]
module Fable.Tests.TupleTypes
open Util.Testing
open Fable.Tests.Util

[<Test>]
let ``Tuple dereferencing can be generated``() =
    let x = 10, true
    let y, z = x
    equal 10 y
    equal true z

[<Test>]
let ``fst function can be generated``() =
    let xy = 10., true
    fst xy |> equal 10.

[<Test>]
let ``snd function can be generated``() =
    let xy = 10., true
    snd xy |> equal true
