[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.TupleTypes
open NUnit.Framework
open Fabel.Tests.Util

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
