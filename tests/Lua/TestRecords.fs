module Fable.Tests.Record

open Util.Testing

type Simple = {
    one: string
    two: int
}

type Parent = {
    a: Simple
    b: Simple
}

// type Recursive = {
//     x: Recursive
// }

[<Fact>]
let testMakeRecord () =
    let r = { one="string_one"; two=2}
    r.one |> equal "string_one"
    r.two |> equal 2

[<Fact>]
let testMakeNestedRecord () =
    let r = {
        a = { one = "a"; two = 2}
        b = { one = "b"; two = 4}
    }
    r.a.one |> equal "a"
    r.b.one |> equal "b"
    r.a.two |> equal 2
    r.b.two |> equal 4

// [<Fact>]
// let testStructuralCompareRecords () =
//     let a = { one="string_one"; two=2}
//     let b = { one="string_one"; two=2}
//     let c = { one="string_two"; two=4}
//     a = a |> equal true
//     a = b |> equal true
//     a = c |> equal false

[<Fact>]
let testMakeAnonRecord () =
    let r = {| x = 3.142; y = true |}
    r.x |> equal 3.142
    r.y |> equal true
