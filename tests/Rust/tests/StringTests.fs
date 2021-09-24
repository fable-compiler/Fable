module Fable.Tests.String

open Util.Testing

[<Fact>]
// TODO: this is not actually adding the strings, it is concatenating them at compile time!
let ``Adding strings works`` () =
    let a = "hello"
    let b = "world"
    let actual = a + " " + b
    actual |> equal "hello world"

[<Fact>]
let ``String length works`` () =
    let s = "hello"
    s.Length |> equal 5
    String.length s |> equal 5

[<Fact>]
let ``String equality works`` () =
    let s1 = "hello"
    let s2 = "hello"
    let s3 = "helloo"
    s1 |> equal s2
    (s1 = s2) |> equal true
    (s1 = s3) |> equal false
