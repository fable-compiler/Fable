module Fable.Tests.String

open Util.Testing

[<Fact>]
let ``Adding strings works`` () =
    let a = "hello"
    let b = "world"
    let actual = a + " " + b
    a |> equal "hello" //bind out a to prevent inlining
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
    //(s1 <> s3) |> equal true
