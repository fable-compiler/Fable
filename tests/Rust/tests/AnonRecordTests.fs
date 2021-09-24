module Fable.Tests.AnonRecord

open Util.Testing

let ``Anon record simple works`` () =
    let r = {| A = 1; B = "hello"; X = 3.141; D = 4|}
    r.A |> equal 1
    r.X |> equal 3.141  //just in case alphabetical ordering is going to throw off index
    r.B |> equal "hello"
    r.D |> equal 4

let anonAcceptingFn (x: {|A: int; B: string|}) =
    {|C = x.A + 1; D = "Z"|}

let ``Anon record can call fn`` () =
    let m = {| A = 1; B = "hello"|}
    let res = anonAcceptingFn m
    res.C |> equal 2
    res.D |> equal "Z"

let ``Anon record structural equality works`` () =
    let a = {| A = 1; B = "hello"|}
    let b = {| A = 1; B = "hello"|}
    let c = {| A = 2; B = "test"|}
    a = a |> equal true
    a = b |> equal true
    a = c |> equal false
    b = c |> equal false