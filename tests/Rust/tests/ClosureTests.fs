module Fable.Tests.Closure

open Util.Testing

let map f x =
    f x

let staticFnAdd1 x = x + 1

[<Fact>]
let ``fn as param should also accept static functions`` () =
    let a = 3
    let b = 2

    a |> equal 3
    b |> equal 2
    a |> map staticFnAdd1 |> equal 4
    b |> map staticFnAdd1 |> equal 3


[<Fact>]
let ``Closure captures trivial case variable and does not break borrow checker`` () =
    let a = 3
    let b = 2

    let res = a |> map (fun x -> b + x)//b is captured, so it is borrowed
    a |> equal 3
    b |> equal 2
    res |> equal 5

type Wrapped = {
    Value: string
}

[<Fact>]
let ``Closure captures and clones`` () =
    let a = { Value = "a" }
    let b = { Value = "b" }

    let res1 = a |> map (fun x -> x.Value + b.Value)//capture b, clone
    let res2 = a |> map (fun x -> x.Value + b.Value + "x")//capture b, clone
    res1 |> equal "ab"
    res2 |> equal "abx"