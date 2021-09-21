module Fable.Tests.Record

open Util.Testing

type MyRecord = {
    a: int
    b: string
    c: float
}

[<Fact>]
let ``Record fields works`` () =
    let x = { a=1; b="2"; c=3.0 }
    x.a |> equal 1
    x.b |> equal "2"
    x.c |> equal 3.0

[<Fact>]
let ``Record structural equality works`` () =
    let x = { a=1; b="2"; c=3.0 }
    let y = { a=1; b="2"; c=3.0 }
    let z = { a=3; b="4"; c=5.0 }

    x |> equal y
    (x = y) |> equal true
    (y = z) |> equal false
    (x = z) |> equal false

type DeepRecord = {
    d: MyRecord
    s: string
}

[<Fact>]
let ``Deep record fields works`` () =
    let x = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    x.d.a |> equal 1
    x.d.b |> equal "2"
    x.d.c |> equal 3.0
    x.s |> equal "hello"
[<Fact>]
let ``Deep record structural equality works`` () =
    let a = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    let b = { d={ a=2; b="3"; c=4.0 }; s="world" }
    let c = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    (a = a) |> equal true
    (a = b) |> equal false
    (a = c) |> equal true
    (b = c) |> equal false

let transformAddA ar =
    { ar with a = ar.a + 1}

[<Fact>]
let ``Call pass record byref to add fns works with borrow`` () =
    let x = { a=1; b="2"; c=3.0 }
    let z = { a=3; b="4"; c=5.0 }

    let x2 = x |> transformAddA |> transformAddA //borrow x
    let z2 = z |> transformAddA |> transformAddA |> transformAddA
    let x3 = x |> transformAddA //borrow x a second time
    x2.a |> equal 3
    x3.a |> equal 2
    z2.a |> equal 6

let transformDeepAddA ar =
    { d=transformAddA ar.d; s=ar.s } // borrow ar, clone s
[<Fact>]
let ``Call pass record byref deep to add fns works with borrow`` () =
    let x = {d ={ a=1; b="2"; c=3.0 }; s="one"}
    let z = {d ={ a=3; b="4"; c=5.0 }; s="two"}

    let x2 = x |> transformDeepAddA |> transformDeepAddA //borrow x
    let z2 = z |> transformDeepAddA |> transformDeepAddA |> transformDeepAddA
    let x3 = x |> transformDeepAddA //borrow x again
    x2.d.a |> equal 3
    x3.d.a |> equal 2
    z2.d.a |> equal 6