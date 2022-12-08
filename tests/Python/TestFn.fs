module Fable.Tests.Fn

open Util.Testing

let add(a, b, cont) =
    cont(a + b)

let square(x, cont) =
    cont(x * x)

let sqrt(x, cont) =
    cont(sqrt(x))

let pythagoras(a, b, cont) =
    square(a, (fun aa ->
        square(b, (fun bb ->
            add(aa, bb, (fun aabb ->
                sqrt(aabb, (fun result ->
                    cont(result)
                ))
            ))
        ))
    ))

[<Fact>]
let ``test pythagoras works`` () =
    let result = pythagoras(10.0, 2.1, id)
    result
    |> equal 10.218121158021175

[<Fact>]
let ``test nonlocal works`` () =
    let mutable value = 0

    let fn () =
        value <- 42

    fn ()

    value |> equal 42

[<Fact>]
let ``test partially apply 1-3`` () =
    let add a b c = a + b + c

    let part = add 1
    part 2 3 |> equal 6


[<Fact>]
let ``test partially apply 2-3`` () =
    let add a b c = a + b + c

    let part = add 1 2
    part 3 |> equal 6

[<Fact>]
let ``test partially apply 1-4`` () =
    let add a b c d = a + b + c + d

    let part = add 1 2 3
    part 4 |> equal 10

[<Fact>]
let ``test partially apply 2-4`` () =
    let add a b c d = a + b + c + d

    let part = add 1 2
    part 3 4 |> equal 10

[<Fact>]
let ``test partially apply 3-4`` () =
    let add a b c d = a + b + c + d

    let part = add 1
    part 2 3 4 |> equal 10
