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
