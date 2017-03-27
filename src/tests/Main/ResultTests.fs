[<Util.Testing.TestFixture>]
module Fable.Tests.Result
open Util.Testing
open Fable.Tests.Util

[<Test>]
let ``constructors can be generated``() =
    let ok = Ok 10
    Ok 10 |> equal ok

    let error = Error 10
    Error 10 |> equal error

[<Test>]
let ``map function can be generated``() =
    let f = (+) 1
    Ok 9 |> Result.map f |> equal (Ok 10)

[<Test>]
let ``mapError function can be generated``() =
    let f = (+) 1
    Error 9 |> Result.mapError f |> equal (Error 10)

[<Test>]
let ``bind function can be generated``() =
    let f = Error
    Ok 10 |> Result.bind f |> equal (Error 10)