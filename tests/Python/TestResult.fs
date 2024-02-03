module Fable.Tests.Result

open Util.Testing

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

[<Fact>]
let ``test constructors can be generated`` () =
    let ok = Ok 10
    Ok 10 |> equal ok

    let error = Error 10
    Error 10 |> equal error

[<Fact>]
let ``test pattern matching works`` () =
    let ok = Ok "foo"
    match ok with
    | Ok x -> Some x
    | Error _ -> None
    |> equal (Some "foo")

    let error = Error 10
    match Error 10 with
    | Ok _ -> None
    | Error y -> Some y
    |> equal (Some 10)

[<Fact>]
let ``test map function can be generated`` () =
    let f = (+) 1
    Ok 9 |> Result.map f |> equal (Ok 10)

[<Fact>]
let ``test mapError function can be generated`` () =
    let f = (+) 1
    Error 9 |> Result.mapError f |> equal (Error 10)

[<Fact>]
let ``test bind function can be generated`` () =
    let f = Error
    Ok 10 |> Result.bind f |> equal (Error 10)

[<Fact>]
let ``test Nesting Result in pattern matching works`` () =
    Ok 5 |> Foo |> foo |> equal true
    Error "error" |> Foo |> foo |> equal false

[<Fact>]
let ``test isOk function can be generated`` () =
    Ok 5 |> Result.isOk |> equal true
    Error "error" |> Result.isOk |> equal false

[<Fact>]
let ``test isError function can be generated`` () =
    Ok 5 |> Result.isError |> equal false
    Error "error" |> Result.isError |> equal true
