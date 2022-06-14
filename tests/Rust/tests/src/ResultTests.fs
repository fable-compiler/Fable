module Fable.Tests.ResultTests

open Util.Testing

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

// [<Fact>]
// let ``Generic constructors can be compared`` () =
//     let ok = Ok 10
//     Ok 10 |> equal ok
//     let error = Error 10
//     Error 10 |> equal error

[<Fact>]
let ``Pattern matching works`` () =
    let ok: Result<string, int> = Ok "foo"
    match ok with
    | Ok x -> Some x
    | Error _ -> None
    |> equal (Some "foo")

    let error: Result<string, int> = Error 10
    match error with
    | Ok _ -> None
    | Error y -> Some y
    |> equal (Some 10)

[<Fact>]
let ``Map function can be generated`` () =
    let f = (+) 1
    let ok: Result<int, float> = Ok 9
    ok |> Result.map f |> equal (Ok 10)

[<Fact>]
let ``MapError function can be generated`` () =
    let f = (+) 1
    let error: Result<float, int> = Error 9
    error |> Result.mapError f |> equal (Error 10)

[<Fact>]
let ``Bind function can be generated`` () =
    let res: Result<int, int> = Ok 10 |> Result.bind Error
    res |> equal (Error 10)

[<Fact>]
let ``Nesting Result in pattern matching works`` () =
    Ok 5 |> Foo |> foo |> equal true
    Error "error" |> Foo |> foo |> equal false

[<Fact>]
let ``Choice matching works`` () =
    let ok: Choice<string, int> = Choice1Of2 "foo"
    match ok with
    | Choice1Of2 x -> Some x
    | Choice2Of2 _ -> None
    |> equal (Some "foo")
