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
let ``pattern matching works``() =
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

type Foo =
    | Foo of Result<int, string>
    | Bar of (int * string) option

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

[<Test>]
let ``Nesting Result in pattern matching works``() = // See #816
    Ok 5 |> Foo |> foo |> equal true
    Error "error" |> Foo |> foo |> equal false

[<Test>]
let ``DU constructor is a function``() =
    let curry f a b = f (a,b)

    curry (Some >> Bar) <| 1 <| "two" 
    |> equal (Bar (Some(1,"two")))
