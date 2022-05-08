module Fable.Tests.Dart.Result

open Util

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

let asError = Error

let tests() =
    testCase "constructors can be generated" <| fun () ->
        let ok: Result<int, int> = Ok 10
        Ok 10 |> equal ok

        let error: Result<int, int> = Error 10
        Error 10 |> equal error

    testCase "pattern matching works" <| fun () ->
        let ok: Result<string, int> = Ok "foo"
        match ok with
        | Ok x -> Some x
        | Error _ -> None
        |> equal (Some "foo")

        match Error 10: Result<string, int> with
        | Ok _ -> None
        | Error y -> Some y
        |> equal (Some 10)

    testCase "map function can be generated" <| fun () ->
        let f = (+) 1
        Ok 9 |> Result.map f |> equal (Ok 10)

    testCase "mapError function can be generated" <| fun () ->
        let f = (+) 1
        Error 9 |> Result.mapError f |> equal (Error 10)

    testCase "bind function can be generated" <| fun () ->
        Ok 10 |> Result.bind asError |> equal (Error 10)

    testCase "Nesting Result in pattern matching works" <| fun () -> // See #816
        Ok 5 |> Foo |> foo |> equal true
        Error "error" |> Foo |> foo |> equal false
