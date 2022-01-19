module Fable.Tests.Result

open Util.Testing

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

let tests =
  testList "Result" [
    testCase "constructors can be generated" <| fun () ->
        let ok = Ok 10
        Ok 10 |> equal ok

        let error = Error 10
        Error 10 |> equal error

    testCase "pattern matching works" <| fun () ->
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

    testCase "map function can be generated" <| fun () ->
        let f = (+) 1
        Ok 9 |> Result.map f |> equal (Ok 10)

    testCase "mapError function can be generated" <| fun () ->
        let f = (+) 1
        Error 9 |> Result.mapError f |> equal (Error 10)

    testCase "bind function can be generated" <| fun () ->
        let f = Error
        Ok 10 |> Result.bind f |> equal (Error 10)

    testCase "Nesting Result in pattern matching works" <| fun () -> // See #816
        Ok 5 |> Foo |> foo |> equal true
        Error "error" |> Foo |> foo |> equal false
  ]