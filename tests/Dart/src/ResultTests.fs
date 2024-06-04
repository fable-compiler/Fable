﻿module Fable.Tests.Dart.Result

open Util

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

let asError = Error

let tests() =
    testCase "Result constructors can be compared" <| fun () ->
        let ok: Result<int, int> = Ok 10
        Ok 10 |> equal ok

        let error: Result<int, int> = Error 10
        Error 10 |> equal error

    testCase "Result pattern matching works" <| fun () ->
        let ok: Result<string, int> = Ok "foo"
        match ok with
        | Ok x -> Some x
        | Error _ -> None
        |> equal (Some "foo")

        match Error 10: Result<string, int> with
        | Ok _ -> None
        | Error y -> Some y
        |> equal (Some 10)

    testCase "Result.map works" <| fun () ->
        let f = (+) 1
        Ok 9 |> Result.map f |> equal (Ok 10)

    testCase "Result.mapError works" <| fun () ->
        let f = (+) 1
        Error 9 |> Result.mapError f |> equal (Error 10)

    testCase "Result.bind works" <| fun () ->
        Ok 10 |> Result.bind asError |> equal (Error 10)

    testCase "Nesting Result in pattern matching works" <| fun () -> // See #816
        Ok 5 |> Foo |> foo |> equal true
        Error "error" |> Foo |> foo |> equal false

    testCase "Result.isOk works" <| fun () ->
        Ok 10 |> Result.isOk |> equal true
        Error 10 |> Result.isOk |> equal false

    testCase "Result.isError works" <| fun () ->
        Ok 10 |> Result.isError |> equal false
        Error 10 |> Result.isError |> equal true

    testCase "Result.contains works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.contains 42 |> equal true
        ok |> Result.contains 43 |> equal false
        err |> Result.contains 42 |> equal false

    testCase "Result.count works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.count |> equal 1
        err |> Result.count |> equal 0

    testCase "Result.defaultValue works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.defaultValue 0 |> equal 42
        err |> Result.defaultValue 0 |> equal 0

    testCase "Result.defaultWith works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.defaultWith (fun e -> e.Length) |> equal 42
        err |> Result.defaultWith (fun e -> e.Length) |> equal 5

    testCase "Result.exists works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.exists ((=) 42) |> equal true
        ok |> Result.exists ((=) 43) |> equal false
        err |> Result.exists ((=) 42) |> equal false

    testCase "Result.fold works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal true
        err |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal false

    testCase "Result.foldBack works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        (ok, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal true
        (err, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal false

    testCase "Result.forall works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.forall ((=) 42) |> equal true
        ok |> Result.forall ((=) 43) |> equal false
        err |> Result.forall ((=) 42) |> equal true

    testCase "Result.iter works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        let mutable count = 0
        ok |> Result.iter (fun x -> count <- count + x)
        equal 42 count

        count <- 0
        err |> Result.iter (fun x -> count <- count + x)
        equal 0 count

    testCase "Result.toArray works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toArray |> equal [| 42 |]
        err |> Result.toArray |> equal [||]

    testCase "Result.toList works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toList |> equal [ 42 ]
        err |> Result.toList |> equal []

    testCase "Result.toOption works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toOption |> Option.get |> equal 42
        err |> Result.toOption |> Option.isNone |> equal true

    testCase "Result.toValueOption works" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toValueOption |> ValueOption.get |> equal 42
        err |> Result.toValueOption |> ValueOption.isNone |> equal true

    testCase "Choice pattern matching works" <| fun () ->
        let ok: Choice<string, int> = Choice1Of2 "foo"
        match ok with
        | Choice1Of2 x -> Some x
        | Choice2Of2 _ -> None
        |> equal (Some "foo")
