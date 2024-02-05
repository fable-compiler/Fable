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

    testCase "isOk function can be generated" <| fun () ->
        Ok 10 |> Result.isOk |> equal true
        Error 10 |> Result.isOk |> equal false

    testCase "isError function can be generated" <| fun () ->
        Ok 10 |> Result.isError |> equal false
        Error 10 |> Result.isError |> equal true

    testCase "contains function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.contains 42 |> equal true
        ok |> Result.contains 43 |> equal false
        err |> Result.contains 42 |> equal false

    testCase "count function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.count |> equal 1
        err |> Result.count |> equal 0

    testCase "defaultValue function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.defaultValue 0 |> equal 42
        err |> Result.defaultValue 0 |> equal 0

    testCase "defaultWith function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.defaultWith (fun e -> e.Length) |> equal 42
        err |> Result.defaultWith (fun e -> e.Length) |> equal 5

    testCase "exists function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.exists ((=) 42) |> equal true
        ok |> Result.exists ((=) 43) |> equal false
        err |> Result.exists ((=) 42) |> equal false

    testCase "fold function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal true
        err |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal false

    testCase "foldBack function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        (ok, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal true
        (err, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal false

    testCase "forAll function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.forall ((=) 42) |> equal true
        ok |> Result.forall ((=) 43) |> equal false
        err |> Result.forall ((=) 42) |> equal true

    testCase "iter function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        let mutable count = 0
        ok |> Result.iter (fun x -> count <- count + x)
        equal 42 count

        count <- 0
        err |> Result.iter (fun x -> count <- count + x)
        equal 0 count

    testCase "toArray function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toArray |> equal [| 42 |]
        err |> Result.toArray |> equal [||]

    testCase "toList function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toList |> equal [ 42 ]
        err |> Result.toList |> equal []

    testCase "toOption function can be generated" <| fun () ->
        let ok: Result<int, string> = Ok 42
        let err: Result<int, string> = Error "error"

        ok |> Result.toOption |> Option.get |> equal 42
        err |> Result.toOption |> Option.isNone |> equal true
