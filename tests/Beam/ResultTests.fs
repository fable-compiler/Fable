module Fable.Tests.ResultTest

open Fable.Tests.Util
open Util.Testing

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

[<Fact>]
let ``test Result constructors can be compared`` () =
    let ok = Ok 10
    Ok 10 |> equal ok

    let error = Error 10
    Error 10 |> equal error

[<Fact>]
let ``test Result pattern matching works`` () =
    let ok = Ok "foo"
    match ok with
    | Ok x -> Some x
    | Error _ -> None
    |> equal (Some "foo")

    let _error: Result<string, int> = Error 10
    match Error 10 with
    | Ok _ -> None
    | Error y -> Some y
    |> equal (Some 10)

[<Fact>]
let ``test Result.map works`` () =
    let f x = x + 1
    Ok 9 |> Result.map f |> equal (Ok 10)

[<Fact>]
let ``test Result.mapError works`` () =
    let f x = x + 1
    Error 9 |> Result.mapError f |> equal (Error 10)

[<Fact>]
let ``test Result.bind works`` () =
    let f x = Error x
    Ok 10 |> Result.bind f |> equal (Error 10)

[<Fact>]
let ``test Nesting Result in DU works`` () =
    Ok 5 |> Foo |> foo |> equal true
    Error "error" |> Foo |> foo |> equal false

[<Fact>]
let ``test Result.isOk works`` () =
    Ok 5 |> Result.isOk |> equal true
    Error "error" |> Result.isOk |> equal false

[<Fact>]
let ``test Result.isError works`` () =
    Ok 5 |> Result.isError |> equal false
    Error "error" |> Result.isError |> equal true

[<Fact>]
let ``test Result.contains works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.contains 42 |> equal true
    ok |> Result.contains 43 |> equal false
    err |> Result.contains 42 |> equal false

[<Fact>]
let ``test Result.count works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.count |> equal 1
    err |> Result.count |> equal 0

[<Fact>]
let ``test Result.defaultValue works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultValue 0 |> equal 42
    err |> Result.defaultValue 0 |> equal 0

[<Fact>]
let ``test Result.defaultWith works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultWith (fun _e -> 0) |> equal 42
    err |> Result.defaultWith (fun e -> String.length e) |> equal 5

[<Fact>]
let ``test Result.exists works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.exists (fun x -> x = 42) |> equal true
    ok |> Result.exists (fun x -> x = 43) |> equal false
    err |> Result.exists (fun x -> x = 42) |> equal false

[<Fact>]
let ``test Result.fold works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.fold (fun s x -> s + x) 0 |> equal 42
    err |> Result.fold (fun s x -> s + x) 0 |> equal 0

[<Fact>]
let ``test Result.foldBack works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    (ok, 0) ||> Result.foldBack (fun x s -> x + s) |> equal 42
    (err, 0) ||> Result.foldBack (fun x s -> x + s) |> equal 0

[<Fact>]
let ``test Result.forall works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.forall (fun x -> x = 42) |> equal true
    ok |> Result.forall (fun x -> x = 43) |> equal false
    err |> Result.forall (fun x -> x = 42) |> equal true

[<Fact>]
let ``test Result.iter works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    let mutable count = 0
    ok |> Result.iter (fun x -> count <- count + x)
    equal 42 count

    count <- 0
    err |> Result.iter (fun x -> count <- count + x)
    equal 0 count

[<Fact>]
let ``test Result.toArray works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toArray |> equal [| 42 |]
    err |> Result.toArray |> equal [||]

[<Fact>]
let ``test Result.toList works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toList |> equal [ 42 ]
    err |> Result.toList |> equal []

[<Fact>]
let ``test Result.toOption works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toOption |> Option.get |> equal 42
    err |> Result.toOption |> Option.isNone |> equal true

[<Fact>]
let ``test Choice pattern matching works`` () =
    let ok: Choice<string, int> = Choice1Of2 "foo"
    match ok with
    | Choice1Of2 x -> Some x
    | Choice2Of2 _ -> None
    |> equal (Some "foo")

    let err: Choice<string, int> = Choice2Of2 42
    match err with
    | Choice1Of2 _ -> None
    | Choice2Of2 y -> Some y
    |> equal (Some 42)
