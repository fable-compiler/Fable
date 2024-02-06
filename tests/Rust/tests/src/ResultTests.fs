module Fable.Tests.ResultTests

open Util.Testing

type Foo =
    | Foo of Result<int, string>

let foo (a: Foo): bool =
    match a with
    | Foo(Ok(_)) -> true
    | _ -> false

// [<Fact>]
// let ``Result constructors can be compared`` () =
//     let ok = Ok 10
//     Ok 10 |> equal ok
//     let error = Error 10
//     Error 10 |> equal error

[<Fact>]
let ``Result pattern matching works`` () =
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
let ``Result.map works`` () =
    let f = (+) 1
    let ok: Result<int, float> = Ok 9
    ok |> Result.map f |> equal (Ok 10)

[<Fact>]
let ``Result.mapError works`` () =
    let f = (+) 1
    let error: Result<float, int> = Error 9
    error |> Result.mapError f |> equal (Error 10)

[<Fact>]
let ``Result.bind works`` () =
    let res: Result<int, int> = Ok 10 |> Result.bind Error
    res |> equal (Error 10)

[<Fact>]
let ``Nesting Result in pattern matching works`` () =
    Ok 5 |> Foo |> foo |> equal true
    Error "error" |> Foo |> foo |> equal false

[<Fact>]
let ``Result.isOk works`` () =
    let ok: Result<int, string> = Ok 5
    let err: Result<int, string> = Error "error"
    ok |> Result.isOk |> equal true
    err |> Result.isOk |> equal false

[<Fact>]
let ``Result.isError works`` () =
    let ok: Result<int, string> = Ok 5
    let err: Result<int, string> = Error "error"
    ok |> Result.isError |> equal false
    err |> Result.isError |> equal true

[<Fact>]
let ``Result.contains works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.contains 42 |> equal true
    ok |> Result.contains 43 |> equal false
    err |> Result.contains 42 |> equal false

[<Fact>]
let ``Result.count works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.count |> equal 1
    err |> Result.count |> equal 0

[<Fact>]
let ``Result.defaultValue works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultValue 0 |> equal 42
    err |> Result.defaultValue 0 |> equal 0

[<Fact>]
let ``Result.defaultWith works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultWith (fun e -> e.Length) |> equal 42
    err |> Result.defaultWith (fun e -> e.Length) |> equal 5

[<Fact>]
let ``Result.exists works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.exists ((=) 42) |> equal true
    ok |> Result.exists ((=) 43) |> equal false
    err |> Result.exists ((=) 42) |> equal false

[<Fact>]
let ``Result.fold works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal true
    err |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal false

[<Fact>]
let ``Result.foldBack works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    (ok, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal true
    (err, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal false

[<Fact>]
let ``Result.forall works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.forall ((=) 42) |> equal true
    ok |> Result.forall ((=) 43) |> equal false
    err |> Result.forall ((=) 42) |> equal true

[<Fact>]
let ``Result.iter works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    let mutable count = 0
    ok |> Result.iter (fun x -> count <- count + x)
    equal 42 count

    count <- 0
    err |> Result.iter (fun x -> count <- count + x)
    equal 0 count

[<Fact>]
let ``Result.toArray works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toArray |> equal [| 42 |]
    err |> Result.toArray |> equal [||]

[<Fact>]
let ``Result.toList works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toList |> equal [ 42 ]
    err |> Result.toList |> equal []

[<Fact>]
let ``Result.toOption works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toOption |> Option.get |> equal 42
    err |> Result.toOption |> Option.isNone |> equal true

[<Fact>]
let ``Result.toValueOption works`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toValueOption |> ValueOption.get |> equal 42
    err |> Result.toValueOption |> ValueOption.isNone |> equal true

[<Fact>]
let ``Choice pattern matching works`` () =
    let ok: Choice<string, int> = Choice1Of2 "foo"
    match ok with
    | Choice1Of2 x -> Some x
    | Choice2Of2 _ -> None
    |> equal (Some "foo")
