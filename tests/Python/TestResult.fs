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

[<Fact>]
let ``test contains function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.contains 42 |> equal true
    ok |> Result.contains 43 |> equal false
    err |> Result.contains 42 |> equal false

[<Fact>]
let ``count function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.count |> equal 1
    err |> Result.count |> equal 0

[<Fact>]
let ``defaultValue function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultValue 0 |> equal 42
    err |> Result.defaultValue 0 |> equal 0

[<Fact>]
let ``defaultWith function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.defaultWith (fun e -> e.Length) |> equal 42
    err |> Result.defaultWith (fun e -> e.Length) |> equal 5

[<Fact>]
let ``exists function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.exists ((=) 42) |> equal true
    ok |> Result.exists ((=) 43) |> equal false
    err |> Result.exists ((=) 42) |> equal false

[<Fact>]
let ``fold function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal true
    err |> Result.fold (fun s x -> s || x % 2 = 0) false |> equal false

[<Fact>]
let ``foldBack function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    (ok, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal true
    (err, false) ||> Result.foldBack (fun x s -> s || x % 2 = 0) |> equal false

[<Fact>]
let ``forAll function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.forall ((=) 42) |> equal true
    ok |> Result.forall ((=) 43) |> equal false
    err |> Result.forall ((=) 42) |> equal true

[<Fact>]
let ``iter function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    let mutable count = 0
    ok |> Result.iter (fun x -> count <- count + x)
    equal 42 count

    count <- 0
    err |> Result.iter (fun x -> count <- count + x)
    equal 0 count

[<Fact>]
let ``toArray function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toArray |> equal [| 42 |]
    err |> Result.toArray |> equal [||]

[<Fact>]
let ``toList function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toList |> equal [ 42 ]
    err |> Result.toList |> equal []

[<Fact>]
let ``toOption function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toOption |> Option.get |> equal 42
    err |> Result.toOption |> Option.isNone |> equal true

[<Fact>]
let ``toValueOption function can be generated`` () =
    let ok: Result<int, string> = Ok 42
    let err: Result<int, string> = Error "error"

    ok |> Result.toValueOption |> ValueOption.get |> equal 42
    err |> Result.toValueOption |> ValueOption.isNone |> equal true
