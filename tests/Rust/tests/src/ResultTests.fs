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
let ``isOk function can be generated`` () =
    let ok: Result<int, string> = Ok 5
    let err: Result<int, string> = Error "error"
    ok |> Result.isOk |> equal true
    err |> Result.isOk |> equal false

[<Fact>]
let ``isError function can be generated`` () =
    let ok: Result<int, string> = Ok 5
    let err: Result<int, string> = Error "error"
    ok |> Result.isError |> equal false
    err |> Result.isError |> equal true

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

[<Fact>]
let ``Choice matching works`` () =
    let ok: Choice<string, int> = Choice1Of2 "foo"
    match ok with
    | Choice1Of2 x -> Some x
    | Choice2Of2 _ -> None
    |> equal (Some "foo")
