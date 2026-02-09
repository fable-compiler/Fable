module Fable.Tests.AsyncTests

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test async return works`` () =
    let comp = async { return 42 }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async let bang works`` () =
    let comp = async {
        let! x = async { return 10 }
        let! y = async { return 20 }
        return x + y
    }
    Async.RunSynchronously comp |> equal 30

[<Fact>]
let ``test async do bang works`` () =
    let mutable x = 0
    let comp = async {
        do! async { x <- 42; return () }
        return x
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async return from works`` () =
    let inner = async { return "hello" }
    let outer = async { return! inner }
    Async.RunSynchronously outer |> equal "hello"

[<Fact>]
let ``test async try-with works`` () =
    let comp = async {
        try
            failwith "boom"
            return 0
        with _e ->
            return 42
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async sleep works`` () =
    let comp = async {
        do! Async.Sleep 10
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async parallel works`` () =
    let makeComp n = async { return n }
    let comps = [| 1; 2; 3 |] |> Array.map makeComp
    let results = Async.RunSynchronously (Async.Parallel comps)
    Array.sum results |> equal 6

[<Fact>]
let ``test async sequential composition works`` () =
    let mutable count = 0
    let comp = async {
        let! a = async { count <- count + 1; return count }
        let! b = async { count <- count + 1; return count }
        return a + b
    }
    Async.RunSynchronously comp |> equal 3

[<Fact>]
let ``test async ignore works`` () =
    let comp = async {
        do! Async.Ignore (async { return 42 })
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async start immediate works`` () =
    let mutable x = 0
    Async.StartImmediate (async { x <- 42 })
    equal 42 x
