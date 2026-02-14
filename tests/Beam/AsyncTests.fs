module Fable.Tests.AsyncTests

open System
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
    let comps = [|
        async { return 1 }
        async { return 2 }
        async { return 3 }
    |]
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

#if FABLE_COMPILER

[<Fact>]
let ``test CancellationTokenSource create and cancel works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    equal false tcs.Token.IsCancellationRequested
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSource pre-cancelled works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSourceRegister works`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg = tcs.Token.Register(fun () -> x <- x + 1)
    equal 0 x
    tcs.Cancel()
    equal 1 x

[<Fact>]
let ``test CancellationTokenSource multiple registers work`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg1 = tcs.Token.Register(fun () -> x <- x + 1)
    let _reg2 = tcs.Token.Register(fun () -> x <- x + 10)
    tcs.Cancel()
    equal 11 x

[<Fact>]
let ``test async cancellation with pre-cancelled token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    Async.StartWithContinuations(
        async {
            do! Async.Sleep 10
            res <- true
        },
        ignore,
        ignore,
        (fun _ -> ()),  // on cancel
        tcs.Token)
    equal false res

[<Fact>]
let ``test async cancellation with auto-cancel token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource(50)
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res <- true
    }, tcs.Token)
    equal false res

[<Fact>]
let ``test CancellationToken Dispose is no-op`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Dispose()
    // Should not throw - Dispose is a no-op
    equal false tcs.Token.IsCancellationRequested

#endif
