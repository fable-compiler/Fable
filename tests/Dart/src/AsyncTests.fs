module Fable.Tests.Dart.Async

open System
open System.Threading
open Fable.Core.Dart
open Util


type DisposableAction(f: unit -> unit) =
    interface IDisposable with
        member _.Dispose() = f ()


let private start (work: Async<'T>) (onSuccess: 'T -> unit) =
    Async.StartWithContinuations(work, onSuccess, raise, raise)


let private startUnit (work: Async<unit>) =
    Async.StartWithContinuations(work, (fun () -> ()), raise, raise)

let tests () =
    testCase "async return works"
    <| fun () ->
        let work = async { return 42 }

        start work (fun actual -> actual |> equal 42)


    testCase "async bind works"
    <| fun () ->
        let work =
            async {
                let! value = async { return 40 }
                return value + 2
            }

        start work (fun actual -> actual |> equal 42)


    testCase "async return-from works"
    <| fun () ->
        let inner = async { return 42 }

        let outer = async { return! inner }

        start outer (fun actual -> actual |> equal 42)


    testCase "async zero and combine work"
    <| fun () ->
        let mutable reached = false

        let work =
            async {
                if false then
                    return ()

                reached <- true
            }

        startUnit work

        reached |> equal true


    testCase "async while binding works"
    <| fun () ->
        let mutable actual = 0

        let work =
            async {
                while actual < 10 do
                    actual <- actual + 1
            }

        startUnit work

        actual |> equal 10


    testCase "async for binding works"
    <| fun () ->
        let values = [| 1; 2; 3 |]
        let mutable actual = 0

        let work =
            async {
                for value in values do
                    actual <- actual + value
            }

        startUnit work

        actual |> equal 6


    testCase "async use disposes resources"
    <| fun () ->
        let mutable disposed = false
        let mutable activeInsideScope = false

        let resource = async { return new DisposableAction(fun () -> disposed <- true) }

        let work =
            async {
                use! _resource = resource
                activeInsideScope <- not disposed
            }

        startUnit work

        activeInsideScope |> equal true
        disposed |> equal true


    testCase "async use disposes resources on error"
    <| fun () ->
        let mutable disposed = false
        let mutable failed = false

        let work =
            async {
                use _resource = new DisposableAction(fun () -> disposed <- true)

                failwith "boom"
            }

        Async.StartWithContinuations(work, ignore, (fun _ -> failed <- true), raise)

        failed |> equal true
        disposed |> equal true


    testCase "async try-with catches errors"
    <| fun () ->
        let work =
            async {
                try
                    failwith "boom"
                    return 0
                with _ ->
                    return 42
            }

        start work (fun actual -> actual |> equal 42)


    testCase "async try-with does not run handler when body succeeds"
    <| fun () ->
        let work =
            async {
                try
                    return 42
                with _ ->
                    return 99
            }

        start work (fun actual -> actual |> equal 42)


    testCase "async try-with propagates errors from handler"
    <| fun () ->
        let mutable message = ""

        let work =
            async {
                try
                    failwith "first"
                    return 0
                with _ ->
                    failwith "second"
                    return 0
            }

        Async.StartWithContinuations(work, ignore, (fun error -> message <- error.Message), raise)

        message |> equal "second"


    testCase "async try-finally runs compensation"
    <| fun () ->
        let mutable finalized = false

        let work =
            async {
                try
                    return 42
                finally
                    finalized <- true
            }

        start work (fun actual -> actual |> equal 42)

        finalized |> equal true


    testCase "async try-finally runs compensation on error"
    <| fun () ->
        let mutable finalized = false
        let mutable failed = false

        let work =
            async {
                try
                    failwith "boom"
                finally
                    finalized <- true
            }

        Async.StartWithContinuations(work, ignore, (fun _ -> failed <- true), raise)

        failed |> equal true
        finalized |> equal true


    testCase "async bind propagates errors from binder"
    <| fun () ->
        let mutable failed = false

        let work =
            async {
                let! _ = async { return 42 }

                failwith "boom"
                return 0
            }

        Async.StartWithContinuations(work, ignore, (fun _ -> failed <- true), raise)

        failed |> equal true


    testCase "continuation errors are not redirected into try-with"
    <| fun () ->
        let mutable continuationCalls = 0

        let work =
            async {
                try
                    return 42
                with _ ->
                    return 99
            }

        throwsAnyError
        <| fun () ->
            Async.StartWithContinuations(
                work,
                (fun _ ->
                    continuationCalls <- continuationCalls + 1
                    failwith "boom from continuation"),
                ignore,
                ignore
            )

        continuationCalls |> equal 1


    testCase "nested async errors propagate through try-with"
    <| fun () ->
        let mutable message = ""

        let first () =
            async {
                try
                    failwith "1"
                    return 0
                with error ->
                    return! failwith ("2 " + error.Message)
            }

        let second () =
            async {
                try
                    return! first ()
                with error ->
                    return! failwith ("3 " + error.Message)
            }

        let work =
            async {
                try
                    let! _ = second ()
                    ()
                with error ->
                    message <- error.Message
            }

        startUnit work

        message |> equal "3 2 1"


    testCase "async try-finally runs throwing compensation exactly once"
    <| fun () ->
        let mutable calls = 0
        let mutable message = ""

        let work =
            async {
                try
                    return 42
                finally
                    calls <- calls + 1
                    failwith "cleanup"
            }

        Async.StartWithContinuations(work, ignore, (fun error -> message <- error.Message), raise)

        calls |> equal 1
        message |> equal "cleanup"


    testCase "CancellationTokenSource.Cancel cancels token"
    <| fun () ->
        let cts = new CancellationTokenSource()

        cts.Token.IsCancellationRequested |> equal false

        cts.Cancel()

        cts.Token.IsCancellationRequested |> equal true


    testCase "CancellationTokenSource.Cancel invokes registrations once"
    <| fun () ->
        let cts = new CancellationTokenSource()
        let mutable calls = 0

        cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

        cts.Cancel()

        calls |> equal 1

        // Cancellation is idempotent.
        cts.Cancel()

        calls |> equal 1


    testCase "CancellationToken registration can be disposed"
    <| fun () ->
        let cts = new CancellationTokenSource()
        let mutable called = false

        let registration = cts.Token.Register(fun () -> called <- true)

        registration.Dispose()

        cts.Cancel()

        called |> equal false


    testCase "CancellationToken registration after cancellation runs immediately"
    <| fun () ->
        let cts = new CancellationTokenSource()
        let mutable called = false

        cts.Cancel()

        cts.Token.Register(fun () -> called <- true) |> ignore

        called |> equal true


    testCase "pre-cancelled token cancels async before body runs"
    <| fun () ->
        let cts = new CancellationTokenSource()
        let mutable executed = false
        let mutable cancelled = false

        cts.Cancel()

        let work = async { executed <- true }

        Async.StartWithContinuations(
            work,
            (fun () -> failwith "Expected cancellation"),
            raise,
            (fun _ -> cancelled <- true),
            cts.Token
        )

        executed |> equal false
        cancelled |> equal true


    testCase "Async.CancellationToken returns current cancellation token"
    <| fun () ->
        let cts = new CancellationTokenSource()

        let work =
            async {
                let! token = Async.CancellationToken
                token = cts.Token |> equal true
            }

        Async.StartWithContinuations(work, ignore, raise, raise, cts.Token)


    testCase "ThrowIfCancellationRequested throws after cancellation"
    <| fun () ->
        let cts = new CancellationTokenSource()

        // Does not throw before cancellation.
        cts.Token.ThrowIfCancellationRequested()

        cts.Cancel()

        throwsAnyError <| fun () -> cts.Token.ThrowIfCancellationRequested()


    testCaseAsync "Async.Sleep suspends then resumes"
    <| fun () ->
        async {
            let mutable state = 0

            let child =
                async {
                    state <- 1

                    do! Async.Sleep 50

                    state <- 2
                }

            Async.Start child

            // The child has entered the computation, but Sleep must suspend it.
            state |> equal 1

            do! Async.Sleep 150

            // The timer has now resumed the suspended continuation.
            state |> equal 2
        }


    testCaseAsync "Async.Sleep can be cancelled while waiting"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()

            let mutable resumed = false
            let mutable cancelled = false
            let mutable failed = false

            let work =
                async {
                    do! Async.Sleep 200
                    resumed <- true
                }

            Async.StartWithContinuations(
                work,
                ignore,
                (fun _ -> failed <- true),
                (fun _ -> cancelled <- true),
                cts.Token
            )

            do! Async.Sleep 20

            cts.Cancel()

            do! Async.Sleep 20

            cancelled |> equal true
            failed |> equal false
            resumed |> equal false
        }


    testCaseAsync "CancellationTokenSource(millisecondsDelay) cancels token"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource(50)
            let mutable calls = 0

            cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

            cts.IsCancellationRequested |> equal false

            do! Async.Sleep 150

            cts.IsCancellationRequested |> equal true
            calls |> equal 1
        }


    testCaseAsync "CancellationTokenSource.CancelAfter cancels token"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()
            let mutable calls = 0

            cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

            cts.CancelAfter(50)

            cts.IsCancellationRequested |> equal false

            do! Async.Sleep 150

            cts.IsCancellationRequested |> equal true
            calls |> equal 1
        }


    testCaseAsync "CancellationTokenSource.CancelAfter invokes registrations only once"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()
            let mutable calls = 0

            cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

            cts.CancelAfter(50)

            do! Async.Sleep 150

            calls |> equal 1

            // Cancellation remains idempotent after the timer has fired.
            cts.Cancel()

            calls |> equal 1
        }


    testCaseAsync "disposed registration is not invoked by delayed cancellation"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()
            let mutable calls = 0

            let registration = cts.Token.Register(fun () -> calls <- calls + 1)

            registration.Dispose()

            cts.CancelAfter(50)

            do! Async.Sleep 150

            cts.IsCancellationRequested |> equal true
            calls |> equal 0
        }


    testCaseAsync "CancellationTokenSource.CancelAfter can be reset"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()

            cts.CancelAfter 300
            do! Async.Sleep 30

            cts.CancelAfter 500
            do! Async.Sleep 350

            cts.IsCancellationRequested |> equal false

            do! Async.Sleep 200

            cts.IsCancellationRequested |> equal true
        }


    testCaseAsync "Async.AwaitFuture returns Future result"
    <| fun () ->
        async {
            let future = async { return 42 } |> Async.StartAsFuture

            let! actual = Async.AwaitFuture future

            actual |> equal 42
        }


    testCaseAsync "Async.AwaitFuture propagates Future errors"
    <| fun () ->
        async {
            let future =
                async {
                    failwith "boom"
                    return 0
                }
                |> Async.StartAsFuture

            let mutable message = ""

            try
                let! _ = Async.AwaitFuture future
                ()
            with error ->
                message <- error.Message

            message |> equal "boom"
        }


    testCaseAsync "Async.AwaitFuture propagates Future cancellation"
    <| fun () ->
        async {
            let cts = new CancellationTokenSource()

            let mutable cancelled = false
            let mutable failed = false

            cts.Cancel()

            let future = Async.StartAsFuture(async { return 42 }, cts.Token)

            let work = Async.AwaitFuture future

            Async.StartWithContinuations(work, ignore, (fun _ -> failed <- true), (fun _ -> cancelled <- true))

            // Let the Future error propagate through its microtask.
            do! Async.Sleep 20

            cancelled |> equal true
            failed |> equal false
        }
