module Fable.Tests.Dart.Async

open System
open Util


type DisposableAction(f: unit -> unit) =
    interface IDisposable with
        member _.Dispose() = f ()

let private start work onSuccess =
    Async.StartWithContinuations(work, onSuccess, raise, raise)


let private startUnit (work: Async<unit>) =
    Async.StartWithContinuations(work, (fun () -> ()), raise, raise)

let tests () =
    testCase "async return works"
    <| fun () ->
        let mutable actual = 0

        async { return 42 } |> fun work -> start work (fun value -> actual <- value)

        actual |> equal 42


    testCase "async bind works"
    <| fun () ->
        let mutable actual = 0

        let work =
            async {
                let! value = async { return 40 }
                return value + 2
            }

        start work (fun value -> actual <- value)

        actual |> equal 42


    testCase "async return-from works"
    <| fun () ->
        let mutable actual = 0

        let inner = async { return 42 }

        let outer = async { return! inner }

        start outer (fun value -> actual <- value)

        actual |> equal 42


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
        let mutable actual = 0

        let work =
            async {
                try
                    failwith "boom"
                    return 0
                with _ ->
                    return 42
            }

        start work (fun value -> actual <- value)

        actual |> equal 42


    testCase "async try-with does not run handler when body succeeds"
    <| fun () ->
        let mutable actual = 0

        let work =
            async {
                try
                    return 42
                with _ ->
                    return 99
            }

        start work (fun value -> actual <- value)

        actual |> equal 42


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
        let mutable actual = 0
        let mutable finalized = false

        let work =
            async {
                try
                    return 42
                finally
                    finalized <- true
            }

        start work (fun value -> actual <- value)

        actual |> equal 42
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
