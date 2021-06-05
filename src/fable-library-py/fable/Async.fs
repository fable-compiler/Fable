module Async

open System
open AsyncBuilder
open Fable.Core

let emptyContinuation<'T> (_x: 'T) =
    // NOP
    ()

let defaultCancellationToken = new CancellationToken()


[<Erase>]
type Async =
    static member StartWithContinuations
        (
            computation: IAsync<'T>,
            continuation,
            exceptionContinuation,
            cancellationContinuation,
            ?cancellationToken
        ) : unit =
        let trampoline = Trampoline()

        computation (
            { new IAsyncContext<'T> with
                member this.onSuccess = continuation
                member this.onError = exceptionContinuation
                member this.onCancel = cancellationContinuation
                member this.cancelToken = defaultArg cancellationToken defaultCancellationToken
                member this.trampoline = trampoline }
        )

    static member StartWithContinuations(computation: IAsync<'T>, ?cancellationToken) : unit =
        Async.StartWithContinuations(
            computation,
            emptyContinuation,
            emptyContinuation,
            emptyContinuation,
            ?cancellationToken = cancellationToken
        )

    static member Start(computation, ?cancellationToken) =
        Async.StartWithContinuations(computation, ?cancellationToken = cancellationToken)


    static member StartImmediate(computation: IAsync<'T>, ?cancellationToken) =
        Async.Start(computation, ?cancellationToken = cancellationToken)

let startImmediate(computation: IAsync<'T>) =
    Async.StartImmediate(computation, ?cancellationToken=None)
