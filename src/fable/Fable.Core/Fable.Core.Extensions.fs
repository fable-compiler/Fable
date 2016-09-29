namespace Fable.Core

[<AutoOpen>]
module Extensions =
    open System.Threading
    open Fable.Core
    open Fable.Import.JS

    type Async with
        static member AwaitPromise(promise: Promise<'T>): Async<'T> = jsNative
        static member StartAsPromise(workflow: Async<'T>, ?token: CancellationToken): Promise<'T> = jsNative
