namespace Fable.Core

[<AutoOpen>]
module Extensions =
    open System.Threading
    open Fable.Core
    open Fable.Import.JS

    type Async with
        static member AwaitPromise(promise: Promise<'T>): Async<'T> = jsNative
        static member StartAsPromise(workflow: Async<'T>, ?token: CancellationToken): Promise<'T> = jsNative

module DynamicExtensions =
    open System

    type Object with
      [<Emit("$0[$1]{{=$2}}")>]
      member __.Item with get(idx: string): obj = jsNative
                     and set(idx: string) (value: obj): unit = jsNative
      [<Emit("$0($1...)")>]
      member __.Invoke([<ParamArray>] args: obj[]): 'a = jsNative
