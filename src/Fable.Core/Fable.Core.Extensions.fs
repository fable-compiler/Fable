namespace Fable.Core

open System

[<AutoOpen>]
module Extensions =
    type Async with
        static member AwaitPromise(promise: JS.Promise<'T>): Async<'T> = jsNative
        static member StartAsPromise(workflow: Async<'T>, ?token: System.Threading.CancellationToken): JS.Promise<'T> = jsNative

module DynamicExtensions =
    type Object with
      [<Emit("$0[$1]{{=$2}}")>]
      member __.Item with get(idx: string): obj = jsNative
                     and set(idx: string) (value: obj): unit = jsNative
      [<Emit("$0($1...)")>]
      member __.Invoke([<ParamArray>] args: obj[]): 'a = jsNative
      [<Emit("new $0($1...)")>]
      member __.Create([<ParamArray>] args: obj[]): 'a = jsNative
