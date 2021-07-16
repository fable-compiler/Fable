namespace Fable.Core

open System

[<AutoOpen>]
module Extensions =
    type Async with
        static member AwaitPromise(promise: JS.Promise<'T>): Async<'T> = nativeOnly
        static member StartAsPromise(workflow: Async<'T>, ?token: System.Threading.CancellationToken): JS.Promise<'T> = nativeOnly

    type 'T``[]`` with
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.buffer")>]
        member __.buffer: JS.ArrayBuffer = nativeOnly
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteOffset")>]
        member __.byteOffset: int = nativeOnly
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteLength")>]
        member __.byteLength: int = nativeOnly

    type Text.RegularExpressions.Regex with
        [<Emit("$0.lastIndex{{=$1}}")>]
        member __.lastIndex with get(): int = nativeOnly and set(i): unit = nativeOnly

module DynamicExtensions =
    type Object with
      [<Emit("$0[$1]{{=$2}}")>]
      member __.Item with get(idx: string): obj = nativeOnly
                     and set(idx: string) (value: obj): unit = nativeOnly
      [<Emit("$0($1...)")>]
      member __.Invoke([<ParamArray>] args: obj[]): 'a = nativeOnly
      [<Emit("new $0($1...)")>]
      member __.Create([<ParamArray>] args: obj[]): 'a = nativeOnly
