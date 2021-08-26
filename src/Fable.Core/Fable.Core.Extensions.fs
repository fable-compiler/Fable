namespace Fable.Core

open System

[<AutoOpen>]
module Extensions =
    type FormattableString with
        member _.GetStrings(): string[] = jsNative

    type Async with
        static member AwaitPromise(promise: JS.Promise<'T>): Async<'T> = jsNative
        static member StartAsPromise(workflow: Async<'T>, ?token: System.Threading.CancellationToken): JS.Promise<'T> = jsNative

    type 'T``[]`` with
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.buffer")>]
        member _.buffer: JS.ArrayBuffer = jsNative
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteOffset")>]
        member _.byteOffset: int = jsNative
        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteLength")>]
        member _.byteLength: int = jsNative

    type Text.RegularExpressions.Regex with
        [<Emit("$0.lastIndex{{=$1}}")>]
        member _.lastIndex with get(): int = jsNative and set(i): unit = jsNative

module DynamicExtensions =
    type Object with
      [<Emit("$0[$1]{{=$2}}")>]
      member _.Item with get(idx: string): obj = jsNative
                     and set(idx: string) (value: obj): unit = jsNative
      [<Emit("$0($1...)")>]
      member _.Invoke([<ParamArray>] args: obj[]): 'a = jsNative
      [<Emit("new $0($1...)")>]
      member _.Create([<ParamArray>] args: obj[]): 'a = jsNative
