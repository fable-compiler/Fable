namespace Fable.Core

open System

[<AutoOpen>]
module Extensions =
    type FormattableString with

        member _.GetStrings() : string[] = nativeOnly

    type Async with

        static member AwaitPromise(promise: JS.Promise<'T>) : Async<'T> =
            nativeOnly

        static member StartAsPromise
            (
                workflow: Async<'T>,
                ?token: System.Threading.CancellationToken
            )
            : JS.Promise<'T>
            =
            nativeOnly

    type 'T ``[]`` with

        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.buffer")>]
        member _.buffer: JS.ArrayBuffer = nativeOnly

        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteOffset")>]
        member _.byteOffset: int = nativeOnly

        /// Only valid on numeric arrays compiled as JS TypedArrays
        [<Emit("$0.byteLength")>]
        member _.byteLength: int = nativeOnly

    type Text.RegularExpressions.Regex with

        [<Emit("$0.lastIndex{{=$1}}")>]
        member _.lastIndex
            with get (): int = nativeOnly
            and set (i): unit = nativeOnly

module DynamicExtensions =
    type Object with

        [<Emit("$0[$1]{{=$2}}")>]
        member _.Item
            with get (idx: string): obj = nativeOnly
            and set (idx: string) (value: obj): unit = nativeOnly

        [<Emit("$0($1...)")>]
        member _.Invoke([<ParamArray>] args: obj[]) : 'a = nativeOnly

        [<Emit("new $0($1...)")>]
        member _.Create([<ParamArray>] args: obj[]) : 'a = nativeOnly
