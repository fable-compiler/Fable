namespace Fable.Import

namespace Fable.Core

open System

module PY =
    [<Import("typing", "Callable")>]
    type [<AllowNullLiteral>] Callable =
        [<Emit "$0.__name__)">] abstract name: string
        [<Emit "$0$1...">] abstract Invoke: [<ParamArray>] args: obj[] -> obj
        [<Emit "$0">] abstract Instance: obj

    [<AbstractClass>]
    type DecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Callable -> Callable

    [<AbstractClass>]
    type ReflectedDecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Callable * info: Reflection.MethodInfo -> Callable

    // Hack because currently Fable doesn't keep information about spread for anonymous functions
    [<Emit("lambda *args: $0(args)")>]
    let argsFunc (fn: obj[] -> obj): Callable = nativeOnly

    type [<AllowNullLiteral>] ArrayConstructor =
        [<Emit "$0([None]*$1...)">]
        abstract Create: size: int -> 'T[]
        [<Emit "isinstance($1, list)">]
        abstract isArray: arg: obj -> bool
        abstract from: arg: obj -> 'T[]

    and [<AllowNullLiteral>] ArrayBuffer =
        abstract byteLength: int
        [<Emit("$0[$1:$1+$2]")>]
        abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer

    [<RequireQualifiedAccess>]
    module Constructors =
        let [<Emit("list")>] Array: ArrayConstructor = nativeOnly
