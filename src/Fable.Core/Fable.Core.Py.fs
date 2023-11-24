namespace Fable.Core

open System

[<RequireQualifiedAccess>]
module Py =
    [<Import("Callable", "typing")>]
    [<AllowNullLiteral>]
    type Callable =
        [<Emit "$0.__name__)">]
        abstract name: string

        [<Emit "$0$1...">]
        abstract Invoke: [<ParamArray>] args: obj[] -> obj

        [<Emit "$0">]
        abstract Instance: obj

    [<AbstractClass>]
    type DecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Callable -> Callable

    [<AbstractClass>]
    type ReflectedDecoratorAttribute() =
        inherit Attribute()

        abstract Decorate:
            fn: Callable * info: Reflection.MethodInfo -> Callable

    [<AllowNullLiteral>]
    type ArrayConstructor =
        [<Emit "$0([None]*$1...)">]
        abstract Create: size: int -> 'T[]

        [<Emit "isinstance($1, list)">]
        abstract isArray: arg: obj -> bool

        abstract from: arg: obj -> 'T[]

    and [<AllowNullLiteral>] ArrayBuffer =
        abstract byteLength: int

        [<Emit("$0[$1:$1+$2]")>]
        abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer

    [<Emit("list")>]
    let Array: ArrayConstructor = nativeOnly

    // Hack because currently Fable doesn't keep information about spread for anonymous functions
    [<Emit("lambda *args: $0(args)")>]
    let argsFunc (fn: obj[] -> obj) : Callable = nativeOnly

    /// Defines a Jupyter-like code cell. Translates to `# %%`
    /// https://code.visualstudio.com/docs/python/jupyter-support-py
    [<Emit("# %%", isStatement = true)>]
    let NEW_CELL: unit = nativeOnly

    /// Embeds literal Python code into F#. Code will be printed as statements,
    /// if you want to return a value use Python `return` keyword within a function.
    let python (template: string) : 'T = nativeOnly
