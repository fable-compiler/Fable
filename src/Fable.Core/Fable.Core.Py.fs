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

        abstract Decorate: fn: Callable * info: Reflection.MethodInfo -> Callable

    /// <summary>
    /// Used on a class to provide Python-specific control over how F# types are transpiled to Python classes.
    /// This attribute implies member attachment (similar to AttachMembers) while offering Python-specific parameters.
    /// </summary>
    /// <remarks>
    /// <para>When placed on a class, all members are attached without mangling (like AttachMembers).</para>
    /// <para>Additional Python-specific parameters control the generated Python class style and features.</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class)>]
    type ClassAttributes() =
        inherit Attribute()

        new(style: string) = ClassAttributes()

        new(style: string, init: bool) = ClassAttributes()

        new(style: string, init: bool, slots: bool) = ClassAttributes()

        new(style: string, init: bool, slots: bool, frozen: bool) = ClassAttributes()

        new(style: string, init: bool, slots: bool, frozen: bool, repr: bool) = ClassAttributes()

        new(style: string, init: bool, slots: bool, frozen: bool, repr: bool, eq: bool) = ClassAttributes()

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
