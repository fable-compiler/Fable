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
    /// Adds Python decorators to generated classes, enabling integration with Python
    /// frameworks like dataclasses, attrs, functools, and any other decorator-based
    /// libraries.
    /// </summary>
    /// <remarks>
    /// <para>The [&lt;Decorate&gt;] attribute is purely for Python interop and does NOT
    /// affect F# compilation behavior.</para>
    /// <para>Multiple [&lt;Decorate&gt;] attributes are applied in reverse order
    /// (bottom to top), following Python's standard decorator stacking behavior.</para>
    /// <para>Examples:</para>
    /// <para>[&lt;Decorate("dataclasses.dataclass")&gt;] - Simple decorator</para>
    /// <para>[&lt;Decorate("functools.lru_cache", "maxsize=128")&gt;] - Decorator with
    /// parameters</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class, AllowMultiple = true)>]
    type DecorateAttribute(decorator: string) =
        inherit Attribute()

        new(decorator: string, parameters: string) = DecorateAttribute(decorator)

        member val Decorator: string = decorator with get, set
        member val Parameters: string = "" with get, set

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
