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
    /// Adds Python decorators to generated classes or methods, enabling integration with Python
    /// frameworks like dataclasses, attrs, functools, Pydantic, and any other decorator-based
    /// libraries.
    /// </summary>
    /// <remarks>
    /// <para>The decorator is emitted verbatim. Use importFrom to specify the module to import from.</para>
    /// <para>Multiple [&lt;Decorate&gt;] attributes are applied in reverse order
    /// (bottom to top), following Python's standard decorator stacking behavior.</para>
    /// <para>Examples:</para>
    /// <para>[&lt;Decorate("dataclass", "dataclasses")&gt;] - imports dataclass from dataclasses</para>
    /// <para>[&lt;Decorate("lru_cache", importFrom="functools", parameters="maxsize=128")&gt;] - with import and parameters</para>
    /// <para>[&lt;Decorate("app.get(\"/\")")&gt;] - Local variable decorator with parameters (no import needed)</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method, AllowMultiple = true)>]
    type DecorateAttribute(decorator: string, importFrom: string, parameters: string) =
        inherit Attribute()
        /// Decorator without import or parameters
        new(decorator: string) = DecorateAttribute(decorator, "", "")
        /// Decorator with import but no parameters
        new(decorator: string, importFrom: string) = DecorateAttribute(decorator, importFrom, "")

    /// Decorator with all parameters
    /// <summary>
    /// Marks a static member to be emitted as a Python @classmethod instead of @staticmethod.
    /// </summary>
    /// <remarks>
    /// <para>Use this attribute on static members when you need the first parameter to receive
    /// the class (cls) instead of being a regular static method.</para>
    /// <para>This is commonly needed for Pydantic validators and other Python frameworks that
    /// require @classmethod decorators.</para>
    /// <para>Example:</para>
    /// <para>[&lt;Py.ClassMethod&gt;]</para>
    /// <para>static member validate_name(cls, v: string) = ...</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Method)>]
    type ClassMethodAttribute() =
        inherit Attribute()

    type ClassAttributeStyle =
        // Translates to properties with instance attributes backing
        | Properties = 0
        // Translates to class attributes
        | Attributes = 1

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

        new(style: ClassAttributeStyle) = ClassAttributes()

        new(style: ClassAttributeStyle, init: bool) = ClassAttributes()

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
