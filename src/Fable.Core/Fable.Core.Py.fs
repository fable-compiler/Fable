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

    /// <summary>
    /// Base class for creating F#-side decorator attributes that transform functions at compile time.
    /// This is similar to Python decorators but operates during Fable compilation, not at Python runtime.
    /// </summary>
    /// <remarks>
    /// <para>Inherit from this class and implement the Decorate method to wrap or transform functions.</para>
    /// <para>The decorated function is passed to Decorate, and the returned Callable replaces it.</para>
    /// <para>Example:</para>
    /// <code>
    /// type LogAttribute() =
    ///     inherit Py.DecoratorAttribute()
    ///     override _.Decorate(fn) =
    ///         Py.argsFunc (fun args ->
    ///             printfn "Calling function"
    ///             fn.Invoke(args))
    /// </code>
    /// <para>Note: This does NOT emit Python @decorator syntax. For emitting Python decorators,
    /// use DecorateAttribute or DecorateTemplateAttribute instead.</para>
    /// </remarks>
    [<AbstractClass>]
    type DecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Callable -> Callable

    /// <summary>
    /// Base class for creating F#-side decorator attributes with access to reflection metadata.
    /// Like DecoratorAttribute, but the Decorate method also receives MethodInfo for the decorated member.
    /// </summary>
    /// <remarks>
    /// <para>Use this when your decorator needs information about the decorated method (name, parameters, etc.).</para>
    /// <para>Example:</para>
    /// <code>
    /// type LogWithNameAttribute() =
    ///     inherit Py.ReflectedDecoratorAttribute()
    ///     override _.Decorate(fn, info) =
    ///         Py.argsFunc (fun args ->
    ///             printfn "Calling %s" info.Name
    ///             fn.Invoke(args))
    /// </code>
    /// <para>Note: This does NOT emit Python @decorator syntax. For emitting Python decorators,
    /// use DecorateAttribute or DecorateTemplateAttribute instead.</para>
    /// </remarks>
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

    /// <summary>
    /// Marks a custom attribute class as a decorator template, enabling library authors to create
    /// ergonomic decorator attributes that users can apply without knowing the underlying Python syntax.
    /// </summary>
    /// <remarks>
    /// <para>Place this attribute on a custom attribute class. The template string uses {0}, {1}, etc.
    /// as placeholders for the custom attribute's constructor arguments.</para>
    /// <para>Example - defining a custom decorator attribute:</para>
    /// <code>
    /// [&lt;Erase; Py.DecorateTemplate("app.get('{0}')")&gt;]
    /// type GetAttribute(path: string) = inherit Attribute()
    /// </code>
    /// <para>Example - using the custom decorator:</para>
    /// <code>
    /// [&lt;Get("/users")&gt;]
    /// static member get_users() = ...
    /// // Generates: @app.get('/users')
    /// </code>
    /// <para>Use [&lt;Erase&gt;] to prevent the attribute type from being emitted to Python.</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class)>]
    type DecorateTemplateAttribute(template: string) =
        inherit Attribute()
        /// Template with import specification
        new(template: string, importFrom: string) = DecorateTemplateAttribute(template)

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

    /// <summary>
    /// Used on a class to provide Python-specific control over how F# types are transpiled to Python classes.
    /// This attribute implies member attachment (similar to AttachMembers) while offering Python-specific parameters.
    /// </summary>
    /// <remarks>
    /// <para>When placed on a class, all members are attached without mangling (like AttachMembers).</para>
    /// <para>Additional Python-specific parameters control the generated Python class style and features.</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class)>]
    type ClassAttributesAttribute() =
        inherit Attribute()

        new(style: ClassAttributeStyle) = ClassAttributesAttribute()

        new(style: ClassAttributeStyle, init: bool) = ClassAttributesAttribute()

    /// <summary>
    /// Marks a custom attribute class as a class attributes template, enabling library authors
    /// to create ergonomic class attributes that users can apply without knowing the underlying parameters.
    /// </summary>
    /// <remarks>
    /// <para>Place this attribute on a custom attribute class to define the class generation style.</para>
    /// <para>Example - defining a custom class attribute:</para>
    /// <code>
    /// [&lt;Erase; Py.ClassAttributesTemplate(Py.ClassAttributeStyle.Attributes, init = false)&gt;]
    /// type BaseModelAttribute() = inherit Attribute()
    /// </code>
    /// <para>Example - using the custom attribute:</para>
    /// <code>
    /// [&lt;BaseModel&gt;]
    /// type User(name: string, age: int) = ...
    /// // Generates class with class-level attributes, no __init__
    /// </code>
    /// <para>Use [&lt;Erase&gt;] to prevent the attribute type from being emitted to Python.</para>
    /// </remarks>
    [<AttributeUsage(AttributeTargets.Class)>]
    type ClassAttributesTemplateAttribute(style: ClassAttributeStyle, init: bool) =
        inherit Attribute()
        /// Template with Attributes style and init = false (common for Pydantic/dataclasses)
        new(style: ClassAttributeStyle) = ClassAttributesTemplateAttribute(style, false)

    /// <summary>
    /// Shorthand for [&lt;Py.ClassAttributes(style = Attributes, init = false)&gt;].
    /// Use this for Python dataclasses, Pydantic models, attrs classes, or any class
    /// that needs class-level type annotations without a generated __init__.
    /// </summary>
    /// <remarks>
    /// <para>Example:</para>
    /// <code>
    /// [&lt;Py.DataClass&gt;]
    /// type User(name: string, age: int) = ...
    /// // Generates:
    /// // class User:
    /// //     name: str
    /// //     age: int
    /// </code>
    /// </remarks>
    [<Erase>]
    [<ClassAttributesTemplate(ClassAttributeStyle.Attributes, false)>]
    type DataClassAttribute() =
        inherit Attribute()

    // Hack because currently Fable doesn't keep information about spread for anonymous functions
    [<Emit("lambda *args: $0(Array(args))")>]
    let argsFunc (fn: obj[] -> obj) : Callable = nativeOnly

    /// Defines a Jupyter-like code cell. Translates to `# %%`
    /// https://code.visualstudio.com/docs/python/jupyter-support-py
    [<Emit("# %%", isStatement = true)>]
    let NEW_CELL: unit = nativeOnly

    /// Embeds literal Python code into F#. Code will be printed as statements,
    /// if you want to return a value use Python `return` keyword within a function.
    let python (template: string) : 'T = nativeOnly

    /// <summary>
    /// Python Stringable marker interface for F# types that need __str__ and __repr__ in Python.
    /// The compiler maps this to StringableBase which provides __str__ and __repr__ from ToString.
    /// The class must override ToString() to provide the string representation.
    /// </summary>
    [<AllowNullLiteral>]
    type Stringable = interface end

    /// <summary>
    /// Python Equatable marker interface for F# types that need __eq__ in Python.
    /// The compiler maps this to EquatableBase which provides __eq__ from Equals.
    /// The class must override Equals(other: obj) to provide equality comparison.
    /// </summary>
    [<AllowNullLiteral>]
    type Equatable = interface end

    /// <summary>
    /// Python Comparable marker interface for F# types that need comparison operators in Python.
    /// The compiler maps this to ComparableBase which provides __lt__, __le__, __gt__, __ge__ from CompareTo.
    /// The class must implement CompareTo(other: obj) to provide comparison.
    /// </summary>
    [<AllowNullLiteral>]
    type Comparable = interface end

    /// <summary>
    /// Python Hashable marker interface for F# types that need __hash__ in Python.
    /// The compiler maps this to HashableBase which provides __hash__ from GetHashCode.
    /// The class must implement GetHashCode() to provide the hash value.
    /// Note: GetHashCode returns int32 while __hash__ returns int, so HashableBase handles the conversion.
    /// </summary>
    [<AllowNullLiteral>]
    type Hashable = interface end

    /// <summary>
    /// Python Sized marker interface for F# types that need __len__ in Python.
    /// The compiler maps this to SizedBase which provides __len__ from Count property.
    /// The class must have a Count property that returns the number of items.
    /// </summary>
    [<AllowNullLiteral>]
    type Sized = interface end

    /// <summary>
    /// Python Iterable interface for F# types that need to be iterable in Python.
    /// Follows Python's collections.abc.Iterable protocol.
    /// </summary>
    [<AllowNullLiteral>]
    type Iterable<'T> =
        /// Python __iter__ - returns iterator
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'T>

    /// <summary>
    /// Python Sequence interface for F# types that need to behave like Python sequences.
    /// Follows Python's collections.abc.Sequence protocol.
    /// </summary>
    [<AllowNullLiteral>]
    type Sequence<'T> =
        /// Python __getitem__(index) - returns item at index
        abstract ``__getitem__``: index: int -> 'T
        /// Python __len__ - returns number of items
        abstract ``__len__``: unit -> int
        /// Python __iter__ - returns iterator
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'T>
        /// Python __contains__(value) - checks if value exists
        abstract ``__contains__``: value: 'T -> bool

    /// <summary>
    /// Python Mapping interface for F# types that need to behave like Python read-only dicts.
    /// Follows Python's collections.abc.Mapping protocol.
    /// </summary>
    [<AllowNullLiteral>]
    type Mapping<'K, 'V> =
        /// Python __getitem__(key) - returns value for key
        abstract ``__getitem__``: key: 'K -> 'V
        /// Python __len__ - returns number of items
        abstract ``__len__``: unit -> int
        /// Python __iter__ - returns iterator over keys
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'K>
        /// Python __contains__(key) - checks if key exists
        abstract ``__contains__``: key: 'K -> bool
        /// Python keys() - returns keys view
        abstract keys: unit -> seq<'K>
        /// Python values() - returns values view
        abstract values: unit -> seq<'V>
        /// Python items() - returns items view
        abstract items: unit -> seq<'K * 'V>
        /// Python get(key, default) - returns value or default
        abstract get: key: 'K * ?defaultValue: 'V -> 'V

    /// <summary>
    /// Python MutableMapping interface for F# types that need to behave like Python dicts.
    /// Follows Python's collections.abc.MutableMapping protocol.
    /// The compiler will transform method names to Python dunder methods.
    /// </summary>
    [<AllowNullLiteral>]
    type MutableMapping<'K, 'V> =
        // Required abstract methods from MutableMapping (compiler maps these to dunder methods)
        /// Python __getitem__(key) - returns value for key
        abstract ``__getitem__``: key: 'K -> 'V
        /// Python __setitem__(key, value) - sets value for key
        abstract ``__setitem__``: key: 'K * value: 'V -> unit
        /// Python __delitem__(key) - deletes key
        abstract ``__delitem__``: key: 'K -> unit
        /// Python __iter__ - returns iterator over keys
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'K>
        /// Python __len__ - returns number of items
        abstract ``__len__``: unit -> int
        // Concrete methods from Mapping
        /// Python __contains__(key) - checks if key exists
        abstract ``__contains__``: key: 'K -> bool
        /// Python keys() - returns keys view
        abstract keys: unit -> seq<'K>
        /// Python values() - returns values view
        abstract values: unit -> seq<'V>
        /// Python items() - returns items view
        abstract items: unit -> seq<'K * 'V>
        /// Python get(key, default) - returns value or default
        abstract get: key: 'K * ?defaultValue: 'V -> 'V
        // Mixin methods from MutableMapping
        /// Python clear() - removes all items
        abstract clear: unit -> unit
        /// Python pop(key, default) - removes and returns value
        abstract pop: key: 'K * ?defaultValue: 'V -> 'V
        /// Python popitem() - removes and returns an arbitrary (key, value) pair
        abstract popitem: unit -> 'K * 'V

    /// <summary>
    /// Python Set interface for F# types that need to behave like Python read-only sets.
    /// Follows Python's collections.abc.Set protocol (frozenset-like).
    /// </summary>
    [<AllowNullLiteral>]
    type Set<'T> =
        /// Python __contains__(value) - checks if value exists
        abstract ``__contains__``: value: 'T -> bool
        /// Python __iter__ - returns iterator
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'T>
        /// Python __len__ - returns number of items
        abstract ``__len__``: unit -> int

    /// <summary>
    /// Python MutableSet interface for F# types that need to behave like Python sets.
    /// Follows Python's collections.abc.MutableSet protocol.
    /// The compiler will transform method names to Python dunder methods.
    /// </summary>
    [<AllowNullLiteral>]
    type MutableSet<'T> =
        /// Python __contains__(value) - checks if value exists
        abstract ``__contains__``: value: 'T -> bool
        /// Python __iter__ - returns iterator
        abstract ``__iter__``: unit -> System.Collections.Generic.IEnumerator<'T>
        /// Python __len__ - returns number of items
        abstract ``__len__``: unit -> int
        /// Python add(value) - adds value to set
        abstract add: value: 'T -> unit
        /// Python discard(value) - removes value if present
        abstract discard: value: 'T -> unit
        // Mixin methods
        /// Python clear() - removes all items
        abstract clear: unit -> unit
        /// Python remove(value) - removes value, raises KeyError if not present
        abstract remove: value: 'T -> unit
        /// Python pop() - removes and returns an arbitrary element
        abstract pop: unit -> 'T
