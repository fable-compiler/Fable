namespace Fable.Core

open System

/// <summary>
/// Parameter used in <c>StringEnum</c> and methods/other attributes that
/// generate strings from unions or other identifiers to change the casing in conversion.
/// </summary>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type CaseRules =
    | None = 0
    /// FooBar -> fooBar
    | LowerFirst = 1
    /// FooBar -> foo_bar
    | SnakeCase = 2
    /// FooBar -> FOO_BAR
    | SnakeCaseAllCaps = 3
    /// FooBar -> foo-bar
    | KebabCase = 4
    /// FooBar -> foobar
    | LowerAll = 5

/// <summary>
/// Used on interfaces to mangle member names. This allows overloading and prevents conflicts
/// other interfaces, but will make interop with native code more difficult.
/// </summary>
/// <remarks>
/// <para>
/// If you are not planning to use an interface to interact with native code and want to have overloaded
/// members, you can decorate the interface declaration with the <c>Mangle</c> attribute.
/// </para>
/// <para>
/// Interface coming from .NET BCL (like System.Collections.IEnumerator) are mangled by default.
/// </para>
/// <para>
/// For example the following code:
/// </para>
/// <code lang="fsharp">
/// type IRenderer =
///     abstract Render : unit -> string
///     abstract Render : indentation : int -> string
///
/// type Renderer() =
///     interface IRenderer with
///         member this.Render() =
///             failwith "Not implemented"
///
///         member this.Render(indentation) =
///             failwith "Not implemented"
/// </code>
/// <para>
/// May be considered invalid code depending on the target. Note the two methods named <c>Render</c> in the same class.
/// This would be a primary example case where <c>Mangle</c> would allow F#/Fable to route the call correctly.
/// </para>
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable documentation
/// </seealso>
type MangleAttribute(mangle: bool) =
    inherit Attribute()
    new() = MangleAttribute(true)

/// <summary>
/// Used on a class to attach all members without mangling.
/// </summary>
/// <remarks>
/// If you want to have all members attached to a class (as is standard with most target languages)
/// and not-mangled, use the <c>AttachMembers</c> attribute. But be aware that overloads won't work
/// in this case.
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
[<AttributeUsage(AttributeTargets.Class)>]
type AttachMembersAttribute() =
    inherit Attribute()

/// <summary>
/// Used for erased union types, erasing default constructors, and to ignore modules in Fable compilation.
/// </summary>
/// <remarks>
/// <para>When placed on unions, the union code is erased, leaving the underlying fields as raw values</para>
/// <code lang="fsharp">
/// [&lt;Erase>]
/// type ValueType =
///     | Number of int
///     | String of string
///     | Object of obj
/// </code>
/// When used on <c>class</c> type declarations, member bindings et al, it will erase any associated
/// target code relating to the construction, reflection, and the definition
/// itself. Be aware that Fable will not stop you from making calls to these
/// functions which may be undefined without replacement.
/// <code lang="fsharp">
/// type AnimationBuilder() = class end
/// let anime = AnimationBuilder()
/// [&lt;Erase>]
/// type TimelineBuilder() = class end
/// let tl = TimelineBuilder()
/// // would fail on runtime as we have erased the constructor from
/// // the compiled code
/// </code>
/// Similar behavior is seen when used on <c>module</c>s. Check the <a href="https://fable.io/docs">
/// documentation for your target language</a> for more information.
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type EraseAttribute() =
    inherit Attribute()
    new(caseRules: CaseRules) = EraseAttribute()

/// Used for "tagged" union types, which is commonly used in TypeScript.
type TypeScriptTaggedUnionAttribute(tagName: string, caseRules: CaseRules) =
    inherit Attribute()

    new(tagName: string) = TypeScriptTaggedUnionAttribute(tagName, CaseRules.LowerFirst)

/// <summary>
/// Used in place of <c>CompiledNameAttribute</c> if the target is not a string.
/// </summary>
/// <remarks>
/// Accepts <c>Enum</c>s, <c>int</c>, <c>float</c> or <c>bool</c>.
/// <br/>
/// You can also generate <c>undefined</c> by passing <c>null</c>.
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type CompiledValueAttribute private () =
    inherit Attribute()
    new(value: int) = CompiledValueAttribute()
    new(value: float) = CompiledValueAttribute()
    new(value: bool) = CompiledValueAttribute()
    new(value: Enum) = CompiledValueAttribute()

/// <summary>
/// The module, type, function... is globally accessible in the target runtime.
/// </summary>
/// <remarks>
/// Consider a function such as <c>printf</c> in <c>F#</c>, this function is accessible
/// without any import or declaration. You would decorate a binding for a target construct
/// using <c>Global</c> to prevent any code being compiled for the declaration, and any
/// import-like code being generated in usage in other <c>Fable</c> code.
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type GlobalAttribute() =
    inherit Attribute()
    new(name: string) = GlobalAttribute()

/// <summary>
/// References to the module, type, function... will be replaced by import statements.
/// </summary>
/// <remarks>
/// <para>Use <c>[&lt;Import("default", "my-package")>]</c> to import the default member.</para>
/// <para>Use <c>[&lt;Import("*", "my-package")>]</c> to import the whole package.</para>
/// <para>You can import proxy-like constructs, or nested submodules as you would natively.</para>
/// </remarks>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type ImportAttribute(selector: string, from: string) =
    inherit Attribute()

/// <summary>
/// Similar (or same) to <c>Import</c>, except the <c>selector</c> parameter is automatically filled with the
/// decorated members name.
/// </summary>
/// <example>
/// <code lang="fsharp">
/// [&lt;Import("Test","TestLibrary")>]
/// let test value = value
/// // SAME AS
/// [&lt;ImportMember("TestLibrary")>]
/// let Test value = value
/// </code>
/// </example>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type ImportMemberAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// Same as <c>[&lt;Import("default", "my-package")>]</c>.
/// </summary>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type ImportDefaultAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// Same as <c>[&lt;Import("*", "my-package")>]</c>
/// </summary>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type ImportAllAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// In supporting target languages, explicitly declares the decorated member
/// as the default export for the file.
/// </summary>
/// <seealso href="https://fable.io/docs/">
/// Fable Documentation
/// </seealso>
type ExportDefaultAttribute() =
    inherit Attribute()

/// Function calls will be replaced by inlined JS code.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#emit-when-f-is-not-enough
type EmitAttribute(macro: string, isStatement: bool) =
    inherit Attribute()
    new(macro: string) = EmitAttribute(macro, isStatement = false)

/// Same as `Emit("$0.methodName($1...)")`
type EmitMethodAttribute(methodName: string) =
    inherit Attribute()

/// Same as `Emit("new $0($1...)")`
type EmitConstructorAttribute() =
    inherit Attribute()

/// Same as `Emit("$0[$1]{{=$2}}")`
type EmitIndexerAttribute() =
    inherit Attribute()

/// Same as `Emit("$0.propertyName{{=$1}}")`
type EmitPropertyAttribute(propertyName: string) =
    inherit Attribute()

/// Compile union types as string literals.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#stringenum-attribute
[<AttributeUsage(AttributeTargets.Class)>]
type StringEnumAttribute(caseRules: CaseRules) =
    inherit Attribute()
    new() = StringEnumAttribute(CaseRules.LowerFirst)

/// Used to spread the last argument. Mainly intended for `React.createElement` binding, not for general use.
[<AttributeUsage(AttributeTargets.Parameter)>]
type ParamListAttribute() =
    inherit Attribute()

type ParamSeqAttribute = ParamListAttribute

/// Converts arguments from the specified index on (0 by default) into an object.
/// IMPORTANT: This should be used only with native bindings.
[<AttributeUsage(AttributeTargets.Constructor ||| AttributeTargets.Method)>]
type ParamObjectAttribute(fromIndex: int) =
    inherit Attribute()
    new() = ParamObjectAttribute(0)

/// Alias for ParamObjectAttribute.
type NamedParamsAttribute = ParamObjectAttribute

/// Experimental: Currently only intended for some specific libraries
[<AttributeUsage(AttributeTargets.Parameter)>]
type InjectAttribute() =
    inherit Attribute()

/// Erased union type to represent one of two possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U2<'a, 'b> =
    | Case1 of 'a
    | Case2 of 'b

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x

/// Erased union type to represent one of three possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U3<'a, 'b, 'c> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x

/// Erased union type to represent one of four possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U4<'a, 'b, 'c, 'd> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x

/// Erased union type to represent one of five possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U5<'a, 'b, 'c, 'd, 'e> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x
    static member op_ErasedCast(x: 'e) = Case5 x

/// Erased union type to represent one of six possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U6<'a, 'b, 'c, 'd, 'e, 'f> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x
    static member op_ErasedCast(x: 'e) = Case5 x
    static member op_ErasedCast(x: 'f) = Case6 x

/// Erased union type to represent one of seven possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    | Case7 of 'g

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x
    static member op_ErasedCast(x: 'e) = Case5 x
    static member op_ErasedCast(x: 'f) = Case6 x
    static member op_ErasedCast(x: 'g) = Case7 x

/// Erased union type to represent one of eight possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    | Case7 of 'g
    | Case8 of 'h

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x
    static member op_ErasedCast(x: 'e) = Case5 x
    static member op_ErasedCast(x: 'f) = Case6 x
    static member op_ErasedCast(x: 'g) = Case7 x
    static member op_ErasedCast(x: 'h) = Case8 x

/// Erased union type to represent one of nine or more possible values.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
[<Erase>]
type U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    | Case7 of 'g
    | Case8 of 'h
    | Case9 of 'i

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member op_ErasedCast(x: 'd) = Case4 x
    static member op_ErasedCast(x: 'e) = Case5 x
    static member op_ErasedCast(x: 'f) = Case6 x
    static member op_ErasedCast(x: 'g) = Case7 x
    static member op_ErasedCast(x: 'h) = Case8 x
    static member op_ErasedCast(x: 'i) = Case9 x

    static member inline op_ErasedCast(x: 't) : U9<_, _, _, _, _, _, _, _, ^U> =
        Case9(^U: (static member op_ErasedCast: 't -> ^U) x)
