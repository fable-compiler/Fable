namespace Fable.Core

open System

/// <summary>
/// Specifies case transformation rules.
/// </summary>
type CaseRules =
    /// <summary>
    /// No transformation is applied.
    /// <example>
    /// <c>FooBar</c> -> <c>FooBar</c>
    /// </example>
    /// </summary>
    | None = 0
    /// <summary>
    /// First character is lowercased.
    /// <example>
    /// <c>FooBar</c> -> <c>fooBar</c>
    /// </example>
    /// </summary>
    | LowerFirst = 1
    /// <summary>
    /// Underscores are used as a separator. All characters are lowercased.
    /// </summary>
    /// <example>
    /// <c>FooBar</c> -> <c>foo_bar</c>
    /// </example>
    | SnakeCase = 2
    /// <summary>
    /// Underscores are used as a separator. All characters are uppercased.
    /// </summary>
    /// <example>
    /// <c>FooBar</c> -> <c>FOO_BAR</c>
    /// </example>
    | SnakeCaseAllCaps = 3
    /// <summary>
    /// Hyphens are used as a separator. All characters are lowercased.
    /// </summary>
    /// <example>
    /// <c>FooBar</c> -> <c>foo-bar</c>
    /// </example>
    | KebabCase = 4
    /// <summary>
    /// All characters are lowercased.
    /// </summary>
    /// <example>
    /// <c>FooBar</c> -> <c>foobar</c>
    /// </example>
    | LowerAll = 5

/// <summary>
/// Used on interfaces to mangle member names. This allows overloading and prevents conflicts with
/// other interfaces, but will make interop with native code more difficult.
/// </summary>
type MangleAttribute(mangle: bool) =
    inherit Attribute()
    new() = MangleAttribute(true)

/// <summary>
/// Used on a class to attach all members without mangling.
/// </summary>
/// <remarks>
/// Be aware that overloads won't work.
/// </remarks>
[<AttributeUsage(AttributeTargets.Class)>]
type AttachMembersAttribute() =
    inherit Attribute()

/// <summary>
/// Used for erased union types, erasing default constructors, and to ignore modules in Fable compilation.
/// </summary>
/// <remarks>
/// <para>When placed on unions, the union code is erased, leaving the underlying fields as raw values</para>
/// <para>When used on <c>class</c> type declarations, member bindings etc, it will erase any associated
/// target code relating to the construction, reflection, and the definition itself.</para>
/// </remarks>
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
/// Accepts <c>Enum</c>, <c>int</c>, <c>float</c> or <c>bool</c>.
/// </remarks>
type CompiledValueAttribute private () =
    inherit Attribute()
    new(value: int) = CompiledValueAttribute()
    new(value: float) = CompiledValueAttribute()
    new(value: bool) = CompiledValueAttribute()
    new(value: Enum) = CompiledValueAttribute()

/// <summary>
/// The value (module, type, function...) is globally accessible in the target runtime.
/// </summary>
/// <remarks>
/// Global accessibility means that no imports are required to use the decorated value
/// in the target runtime.
/// </remarks>
type GlobalAttribute() =
    inherit Attribute()
    new(name: string) = GlobalAttribute()

/// <summary>
/// References to the module, type, function... will be replaced by import statements.
/// </summary>
/// <remarks>
/// Example selectors:
/// <para><c>[&lt;Import("default", "my-package")>]</c></para>
/// <para><c>[&lt;Import("*", "my-package")>]</c></para>
/// <para><c>[&lt;Import("nested.import", "my-package")>]</c></para>
/// </remarks>
type ImportAttribute(selector: string, from: string) =
    inherit Attribute()

/// <summary>
/// Imports the decorated value, by name, from the given source.
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
type ImportMemberAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// Same as <c>[&lt;Import("default", "my-package")>]</c>.
/// </summary>
type ImportDefaultAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// Same as <c>[&lt;Import("*", "my-package")>]</c>
/// </summary>
type ImportAllAttribute(from: string) =
    inherit Attribute()

/// <summary>
/// In supporting target languages, explicitly declares the decorated member
/// as the default export for the file.
/// </summary>
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
