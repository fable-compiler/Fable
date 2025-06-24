namespace Fable.Core

open System

/// <summary>
/// Parameter used in <c>StringEnum</c> and methods/other attributes that
/// generate strings from unions to change the casing in conversion.
/// </summary>
/// <remarks>
/// Conversion of <c>MouseOver</c> with different enums (the qualified access is
/// ignored for brevity):
/// <list type="table">
/// <item>
/// <term><c>None</c></term>
/// <term><c>"MouseOver"</c></term>
/// <description><c>"MouseOver"</c></description>
/// </item>
/// <item>
/// <term><c>LowerFirst</c></term>
/// <term><c>"mouseOver"</c></term>
/// <description><c>"mouseOver"</c></description>
/// </item>
/// <item>
/// <term><c>SnakeCase</c></term>
/// <term><c>"mouse_over"</c></term>
/// <description><c>"mouse_over"</c></description>
/// </item>
/// <item>
/// <term><c>SnakeCaseAllCaps</c></term>
/// <term><c>"MOUSE_OVER"</c></term>
/// <description><c>"MOUSE_OVER"</c></description>
/// </item>
/// <item>
/// <term><c>KebabCase</c></term>
/// <term><c>"mouse-over"</c></term>
/// <description><c>"mouse-over"</c></description>
/// </item>
/// <item>
/// <term><c>LowerAll</c></term>
/// <term><c>"mouseover"</c></term>
/// <description><c>"mouseover"</c></description>
/// </item>
/// </list>
/// You can also use <c>[&lt;CompiledName>]</c> and <c>[&lt;CompiledValue>]</c> to
/// specify the name or literal of the union case in the generated code:
/// <code lang="fsharp">
/// [&lt;StringEnum>]
/// type EventType =
///     | [&lt;CompiledName("Abracadabra")>] MouseOver
///     | [&lt;CompiledValue(false)>] RealMagic
/// let eventType = EventType.MouseOver
/// let magicPower = EventType.RealMagic
/// </code>
/// Generates:
/// <code lang="js">
/// // Pseudocode. See the Fable docs for your target for actual transpiled code.
/// eventType = "Abracadabra"
/// magicPower = false
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/javascript/features.html#caserules">
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
/// </para>
/// <code lang="js">
/// // Pseudocode
/// class Renderer
///     method Render
///         Error("Not implemented")
///
///     method Render(indentation)
///         Error("Not implemented")
/// </code>
/// <para>
/// Using <c>Mangle</c>, we would instead generate:
/// </para>
/// <code lang="js">
/// // Pseudocode
/// class Renderer
///     "Program.IRenderer.Render"()
///         Error("Not implemented")
///
///     "Program.IRenderer.RenderZ524259A4"(indentation)
///         Error("Not implemented")
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/javascript/features.html#mangle">
/// Fable documentation
/// </seealso>
type MangleAttribute(mangle: bool) =
    inherit Attribute()
    new() = MangleAttribute(true)

/// Used on a class to attach all members, useful when you want to use the class from JS.
[<AttributeUsage(AttributeTargets.Class)>]
type AttachMembersAttribute() =
    inherit Attribute()

/// Used for erased union types and to ignore modules in JS compilation.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#erase-attribute
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
/// <para>
/// This is typically used in conjunction with attributes such as <c>StringEnum</c>
/// </para>
/// </remarks>
/// <example>
/// <code lang="fsharp">
/// [&lt;StringEnum>]
/// type EventType =
///     | [&lt;CompiledValue(false)>] Absent
/// let eventType = EventType.Absent
/// </code>
/// Generates:
/// <code lang="js">
/// export const eventType = false;
/// </code>
/// </example>
type CompiledValueAttribute private () =
    inherit Attribute()
    new(value: int) = CompiledValueAttribute()
    new(value: float) = CompiledValueAttribute()
    new(value: bool) = CompiledValueAttribute()
    new(value: Enum) = CompiledValueAttribute()

/// The module, type, function... is globally accessible in JS.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#type-safety-with-imports-and-interfaces
type GlobalAttribute() =
    inherit Attribute()
    new(name: string) = GlobalAttribute()

/// References to the module, type, function... will be replaced by import statements.
/// Use `[<Import("default", "my-package")>] to import the default member.
/// Use `[<Import("*", "my-package")>] to import the whole package.
/// More info: https://fable.io/docs/communicate/js-from-fable.html#type-safety-with-imports-and-interfaces
type ImportAttribute(selector: string, from: string) =
    inherit Attribute()

/// Takes the member name from the value it decorates
type ImportMemberAttribute(from: string) =
    inherit Attribute()

/// Same as `Import("default", "my-package")`
type ImportDefaultAttribute(from: string) =
    inherit Attribute()

/// Same as `Import("*", "my-package")`
type ImportAllAttribute(from: string) =
    inherit Attribute()

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
