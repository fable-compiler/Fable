namespace Fable.Core

open System

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

/// Used on interfaces to mangle member names. This allows overloading and prevents conflicts
/// other interfaces, but will make interop with native code more difficult.
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

/// Used in place of `CompiledNameAttribute` if the target is not a string.
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

/// <summary>
/// Erased union type to represent one of two possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
[<Erase>]
type U2<'a, 'b> =
    | Case1 of 'a
    | Case2 of 'b

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member inline op_Implicit(x: 'a) : U2<'a, 'b> = Case1 x
    static member inline op_Implicit(x: 'b) : U2<'a, 'b> = Case2 x

/// <summary>
/// Erased union type to represent one of three possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>

[<Erase>]
type U3<'a, 'b, 'c> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c

    static member op_ErasedCast(x: 'a) = Case1 x
    static member op_ErasedCast(x: 'b) = Case2 x
    static member op_ErasedCast(x: 'c) = Case3 x
    static member inline op_Implicit(x: 'a) : U3<'a, 'b, 'c> = Case1 x
    static member inline op_Implicit(x: 'b) : U3<'a, 'b, 'c> = Case2 x
    static member inline op_Implicit(x: 'c) : U3<'a, 'b, 'c> = Case3 x

/// <summary>
/// Erased union type to represent one of four possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U4<'a, 'b, 'c, 'd> = Case1 x
    static member inline op_Implicit(x: 'b) : U4<'a, 'b, 'c, 'd> = Case2 x
    static member inline op_Implicit(x: 'c) : U4<'a, 'b, 'c, 'd> = Case3 x
    static member inline op_Implicit(x: 'd) : U4<'a, 'b, 'c, 'd> = Case4 x

/// <summary>
/// Erased union type to represent one of five possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U5<'a, 'b, 'c, 'd, 'e> = Case1 x
    static member inline op_Implicit(x: 'b) : U5<'a, 'b, 'c, 'd, 'e> = Case2 x
    static member inline op_Implicit(x: 'c) : U5<'a, 'b, 'c, 'd, 'e> = Case3 x
    static member inline op_Implicit(x: 'd) : U5<'a, 'b, 'c, 'd, 'e> = Case4 x
    static member inline op_Implicit(x: 'e) : U5<'a, 'b, 'c, 'd, 'e> = Case5 x

/// <summary>
/// Erased union type to represent one of six possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case1 x
    static member inline op_Implicit(x: 'b) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case2 x
    static member inline op_Implicit(x: 'c) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case3 x
    static member inline op_Implicit(x: 'd) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case4 x
    static member inline op_Implicit(x: 'e) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case5 x
    static member inline op_Implicit(x: 'f) : U6<'a, 'b, 'c, 'd, 'e, 'f> = Case6 x

/// <summary>
/// Erased union type to represent one of seven possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case1 x
    static member inline op_Implicit(x: 'b) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case2 x
    static member inline op_Implicit(x: 'c) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case3 x
    static member inline op_Implicit(x: 'd) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case4 x
    static member inline op_Implicit(x: 'e) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case5 x
    static member inline op_Implicit(x: 'f) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case6 x
    static member inline op_Implicit(x: 'g) : U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> = Case7 x

/// <summary>
/// Erased union type to represent one of eight possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case1 x
    static member inline op_Implicit(x: 'b) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case2 x
    static member inline op_Implicit(x: 'c) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case3 x
    static member inline op_Implicit(x: 'd) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case4 x
    static member inline op_Implicit(x: 'e) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case5 x
    static member inline op_Implicit(x: 'f) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case6 x
    static member inline op_Implicit(x: 'g) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case7 x
    static member inline op_Implicit(x: 'h) : U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> = Case8 x

/// <summary>
/// Erased union type to represent one of nine possible value types mainly intended for typing the signature of imported
/// JS functions.
/// </summary>
/// <remarks>
/// Pattern matching is possible, but should consider the implications of the Erased union and JS type testing (see the
/// docs for details).
/// <br/>
/// Member concrete types will be implicitly cast into the union, and will provide a warning to this effect. Usage of
/// the explicit cast operator <c>!^</c> available in <c>Fable.Core.JsInterop</c> will remove this warning.
/// <a href="https://github.com/fable-compiler/Fable/pull/4143">Collection types, can provide an error</a> that will
/// only be resolved with the explicit operator. <a href="https://github.com/glutinum-org/cli/issues/80">Anonymous
/// records have other considerations that may be relevant if you are encountering issues.</a>
/// <code lang="fsharp">
/// let test(arg: U3&lt;string, int, float[]>) =
///     match arg with
///     | U3.Case1 x -> printfn "A string %s" x
///     | U3.Case2 x -> printfn "An int %i" x
///     | U3.Case3 xs -> Array.sum xs |> printfn "An array with sum %f"
/// </code>
/// </remarks>
/// <seealso href="https://fable.io/docs/communicate/js-from-fable.html#erase-attribute"/>
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
    static member inline op_Implicit(x: 'a) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case1 x
    static member inline op_Implicit(x: 'b) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case2 x
    static member inline op_Implicit(x: 'c) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case3 x
    static member inline op_Implicit(x: 'd) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case4 x
    static member inline op_Implicit(x: 'e) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case5 x
    static member inline op_Implicit(x: 'f) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case6 x
    static member inline op_Implicit(x: 'g) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case7 x
    static member inline op_Implicit(x: 'h) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case8 x
    static member inline op_Implicit(x: 'i) : U9<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i> = Case9 x

    static member inline op_ErasedCast(x: 't) : U9<_, _, _, _, _, _, _, _, ^U> =
        Case9(^U: (static member op_ErasedCast: 't -> ^U) x)
