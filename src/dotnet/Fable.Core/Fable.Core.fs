namespace Fable.Core
open System

#nowarn "1204"

[<AutoOpen>]
module Exceptions =
    /// Used to indicate that a member is only implemented in native Javascript
    let jsNative<'T> : 'T = failwith "JS only"

type CaseRules =
    | None = 0
    | LowerFirst = 1

/// Used for erased union types and to ignore modules in JS compilation.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type EraseAttribute() =
    inherit Attribute()

/// The module, type, function... is globally accessible in JS.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type GlobalAttribute() =
    inherit Attribute()
    new (name: string) = GlobalAttribute()

/// References to the module, type, function... will be replaced by import statements.
/// Use `[<Import("default", "my-package")>] to import the default member.
/// Use `[<Import("*", "my-package")>] to import the whole package.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type ImportAttribute(selector: string, from: string) =
    inherit Attribute()

/// ATTENTION: Use only in the last file of a project to export a member as default for JS consumption.
type ExportDefaultAttribute() =
    inherit Attribute()

/// LambdaType calls will be replaced by inlined JS code.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type EmitAttribute(macro: string) =
    inherit Attribute()

[<Obsolete("PassGenerics doesn't work in Fable 2, please remove the attribute.")>]
[<AttributeUsage(AttributeTargets.Method)>]
type PassGenericsAttribute() =
    inherit Attribute()

[<Obsolete("Records are compiled as pojos now by default, please remove the attribute.")>]
type PojoAttribute() =
    inherit Attribute()

/// Compile union types as string literals.
/// More info: http://fable.io/docs/interacting.html#StringEnum-attribute
[<AttributeUsage(AttributeTargets.Class)>]
type StringEnumAttribute() =
    inherit Attribute()
    new (caseRules: CaseRules) = StringEnumAttribute()

/// Used to spread the last argument. Mainly intended for `React.createElement` binding, not for general use.
/// Fable 1 only accepted lists, but Fable 2 accepts seq as well.
[<AttributeUsage(AttributeTargets.Parameter)>]
type ParamListAttribute() =
    inherit Attribute()

/// Experimental: Currently only intended for some specific libraries
[<AttributeUsage(AttributeTargets.Parameter)>]
type InjectAttribute() =
    inherit Attribute()

type ITypeResolver<'T> =
    abstract ResolveType: unit -> Type

/// Erased union type to represent one of two possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U2<'a, 'b> =
    | Case1 of 'a
    | Case2 of 'b
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x

/// Erased union type to represent one of three possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U3<'a, 'b, 'c> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x

/// Erased union type to represent one of four possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U4<'a, 'b, 'c, 'd> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x
    static member op_ErasedCast(x:'d) = Case4 x

/// Erased union type to represent one of five possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U5<'a, 'b, 'c, 'd, 'e> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x
    static member op_ErasedCast(x:'d) = Case4 x
    static member op_ErasedCast(x:'e) = Case5 x

/// Erased union type to represent one of six possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U6<'a, 'b, 'c, 'd, 'e, 'f> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x
    static member op_ErasedCast(x:'d) = Case4 x
    static member op_ErasedCast(x:'e) = Case5 x
    static member op_ErasedCast(x:'f) = Case6 x

/// Erased union type to represent one of seven possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U7<'a, 'b, 'c, 'd, 'e, 'f, 'g> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    | Case7 of 'g
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x
    static member op_ErasedCast(x:'d) = Case4 x
    static member op_ErasedCast(x:'e) = Case5 x
    static member op_ErasedCast(x:'f) = Case6 x
    static member op_ErasedCast(x:'g) = Case7 x

/// Erased union type to represent one of eight possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U8<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
    | Case4 of 'd
    | Case5 of 'e
    | Case6 of 'f
    | Case7 of 'g
    | Case8 of 'h
    static member op_ErasedCast(x:'a) = Case1 x
    static member op_ErasedCast(x:'b) = Case2 x
    static member op_ErasedCast(x:'c) = Case3 x
    static member op_ErasedCast(x:'d) = Case4 x
    static member op_ErasedCast(x:'e) = Case5 x
    static member op_ErasedCast(x:'f) = Case6 x
    static member op_ErasedCast(x:'g) = Case7 x
    static member op_ErasedCast(x:'h) = Case8 x

module Testing =
    type Assert =
        static member AreEqual(expected: 'T, actual: 'T, ?msg: string): unit = jsNative

