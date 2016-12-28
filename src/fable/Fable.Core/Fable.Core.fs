namespace Fable.Core
open System

[<AutoOpen>]
module Exceptions =
    /// Used to indicate that a member is only implemented in native Javascript
    let jsNative<'T> : 'T = failwith "JS only"

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

/// Function calls will be replaced by inlined JS code.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type EmitAttribute private () =
    inherit Attribute()
    new (macro: string) = EmitAttribute()
    new (emitterType: Type, methodName: string) = EmitAttribute()
    new (emitterType: Type, methodName: string, extraArg: string) = EmitAttribute()

/// When this is attached to a method, Fable will add the generic info
/// as an extra argument to every call, making it possible to access
/// a type 'T with `typeof<'T>` within the method body
[<AttributeUsage(AttributeTargets.Method)>]
type PassGenericsAttribute() =
    inherit Attribute()

/// Compile a record as a JS object literals.
/// More info: http://fable.io/docs/interacting.html
type PojoAttribute() =
    inherit Attribute()

/// Compile union case lists as JS object literals.
/// More info: http://fable.io/docs/interacting.html#KeyValueList-attribute
[<AttributeUsage(AttributeTargets.Class)>]
type KeyValueListAttribute() =
    inherit Attribute()

/// Compile union types as string literals.
/// More info: http://fable.io/docs/interacting.html#StringEnum-attribute
[<AttributeUsage(AttributeTargets.Class)>]
type StringEnumAttribute() =
    inherit Attribute()

/// [EXPERIMENTAL] Record updates will be compiled as mutations: { x with a = 5 }
/// Fable will fail if the original value is used after being updated or passed to a function.
/// More info: http://fable.io/docs/interacting.html#MutatingUpdate-attribute
[<AttributeUsage(AttributeTargets.Class)>]
type MutatingUpdateAttribute() =
    inherit Attribute()

/// Erased union type to represent one of two possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U2<'a, 'b> = Case1 of 'a | Case2 of 'b

/// Erased union type to represent one of three possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U3<'a, 'b, 'c> = Case1 of 'a | Case2 of 'b | Case3 of 'c

/// Erased union type to represent one of four possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U4<'a, 'b, 'c, 'd> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd

/// Erased union type to represent one of five possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U5<'a, 'b, 'c, 'd, 'e> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e

/// Erased union type to represent one of six possible values.
/// More info: http://fable.io/docs/interacting.html#Erase-attribute
type [<Erase>] U6<'a, 'b, 'c, 'd, 'e, 'f> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e | Case6 of 'f

/// DO NOT USE: Internal type for Fable dynamic operations
type Applicable = obj->obj

module JsInterop =
    /// Has same effect as `unbox` (dynamic casting erased in compiled JS code).
    /// The casted type can be defined on the call site: `!!myObj?bar(5): float`
    let (!!) x: 'T = jsNative

    /// Dynamically access a property of an arbitrary object.
    /// `myObj?propA` in JS becomes `myObj.propA`
    /// `myObj?(propA)` in JS becomes `myObj[propA]`
    let (?) (o: obj) (prop: obj): Applicable = jsNative

    /// Dynamically assign a value to a property of an arbitrary object.
    /// `myObj?propA <- 5` in JS becomes `myObj.propA = 5`
    /// `myObj?(propA) <- 5` in JS becomes `myObj[propA] = 5`
    let (?<-) (o: obj) (prop: obj) (v: obj): unit = jsNative

    /// Destructure and apply a tuple to an arbitrary value.
    /// E.g. `myFn $ (arg1, arg2)` in JS becomes `myFn(arg1, arg2)`
    let ($) (callee: obj) (args: obj): obj = jsNative

    /// Upcast the right operand to obj and create a key-value tuple.
    /// Mostly convenient when used with `createObj`.
    /// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
    let (==>) (key: string) (v: obj): string*obj = jsNative

    /// Destructure and apply a tuple to an arbitrary value with `new` keyword.
    /// E.g. `createNew myCons (arg1, arg2)` in JS becomes `new myCons(arg1, arg2)`
    let createNew (o: obj) (args: obj): obj = jsNative

    /// Create a literal JS object from a collection of key-value tuples.
    /// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
    let createObj (fields: #seq<string*obj>): obj = jsNative

    /// Create an empty JS object: {}
    let createEmpty<'T> : 'T = jsNative

    /// Works like `ImportAttribute` (same semantics as ES6 imports).
    /// You can use "*" or "default" selectors.
    let import<'T> (selector: string) (path: string):'T = jsNative

    /// F#: let myMember = importMember<string> "myModule"
    /// JS: import { myMember } from "myModule"
    /// Note the import must be immediately assigned to a value in a let binding
    let importMember<'T> (path: string):'T = jsNative

    /// F#: let defaultMember = importDefault<unit->obj> "myModule"
    /// JS: import defaultMember from "myModule"
    let importDefault<'T> (path: string):'T = jsNative

    /// F#: let myLib = importAll<obj> "myLib"
    /// JS: import * as myLib from "myLib"
    let importAll<'T> (path: string):'T = jsNative

    /// Convert F# unions, records and classes into plain JS objects
    /// DEPRECATED: Use a Pojo record
    let [<Obsolete>] toPlainJsObj (o: 'T): obj = jsNative

    /// Serialize F# objects to JSON
    let toJson(o: 'T): string = jsNative

    /// Instantiate F# objects from JSON
    let [<PassGenerics>] ofJson<'T>(json: string): 'T = jsNative

    /// Serialize F# objects to JSON adding $type info
    let toJsonWithTypeInfo(o: 'T): string = jsNative

    /// Instantiate F# objects from JSON containing $type info
    let [<PassGenerics>] ofJsonWithTypeInfo<'T>(json: string): 'T = jsNative

    /// Converts a plain JS object (POJO) to an instance of the specified type.
    /// This is only intended if you're using a custom serialization method
    /// (that must produce same objects as `toJson`) instead of `ofJson`.
    let [<PassGenerics>] inflate<'T>(pojo: obj): 'T = jsNative


    /// Compiles to JS `this` keyword. Can only be used within a delegate.
    ///
    /// ## Sample
    ///     let fn = JsFunc2(fun x y -> jsThis?add(x, y))
    let [<Emit("this")>] jsThis<'T> : 'T = jsNative

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor =
        [<Emit("new $0($1...)")>]
        abstract Create: [<ParamArray>]args: obj[] -> obj

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Out> =
        [<Emit("new $0()")>]
        abstract Create: unit->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Arg2,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1*'Arg2->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Arg2,'Arg3,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1*'Arg2*'Arg3->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Arg2,'Arg3,'Arg4,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1*'Arg2*'Arg3*'Arg4->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1*'Arg2*'Arg3*'Arg4*'Arg5->'Out

    /// Use it when importing a constructor from a JS library.
    type [<AllowNullLiteral>] JsConstructor<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Arg6,'Out> =
        [<Emit("new $0($1...)")>]
        abstract Create: 'Arg1*'Arg2*'Arg3*'Arg4*'Arg5*'Arg6->'Out

    /// Use it to cast dynamic functions coming from JS. If you know the argument
    /// and return types, use `System.Func<>` instead. If you need a constructor
    /// (must be applied with `new` keyword), use `JsConstructor`.
    ///
    /// ## Sample
    ///     let f: JsFunc = import "myFunction" "./myLib"
    ///     f.Invoke(5, "bar")
    type [<AllowNullLiteral>] JsFunc =
        [<Emit("$0($1...)")>]
        abstract Invoke: [<ParamArray>]args:obj[]->obj

    /// Same as `System.Func<unit,'Out>`
    type JsFunc0<'Out> = Func<unit,'Out>

    /// Same as `System.Func<'Arg1,'Out>`
    type JsFunc1<'Arg1,'Out> = Func<'Arg1,'Out>

    /// Same as `System.Func<'Arg1,'Arg2,'Out>`
    type JsFunc2<'Arg1,'Arg2,'Out> = Func<'Arg1,'Arg2,'Out>

    /// Same as `System.Func<'Arg1,'Arg2,'Arg3,'Out>`
    type JsFunc3<'Arg1,'Arg2,'Arg3,'Out> = Func<'Arg1,'Arg2,'Arg3,'Out>

    /// Same as `System.Func<'Arg1,'Arg2,'Arg3,'Arg4,'Out>`
    type JsFunc4<'Arg1,'Arg2,'Arg3,'Arg4,'Out> = Func<'Arg1,'Arg2,'Arg3,'Arg4,'Out>

    /// Same as `System.Func<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Out>`
    type JsFunc5<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Out> = Func<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Out>

    /// Same as `System.Func<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Arg6,'Out>`
    type JsFunc6<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Arg6,'Out> = Func<'Arg1,'Arg2,'Arg3,'Arg4,'Arg5,'Arg6,'Out>

module Testing =
    type TestAttribute() =
        inherit Attribute()

    type TestFixtureAttribute() =
        inherit Attribute()

    type TestFixtureSetUpAttribute() =
        inherit Attribute()

    type TestFixtureTearDownAttribute() =
        inherit Attribute()

    type SetUpAttribute() =
        inherit Attribute()

    type TearDownAttribute() =
        inherit Attribute()

    type Assert =
        static member AreEqual(expected: 'T, actual: 'T, ?msg: string): unit = jsNative

