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

/// References to the module, type, function... will be replaced by import statements.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type ImportAttribute(get: string, from: string) =
    inherit Attribute()

/// Function calls will be replaced by inlined JS code.
/// More info: http://fable.io/docs/interacting.html#Import-attribute
type EmitAttribute private () =
    inherit Attribute()
    new (macro: string) = EmitAttribute()
    new (emitterType: Type, methodName: string) = EmitAttribute()

/// When this is attached to a method, Fable will add the generic info
/// as an extra argument to every call, making it possible to access
/// a type 'T with `typeof<'T>` within the method body
[<AttributeUsage(AttributeTargets.Method)>]
type PassGenericsAttribute() =
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
    /// Mostly convenient when used with createObj.
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
    let toPlainJsObj (o: 'T): obj = jsNative

    /// Serialize F# objects to JSON
    let toJson(o: 'T): string = jsNative

    /// Instantiate F# objects from JSON
    let [<PassGenerics>] ofJson<'T>(json: string): 'T = jsNative

    /// Serialize F# objects to JSON adding $type info
    let toJsonWithTypeInfo(o: 'T): string = jsNative

    /// Instantiate F# objects from JSON containing $type info
    let [<PassGenerics>] ofJsonWithTypeInfo<'T>(json: string): 'T = jsNative

    /// Converts a plain JS object (POJO) to an instance of the specified type
    let [<PassGenerics>] inflate<'T>(pojo: obj): 'T = jsNative

    /// Use it when importing a constructor from a JS library.
    ///
    /// ## Sample
    ///     type IFoo =
    ///         abstract foo: unit -> unit
    ///
    ///     let Foo: JsConstructor<IFoo> = importMember "../js/lib.js"
    ///     let x = Foo.Create("foo", "bar")
    ///     x.foo()
    type JsConstructor<'T> =
        [<Emit("new $0($1...)")>]
        abstract Create: [<ParamArray>] args: obj[] -> 'T

    /// Use it when you need an plain old JS function that doesn't capture
    /// the enclosing `this`. The first argument of the lambda becomes `this` in JS.
    ///
    /// ## Sample
    ///     let f = JsFunc2(fun (this: obj) (x: int) (y: int) -> this?add(x, y))
    ///
    ///     // JS
    ///     // var f = function(x, y) { return this.add(x, y) }
    type JsFunc0<'This,'Out>(f: 'This->'Out) =
        [<Emit("$0()")>]
        member __.Invoke(): 'Out = jsNative

    /// Use it when you need an plain old JS function that doesn't capture
    /// the enclosing `this`. The first argument of the lambda becomes `this` in JS.
    /// (See `JsFunc0` comments for an example.)
    type JsFunc1<'This,'Arg1,'Out>(f: 'This->'Arg1->'Out) =
        [<Emit("$0($1)")>]
        member __.Invoke(arg1:'Arg1): 'Out = jsNative

    /// Use it when you need an plain old JS function that doesn't capture
    /// the enclosing `this`. The first argument of the lambda becomes `this` in JS.
    /// (See `JsFunc0` comments for an example.)
    type JsFunc2<'This,'Arg1,'Arg2,'Out>(f: 'This->'Arg1->'Arg2->'Out) =
        [<Emit("$0($1,$2)")>]
        member __.Invoke(arg1:'Arg1, arg2:'Arg2): 'Out = jsNative

    /// Use it when you need an plain old JS function that doesn't capture
    /// the enclosing `this`. The first argument of the lambda becomes `this` in JS.
    /// (See `JsFunc0` comments for an example.)
    type JsFunc3<'This,'Arg1,'Arg2,'Arg3,'Out>(f: 'This->'Arg1->'Arg2->'Arg3->'Out) =
        [<Emit("$0($1,$2,$3)")>]
        member __.Invoke(arg1:'Arg1, arg2:'Arg2, arg3:'Arg3): 'Out = jsNative

    /// Use it when you need an plain old JS function that doesn't capture
    /// the enclosing `this`. The first argument of the lambda becomes `this` in JS.
    /// (See `JsFunc0` comments for an example.)
    type JsFunc4<'This,'Arg1,'Arg2,'Arg3,'Arg4,'Out>(f: 'This->'Arg1->'Arg2->'Arg3->'Arg4->'Out) =
        [<Emit("$0($1,$2,$3,$4)")>]
        member __.Invoke(arg1:'Arg1, arg2:'Arg2, arg3:'Arg3, arg4:'Arg4): 'Out = jsNative

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
        static member AreEqual(x: 'T, y: 'T): unit = jsNative

