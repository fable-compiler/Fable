module Fable.Core.JsInterop

open System
open Fable.Core
open Fable.Import

/// Has same effect as `unbox` (dynamic casting erased in compiled JS code).
/// The casted type can be defined on the call site: `!!myObj?bar(5): float`
let (!!) x: 'T = jsNative

// Implicit cast for erased types
let inline (!^) (x:^t1) : ^t2 = ((^t1 or ^t2) : (static member op_ErasedCast : ^t1 -> ^t2) x)

/// Dynamically access a property of an arbitrary object.
/// `myObj?propA` in JS becomes `myObj.propA`
/// `myObj?(propA)` in JS becomes `myObj[propA]`
let (?) (o: obj) (prop: obj): DynamicApplicable = jsNative

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

/// Create a literal JS object from a collection of union constructors.
/// E.g. `keyValueList  [ MyUnion 4 ]` in JS becomes `{ myUnion: 4 }`
let keyValueList (caseRule: CaseRules) (li: 'T list): obj = jsNative

/// Create a literal JS object from a mutator lambda. Normally used when
/// the options interface has too many fields to be represented with a Pojo record.
/// E.g. `jsOptions<MyOpt> (fun o -> o.foo <- 5)` in JS becomes `{ foo: 5 }`
let jsOptions<'T> (f: 'T->unit): 'T = jsNative

/// Create an empty JS object: {}
let createEmpty<'T> : 'T = jsNative

/// Internally used by Fable, not intended for general use
let [<Obsolete>] applySpread (callee: obj) (args: obj) : 'T = jsNative

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

/// Imports a file only for its side effects
let importSideEffects (path: string): unit = jsNative

/// Imports a file dynamically at runtime
let importDynamic<'T> (path: string): JS.Promise<'T> = jsNative

/// Convert F# unions, records and classes into plain JS objects
/// When designing APIs, consider also using a Pojo record or union
let toPlainJsObj (o: 'T): obj = jsNative

/// Converts an F# object into a plain JS object (POJO)
/// This is only intended if you're using a custom serialization method
/// and will produce the same object structure that `toJson` encodes
/// NOTE: `deflate` is currently NOT recursive
let deflate(o: 'T): obj = jsNative

/// Serialize F# objects to JSON
let toJson(o: 'T): string = jsNative

/// Instantiate F# objects from JSON
let [<PassGenerics>] ofJson<'T>(json: string): 'T = jsNative

/// Instantiate F# objects from a JSON string and a type object
let ofJsonAsType (json: string) (typ: System.Type) : obj = jsNative

/// Serialize F# objects to JSON adding $type info
let toJsonWithTypeInfo(o: 'T): string = jsNative

/// Instantiate F# objects from JSON containing $type info
let [<PassGenerics>] ofJsonWithTypeInfo<'T>(json: string): 'T = jsNative

/// Converts a plain JS object (POJO) to an instance of the specified type.
/// This is only intended if you're using a custom serialization method
/// (that must produce same objects as `toJson`) instead of `ofJson`.
/// NOTE: `inflate` is currently NOT recursive
let [<PassGenerics>] inflate<'T>(pojo: obj): 'T = jsNative

/// Reads the name of an identifier, a property or a type
let nameof(expr: obj): string = jsNative

/// Reads the name of a property or a type from the lambda body
let nameofLambda(f: 'a->'b): string = jsNative

/// Compiles to JS `this` keyword.
///
/// ## Sample
///     jqueryMethod(fun x y -> jsThis?add(x, y))
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
type [<AllowNullLiteral>] JsFunc private () =
    [<Emit("$0($1...)")>]
    member __.Invoke([<ParamArray>]args:obj[]): obj = jsNative
    [<Emit("$0")>] static member From0(f:unit->'b): JsFunc = jsNative
    [<Emit("$0")>] static member From1(f:'a->'b): JsFunc = jsNative
    [<Emit("$0")>] static member From2(f:'a->'b->'c): JsFunc = jsNative
    [<Emit("$0")>] static member From3(f:'a->'b->'c->'d): JsFunc = jsNative
    [<Emit("$0")>] static member From4(f:'a->'b->'c->'d->'e): JsFunc = jsNative
    [<Emit("$0")>] static member From5(f:'a->'b->'c->'d->'e->'f): JsFunc = jsNative
    [<Emit("$0")>] static member From6(f:'a->'b->'c->'d->'e->'f->'g): JsFunc = jsNative
