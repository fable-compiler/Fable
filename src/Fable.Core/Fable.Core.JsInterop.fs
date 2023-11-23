module Fable.Core.JsInterop

open System
open Fable.Core

/// Compiles to ?? operator in JavaScript
[<Emit("$0 ?? $1")>]
let (??=) (nullable: 'T) (defaultValue: 'T) : 'T = nativeOnly

/// Has same effect as `unbox` (dynamic casting erased in compiled JS code).
/// The casted type can be defined on the call site: `!!myObj?bar(5): float`
let (!!) x : 'T = nativeOnly

/// Implicit cast for erased unions (U2, U3...)
let inline (!^) (x: ^t1) : ^t2 =
    ((^t1 or ^t2): (static member op_ErasedCast: ^t1 -> ^t2) x)

/// Dynamically access a property of an arbitrary object.
/// `myObj?propA` in JS becomes `myObj.propA`
/// `myObj?(propA)` in JS becomes `myObj[propA]`
let (?) (o: obj) (prop: obj) : 'a = nativeOnly

/// Dynamically assign a value to a property of an arbitrary object.
/// `myObj?propA <- 5` in JS becomes `myObj.propA = 5`
/// `myObj?(propA) <- 5` in JS becomes `myObj[propA] = 5`
let (?<-) (o: obj) (prop: obj) (v: obj) : unit = nativeOnly

/// Destructure and apply a tuple to an arbitrary value.
/// E.g. `myFn $ (arg1, arg2)` in JS becomes `myFn(arg1, arg2)`
let ($) (callee: obj) (args: obj) : 'a = nativeOnly

/// Upcast the right operand to obj (and uncurry it if it's a function) and create a key-value tuple.
/// Mostly convenient when used with `createObj`.
/// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
let (==>) (key: string) (v: obj) : string * obj = nativeOnly

/// Destructure and apply a tuple to an arbitrary value with `new` keyword.
/// E.g. `createNew myCons (arg1, arg2)` in JS becomes `new myCons(arg1, arg2)`
let createNew (o: obj) (args: obj) : obj = nativeOnly

/// Destructure a tuple of arguments and applies to literal JS code as with EmitAttribute.
/// E.g. `emitJsExpr (arg1, arg2) "$0 + $1"` in JS becomes `arg1 + arg2`
let emitJsExpr<'T> (args: obj) (jsCode: string) : 'T = nativeOnly

/// Same as emitJsExpr but intended for JS code that must appear in a statement position
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements
/// E.g. `emitJsStatement aValue "while($0 < 5) doSomething()"`
let emitJsStatement<'T> (args: obj) (jsCode: string) : 'T = nativeOnly

/// Create a literal JS object from a collection of key-value tuples.
/// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
let createObj (fields: #seq<string * obj>) : obj = nativeOnly

/// Create a literal JS object from a collection of union constructors.
/// E.g. `keyValueList CaseRules.LowerFirst [ MyUnion 4 ]` in JS becomes `{ myUnion: 4 }`
let keyValueList (caseRule: CaseRules) (li: 'T seq) : obj = nativeOnly

/// Create a literal JS object from a mutator lambda. Normally used when
/// the options interface has too many fields to be represented with a Pojo record.
/// E.g. `jsOptions<MyOpt> (fun o -> o.foo <- 5)` in JS becomes `{ foo: 5 }`
let jsOptions<'T> (f: 'T -> unit) : 'T = nativeOnly

/// Create an empty JS object: {}
let createEmpty<'T> : 'T = nativeOnly

/// Used when you need to send an F# record to a JS library accepting only plain JS objects (POJOs)
let toPlainJsObj (o: 'T) : obj = nativeOnly

/// Get the JS function constructor for class types
let jsConstructor<'T> : obj = nativeOnly

[<Emit("typeof $0")>]
let jsTypeof (x: obj) : string = nativeOnly

[<Emit("$0 instanceof $1")>]
let jsInstanceof (x: obj) (cons: obj) : bool = nativeOnly

[<Emit("this")>]
let jsThis<'T> : 'T = nativeOnly

[<Emit("$0 in $1")>]
let jsIn (key: string) (target: obj) : bool = nativeOnly

/// Alias of `jsIn`
[<Emit("$0 in $1")>]
let isIn (key: string) (target: obj) : bool = nativeOnly

/// JS non-strict null checking
[<Emit("$0 == null")>]
let isNullOrUndefined (target: obj) : bool = nativeOnly

/// Makes an expression the default export for the JS module.
/// Used to interact with JS tools that require a default export.
/// ATTENTION: This statement must appear on the root level of the file module.
[<Emit("export default $0")>]
let exportDefault (x: obj) : unit = nativeOnly

/// Works like `ImportAttribute` (same semantics as ES6 imports).
/// You can use "*" or "default" selectors.
let import<'T> (selector: string) (path: string) : 'T = nativeOnly

/// F#: let myMember = importMember<string> "myModule"
/// JS: import { myMember } from "myModule"
/// Note the import must be immediately assigned to a value in a let binding
let importMember<'T> (path: string) : 'T = nativeOnly

/// F#: let defaultMember = importDefault<unit->obj> "myModule"
/// JS: import defaultMember from "myModule"
let importDefault<'T> (path: string) : 'T = nativeOnly

/// F#: let myLib = importAll<obj> "myLib"
/// JS: import * as myLib from "myLib"
let importAll<'T> (path: string) : 'T = nativeOnly

/// Imports a file only for its side effects
let importSideEffects (path: string) : unit = nativeOnly

/// Imports a file dynamically at runtime
let importDynamic<'T> (path: string) : JS.Promise<'T> = nativeOnly

/// Imports a reference from an external file dynamically at runtime
/// ATTENTION: Pass the reference directly in argument position (avoid pipes)
let importValueDynamic (x: 'T) : JS.Promise<'T> = nativeOnly

/// Use it when importing a constructor from a JS library.
[<AllowNullLiteral>]
type JsConstructor =
    [<Emit("new $0($1...)")>]
    abstract Create: [<ParamArray>] args: obj[] -> obj

/// Use it when importing dynamic functions from JS. If you need a constructor, use `JsConstructor`.
///
/// ## Sample
///     let f: JsFunc = import "myFunction" "./myLib"
///     f.Invoke(5, "bar")
[<AllowNullLiteral>]
type JsFunc private () =
    [<Emit("$0($1...)")>]
    member _.Invoke([<ParamArray>] args: obj[]) : obj = nativeOnly
