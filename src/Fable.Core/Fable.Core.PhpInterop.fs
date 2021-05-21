module Fable.Core.PhpInterop

open System
open Fable.Core

/// Has same effect as `unbox` (dynamic casting erased in compiled JS code).
/// The casted type can be defined on the call site: `!!myObj?bar(5): float`
let (!!) x: 'T = phpNative

/// Implicit cast for erased unions (U2, U3...)
let inline (!^) (x:^t1) : ^t2 = ((^t1 or ^t2) : (static member op_ErasedCast : ^t1 -> ^t2) x)

/// Dynamically access a property of an arbitrary object.
/// `myObj?propA` in JS becomes `myObj.propA`
/// `myObj?(propA)` in JS becomes `myObj[propA]`
let (?) (o: obj) (prop: obj): 'a = phpNative

/// Dynamically assign a value to a property of an arbitrary object.
/// `myObj?propA <- 5` in JS becomes `myObj.propA = 5`
/// `myObj?(propA) <- 5` in JS becomes `myObj[propA] = 5`
let (?<-) (o: obj) (prop: obj) (v: obj): unit = phpNative

/// Destructure and apply a tuple to an arbitrary value.
/// E.g. `myFn $ (arg1, arg2)` in JS becomes `myFn(arg1, arg2)`
let ($) (callee: obj) (args: obj): 'a = phpNative

/// Upcast the right operand to obj (and uncurry it if it's a function) and create a key-value tuple.
/// Mostly convenient when used with `createObj`.
/// E.g. `createObj [ "a" ==> 5 ]` in Python becomes `{ a: 5 }`
let (==>) (key: string) (v: obj): string*obj = phpNative

/// Destructure and apply a tuple to an arbitrary value with `new` keyword.
/// E.g. `createNew myCons (arg1, arg2)` in JS becomes `new myCons(arg1, arg2)`
/// let createNew (o: obj) (args: obj): obj = phpNative

/// Destructure a tuple of arguments and applies to literal JS code as with EmitAttribute.
/// E.g. `emitJsExpr (arg1, arg2) "$0 + $1"` in Python becomes `arg1 + arg2`
let emitPhpExpr<'T> (args: obj) (jsCode: string): 'T = phpNative

/// Same as emitJsExpr but intended for JS code that must appear in a statement position
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements
/// E.g. `emitJsExpr aValue "while($0 < 5) doSomething()"`
let emitPhpStatement<'T> (args: obj) (pyCode: string): 'T = phpNative

/// Create a literal Python object from a collection of key-value tuples.
/// E.g. `createObj [ "a" ==> 5 ]` in Python becomes `{ a: 5 }`
let createObj (fields: #seq<string*obj>): obj = phpNative

/// Create a literal Python object from a collection of union constructors.
/// E.g. `keyValueList CaseRules.LowerFirst [ MyUnion 4 ]` in Python becomes `{ myUnion: 4 }`
let keyValueList (caseRule: CaseRules) (li: 'T seq): obj = phpNative

/// Create a literal Py object from a mutator lambda. Normally used when
/// the options interface has too many fields to be represented with a Pojo record.
/// E.g. `jsOptions<MyOpt> (fun o -> o.foo <- 5)` in JS becomes `{ foo: 5 }`
let phpOptions<'T> (f: 'T->unit): 'T = phpNative

/// Create an empty JS object: {}
///let createEmpty<'T> : 'T = phpNative

///// Get the Py function constructor for class types
//let pyConstructor<'T> : obj = phpNative

/// Makes an expression the default export for the JS module.
/// Used to interact with JS tools that require a default export.
/// ATTENTION: This statement must appear on the root level of the file module.
//[<Emit("export default $0")>]
//let exportDefault (x: obj): unit = phpNative

/// Works like `ImportAttribute` (same semantics as ES6 imports).
/// You can use "*" or "default" selectors.
//let import<'T> (selector: string) (path: string):'T = phpNative

/// F#: let myMember = importMember<string> "myModule"
/// Py: from my_module import my_member
/// Note the import must be immediately assigned to a value in a let binding
//let importMember<'T> (path: string):'T = phpNative

/// F#: let defaultMember = importDefault<unit->obj> "myModule"
/// JS: import defaultMember from "myModule"
//let importDefault<'T> (path: string):'T = phpNative

/// F#: let myLib = importAll<obj> "myLib"
/// JS: import * as myLib from "myLib"
//let importAll<'T> (path: string):'T = phpNative

/// Imports a file only for its side effects
//let importSideEffects (path: string): unit = phpNative

/// Imports a file dynamically at runtime
//let importDynamic<'T> (path: string): JS.Promise<'T> = phpNative

/// Imports a reference from an external file dynamically at runtime
/// ATTENTION: Needs fable-compiler 2.3, pass the reference directly in argument position (avoid pipes)
//let importValueDynamic (x: 'T): JS.Promise<'T> = phpNative

/// Used when you need to send an F# record to a JS library accepting only plain JS objects (POJOs)
//let toPlainJsObj(o: 'T): obj = phpNative

/// Compiles to JS `this` keyword.
///
/// ## Sample
///     jqueryMethod(fun x y -> jsThis?add(x, y))
let [<Emit("this")>] phpThis<'T> : 'T = phpNative

/// JS `in` operator
//let [<Emit("$0 in $1")>] isIn (key: string) (target: obj): bool = phpNative

/// JS non-strict null checking
//let [<Emit("$0 == null")>] isNullOrUndefined (target: obj): bool = phpNative

/// Use it when importing a constructor from a JS library.
//type [<AllowNullLiteral>] JsConstructor =
//    [<Emit("new $0($1...)")>]
//    abstract Create: [<ParamArray>]args: obj[] -> obj

/// Use it when importing dynamic functions from JS. If you need a constructor, use `JsConstructor`.
///
/// ## Sample
///     let f: JsFunc = import "myFunction" "./myLib"
///     f.Invoke(5, "bar")
//type [<AllowNullLiteral>] JsFunc private () =
//    [<Emit("$0($1...)")>]
//    member __.Invoke([<ParamArray>]args:obj[]): obj = phpNative
