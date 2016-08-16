namespace Fable.Core
open System

/// Used for erased union types and to ignore modules in JS compilation.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type EraseAttribute() =
    inherit Attribute()

/// The module, type, function... is globally accessible in JS.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Import-attribute
type GlobalAttribute() =
    inherit Attribute()

/// References to the module, type, function... will be replaced by import statements.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Import-attribute
type ImportAttribute(get: string, from: string) =
    inherit Attribute()

/// Function calls will be replaced by inlined JS code.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Import-attribute
type EmitAttribute private () =
    inherit Attribute()
    new (macro: string) = EmitAttribute()
    new (emitterType: Type, methodName: string) = EmitAttribute()

/// Compile union case lists as JS object literals.
/// More info: http://fable-compiler.github.io/docs/interacting.html#KeyValueList-attribute
type KeyValueListAttribute() =
    inherit Attribute()

/// Compile union types as string literals.
/// More info: http://fable-compiler.github.io/docs/interacting.html#StringEnum-attribute
type StringEnumAttribute() =
    inherit Attribute()    

/// Erased union type to represent one of two possible values.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type [<Erase>] U2<'a, 'b> = Case1 of 'a | Case2 of 'b

/// Erased union type to represent one of three possible values.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type [<Erase>] U3<'a, 'b, 'c> = Case1 of 'a | Case2 of 'b | Case3 of 'c    

/// Erased union type to represent one of four possible values.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type [<Erase>] U4<'a, 'b, 'c, 'd> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd    

/// Erased union type to represent one of five possible values.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type [<Erase>] U5<'a, 'b, 'c, 'd, 'e> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e    

/// Erased union type to represent one of six possible values.
/// More info: http://fable-compiler.github.io/docs/interacting.html#Erase-attribute
type [<Erase>] U6<'a, 'b, 'c, 'd, 'e, 'f> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e | Case6 of 'f    

/// DO NOT USE: Internal type for Fable dynamic operations
type Applicable = obj->obj

module JsInterop =
    /// Dynamically access a property of an arbitrary object.
    /// `myObj?propA` in JS becomes `myObj.propA`
    /// `myObj?(propA)` in JS becomes `myObj[propA]`
    let (?) (o: obj) (prop: obj): Applicable = failwith "JS only"
    
    /// Dynamically assign a value to a property of an arbitrary object.
    /// `myObj?propA <- 5` in JS becomes `myObj.propA = 5`
    /// `myObj?(propA) <- 5` in JS becomes `myObj[propA] = 5`
    let (?<-) (o: obj) (prop: obj) (v: obj): unit = failwith "JS only"

    /// Destructure and apply a tuple to an arbitrary value.
    /// E.g. `myFn $ (arg1, arg2)` in JS becomes `myFn(arg1, arg2)`
    let ($) (callee: obj) (args: obj): obj = failwith "JS only"
    
    /// Upcast the right operand to obj and create a key-value tuple.
    /// Mostly convenient when used with createObj.
    /// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
    let (==>) (key: string) (v: obj): string*obj = failwith "JS only"
    
    /// Destructure and apply a tuple to an arbitrary value with `new` keyword.
    /// E.g. `createNew myCons (arg1, arg2)` in JS becomes `new myCons(arg1, arg2)`
    let createNew (o: obj) (args: obj): obj = failwith "JS only"

    /// Create a literal JS object from a collection of key-value tuples.
    /// E.g. `createObj [ "a" ==> 5 ]` in JS becomes `{ a: 5 }`
    let createObj (fields: #seq<string*obj>): obj = failwith "JS only"
    
    /// Create an empty JS object: {}
    let createEmpty<'T> : 'T = failwith "JS only"

    /// F#: let myMember = importMember<string> "myModule"
    /// JS: import { myMember } from "myModule"
    /// Note the import must be immediately assigned to a value in a let binding
    let importMember<'T> (path: string):'T = failwith "JS only"

    /// F#: let defaultMember = importDefault<unit->obj> "myModule"
    /// JS: import defaultMember from "myModule"
    let importDefault<'T> (path: string):'T = failwith "JS only"

    /// F#: let myLib = importDefault<obj> "myLib"
    /// JS: import * as myLib from "myLib"
    let importAll<'T> (path: string):'T = failwith "JS only"

    /// Serialize F# objects to JSON with type info
    let toJson (o: 'T): string = failwith "JS only"

    /// Instantiate F# objects from JSON with type info
    /// (compatible with Newtonsoft.Json)
    let ofJson<'T> (json: string): 'T = failwith "JS only"

    /// Convert F# unions, records and classes into plain JS objects
    let toPlainJsObj (o: 'T): obj = failwith "JS only"

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
        static member AreEqual(x: 'T, y: 'T): unit = failwith "JS only"

    