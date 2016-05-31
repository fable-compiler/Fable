- tagline: Fable features for easy interoperability

# Interacting with JavaScript

There are several ways to interact with the JavaScript world:

- [Dynamic programming](#dynamic-programming)
- [Foreign interfaces](#foreign-interfaces)
- [Special attributes](#special-attributes)
- [Calling F# code from JavaScript](#calling-f-code-from-javascript)

## Dynamic programming

[Fable.Core](https://github.com/fsprojects/Fable/blob/master/import/core/Fable.Core.fs) implements the F# dynamic operators so
you can easily access an object property by name (without static check) as follows:

```fsharp
open Fable.Core

printfn "Value: %O" jsObject?myProperty

jsObject?myProperty <- 5 // Assignment is also possible
```

However, the F# compiler won't let you apply the property directly to other expressions.
For that, use the `$` operator and pass the arguments as a tuple.

```fsharp
open Fable.Core

let result = jsObject?myMethod $ (1, 2)
```

If you want to call the function with the `new` keyword, use `Fable.Core.createNew` instead.

```fsharp
open Fable.Core

let instance = createNew jsObject?method (1, 2)
```

And when you need to create JS object literals, use `createObj`:

```fsharp
open Fable.Core

let data =
    createObj [
        "todos" ==> Storage.fetch()
        "newTodo" ==> ""
        "editedTodo" ==> None
        "visibility" ==> "all"
    ]
```

## Foreign interfaces

Defining a foreign interface is trivial: just create a F# interface and the
compiler will call its properties or methods by name. The tricky part is to
tell the compiler where the objects should be retrieved from. Normally, they
will be exposed as values of an imported module, so you just need to indicate
the compiler where this module is coming from using the `Import` attribute (see below).
For example, if you want to use `string_decoder` from node, just write:

```fsharp
[<Import("*","string_decoder")>]
module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> strings
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = failwith "JS only"
```

> If a method accepts a lambda make sure to use `System.Func` in the signature to force
the compiler _uncurry_ any lambda passed as parameter (see below).

A good starting point for foreign interfaces are [Typescript definition files](http://definitelytyped.org)
and there's a script to make the bulk work of translating the file into F#. You can install it from npm.
See the [README](https://www.npmjs.com/package/ts2fable) for more information.

```shell
npm install -g ts2fable
```

You can find common definitions already parsed [here](https://github.com/fsprojects/Fable/blob/master/import).
Some of them are available in npm, just search for `fable-import` packages.

## Special attributes

There are some attributes available in the `Fable.Core` namespace to ease the interaction with JS.

### Emit attribute

You can use the `Emit` attribute to decorate a function. Every call to the
function will then be replaced **inline** by the content of the attribute
with the placeholders `$0`, `$1`, `$2`... replaced by the arguments. For example,
the following code will generate JavaScript as seen below.

```fsharp
open Fable.Core

[<Emit("$0 + $1")>]
let add (x: int) (y: string): float = failwith "JS only"

let result = add 1 "2"
```

```js
var result = 1 + "2"
```

When you don't know the exact number of arguments you can use the following syntax:

```fsharp
type Test() =
    [<Emit("$0($1...)")>]
    member __.Invoke([<ParamArray>] args: int[]): obj = failwith "JS only"
```

It's also possible to pass syntax conditioned to optional parameters.

```fsharp
type Test() =
    [<Emit("$0[$1]{{=$2}}")>]
    member __.Item with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
```

The content of `Emit` will actually be parsed by Babel so it will still be
validated somehow. However, it's not advised to abuse this method, as the
code in the template will remain obscure to Fable and may prevent some
optimizations.

### Import attribute

The `Import` attribute can be applied to modules, types and even functions.
It will translate to [ES2015 import statements](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import),
which can be later transformed to `commonjs`, `amd` or `umd` imports by Babel.

```js
// Namespace imports
[<Import("*", from="my-module")>]          // F#
import * from "my-module"                  // JS

// Member imports
[<Import("myFunction", from="my-module")>] // F#
import { myFunction } from "my-module"     // JS

// Default imports
[<Import("default", from="express")>]      // F#
import express from express                // JS
```

> If the module or value is globally accessible in JavaScript,
you can use the `Global` attribute without parameters instead.

### Erase attribute

In TypeScript there's a concept of [Union Types](https://www.typescriptlang.org/docs/handbook/advanced-types.html#union-types)
which differs from union types in F#. The former are just used to statically check a function argument
accepting different types. In Fable, they're translated as **Erased Union Types**
whose cases must have one and only one single data field. After compilation, the wrapping
will be erased and only the data field will remain. To define an erased union type, just attach
the `Erase` attribute to the type. Example:

```fsharp
open Fable.Core

[<Erase>]
type MyErasedType =
    | String of string
    | Number of int

myLib.myMethod(String "test")
```

```js
myLib.myMethod("test")
```

`Fable.Core` already includes predefined erased types which can be used as follows:

```fsharp
open Fable.Core

type Test() =
    member x.Value = "Test"

let myMethod (arg: U3<string, int, Test>) =
    match arg with
    | U3.Case1 s -> s
    | U3.Case2 i -> string i
    | U3.Case3 t -> t.Value
```

### StringEnum attribute

Similarly, in TypeScript it's possible to define [String Literal Types](https://www.typescriptlang.org/docs/handbook/advanced-types.html#string-literal-types)
which are similar to enumerations with an underlying string value.
Fable allows the same feature by using union types and the `StringEnum` attribute.
These union types must not have any data fields as they will be compiled
to a string matching the name of the union case.

By default, the compiled string will have the first letter lowered.
If you want to prevent this or use a different text than the union
case name, use the `CompiledName` attribute:

```fsharp
open Fable.Core

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

myLib.myMethod(Vertical, Horizontal)
```

```js
myLib.myMethod("vertical", "Horizontal")
```

### KeyValueList attribute

Many JS libraries accept a plain object to specify different options.
With Fable, you can use union types to define these options in a more
static-safe and F#-idiomatic manner. The union cases of a type with the
`KeyValueList` attribute act as a key value pair, so they should have a
single data field (if there's no data field the value is assumed to be `true`).
When Fable encounters a **list** of such an union type, it will compile it as
a plain JS object.

As with `StringEnum` the first letter of the key (the union case name)
will be lowered. Again, you can modify this behaviour with the `CompiledName`
attribute.

```fsharp
open Fable.Core

[<KeyValueList>]
type MyOptions =
    | Flag1
    | Name of string
    | [<CompiledName("QTY")>] QTY of int

myLib.myMethod [
    Name "Fable"
    QTY 5
    Flag1
]
```

```js
myLib.myMethod({
    name: "Fable",
    QTY: 5,
    flag1: true
})
```

If necessary you can cheat the compiler using tuples:

```fsharp
myLib.myMethod [Name "Fable"; unbox("level", 4)]
```

```js
myLib.myMethod({ name: "Fable", level: 4 })
```

As these lists will be compiled as JS objects, please note you
cannot apply the usual list operations to them (e.g. appending).
If you want to manipulate the "fake" lists you must implement the
methods yourself. For example:

```fsharp
[<KeyValueList>]
type CSSProp =
    | Border of string
    | Display of string

[<Emit("Object.assign({}, $0, $1)")>]
let ( ++ ) (a:'a list) (b:'a list) : 'a list = failwith "JS Only"

let niceBorder = [ Border "1px solid blue" ]
let style = [ Display "inline-block" ] ++ niceBorder
```

## Calling F# code from JavaScript

The F# compiler compiles type and module members as usual for an OOP
language like C#, no matter whether the arguments are curried or tupled.
Thanks to this, they can be called "normally" from an external language
like C# or JavaScript. For example:

```fsharp
module MyModule =
    let myCurriedFunction a b = a + b
    let myTupledFunction(a, b) = a + b

type MyType() =
    member x.MyCurriedMethod a b = a + b
    member x.MyTupledMethod(a, b) = a + b
```

```js
// From JS
var lib = require("lib/myFSLib");
console.log(lib.MyModule.myCurriedFunction(2, 2));  // 4
console.log(lib.MyModule.myTupledFunction(2, 2));   // 4

var x = new lib.MyType();
console.log(x.MyCurriedMethod(2, 2));               // 4
console.log(x.MyTupledMethod(2, 2));                // 4
```

Things get a bit more complicated with anonymous lambdas, inner functions,
partial applications or even just top level function references, as they do
follow proper F# semantics. For example, check how the following code is compiled:

```fsharp
let topLevelFn1 a b c = a + b + c
let topLevelFn2(a, b, c) = a + b + c

let test() =
    // Note we're now within a function's scope
    let f1 a b = a + b
    let f2(a, b) = a + b
    let f3 = fun a b -> a + b
    let f4 = topLevelFn1 2
    let f5 = topLevelFn2
    f1 2 2      |> ignore
    f2(2, 2)    |> ignore
    f3 2 2      |> ignore
    f4 2 2      |> ignore
    f5(2, 2, 2) |> ignore
```

```js
function test() {
    var f1 = function (a) {
        return function (b) {
            return a + b;
        };
    };
    var f2 = function (tupledArg) {
        return tupledArg[0] + tupledArg[1];
    };
    var f3 = function (a) {
        return function (b) {
            return a + b;
        };
    };
    var f4 = function (b) {
        return function (c) {
            return topLevelFn1(2, b, c);
        };
    };
    var f5 = function (tupledArg) {
        return topLevelFn2(tupledArg[0], tupledArg[1], tupledArg[2]);
    };
    f1(2)(2);    // Double application
    f2([2, 2]);  // Tupled arg
    f3(2)(2);    // Inner functions and anonymous lambdas are the same
    f4(2)(2);    // Partial application
    f5([2,2,2]); // Top level functions get wrapped when referenced
}
```

This means you need to be careful when passing a lambda with more than
one argument to JS, as they will probably not be called correctly. To prevent
this you must convert the lambda to a delegate (`System.Func<...>`), the same
way you would do when interacting with C#.

```fsharp
let f1 a b = a + b

jsObj?callback1 <- f1 // produces curried version, probably not what you want
jsObj?callback2 <- Func<_,_,_> f1 // creates a delegate instead
```

```javascript
jsObj.callback1 = function (a) {
    return function (b) {
        return f1(a, b);
    };
};

jsObj.callback2 = function (delegateArg0, delegateArg1) {
    return f1(delegateArg0, delegateArg1);
};
```

If the signature of the method you're calling accepts a delegate instead
of a F# lambda, the conversion is done automatically for you.

```fsharp
type IForeign =
    abstract myMethod: Func<int, int> -> unit

foreignObj.myMethod(fun a b -> a + b)
```

> Note: If you still experience problems make the conversion explicit
as in the above example.
