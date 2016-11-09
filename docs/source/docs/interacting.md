- tagline: Fable features for easy interoperability

**Attention**: This document corresponds to Fable 0.6.x and needs to be updated to the latest version. Please check the [migration guide](../blog/Introducing-0-7.html).

# Interacting with JavaScript

There are several ways to interact with the JavaScript world:

- [Dynamic programming](#Dynamic-programming)
- [Foreign interfaces](#Foreign-interfaces)
- [Special attributes](#Special-attributes)
- [Calling F# code from JavaScript](#Calling-F-code-from-JavaScript)
- [JSON serialization](#JSON-serialization)
- [Publishing a Fable package](#Publishing-a-Fable-package)

## Dynamic programming

[Fable.Core.JsInterop](https://github.com/fable-compiler/Fable/blob/master/src/fable/Fable.Core/Fable.Core.fs)
implements the F# dynamic operators so you can easily access an object property by name (without static check)
as follows:

```fsharp
open Fable.Core.JsInterop

printfn "Value: %O" jsObject?myProperty

let pname = "myProperty"

printfn "Value: %O" jsObject?(pname) // Access with a string reference

jsObject?myProperty <- 5 // Assignment is also possible
```

When you combine the dynamic operator with application, Fable will destructure
tuple arguments as with normal method calls. These operations can also be chained
to replicate JS fluent APIs.

```fsharp
open Fable.Core.JsInterop

let result = jsObject?myMethod(1, 2)
// var result = jsObject.myMethod(1, 2)

chart
    ?width(768.)
    ?height(480.)
    ?group(speedSumGroup)
    ?on("renderlet", fun chart ->
        chart?selectAll("rect")?on("click", fun d ->
            Browser.console.log("click!", d))
// chart
//     .width(768)
//     .height(480)
//     .group(speedSumGroup)
//     .on("renderlet", function (chart) {
//         return chart.selectAll("rect").on("click", function (d) {
//             return console.log("click!", d);
//         });
//      });
```

> CAUTION: When you don't use the dynamic operator and apply a tuple
to a function value, it won't be destructured (tuples are translated
to JS arrays). See _Calling F# code from JavaScript_ below for more info.
However, you can still use the `$` operator to destructure and apply a
tuple to an arbitrary value.

If you want to call the function with the `new` keyword, use `Fable.Core.createNew` instead.

```fsharp
open Fable.Core.JsInterop

let instance = createNew jsObject?method (1, 2)
```

And when you need to create JS object literals, use `createObj`:

```fsharp
open Fable.Core.JsInterop

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
open Fable.Core

[<Import("*","string_decoder")>]
module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> strings
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = jsNative
```

> If a method accepts a lambda make sure to use `System.Func` in the signature to force
the compiler _uncurry_ any lambda passed as parameter (see below).

A good starting point for foreign interfaces are [Typescript definition files](http://definitelytyped.org)
and there's a script to make the bulk work of translating the file into F#. You can install it from npm.
See the [README](https://www.npmjs.com/package/ts2fable) for more information.

```shell
npm install -g ts2fable
```

You can find common definitions already parsed [here](https://github.com/fable-compiler/Fable/blob/master/import).
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
let add (x: int) (y: string): float = jsNative

let result = add 1 "2"
```

```js
var result = 1 + "2"
```

When you don't know the exact number of arguments you can use the following syntax:

```fsharp
type Test() =
    [<Emit("$0($1...)")>]
    member __.Invoke([<ParamArray>] args: int[]): obj = jsNative
```

It's also possible to pass syntax conditioned to optional parameters.

```fsharp
type Test() =
    [<Emit("$0[$1]{{=$2}}")>]
    member __.Item with get(): float = jsNative and set(v: float): unit = jsNative

    // This syntax means: if second arg evals to true in JS print 'i' and nothing otherwise
    [<Emit("new RegExp($0,'g{{$1?i:}}')")>]
    member __.ParseRegex(pattern: string, ?ignoreCase: bool): Regex = jsNative
```

The content of `Emit` will actually be parsed by Babel so it will still be
validated somehow. However, it's not advised to abuse this method, as the
code in the template will remain obscure to Fable and may prevent some
optimizations.

### Import attribute

The `Import` attribute can be applied to modules, types and even functions.
It will translate to [ES2015 import statements](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import),
which can be later transformed to `commonjs`, `amd` or `umd` imports by Babel.

```fsharp
// Namespace imports
[<Import("*", from="my-module")>]
// import * from "my-module"

// Member imports
[<Import("myFunction", from="my-module")>]
// import { myFunction } from "my-module"

// Default imports
[<Import("default", from="express")>]
// import express from "express"
```

> If the module or value is globally accessible in JavaScript,
you can use the `Global` attribute without parameters instead.

> When importing a relative path (starting with `.` as in `./js/myLib.js`),
the path will be resolved so it can be reached from `outDir` in the compiled JS code.

`Fable.Core.JsInterop` also contains import expressions. These are mostly
useful when you need to import JS libraries without a foreign interface.

```fsharp
open Fable.Core.JsInterop

let buttons = importAll<obj> "material-ui/buttons"
// import * as buttons from "material-ui/buttons"

let deepOrange500 = importMember<string> "material-ui/styles/colors"
// import { deepOrange500 } from "material-ui/styles/colors"

let getMuiTheme = importDefault<obj->obj> "material-ui/styles/getMuiTheme"
// import getMuiTheme from "material-ui/styles/getMuiTheme"
```

> Note that Fable automatically uses the name of the let-bound variable
in the second example, this means you must always immediately assign the
result of `importMember` to a named value.

> A difference between import attributes and expressions is the former are
not resolved in the file where they're declared (as this can be erased
bindings) but in the file where thery're used. While import expressions are
always resolved where they're declared. But if you don't understand this,
don't worry too much, this is usually transparent to the user :)

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

You can also allow custom key value pairs by adding an extra
union case with the `Erase` attribute.

```fsharp
open Fable.Core

[<KeyValueList>]
type MyOptions =
    | Flag1
    | Name of string
    | [<CompiledName("QTY")>] QTY of int
    | [<Erase>] Extra of string * obj

myLib.myMethod [
    Name "Fable"
    QTY 5
    Flag1
    Extra ("newOption", 10.5)
]
```

```js
myLib.myMethod({
    name: "Fable",
    QTY: 5,
    flag1: true,
    newOption: 10.5
})
```

> Note: Using tuples directly will have the same effect as the `Erase` attribute:

```fsharp
myLib.myMethod [Name "Fable"; unbox("level", 4)]
// myLib.myMethod({ name: "Fable", level: 4 })
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
let ( ++ ) (a:'a list) (b:'a list) : 'a list = jsNative

let niceBorder = [ Border "1px solid blue" ]
let style = [ Display "inline-block" ] ++ niceBorder
```

## Calling F# code from JavaScript

When interacting with JavaScript libraries, it's usual to send callbacks
that will be called from JS code. For this to work properly with F# functions
you must take a few things into consideration:

- In F# **commas are always used for tuples**:

```fsharp
fun (x, y) -> x + y
// JS: function (tupledArg) { return tupledArg[0] + tupledArg[1]; }
```

- In F# **function arguments are always curried**, so lambdas with multiple
  arguments translate to single-argument functions returning another function.
  To make it a multi-argument function in JS, you must **convert it to a delegate**
  (like `System.Func<_,_,_>`):

```fsharp
fun x y -> x + y
// JS: function (x) { return function (y) { return x + y; } }

System.Func<_,_,_>(fun x y -> x + y)
// JS: function (x, y) { return x + y; }
```

- Fable will **automatically convert F# functions to delegates** in some situations:
    - When passing an F# lambda to a method accepting a delegate.
    - When passing functions as arguments to `EmitAttribute`.
    - When using dynamic programming, with `?`, `$`, `createObj` or `createNew`.

> Note: If you experience problems make the conversion explicit.



```fsharp
type ITest =
    abstract setCallback: Func<int,int,int> -> unit

let test(o: ITest) =
    o.setCallback(fun x y -> x + y)

// function test(o) {
//   o.setCallback(function (x, y) {
//     return x + y;
//   });
// }
```

```fsharp
let myMeth x y = x + y

let test(o: obj) =
    o?foo(fun x y -> x + y) |> ignore
    o?bar <- fun x y -> x + y

    // Direct application with ($) operator
    o $ (fun x y -> x * y) |> ignore

    createObj [
        "bar" ==> fun x y z -> x * y * z
        "bar2" ==> myMeth
    ]

// function test(o) {
//   o.foo(function (x, y) {
//     return x + y;
//   });

//   o.bar = function (x, y) {
//     return x + y;
//   };

//   o(function (x, y) {
//     return x * y;
//   });

//   return {
//     bar: function bar(x, y, z) {
//       return x * y * z;
//     },
//     bar2: function bar2(x, y) {
//       return myMeth(x, y);
//     }
//   };
// }
```

## JSON serialization

It's possible to use `JSON.stringify` and `JSON.parse` to serialize objects back
and forth, particularly with record and union types. Records will serialize as plain
JS objects and unions will be serialized the same way as with [Json.NET](http://www.newtonsoft.com/json),
making it easier to interact with a .NET server.

The only problem is `JSON.parse` will produce a plain JS object which won't work if
you need to type test it or access the prototype members. When this is necessary, you
can use `toJson` and `ofJson` functions in `Fable.Core.JsInterop` module. This will
save the type full name in a `$type` field so Fable will know which type to construct
when deserializing:

```fsharp
open Fable.Core.JsInterop

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees ->
            trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

let tree =
    Branch [|Leaf 1; Leaf 2; Branch [|Leaf 3; Leaf 4|]|]

let json = toJson tree
let tree2 = ofJson<Tree> json

let typeTest = box tree2 :? Tree    // Type is kept
let sum = tree2.Sum()   // Prototype members can be accessed
```

> This will work when exchanging objects with a server, if the
server includes the type full name in a `$type` field and the
client code knows the type definiton. With Json.NET you can do
this by simply using the `TypeNameHandling.All` setting.

```fsharp
type R = { i: int; s: string }

let x = { i=1; s="1" }
let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))

// {"$type":"Fable.Tests.Lists+R, Fable.Tests","i":1,"s":"1"}
```

Caveats:

- Works with records, unions and classes with an argumentless primary constructor.
- For classes, only properties (getters) will be serialized. Properties must also
  have a public setter for correct deserialization (`[<CLIMutable>]` doesn't work).
- Arrays will always be deserialized as dynamic JS arrays, not typed arrays.
- Not compatible with atttibutes like `[<JsonIgnore>]`.
- Fable doesn't include the same type information as Json.NET (generic arguments and assembly
  name are missing), so you need to specify the target type for proper deserialization on
  the server side.
- `DateTime` will always be serialized in UTC format and any string matching the [DateTime ISO Format](https://www.w3.org/TR/NOTE-datetime)
  will be deserialized as a string.
- Json.NET doesn't serialize type info for F# unions even when using the `TypeNameHandling.All` setting,
  but you can use the following converter:

```fsharp
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

type UnionTypeInfoConverter() =
    inherit JsonConverter()
    override x.CanConvert t = FSharpType.IsUnion t
    override x.ReadJson(reader, t, _, serializer) =
        let token = JObject()
        for prop in JToken.ReadFrom(reader).Children<JProperty>() do
            if prop.Name <> "$type" then
                token.Add(prop)
        token.ToObject(t)
    override x.WriteJson(writer, v, serializer) =
        let t = v.GetType()
        let typeFulName = t.FullName.Substring(0, t.FullName.LastIndexOf("+"))
        let uci, fields = FSharpValue.GetUnionFields(v, t)
        writer.WriteStartObject()
        writer.WritePropertyName("$type")
        writer.WriteValue(typeFulName)
        writer.WritePropertyName("Case")
        writer.WriteValue(uci.Name)
        writer.WritePropertyName("Fields")
        writer.WriteStartArray()
        for field in fields do writer.WriteValue(field)
        writer.WriteEndArray()
        writer.WriteEndObject()

let settings =
    JsonSerializerSettings(
        Converters = [|UnionTypeInfoConverter()|],
        TypeNameHandling = TypeNameHandling.All)

type U = A of int | B of string * float
JsonConvert.SerializeObject(A 4, settings)
JsonConvert.SerializeObject(B("hi",5.6), settings)
```

## Publishing a Fable package

See [fable-helpers-sample](https://www.npmjs.com/package/fable-helpers-sample) to know how to publish a Fable package.
