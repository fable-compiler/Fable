# Interacting with JavaScript

This page is structured as a reference document. For a more practical approach to Fable and JavaScript interop, please check [this great guide by Zaid Ajaj](https://medium.com/@zaid.naom/f-interop-with-javascript-in-fable-the-complete-guide-ccc5b896a59f).

## Importing JavaScript code

To use code from JS libraries first you need to import it into F#. For this Fables uses [ES2015 imports](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import), which can be later transformed to other JS module systems like `commonjs` or `amd` by Babel.

There are two ways to declare ES2015 imports in the Fable: by using either the **Import attribute** or the **import expressions**. The `ImportAttribute` can decorate members, types or modules and works as follows:

```fsharp
// Namespace imports
[<Import("*", from="my-module")>]
// import * from "my-module"

// Member imports
[<Import("myFunction", from="my-module")>]
// import { myFunction } from "my-module"

// Default imports
[<Import("default", from="express")>]
// import Express from "express"
```

> If the value is globally accessible in JS, you can use the `Global` attribute with an optional name parameter instead.

Unless you are decorating an abstract member, you can use `jsNative` as a placeholder for the body:

```fsharp
open Fable.Core.JsInterop

[<Import("default", from="express")>]
let Express: obj = jsNative
```

By opening `Fable.Core.JsInterop`, you will also have access to **import expressions**. These expressions are generic so be sure to make the type of the imported value explicit.

```fsharp
open Fable.Core.JsInterop

let buttons: obj = importAll "my-lib/buttons"
// import * as buttons from "my-lib/buttons"

// It works for function declarations too
let getTheme(x: int): IInterface = importDefault "my-lib"
// import getTheme from "my-lib"

let myString: string = importMember "my-lib"
// import { myString } from "my-lib"

// Use just `import` to make the member name explicit
// as with the ImportAttribute
let aDifferentName: string = import "myString" "my-lib"
// import { myString } from "my-lib"
```

## Dynamic typing

[Fable.Core.JsInterop](https://github.com/fable-compiler/Fable/blob/master/src/fable/Fable.Core/Fable.Core.fs) implements the F# dynamic operators so you can easily access an object property by name (without static check)
as follows:

```fsharp
open Fable.Core.JsInterop

printfn "Value: %O" jsObject?myProperty

let pname = "myProperty"

printfn "Value: %O" jsObject?(pname) // Access with a string reference

jsObject?myProperty <- 5 // Assignment is also possible
```

When you combine the dynamic operator with application, Fable will destructure tuple arguments as with normal method calls. These operations can also be chained to replicate JS fluent APIs.

```fsharp
let result = jsObject?myMethod(1, 2)
// var result = jsObject.myMethod(1, 2)

chart
    ?width(768.)
    ?height(480.)
    ?group(speedSumGroup)
    ?on("renderlet", fun chart ->
        chart?selectAll("rect")?on("click", fun sender args ->
            Browser.console.log("click!", args))
// chart
//     .width(768)
//     .height(480)
//     .group(speedSumGroup)
//     .on("renderlet", function (chart) {
//         return chart.selectAll("rect").on("click", function (sender, args) {
//             return console.log("click!", args);
//         });
//      });
```

> Note that in order to make this possible, the output of the `?` is an applicable value. If you don't want this behaviour, `unbox` or the `!!` dynamic cast operator: `let myValue: int = !!myObj?otherMethod("foo", "bar")`

When you have to call a function with the `new` keyword in JS, use `createNew`.

```fsharp
open Fable.Core.JsInterop

let instance = createNew jsObject?myMethod(1, 2)
// var instance = new jsObject.myMethod(1, 2)
```

If you prefer member extensions rather than operators for dynamic typing, you can open `Fable.Core.DynamicExtensions` (Fable.Core 1.3.8 or higher) to have the methods `.Item` and `.Invoke` available on any object.

```fsharp
open Fable.Core.DynamicExtensions

let foo = obj()
let bar1 = foo.["b"]  // Same as foo.Item("b")
foo.["c"] <- 14
let bar2 = foo.Invoke(4, "a")
```

## Plain Old JavaScript Objects

To create a plain JS object (aka POJO), use `createObj`:

```fsharp
open Fable.Core.JsInterop

let data =
    createObj [
        "todos" ==> Storage.fetch()
        "editedTodo" ==> None
        "visibility" ==> "all"
    ]
```

You can also create a JS object from an interface by using `createEmpty` and then assigning manually:

```fsharp
type IMyInterface =
    abstract foo: string with get, set
    abstract bar: float with get, set

let x = createEmpty<IMyInterface> // var x = {}
x.foo <- "abc"                    // x.foo = "abc"
x.bar <- 8.5                      // val.bar = 8.5
```

It is also possible to instantiate a plain JS object in a type-safe manner by declaring an F# record with the `Pojo` attribute:

```fsharp
open Fable.Core

type [<Pojo>] MyRecord =
    { aNumber: int; aString: string }

let foo = { aNumber = 5; aString = "bar" }
// var foo = { aNumber: 5, aString: "bar" }
```

This is mainly useful to interact with JS libraries that expect a plain object (e.g., for configuration) instead of a class instance. But do not abuse this solution because records with `Pojo` attribute will miss features like members or reflection.

## Foreign interfaces

Defining a foreign interface is trivial: just create an F# interface and the compiler will call its properties or methods by name. However doing this may be a bit tedious for JS libraries with large APIs, so we better take advantage of [Typescript definition files](http://definitelytyped.org) using [ts2fable](https://www.npmjs.com/package/ts2fable), a tool to translate `.d.ts` files into F# sources with Fable metadata. Check the [README file](https://www.npmjs.com/package/ts2fable) for installation and usage instructions.

> Please note ts2fable is currently being updated and you may need to do some manual fixes after the automatic translation.

You can find common definitions already parsed [here](https://github.com/fable-compiler/fable-import). These are already published in Nuget, where you can find more packages maintained by other contributors, usually [prefixed by Fable.Import](https://www.nuget.org/packages?q=Fable.Import). You can also find some other bindings and helpers in the [fable-awesome list](https://github.com/kunjee17/awesome-fable#libraries).

## Special attributes

There are some attributes available in the `Fable.Core` namespace to ease the interaction with JS.

### Emit attribute

You can use the `Emit` attribute to decorate a function. Every call to the function will then be replaced **inline** by the content of the attribute with the placeholders `$0`, `$1`, `$2`... replaced by the arguments. For example, the following code will generate JavaScript as seen below.

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

The content of `Emit` will actually be parsed by Babel so it will still be validated somehow. However, it's not advised to abuse this method, as the code in the template will remain obscure to Fable and may prevent some optimizations.

### Erase attribute

In TypeScript there's a concept of [Union Types](https://www.typescriptlang.org/docs/handbook/advanced-types.html#union-types) which differs from union types in F#. The former are just used to statically check a function argument accepting different types. In Fable, they're translated as **Erased Union Types** whose cases must have one and only one single data field. After compilation, the wrapping will be erased and only the data field will remain. To define an erased union type, just attach the `Erase` attribute to the type. Example:

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

Similarly, in TypeScript it's possible to define [String Literal Types](https://www.typescriptlang.org/docs/handbook/advanced-types.html#string-literal-types) which are similar to enumerations with an underlying string value. Fable allows the same feature by using union types and the `StringEnum` attribute. These union types must not have any data fields as they will be compiled to a string matching the name of the union case.

By default, the compiled string will have the first letter lowered. If you want to prevent this or use a different text than the union case name, use the `CompiledName` attribute:

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

## JSON serialization

To make it easier to share types when you are also using F# on the server side, the functions `toJson` and `ofJson` are available in `Fable.Core.JsInterop` module. Unlike native JSON parsing in JavaScript, `ofJson` will _inflate_ a proper instance of the target type if the string is well formed, that is, if it is the result of serializing the same type with `toJson` or `Fable.JsonConverter` (see below).

```fsharp
open Fable.Core.JsInterop

type Tree =
    | Leaf of int
    | Branch of Tree list
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees ->
            trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

let tree = Branch [Leaf 1; Leaf 2; Branch [Leaf 3; Leaf 4]]

let json = toJson tree
let tree2 = ofJson<Tree> json

let typeTest = (box tree2) :? Tree      // Type is kept
let sum = tree2.Sum()   // Prototype members can be accessed
```

On the server side, you can use [Newtonsoft.Json](https://www.newtonsoft.com/json) + [Fable.JsonConverter](https://www.nuget.org/packages/Fable.JsonConverter/), as follows:

```fsharp
open Newtonsoft.Json

// Always use the same instance of the converter
// as it will create a cache to improve performance
let jsonConverter = Fable.JsonConverter() :> JsonConverter

// Serialization
JsonConvert.SerializeObject(value, [|jsonConverter|])

// Deserialization
JsonConvert.DeserializeObject<MyType>(json, [|jsonConverter|])
```

If you need more fine-grained control over the JSON serialization, please use the [Thoth library](https://mangelmaxime.github.io/Thoth/json/decode.html).
