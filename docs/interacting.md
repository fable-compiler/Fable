# Interacting with JavaScript code

There are three ways to interact with the JavaScript world:

- Dynamically
- Using `Emit` attributes
- Through a foreign interface


## Dynamic programming

[Fable.Core](/import/core/Fable.Core.fs) implements the F# dynamic operators so
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

> The [todomvc sample](/samples/browser/todomvc/) is a good example
on how to program dynamically with Fable.


## Emit attribute

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

> If the module or value is globally accessible in JavaScript,
you can use the `Global` attribute without parameters instead.

> If a method accepts a lambda make sure to use `System.Func` in the signature to force
the compiler _uncurry_ any lambda passed as parameter.

A good starting point for foreign interfaces are [Typescript definition files](http://definitelytyped.org)
and there's a script to make the bulk work of translating the file into F#. You can install it from npm.
See the [README](https://www.npmjs.com/package/ts2fable) for more information.

```shell
npm install -g ts2fable
```

You can find common definitions already parsed [here](/import). Some of them are available
in npm, just search for `fable-import` packages.


## Import attribute

The `Import` attribute can be applied to modules, types and even functions.
It will translate to [ES2015 import statements][import statement](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import),
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
