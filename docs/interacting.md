# Interacting with JavaScript code

There are three ways to interact with the JavaScript world:

- Dynamically
- Using `Emit` attributes
- Through a foreign interface


## Dynamic programming

[Fable.Core](../lib/Fable.Core.fs) implements the F# dynamic operators so
you can easily access an object property by name (without static check) as follows:

```
open Fable.Core

printfn "Value: %O" jsObject?myProperty

jsObject?myProperty <- 5 // Assignment is also possible
```

However, the F# compiler won't let you apply the property directly to other expressions.
For that, use the operator `$` and pass the arguments as a tuple.

```
open Fable.Core

let result = jsObject?myMethod $ (1, 2)
```

If you want to call the function with the `new` keyword, use the `Fable.Core.createNew` function instead.

```
open Fable.Core

let instance = createNew jsObject?method (1, 2)
```

And when you need to create JS object literals, use `createObj`:

```
open Fable.Core

let data =
    createObj [
        "todos" ==> Storage.fetch()
        "newTodo" ==> ""
        "editedTodo" ==> None
        "visibility" ==> "all"
    ]
```
> The [todomvc sample](/sample/browser/todomvc/app.fsx) is a good example
  on how to program dynamically with Fable.


## Emit attribute

You can use the `Emit` attribute to decorate a function. Every call to the
function will then be replaced **inline** by the content of the attribute
with the placeholders `$0`, `$1`, `$2`... replaced by the arguments. For example, the following code will generate JavaScript as seen below.

```
// F#
open Fable.Core

[<Emit("$0 + $1")>]
let add (x: int) (y: string): float = failwith "JS only"

let result = add 1 "2"


// JavaScript

var result = 1 + "2"
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
the compiler where this module is coming from using the `Import` attribute.
For example, if you want to use `string_decoder` from node, just write:

```
[<Import("string_decoder")>]
module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> strings
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = failwith "JS only"
```

> If the module or value is accessible globally in JavaScript,
  you can use the `Global` attribute without parameters instead.

> If a method accepts a lambda make sure to use `System.Func` in the signature to force
  the compiler _uncurry_ any lambda passed as parameter.

> To simulate a JS constructor, name the method `createNew`.

However, modules usually expose functions and many times these functions
have elements like optional or rest parameter that are not supported by
F# module functions. Another possible technique then is to wrap the
functions in an interface an create a dummy value named `Globals` to
hold an instance of that interface:

```
module path =
    type Globals =
        abstract sep: string with get, set
        abstract delimiter: string with get, set
        abstract normalize: p: string -> string
        abstract join: [<ParamArray>] paths: obj[] -> string
        abstract join: [<ParamArray>] paths: string[] -> string
        abstract resolve: [<ParamArray>] pathSegments: obj[] -> string
        abstract isAbsolute: path: string -> bool
        abstract relative: from: string * ``to``: string -> string
        abstract dirname: p: string -> string
        abstract basename: p: string * ?ext: string -> string
        abstract extname: p: string -> string
        abstract parse: pathString: string -> ParsedPath
        abstract format: pathObject: ParsedPath -> string

    [<Import("path")>]
    let Globals: Globals = failwith "JS only"
```

Note that this time the `Import` attribute is on the `Globals` value, not the module.

A good starting point for foreign interfaces are [Typescript definition files](http://definitelytyped.org)
and there's a script to make the bulk work of translating the file into F#. You can test it as follows:

```
node tools/typescript2fable.js path/to/definition.d.ts > path/to/outfile.fs
```

> As most times, some tweaking by hand will be needed, it's planned to keep a repository with curated definitions
of the most commonly used libraries (core JS, node, React...). As of now you can find some parsed files [here](/import).


### Import parameters

The import attribute accepts query parameters, like:

```
[<Import("myModule?asDefault=true&fromLib=true")>]
```

The accepted parameters are:

```
    asDefault       Import as default: import $M1 from "myModule"
                    instead of namespace: import * as $M1 from "myModule"

    fromLib         The module is to be found in the path passed as "lib"
    	            to the compiler
                    
    get             The value is the name of a property on the module root,
                    used mainly for classes.
```
