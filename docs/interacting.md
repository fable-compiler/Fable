# Interacting with JavaScript code

There are three ways to interact with the JavaScript world:

- Dynamically
- Using `Emit` attributes
- Through a foreign interface

> NOTE: Several of these features still need to be implemented


## Dynamic programming

`Fabel.Core` implements the F# dynamic operators so you can easily access an object property by name (without static check) as follows:

```
open Fabel.Core

printfn "Value: %O" jsObject?property

jsObject?property <- 5 // Assignment is also possible
```

However, the F# compiler won't let you apply the property, so to convince it that's actually a function use the operator `$` and pass the arguments as a tuple.

```
open Fabel.Core

let result = jsObject?method $ (1, 2)
```

If you want to call the function with the `new` keyword, use the `Fabel.Core.createNew` function instead.

```
open Fabel.Core

let instance = createNew jsObject?method (1, 2)
```


## Emit attribute

You can use the `Emit` attribute to decorate a function. Every call to the function will then be replaced **inline** by the content of the attribute with the placeholders replaced by the arguments. For example, the following code will generate JavaScript as seen below.

```
// F#
open Fabel.Core

[<Emit("$0 + $1")>]
let add (x: int) (y: string): float = failwith "JS only"

let result = add 1 "2"


// JavaScript

var result = 1 + "2"
```

The content of `Emit` will actually be parsed by Babel so it will still validated somehow. However, it's not advised to abuse of this method, because the code in the template will remain obscure to Fabel preventing some optimizations.


## Foreign interfaces

Defining a foreign interface is trivial: just create a F# interface and the compiler will call the interface properties or methods by name. The tricky part is to tell the compiler where the objects should be retrieved from. Normally, they will be exposed as values of an imported module, so you just need to indicate the compiler where this module is coming from using the `Import` attribute. For example, if you want to use the `string_decoder` module from node, just write:

```
[<Import("string_decoder")>]
module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> string
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = failwith "JS only"
```

However, modules usually expose functions and many times these functions have elements like optional or rest parameter that are not supported by F# module functions. So another technique is to wrap the functions in an interface an create a dummy value named `Global` as an instance of that interface:

```
module path =
    type Global =
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
    let Global: Global = failwith "JS only"
```

Note that this time the `Import` attribute is on the `Global` property, not the module.

If a method accepts a lambda make sure to use `System.Func` to force the compiler _uncurry_ any lambda passed as parameter.

> TODO: Classes

A good starting point for foreign interfaces are [Typescript definition files](http://definitelytyped.org) and there's a script (still in development) to make the bulk work of translating the file to F#. You can test it as follows:

```
npm install typescript      // First time if not installed globally

node tools/typescript2fabel.js
```

> As most times, some tweaking by hand will be needed, it's planned to keep a repo with curated definitions of the most commonly used libraries (core JS, node, React...)


### Import parameters

The import attribute accepts arguments passed as if they were URL parameters, like follows:

```
[<Import("myModule?asDefault=true&fromLib=true")>]
```

The accepted parameters are:

```
    asDefault       Import as default: import $M1 from "myModule"
                    instead of namespace: import * as $M1 from "myModule"

    fromLib         The module is to be found in the path passed as "lib"
    	            to the compiler
                    
    get             The value is the name of a property on the module route,
                    used mainly for classes.
```