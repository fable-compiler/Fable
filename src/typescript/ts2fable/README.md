# **ts2fable**  [![npm version](https://badge.fury.io/js/ts2fable.svg)](https://www.npmjs.com/package/ts2fable)

[Fable](https://github.com/fable-compiler/Fable) parser for [TypeScript declaration files](https://www.typescriptlang.org/docs/handbook/writing-declaration-files.html).

## Usage

Install it with npm and run the `ts2fable` command on a TypeScript declaration file.

```
npm install -g ts2fable
ts2fable react.d.ts
```

The parser just displays the results on screen, but you can
redirect the output to a file:
```
ts2fable react.d.ts > Fable.Import.React.fs
```

You can find more information about how to interact with JavaScript
from F# [here](https://github.com/fable-compiler/Fable/blob/master/docs/source/docs/interacting.md).
Please note the parser is not perfect and some tweaking by hand may be needed.

## Conventions

Some JavaScript/TypeScript features have no direct translation to F#. Here is
a list of common workarounds adopted by the parser to solve these problems:

* **Module methods**: Module functions in F# have several limitations (no overload,
optional or rest parameters). To overcome this, the parser wraps module methods
in a dummy static typed named `Globals`. For example:

```fsharp
module ReactDom =
    type [<Import("react-dom")>] Globals =
        static member render(element: DOMElement<'P>, container: Element, ?callback: Func<Element, obj>): Element = jsNative
```

> Note the `Import` attribute is on the `Globals` value, not the module.


* **Erased unions**: TypeScript union types work differently from F# and its only
purpose is to specify the types allowed for a function argument. In F# they are
translated as _erased unions_: they're checked at compiled time but they'll be
removed from the generated JS code.

```fsharp
type CanvasRenderingContext2D =
    abstract fillStyle: U3<string, CanvasGradient, CanvasPattern> with get, set

let ctx: CanvasRenderingContext2D = failwith "dummy"
ctx.fillStyle <- U3.Case1 "#FF0000"
```

* **Constructor functions**: In JS any function can become a constructor just by
calling it with the `new` keyword. In the parsed files, interfaces with this
capability will have a `Create` method attached:

```fsharp
type CanvasRenderingContext2DType =
    abstract prototype: CanvasRenderingContext2D with get, set
    [<Emit("new $0($1...)")>] abstract Create: unit -> CanvasRenderingContext2D
```

* **Callable interfaces**: In the same way, JS functions are just objects which
means applying arguments directly to any object is legal in JS. To convey, the
parser attaches an `Invoke` method to callable interfaces:

```fsharp
type Express =
    inherit Application
    abstract version: string with get, set
    abstract application: obj with get, set
    [<Emit("$0($1...)")>] abstract Invoke: unit -> Application
```


