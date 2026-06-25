module OptionalArgs

open Fable.Core
open Fable.Core.JsInterop

// An optional argument (`?x: obj`) passed to a native binding must reach the native
// function as the raw value. `obj`-typed optionals are not flattened, so without
// unwrapping the F# `Some` wrapper (`some(...)`) would leak into the generated code.

// JS binding (Fable.Core.JS)
let jsBinding (v: obj) (sp: obj) : string = JS.JSON.stringify (v, space = sp)

// Imported binding
[<Import("Thing", "my-lib")>]
type Thing =
    abstract doIt: x: int * ?y: obj -> string

let imported (t: Thing) (n: obj) : string = t.doIt (1, y = n)

// Erased binding
[<Erase>]
type Erased =
    abstract run: a: int * ?b: obj -> string

let erased (e: Erased) (n: obj) : string = e.run (1, b = n)

// Emit member
type IFoo =
    [<Emit("$0.foo($1, $2)")>]
    abstract foo: x: int * ?y: obj -> string

let emitted (foo: IFoo) (n: obj) : string = foo.foo (1, y = n)

// Plain F# member: the implementation consumes the argument as `obj option`,
// so the `some(...)` wrapper must be kept.
type Native() =
    static member Show(x: int, ?y: obj) : string = string x + string (defaultArg y (box "-"))

let fSharpMember (n: obj) : string = Native.Show(1, y = n)
