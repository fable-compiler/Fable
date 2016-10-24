module Fable.Tests.Util4

open Fable.Core

#if FABLE_COMPILER
let foo: string = JsInterop.importMember "../tests/js/foo.js"

let bar: int = JsInterop.importMember "./bar.js"

[<Import("bar", "./bar.js")>]
let bar2: int = jsNative
#endif