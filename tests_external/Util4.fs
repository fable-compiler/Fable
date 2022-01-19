module Fable.Tests.Util4

open Fable.Core

#if FABLE_COMPILER
let foo: string = JsInterop.importMember "../tests/Js/Main/js/1foo.js"

let bar: int = JsInterop.importMember "./bar.js"

[<Import("bar", "./bar.js")>]
let bar2: int = jsNative
#else
let foo = "foo"
let bar = 5
let bar2 = 5
#endif