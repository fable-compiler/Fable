module Fable.Tests.DllRef2.Lib

open Fable.Core
open Fable.Core.JsInterop

#if FABLE_COMPILER
/// Including JS files in compilation works
let foo: string = importMember "./js1/lib.js"

[<Import("fooGenerator","./js2/lib.js")>]
let fooGenerator(i: int): string = jsNative
#else
let foo = "foo"
let fooGenerator(i: int): string =
    String.replicate i "foo"
#endif
