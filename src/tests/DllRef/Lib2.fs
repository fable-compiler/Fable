module Fable.Tests.DllRef.Lib2

open Fable.Core
open Fable.Core.JsInterop

#if FABLE_COMPILER
/// Including same JS file from different F# sources works
let foo: string = importMember "./js1/lib.js"

/// Classes from included JS files work
[<Import("Bar","./js2/lib.js")>]
type Bar(i: int) =
    member __.generator() = jsNative
#else
let foo = "foo"

type Bar2(i: int) =
    member __.generator() =
        String.replicate i "bar"
#endif
