module Fable.Tests.DllRef2.Lib2

open Fable.Core
open Fable.Core.JsInterop

#if FABLE_COMPILER
/// Including same JS file from different F# sources works
let foo: string = importMember "./js1/lib.js"
#else
let foo = "foo"
#endif
