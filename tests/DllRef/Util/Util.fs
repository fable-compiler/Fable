module Fable.Tests.DllRef.Util

/// Check that referencing a subfolder works with dlls
let add2 x = x + 2

#if FABLE_COMPILER
[<Fable.Core.Import("two", "../numbers.js")>]
let two: int = Fable.Core.Util.jsNative
let four: int = Fable.Core.JsInterop.importMember "../numbers.js"
#else
let two: int = 2
let four: int = 4
#endif

