module Fable.Tests.Sibling.Util

/// Check that referencing a subfolder works with dlls
let add2 x = x + 2

[<Fable.Core.Import("two", "../numbers.js")>]
let two: int = failwith "JS only"

let four: int = Fable.Core.JsInterop.importMember "../numbers.js"
