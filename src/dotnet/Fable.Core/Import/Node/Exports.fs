[<AutoOpen>]
module Fable.Import.Node.Exports

open Fable.Import.Node
open Fable.Core

[<Import("*", "querystring")>]
let querystring: Querystring.IExports = jsNative

[<Import("*", "url")>]
let url: Url.IExports = jsNative