[<AutoOpen>]
module Fable.Import.Node.Exports

open Fable.Import.Node
open Fable.Core

[<Import("*", "querystring")>]
let Querystring: Querystring.IExports = jsNative

[<Import("*", "url")>]
let Url: Url.IExports = jsNative