[<AutoOpen>]
module Fable.Import.Node.Exports

open Fable.Import.Node
open Fable.Core

[<Import("*", "buffer")>] 
let buffer: Buffer.IExports = jsNative

[<Import("*", "child_process")>]
let child_process: ChildProcess.IExports = jsNative

[<Import("*", "events")>]
let events: Events.IExports = jsNative

[<Import("*", "fs")>]
let fs: Fs.IExports = jsNative

[<Import("*","net")>]
let net: Net.IExports = jsNative

[<Import("*", "querystring")>]
let querystring: Querystring.IExports = jsNative

[<Import("*", "stream")>]
let stream: Stream.IExports = jsNative

[<Import("*", "url")>]
let url: Url.IExports = jsNative