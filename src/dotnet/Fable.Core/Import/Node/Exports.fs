[<AutoOpen>]
module Fable.Import.Node.Exports

open Fable.Import.Node
open Fable.Core

[<Import("*", "buffer")>] 
let Buffer: Buffer.IExports = jsNative

[<Import("*", "child_process")>]
let ChildProcess: ChildProcess.IExports = jsNative

[<Import("*", "events")>]
let Events: Events.IExports = jsNative

[<Import("*", "fs")>]
let Fs: Fs.IExports = jsNative

[<Import("*","net")>]
let Net: Net.IExports = jsNative

[<Import("*", "querystring")>]
let Querystring: Querystring.IExports = jsNative

[<Import("*", "stream")>]
let Stream: Stream.IExports = jsNative

[<Import("*", "url")>]
let Url: Url.IExports = jsNative