module rec Fable.Import.Node.Globals

open Fable.Import.Node
open Fable.Import.Node.Process
open Fable.Import.Node.Base

open Fable.Core


let [<Global>] ``global``: NodeJS.Global = jsNative
let [<Global>] __filename: string = jsNative
let [<Global>] __dirname: string = jsNative
let [<Global>] require: NodeJS.NodeRequire = jsNative
let [<Global>] ``module``: NodeJS.NodeModule = jsNative

let [<Global>] exports: obj = jsNative

let [<Global>] undefined: obj = jsNative
let [<Global>] SlowBuffer: Buffer.SlowBufferStatic = jsNative
let [<Global>] Buffer: Buffer.BufferStatic = jsNative
let [<Global>] ``process``:Process = jsNative