module ConsoleApp

open Fable.Core
open Fable.Core.JsInterop

[<Emit("console.log($0)")>]
let log (x: 'a) : unit = jsNative

log "Hello from Node"