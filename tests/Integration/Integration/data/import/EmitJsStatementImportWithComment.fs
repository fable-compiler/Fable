module EmitJsStatementImportWithComment

open Fable.Core.JsInterop

let getSquare (x: int) : int =
    emitJsStatement x """import { square } from "./js/1foo.js" // trailing comment
return square($0)"""
