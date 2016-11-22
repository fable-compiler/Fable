#r "../node_modules/fable-core/Fable.Core.dll"
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Node

let finalhandler = importDefault<obj> "finalhandler"
let serveStatic = importDefault<obj> "serve-static"
let opener = importDefault<string->unit> "open"

let port = 8080
let servingDir = "."

let server =
    let server =
        let serve = serveStatic$(servingDir)
        http.createServer(Func<_,_,_>(fun req res ->
            let isDone = finalhandler$(req, res)
            serve$(req, res, isDone) |> ignore))
    server.listen(port)

match ``process``.argv.[2] with null -> "" | s -> s
|> sprintf "http://localhost:%i/%s" port
|> opener
