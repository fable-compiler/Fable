// Simple static server with node

// Load and open Fable.Core to get access to Fable attributes and operators
#load "node_modules/fable-core/Fable.Core.fs"
#load "node_modules/fable-import-js/Fable.Import.JS.fs"
#load "node_modules/fable-import-node/Fable.Import.Node.fs"

open System
open Fable.Core
open Fable.Import.Node

// Note we must set `asDefault` option when importing these
// components to prevent they're imported as namespaces
let [<Import("finalhandler?asDefault=true")>] finalhandler = obj()
let [<Import("serve-static?asDefault=true")>] serveStatic = obj()

let port =
    match Globals.``process``.argv with
    | args when args.Count >= 3 -> int args.[2]
    | _ -> 8080

let server =
    let serve = serveStatic $ "./"
    let server =
        // As this lambda has more than one argument, it must be
        // converted to delegate so it's called back correctly
        http.Globals.createServer(Func<_,_,_>(fun req res ->
            let isDone = finalhandler $ (req, res)
            serve $ (req, res, isDone)
            |> ignore))
    server.listen port

// We can use also `printfn` here
Console.WriteLine("Server running at localhost:{0}", port)
