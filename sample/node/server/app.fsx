// Simple static server with node

// Load and open Fable.Core to get access to Fable attributes and operators
#load "../../../lib/Fabel.Core.fs"
open System
open Fabel.Core

let [<Global>] ``process`` = obj()
let [<Import("http")>] http = obj()
// Note we must set `asDefault` option when importing these
// components to prevent they're imported as namespaces
let [<Import("finalhandler?asDefault=true")>] finalhandler = obj()
let [<Import("serve-static?asDefault=true")>] serveStatic = obj()

let port =
    let args: string[] = unbox ``process``?argv
    if args.Length >= 3
    then int args.[2]
    else 8080

let server =
    let serve = serveStatic $ "./"
    let server =
        // As this lambda has more than one argument, it must be
        // converted to delegate so it's called back correctly
        http?createServer $ Func<_,_,_>(fun req res ->
            let isDone = finalhandler $ (req, res)
            serve $ (req, res, isDone))
    server?listen $ port

// We can use also `printfn` here
Console.WriteLine("Server running at localhost:{0}", port)
