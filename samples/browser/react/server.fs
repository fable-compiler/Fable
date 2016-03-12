module Server

module Util =
    open System
    open Fable.Core
    open Fable.Import
    open Fable.Import.Node.http

    let handleError onErr onSuccess err =
        match unbox err with
        | Some err -> onErr err |> ignore
        | None -> onSuccess() |> ignore
        
    let failExit err =
        Console.WriteLine (string err)
        Node.Globals.``process``.exit 1.
       
    type ExpressHelper =
        static member inline use3rdParty (app: express.Express, middleWare) =
            app?``use`` $ middleWare |> ignore

        static member inline useStatic (app: express.Express, targetPath: string, sourcePath: string) =
            app?``use`` $ (targetPath, express.Globals.``static`` $ sourcePath) |> ignore

        static member inline useFn (app: express.Express, f: Func<ServerRequest, ServerResponse, (unit->unit), unit>) =
            ignore(app?``use`` $ f)
        
        static member inline get (app: express.Express, path: string, f: Func<ServerRequest, ServerResponse, unit>) =
            ignore(app?get $ (path, f))

        static member inline post (app: express.Express, path: string, f: Func<ServerRequest, ServerResponse, unit>) =
            ignore(app?post $ (path, f))

        static member inline listen (app: express.Express, port: int, cb: unit->unit) =
            app?listen $ (port, cb) |> ignore
        
open Util
open Models
open Fable.Core
open Fable.Import
open Fable.Import.Node
type E = ExpressHelper

let app = express.Globals.callSelf()
let COMMENTS_FILE = path.Globals.join(Globals.__dirname, "comments.json")

let [<Import("body-parser?asDefault=true")>] bodyParser = obj()

E.use3rdParty(app, bodyParser?json $ ())
E.use3rdParty(app, bodyParser?urlencoded $ (createObj ["extended" ==> true]))

E.useStatic(app, "/", path.Globals.join(Globals.__dirname, "public"))

// Additional middleware which will set headers that we need on each request.
E.useFn(app, fun req res next ->
    // Set permissive CORS header - this allows this server to be used only as
    // an API server in conjunction with something like webpack-dev-server.
    res.setHeader("Access-Control-Allow-Origin", "*")
    // Disable caching so we'll always get the latest comments.
    res.setHeader("Cache-Control", "no-cache")
    next())

E.get(app, "/api/comments/", fun req res ->
    fs.Globals.readFile(COMMENTS_FILE, fun err data ->
        err |> handleError failExit (fun () ->
            string data |> JS.Globals.JSON.parse |> ($) res?json)))

E.post(app, "/api/comments", fun req res ->
    fs.Globals.readFile(COMMENTS_FILE, fun err data ->
        err |> handleError failExit (fun () ->
            let comments: ResizeArray<_> =
                string data |> JS.Globals.JSON.parse |> unbox
            // NOTE: In a real implementation, we would likely rely on a database or
            // some other approach (e.g. UUIDs) to ensure a globally unique id. We'll
            // treat Date.now() as unique-enough for our purposes.
            comments.Add {
                id = Some System.DateTime.Now
                author = string req?body?author
                text = string req?body?text
            }
            fs.Globals.writeFile(
                COMMENTS_FILE,
                // The F# compiler gets confused about which overload of JSON.stringify
                // to use here so just apply the method dynamically
                JS.Globals.JSON.stringify $ (comments, None, 4.),
                fun err -> err |> handleError failExit (fun () -> res?json $ comments)))))

let port =
    match unbox Globals.``process``.env?PORT with
    | Some x -> x
    | None -> 3000

E.listen(app, port, fun () ->
    printfn "Server started: http://localhost:%i/" port)
