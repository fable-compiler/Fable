module Server

open System
open Models
open Fable.Core
open Fable.Import
open Fable.Import.express

module Util =
    let handleError onErr onSuccess err =
        match unbox err with
        | Some err -> onErr err |> ignore
        | None -> onSuccess() |> ignore
        
    let failExit err =
        Console.WriteLine (string err)
        Node.Globals.``process``.exit 1.

open Util

let app = express.Globals.callSelf()
let COMMENTS_FILE = Node.path.Globals.join(Node.Globals.__dirname, "comments.json")

// Just use dynamic programming to set body-parser middleware
let [<Import("body-parser?asDefault=true")>] bodyParser = obj()

// As app.use expects a ParamArray, the explicit unbox is necessary
// so the compiler doesn't think it's an array
bodyParser?json $ ()
|> unbox<RequestHandler> |> app.``use``
|> ignore

bodyParser?urlencoded $ (createObj ["extended" ==> true])
|> unbox<RequestHandler> |> app.``use``
|> ignore

// Serve static files from public folder
Node.path.Globals.join(Node.Globals.__dirname, "public")
|> express.Globals.``static``.callSelf
|> fun sta -> app.``use``("/", sta)
|> ignore

// Additional middleware which will set headers that we need on each request.
app.``use``(fun (req: Request) (res: Response) (next: obj->unit) ->
    // Set permissive CORS header - this allows this server to be used only as
    // an API server in conjunction with something like webpack-dev-server.
    res.setHeader("Access-Control-Allow-Origin", "*")
    // Disable caching so we'll always get the latest comments.
    res.setHeader("Cache-Control", "no-cache")
    box <| next())
|> ignore

app.get(U2.Case1 "/api/comments/", fun req res _ ->
    box <| Node.fs.Globals.readFile(COMMENTS_FILE, fun err data ->
        err |> handleError failExit (fun () ->
            string data |> JS.Globals.JSON.parse |> ($) res?json)))
|> ignore

app.post(U2.Case1 "/api/comments/", fun req res _ ->
    box <| Node.fs.Globals.readFile(COMMENTS_FILE, fun err data ->
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
            Node.fs.Globals.writeFile(
                COMMENTS_FILE,
                // The F# compiler gets confused about which overload of JSON.stringify
                // to use here so just apply the method dynamically
                JS.Globals.JSON.stringify $ (comments, None, 4.),
                fun err -> err |> handleError failExit (fun () -> res?json $ comments)))))
|> ignore

let port =
    match unbox Node.Globals.``process``.env?PORT with
    | Some x -> x
    | None -> 3000

app.listen(port, unbox (fun () ->
    printfn "Server started: http://localhost:%i/" port))
|> ignore
