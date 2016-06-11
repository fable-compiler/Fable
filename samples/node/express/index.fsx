(**
 - title: Using Express web framework
 - tagline: Interoperating with JavaScript libraries

This demo uses Fable to create a web application using [Express](http://expressjs.com/),
a fast, unopinionated, minimalist web framework for Node.js. You can see full source code here, or
view the [raw source](https://github.com/fsprojects/Fable/blob/master/samples/node/server/index.fsx)
on GitHub. The application configuration is in 
[package.json](https://github.com/fsprojects/Fable/blob/master/samples/node/server/package.json) and
[fableconfig.json](https://github.com/fsprojects/Fable/blob/master/samples/node/server/fableconfig.json)
specifies Fable parameters.

## Using Express bindings

Fable comes with bindings for a number of sample node.js libraries including Express. You can
[view and contribute to it](https://github.com/fsprojects/Fable/tree/master/import/express) on GitHub.
The following references the Express bindings:

*)
#r "node_modules/fable-core/Fable.Core.dll"
#I "node_modules/fable-import-express"
#load "Fable.Import.Express.fs"

open System
open Fable.Core
open Fable.Import
(**

## Defining Express bindings

For detailed documentation on defining Fable bindings, see the [Interacting with JavaScript](/docs/interacting.html)
page. We won't cover all the details, but we briefly look at two snippets from the 
[Fable.Import.Express.fs](https://github.com/fsprojects/Fable/blob/master/import/express/Fable.Import.Express.fs) file
on GitHub.

The first snippet defines the `Express` type. In JavaScript, this is a callable function, which is mapped
to a method with a special `Emit` attribute in F#:

    type Express =
      inherit Application
      abstract version: string with get, set
      abstract mime: string with get, set
      abstract application: obj with get, set
      abstract request: Request with get, set
      abstract response: Response with get, set
      
      [<Emit("$0($1...)")>] 
      abstract Invoke: unit -> Application

The second interesting type definition defines the `IRouter<'T>` type. The `Application` object
inherits from router and so once we obtain `Application` using `express.Invoke`, we will be able
to call its methods to specify handlers for routes:

    type IRouter<'T> =
      abstract get: name: U2<string, Regex> 
        * [<ParamArray>] handlers: RequestHandler[] -> obj
      abstract post: name: U2<string, Regex> 
        * [<ParamArray>] handlers: RequestHandler[] -> obj
      abstract put: name: U2<string, Regex> 
        * [<ParamArray>] handlers: RequestHandler[] -> obj

One interesting thing here is that `get`, `put` and `post` can take either a regular expression
or a string. In JavaScript, you just pass the object to the function. In F#, this is mapped to a simple
two-case discriminated union, so that you get static checking. We'll see how to use these mappings next.

## Calling Express bindings

The application itself is now very simple. We call `express.Invoke` to get an instance of the 
Express server, we specify one sample binding for GET request using `app.get`, we get a port
for our server and start it:
*)
let app = express.Invoke()

// Handle request using plain `string` route specification
app.get
  ( U2.Case1 "/hello/:name", 
    fun (req:express.Request) (res:express.Response) _ ->
      res.send(sprintf "Hello %O" req.``params``?name) |> box)
|> ignore

// Get PORT environment variable or use default
let port =
  match unbox Node.``process``.env?PORT with
  | Some x -> x | None -> 8080

// Start the server on the port
app.listen(port, unbox (fun () ->
  printfn "Server started: http://localhost:%i/" port))
|> ignore
(**
Note that when calling `app.get`, the first parameter is a plain string value and so we wrap
it using `U2.Case1`. If you wanted to specify a regular expression, you'd use a `RegEx` object
wrapped in `U2.Case2`.
*)