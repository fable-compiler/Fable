#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-express/Fable.Import.Express.fs"

open System
open Fable.Core
open Fable.Import

let app = express.Invoke()

app.get(U2.Case1 "/hello/:name", fun (req: express.Request) (res: express.Response) _ ->
    res.send(sprintf "Hello %O" req.``params``?name)
    |> box)
|> ignore

let port =
    match unbox Node.``process``.env?PORT with
    | Some x -> x | None -> 8080

app.listen(port, unbox (fun () ->
    printfn "Server started: http://localhost:%i/" port))
|> ignore
