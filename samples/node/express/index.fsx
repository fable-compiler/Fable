#r "node_modules/fable-import/Fable.Import.dll"
#load "node_modules/fable-import-express/Fable.Import.Express.fs"

open System
open Fable.Core
open Fable.Import
open Fable.Import.express

let app = express.Globals.Invoke()

app.get(U2.Case1 "/hello/:name", fun (req: Request) (res: Response) _ ->
    res.send(sprintf "Hello %O" req.``params``?name)
    |> box)
|> ignore

let port =
    match unbox Node.Globals.``process``.env?PORT with
    | Some x -> x | None -> 8080

app.listen(port, unbox (fun () ->
    printfn "Server started: http://localhost:%i/" port))
|> ignore
