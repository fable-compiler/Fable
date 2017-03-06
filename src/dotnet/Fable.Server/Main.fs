module Fable.Client.Webpack.Main

open System
open State

let [<Literal>] DEFAULT_PORT = 61225

[<EntryPoint>]
let main argv =
    let port =
        if argv.Length > 2
        then
            match Int32.TryParse(argv.[1]) with
            | true, port -> port
            | false, _ -> DEFAULT_PORT
        else DEFAULT_PORT
    let agent = startAgent()
    // TODO: Add help text
    Server.start port agent.Post
    |> Async.RunSynchronously
    0
