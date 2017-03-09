module Fable.Client.Webpack.Main

open System
open State

let [<Literal>] DEFAULT_PORT = 61225

let parseArgs (argv: string[]) =
    argv
    |> Array.windowed 2
    |> Array.map (fun kv -> kv.[0].Substring(2), kv.[1])
    |> Map

[<EntryPoint>]
let main argv =
    let port, timeout =
        try
            let args = parseArgs argv
            let port =
                match Map.tryFind "port" args with
                | Some port -> int port
                | None -> DEFAULT_PORT
            let timeout =
                match Map.tryFind "timeout" args with
                | Some timeout -> int timeout
                | None -> -1
            port, timeout
        with ex ->
            failwithf "Cannot parse arguments: %A\n%s" argv ex.Message
    let agent = startAgent()
    // TODO: Add help text
    Server.start port timeout agent.Post
    |> Async.RunSynchronously
    0
