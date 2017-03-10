module Fable.Client.Webpack.Main

open System
open System.Reflection
open State

let [<Literal>] DEFAULT_PORT = 61225

let version = lazy Assembly.GetEntryAssembly().GetName().Version

let argsToMap (argv: string[]) =
    try
        argv
        |> Array.windowed 2
        |> Array.map (fun kv -> kv.[0].Substring(2), kv.[1]) // Remove hyphens `--`
        |> Map
    with ex ->
        failwithf "Cannot convert arguments to dictionary: %A\n%s" argv ex.Message

let startServer argsMap =
    let port, timeout =
        let port =
            match Map.tryFind "port" argsMap with
            | Some port -> int port
            | None -> DEFAULT_PORT
        let timeout =
            match Map.tryFind "timeout" argsMap with
            | Some timeout -> int timeout
            | None -> -1
        port, timeout
    let agent = startAgent()
    Server.start port timeout agent.Post
    |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    match Array.tryHead argv with
    | Some ("--help"|"-h") ->
        printfn """Available options:
  -h|--help      Show help
  --version      Print version
  start          Start Fable server
    --port         Port number (default 61225)
    --timeout      Stop the server if timeout (ms) is reached
"""
    | Some "--version" -> printfn "%O" version.Value
    | Some "start" -> argv.[1..] |> argsToMap |> startServer
    | Some cmd -> failwithf "Unrecognized command: %s. Use `dotnet fable --help` to see available options" cmd
    | None -> failwith "Command missing. Use `dotnet fable --help` to see available options"
    0
