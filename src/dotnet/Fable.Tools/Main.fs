module Fable.Client.Webpack.Main

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open State

let [<Literal>] VERSION = "1.0.0-narumi-14"
let [<Literal>] DEFAULT_PORT = 61225

let startProcess workingDir fileName args =
    let fileName, args =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
        then "cmd", ("/C " + fileName + " " + args)
        else fileName, args
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- fileName
    p.StartInfo.Arguments <- args
    p.StartInfo.WorkingDirectory <- workingDir
    p.Start() |> ignore
    p

let runProcess workingDir fileName args =
    let p = startProcess workingDir fileName args
    p.WaitForExit()
    match p.ExitCode with
    | 0 -> ()
    | c -> failwithf "Process %s %s finished with code %i" fileName args c

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
  npm-run        Start a server, run an npm script and shut it down
    <script>       Name of the npm script, e.g.: `dotnet fable npm-run start`
    --port         Port number (default 61225)
  add            Adds one or several Fable npm packages
"""
    | Some "--version" -> printfn "%s" VERSION
    | Some "start" -> argv.[1..] |> argsToMap |> startServer |> Async.RunSynchronously
    | Some "npm-run" ->
        argv.[2..] |> argsToMap |> startServer |> Async.Start
        let workingDir = Directory.GetCurrentDirectory()
        let p = startProcess workingDir "npm" ("run " + argv.[1])
        p.WaitForExit()
    | Some "add" ->
        let packages = argv.[1..]
        let workingDir = Directory.GetCurrentDirectory()
        runProcess workingDir "npm" ("install --save" + (String.concat " " packages))
        let rec findPackageJsonDir dir =
            if File.Exists(Path.Combine(dir, "package.json"))
            then dir
            else
                let parent = Directory.GetParent(dir)
                if isNull parent then
                    failwith "Couldn't find package.json directory"
                findPackageJsonDir parent.FullName
        let pkgJsonDir = findPackageJsonDir workingDir
        for pkg in packages do
            let pkg =
                match pkg.IndexOf("@") with
                | -1 -> pkg
                | i -> pkg.Substring(0,i)
            let projRef = Path.Combine(pkgJsonDir, "node_modules", pkg, pkg.Replace("-", ".") + ".fsproj")
            runProcess workingDir "dotnet" ("add reference " + projRef)

    | Some cmd -> printfn "Unrecognized command: %s. Use `dotnet fable --help` to see available options" cmd
    | None -> printfn "Command missing. Use `dotnet fable --help` to see available options"
    0
