module Fable.Tools.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Net
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Parser
open State

type ProcessOptions(?envVars, ?redirectOutput) =
    member val EnvVars = defaultArg envVars Map.empty<string,string>
    member val RedirectOuput = defaultArg redirectOutput false

let startProcess workingDir fileName args (opts: ProcessOptions) =
    let fileName, args =
        let isWindows =
            #if NETFX
            true
            #else
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
            #endif
        if isWindows
        then "cmd", ("/C " + fileName + " " + args)
        else fileName, args
    printfn "CWD: %s" workingDir
    printfn "%s %s" fileName args
    let p = new Process()
    p.StartInfo.FileName <- fileName
    p.StartInfo.Arguments <- args
    p.StartInfo.WorkingDirectory <- workingDir
    p.StartInfo.RedirectStandardOutput <- opts.RedirectOuput
    opts.EnvVars |> Map.iter (fun k v ->
        p.StartInfo.Environment.[k] <- v)
    p.Start() |> ignore
    p

let runProcess workingDir fileName args =
    let p =
        ProcessOptions()
        |> startProcess workingDir fileName args
    p.WaitForExit()
    match p.ExitCode with
    | 0 -> ()
    | c -> failwithf "Process %s %s finished with code %i" fileName args c

let runProcessAndReadOutput workingDir fileName args =
    let p =
        ProcessOptions(redirectOutput=true)
        |> startProcess workingDir fileName args
    let output = p.StandardOutput.ReadToEnd()
    printfn "%s" output
    p.WaitForExit()
    output

let rec findPackageJsonDir dir =
    if File.Exists(Path.Combine(dir, "package.json"))
    then dir
    else
        let parent = Directory.GetParent(dir)
        if isNull parent then
            failwith "Couldn't find package.json directory"
        findPackageJsonDir parent.FullName

let argsToMap (argv: string[]) =
    try
        argv
        |> Array.windowed 2
        |> Array.map (fun kv -> kv.[0].Substring(2), kv.[1]) // Remove hyphens `--`
        |> Map
    with ex ->
        failwithf "Cannot convert arguments to dictionary: %A\n%s" argv ex.Message

let getFreePort () =
    let l = Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> IPEndPoint).Port
    l.Stop()
    port

let getPortAndTimeout argsMap =
    let port =
        match Map.tryFind "port" argsMap with
        | Some "free" -> getFreePort()
        | Some port -> int port
        | None -> Constants.DEFAULT_PORT
    let timeout =
        match Map.tryFind "timeout" argsMap with
        | Some timeout -> int timeout
        | None -> -1
    port, timeout

let debug (projFile: string) (define: string[]) =
    let com = Compiler()
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    try
        let state = updateState checker com (State()) None define (Path.GetFullPath projFile)
        for file in state.ActiveProject.CompiledFiles.Keys |> Seq.rev do
            com.Reset()
            compile com state.ActiveProject file |> printfn "%A"
    with
    | ex -> printfn "ERROR: %s\n%s" ex.Message ex.StackTrace

let startServer port timeout onMessage continuation =
    try
        let work = Server.start port timeout onMessage
        continuation work
    with
    | ex ->
        printfn "Cannot start server, please check the port %i is free: %s" port ex.Message

let startServerWithProcess port exec args =
    let agent = startAgent()
    startServer port -1 agent.Post <| fun listen ->
        Async.Start listen
        let workingDir = Directory.GetCurrentDirectory()
        let p =
            ProcessOptions(envVars=Map["FABLE_SERVER_PORT", string port])
            |> startProcess workingDir exec args
        Console.CancelKeyPress.Add (fun _ ->
            Server.stop port |> Async.RunSynchronously
            printfn "Killing process..."
            p.Kill())
        p.WaitForExit()
        Server.stop port |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    match Array.tryHead argv with
    | Some ("--help"|"-h") ->
        printfn """Available options:
  -h|--help           Show help
  --version           Print version
  add                 Add one or several Fable npm packages
  start               Start Fable server
  --port              Port number (default 61225) or "free" to choose a free port.
  --timeout           Stop the server if timeout (ms) is reached
  npm-run             Start a server, run an npm script and shut it down
    <script>            Name of the npm script, e.g.: `dotnet fable npm-run start`
    --port              Port number (default 61225) or "free" to choose a free port.
    --args              Args for the npm script, e.g.: `dotnet fable npm-run build --args "-p --output-filename bundle.js"`
  webpack             Start a server and invoke webpack (must be installed in current or a parent dir)
    --port              Port number (default 61225) or "free" to choose a free port.
    --args              Args for Webpack, e.g.: `dotnet fable webpack --args -p`
  shell-run           Start a server, run an abritrary command and shut it down
    <cmd>            Name of the command to run, e.g.: `dotnet fable shell-run make`
    --port              Port number (default 61225) or "free" to choose a free port.
    --args              Args for the command, e.g.: `dotnet fable shell-run make --args "build"`
  webpack-dev-server  Same as `webpack` command but invokes webpack-dev-server
"""
    | Some "--version" -> printfn "%s" Constants.VERSION
    | Some "start" ->
        let port, timeout = argv.[1..] |> argsToMap |> getPortAndTimeout
        let agent = startAgent()
        startServer port timeout agent.Post Async.RunSynchronously
    | Some "npm-run" ->
        let argsMap = argv.[2..] |> argsToMap
        let port, _ = argsMap |> getPortAndTimeout
        let execArgs =
            match Map.tryFind "args" argsMap with
            | Some npmArgs -> "run " + argv.[1] + " -- " + npmArgs
            | None -> "run " + argv.[1]
        startServerWithProcess port "npm" execArgs
    | Some ("webpack" | "webpack-dev-server" as webpack) ->
        let argsMap = argv.[1..] |> argsToMap
        let port, _ = argsMap |> getPortAndTimeout
        let workingDir = Directory.GetCurrentDirectory()
        let webpackScript =
            let webpackScript = Path.Combine(findPackageJsonDir workingDir, "node_modules", webpack, "bin", webpack + ".js")
            match Map.tryFind "args" argsMap with
            | Some args -> webpackScript + " " + args
            | None -> webpackScript
        startServerWithProcess port "node" webpackScript
    | Some "shell-run" ->
        let cmd = argv.[1]
        let argsMap = argv.[2..] |> argsToMap
        let port, _ = argsMap |> getPortAndTimeout
        let execArgs =
            match Map.tryFind "args" argsMap with
            | Some args -> args
            | None -> ""
        startServerWithProcess port cmd execArgs
    | Some "add" ->
        let workingDir = Directory.GetCurrentDirectory()
        let packages = argv.[1..]
        let packages =
            match packages with
            | [||] -> failwith "Missing packages to install"
            | [|package|] ->
                let peers =
                    sprintf "show %s peerDependencies" package
                    |> runProcessAndReadOutput workingDir "npm"
                if String.IsNullOrWhiteSpace(peers)
                then [|package|]
                else
                    JsonConvert.DeserializeObject<Map<string,string>>(peers)
                    |> Seq.map (fun kv -> kv.Key + "@" + kv.Value)
                    |> Seq.append [|package|]
                    |> Seq.toArray
            | packages -> packages
        runProcess workingDir "npm" ("install --save-dev " + (String.concat " " packages))
        let nodeModulesDir = Path.Combine(findPackageJsonDir workingDir, "node_modules")
        for pkg in packages do
            let pkg =
                match pkg.IndexOf("@") with
                | -1 -> pkg
                | i -> pkg.Substring(0,i)
            let pkgDir = Path.Combine(nodeModulesDir, pkg)
            Directory.GetFiles(pkgDir, "*.fsproj") |> Array.tryHead |> function
            | Some projRef ->
                runProcess workingDir "dotnet" ("add reference \"" + projRef + "\"")
            | None ->
                printfn "Cannot find .fsproj in %s" pkgDir
        runProcess workingDir "dotnet" "restore"
    | Some "debug" ->
        debug argv.[1] argv.[2..]
    | Some cmd -> printfn "Unrecognized command: %s. Use `dotnet fable --help` to see available options" cmd
    | None -> printfn "Command missing. Use `dotnet fable --help` to see available options"
    0
