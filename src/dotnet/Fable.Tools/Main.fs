module Fable.Tools.Main

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Parser
open State

let startProcess workingDir fileName args =
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

let getPortAndTimeout argsMap =
    let port =
        match Map.tryFind "port" argsMap with
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
        let state = updateState checker com None None define (Path.GetFullPath projFile)
        for file in state.CompiledFiles.Keys |> Seq.rev do
            com.Reset()
            compile com state file |> printfn "%A"
    with
    | ex -> printfn "ERROR: %s\n%s" ex.Message ex.StackTrace

let startServerWithProcess port exec args =
    let agent = startAgent()
    Server.start port -1 agent.Post |> Async.Start
    let workingDir = Directory.GetCurrentDirectory()
    let p = startProcess workingDir exec args
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
  --port              Port number (default 61225)
  --timeout           Stop the server if timeout (ms) is reached
  npm-run             Start a server, run an npm script and shut it down
    <script>            Name of the npm script, e.g.: `dotnet fable npm-run start`
    --port              Port number (default 61225)
    --args              Args for the npm script, e.g.: `dotnet fable npm-run build --args "-p --output-filename bundle.js"`
  webpack             Start a server and invoke webpack (must be installed in current or a parent dir)
    --port              Port number (default 61225)
    --args              Args for Webpack, e.g.: `dotnet fable webpack --args -p`
  webpack-dev-server  Same as `webpack` command but invokes webpack-dev-server
"""
    | Some "--version" -> printfn "%s" Constants.VERSION
    | Some "start" ->
        let port, timeout = argv.[1..] |> argsToMap |> getPortAndTimeout
        let agent = startAgent()
        Server.start port timeout agent.Post |> Async.RunSynchronously
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
    | Some "add" ->
        let packages = argv.[1..]
        let workingDir = Directory.GetCurrentDirectory()
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
                runProcess workingDir "dotnet" ("add reference " + projRef)
            | None ->
                printfn "Cannot find .fsproj in %s" pkgDir
        runProcess workingDir "dotnet" "restore"
    | Some "debug" ->
        debug argv.[1] argv.[2..]
    | Some cmd -> printfn "Unrecognized command: %s. Use `dotnet fable --help` to see available options" cmd
    | None -> printfn "Command missing. Use `dotnet fable --help` to see available options"
    0
