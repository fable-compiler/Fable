module Fable.CLI.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Net
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Parser
open StateUtil

type ProcessOptions(?envVars, ?redirectOutput) =
    member val EnvVars = defaultArg envVars Map.empty<string,string>
    member val RedirectOuput = defaultArg redirectOutput false

type Arguments =
    { timeout: int; port: int; commandArgs: string option }

let konst k _ = k

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
    #if NETFX
    p.StartInfo.UseShellExecute <- false
    #endif
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

let getFreePort () =
    let l = Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
    l.Start()
    let port = (l.LocalEndpoint :?> IPEndPoint).Port
    l.Stop()
    port

let tryFindArgValue key (args: string[]) =
    args
    |> Array.takeWhile (fun arg -> arg <> "--")
    |> Array.tryFindIndex ((=) key)
    |> function
    // i is the index of the key
    // i + 1 is the index of key value
    | Some i ->
        // Check if args.[i] is the last element or has no value
        if args.Length = i + 1 || args.[i + 1].StartsWith("--")
        then Some ""
        else Some args.[i + 1]
    | None ->
        None

let parseArguments args =
    let port =
        match tryFindArgValue "--port" args with
        | Some "free" -> getFreePort()
        | Some portArg ->
            // make sure port is parsable as an integer
            match Int32.TryParse portArg with
            | true, port -> port
            | false, _ ->
                printfn "Value for --port is not a valid integer, using default port"
                Constants.DEFAULT_PORT
        | None -> Constants.DEFAULT_PORT
    let timeout =
        match tryFindArgValue "--timeout" args with
        | Some timeout -> int timeout
        | None -> -1
    let commandArgs =
        // Check first --args for compatibility with the old way
        match tryFindArgValue "--args" args with
        | Some commandArgs -> Some commandArgs
        | None ->
            match args |> Array.tryFindIndex ((=) "--") with
            | Some i -> args.[(i+1)..] |> String.concat " " |> Some
            | None -> None
    { port = port; timeout = timeout; commandArgs = commandArgs}

let debug (projFile: string) (define: string[]) =
    try
        let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
        let msg = { path=(Path.GetFullPath projFile); define=define; plugins=[||]; options=Fable.State.getDefaultOptions(); extra = dict [] }
        let _, state, project = updateState checker Map.empty msg
        for file in project.ProjectOptions.ProjectFileNames |> Seq.rev do
            let com = Fable.State.Compiler()
            compile com project file |> printfn "%A"
    with
    | ex -> printfn "ERROR: %s\n%s" ex.Message ex.StackTrace

let startServer port timeout onMessage continuation =
    try
        let work = Server.start port timeout onMessage
        continuation work
    with
    | ex ->
        printfn "Cannot start Fable daemon, please check the port %i is free: %s" port ex.Message
        1

let startServerWithProcess workingDir port exec args =
    let mutable disposed = false
    let killProcessAndServer =
        fun (p: Process) ->
            if not disposed then
                disposed <- true
                printfn "Killing process..."
                p.Kill()
                Server.stop port |> Async.RunSynchronously
    let agent = startAgent()
    startServer port -1 agent.Post <| fun listen ->
        Async.Start listen
        let p =
            ProcessOptions(envVars=Map["FABLE_SERVER_PORT", string port])
            |> startProcess workingDir exec args
        Console.CancelKeyPress.Add (fun _ -> killProcessAndServer p)
        #if NETFX
        System.AppDomain.CurrentDomain.ProcessExit.Add (fun _ -> killProcessAndServer p)
        #else
        System.Runtime.Loader.AssemblyLoadContext.Default.add_Unloading(fun _ -> killProcessAndServer p)
        #endif
        p.WaitForExit()
        disposed <- true
        Server.stop port |> Async.RunSynchronously
        p.ExitCode

let checkFlags(args: string[]) =
    let hasFlag flag =
        match tryFindArgValue flag args with
        | Some _ -> true
        | None -> false
    Flags.logVerbose <- hasFlag "--verbose"
    Flags.checkCoreVersion <- not(hasFlag "--no-version-check")

let (|StartsWith|_|) (pattern: string) (str: string) =
    if str.StartsWith(pattern)
    then str.Substring(pattern.Length) |> Some
    else None

let printHelp() =
    (Constants.VERSION, Constants.DEFAULT_PORT) ||> printfn """Fable F# to JS compiler (%s)
Usage: dotnet fable [command] [script] [fable arguments] [-- [script arguments]]

Commands:
  -h|--help           Show help
  --version           Print version
  start               Start Fable daemon
  npm-run             Run Fable while an npm script is running
  yarn-run            Run Fable while a yarn script is running
  node-run            Run Fable while a node script is running
  shell-run           Run Fable while a shell script is running
  webpack             Start Fable daemon, invoke Webpack and shut it down
  webpack-dev-server  Run Fable while Webpack development server is running

Fable arguments:
  --timeout           Stop the daemon if timeout (ms) is reached
  --port              Port number (default %d) or "free" to choose a free port
  --verbose           Print more info during execution

To pass arguments to the script, write them after `--`. Example:

    dotnet fable npm-run build --port free -- -p --config webpack.production.js

You can use shortcuts for npm and yarn scripts in the following way:

    dotnet fable yarn-start       # Same as `dotnet fable yarn-run start`
"""

let runNpmOrYarn npmOrYarn (args: string[]) =
    if args.Length = 0 then
        printfn """Missing argument after %s-run, expected the name of a script. Examples:

    dotnet fable %s-run start
    dotnet fable %s-run build

Where 'start' and 'build' are the names of scripts in package.json:

    "scripts" :{
        "start": "webpack-dev-server"
        "build": "webpack"
    }""" npmOrYarn npmOrYarn npmOrYarn
        0
    else
        let fableArgs = args.[1..] |> parseArguments
        let execArgs =
            match fableArgs.commandArgs with
            | Some cargs -> "run " + args.[0] + " -- " + cargs
            | None -> "run " + args.[0]
        let workingDir =
            Directory.GetCurrentDirectory() |> findPackageJsonDir
        startServerWithProcess workingDir fableArgs.port npmOrYarn execArgs

[<EntryPoint>]
let main argv =
    checkFlags(argv)
    match Array.tryHead argv with
    | Some ("--help"|"-h") ->
        printHelp(); 0
    | Some "--version" -> printfn "%s" Constants.VERSION; 0
    | Some "start" ->
        let args = argv.[1..] |> parseArguments
        let agent = startAgent()
        startServer args.port args.timeout agent.Post (Async.RunSynchronously >> konst 0)
    | Some "npm-run" ->
        runNpmOrYarn "npm" argv.[1..]
    | Some (StartsWith "npm-" command) ->
        Array.append [|command|] argv.[1..] |> runNpmOrYarn "npm"
    | Some "yarn-run" ->
        runNpmOrYarn "yarn" argv.[1..]
    | Some (StartsWith "yarn-" command) ->
        Array.append [|command|] argv.[1..] |> runNpmOrYarn "yarn"
    | Some "node-run" ->
        let args = argv.[2..] |> parseArguments
        let execArgs =
            match args.commandArgs with
            | Some scriptArgs -> argv.[1] + " " + scriptArgs
            | None -> argv.[1]
        let workingDir = Directory.GetCurrentDirectory()
        startServerWithProcess workingDir args.port "node" execArgs
    | Some ("webpack" | "webpack-dev-server" as webpack) ->
        let args = argv.[1..] |> parseArguments
        let workingDir = Directory.GetCurrentDirectory()
        let webpackScript =
            let webpackScript =
                Path.Combine(findPackageJsonDir workingDir, "node_modules", webpack, "bin", webpack + ".js")
                |> sprintf "\"%s\""
            match args.commandArgs with
            | Some args -> webpackScript + " " + args
            | None -> webpackScript
        startServerWithProcess workingDir args.port "node" webpackScript
    | Some "shell-run" ->
        let cmd = argv.[1]
        let args = argv.[2..] |> parseArguments
        let execArgs = defaultArg args.commandArgs ""
        let workingDir = Directory.GetCurrentDirectory()
        startServerWithProcess workingDir args.port cmd execArgs
    | Some "debug" ->
        debug argv.[1] argv.[2..]; 0
    | Some "add" -> printfn "The add command has been deprecated. Use Paket to manage Fable libraries."; 0
    | Some cmd -> printfn "Unrecognized command: %s. Use `dotnet fable --help` to see available options." cmd; 0
    | None -> printfn "Command missing. Use `dotnet fable --help` to see available options."; 0
