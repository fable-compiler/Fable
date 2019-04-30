module Fable.Cli.Main

open System
open System.IO
open System.Net
open Fable
open Agent

type Arguments =
    { port: int; cwd: string; commandArgs: string option }

let konst k _ = k

let rec findPackageJsonDir dir =
    if File.Exists(IO.Path.Combine(dir, "package.json"))
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
        if args.Length = i + 1 || (key <> "--args" && args.[i + 1].StartsWith("--"))
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
                Literals.DEFAULT_PORT
        | None ->
            // Literals.DEFAULT_PORT
            getFreePort() // Make free port the default
    let workingDir =
        match tryFindArgValue "--cwd" args with
        | Some cwd -> Path.GetFullPath(cwd)
        | None -> Directory.GetCurrentDirectory()
    let commandArgs =
        // Check first --args for compatibility with the old way
        match tryFindArgValue "--args" args with
        | Some commandArgs -> Some commandArgs
        | None ->
            match args |> Array.tryFindIndex ((=) "--") with
            | Some i -> args.[(i+1)..] |> String.concat " " |> Some
            | None -> None
    { port = port; cwd = workingDir; commandArgs = commandArgs}

let startServer port onMessage continuation =
    try
        let work = Server.start port (AgentMsg.Received >> onMessage)
        continuation work
    with
    | ex ->
        printfn "Cannot start Fable daemon, please check the port %i is free: %s" port ex.Message
        1

let startServerWithProcess workingDir port exec args =
    GlobalParams.Singleton.SetValues(workingDir = workingDir)
    let mutable disposed = false
    let killProcessAndServer =
        fun (p: System.Diagnostics.Process) ->
            if not disposed then
                disposed <- true
                printfn "Killing process..."
                p.Kill()
                Server.stop port
    let agent = startAgent()
    startServer port agent.Post <| fun listen ->
        Async.Start listen
        let p =
            printfn "CWD: %s" workingDir
            printfn "%s %s" exec args
            Process.Options(envVars=Map["FABLE_SERVER_PORT", string port])
            |> Process.start workingDir exec args
        Console.CancelKeyPress.Add (fun _ -> killProcessAndServer p)
        #if NETFX
        System.AppDomain.CurrentDomain.ProcessExit.Add (fun _ -> killProcessAndServer p)
        #else
        System.Runtime.Loader.AssemblyLoadContext.Default.add_Unloading(fun _ -> killProcessAndServer p)
        #endif
        p.WaitForExit()
        disposed <- true
        Server.stop port
        p.ExitCode

let setGlobalParams(args: string[]) =
    GlobalParams.Singleton.SetValues(
        verbose   = (tryFindArgValue "--verbose" args |> Option.isSome),
        forcePkgs = (tryFindArgValue "--force-pkgs" args |> Option.isSome),
        ?replaceFiles = (tryFindArgValue "--replace-files" args),
        ?experimental = (tryFindArgValue "--experimental" args),
        ?fableLibraryPath =
            (tryFindArgValue "--fable-library" args
             |> Option.map (fun path ->
                if path.StartsWith(Literals.FORCE)
                then path
                else Fable.Path.normalizeFullPath path))
    )

let printHelp() =
    (Literals.VERSION, Literals.DEFAULT_PORT) ||> printfn """Fable F# to JS compiler (%s)
Usage: dotnet fable [command] [script] [fable arguments] [-- [script arguments]]

Commands:
  -h|--help           Show help
  --version           Print version
  npm-run             Run Fable while an npm script is running
  yarn-run            Run Fable while a yarn script is running
  node-run            Run Fable while a node script is running
  shell-run           Run Fable while a shell script is running
  start               Start Fable as a standalone daemon (default port: %i)
  [webpack-cli]       Other commands will be assumed to be binaries in `node_modules/.bin`

Fable arguments:
  --cwd               Working directory where the subprocess should run
  --port              Port number where the Fable daemon should run
  --verbose           Print more info during execution
  --force-pkgs        Force a new copy of package sources into `.fable` folder.

To pass arguments to the script, write them after `--`. Example:

    dotnet fable webpack-cli -- --mode production

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
            | Some cargs ->
                // Yarn 1.0 doesn't require "--" to forward options to scripts
                let separator = if npmOrYarn = "yarn" then " " else " -- "
                "run " + args.[0] + separator + cargs
            | None -> "run " + args.[0]
        let workingDir = fableArgs.cwd |> findPackageJsonDir
        startServerWithProcess workingDir fableArgs.port npmOrYarn execArgs

[<EntryPoint>]
let main argv =
    setGlobalParams(argv)
    match Array.tryHead argv with
    | Some ("--help"|"-h") ->
        printHelp(); 0
    | Some "--version" -> printfn "%s" Literals.VERSION; 0
    | Some "start" ->
        let args = argv.[1..] |> parseArguments
        let agent = startAgent()
        startServer args.port agent.Post (Async.RunSynchronously >> konst 0)
    | Some "start-stdin" ->
        let agent = startAgent()
        Stdin.start (AgentMsg.Received >> agent.Post)
    | Some "npm-run" ->
        runNpmOrYarn "npm" argv.[1..]
    | Some (Naming.StartsWith "npm-" command) ->
        Array.append [|command|] argv.[1..] |> runNpmOrYarn "npm"
    | Some "yarn-run" ->
        runNpmOrYarn "yarn" argv.[1..]
    | Some (Naming.StartsWith "yarn-" command) ->
        Array.append [|command|] argv.[1..] |> runNpmOrYarn "yarn"
    | Some "node-run" ->
        let args = argv.[2..] |> parseArguments
        let execArgs =
            match args.commandArgs with
            | Some scriptArgs -> argv.[1] + " " + scriptArgs
            | None -> argv.[1]
        startServerWithProcess args.cwd args.port "node" execArgs
    | Some "shell-run" ->
        let cmd = argv.[1]
        let args = argv.[2..] |> parseArguments
        let execArgs = defaultArg args.commandArgs ""
        startServerWithProcess args.cwd args.port cmd execArgs
    | Some npmBin ->
        let args = argv.[1..] |> parseArguments
        let pkgJsonDir = findPackageJsonDir args.cwd
        let binPath =
            let binPath = IO.Path.Combine(pkgJsonDir, "node_modules/.bin/" + npmBin)
            if Process.isWindows then binPath + ".cmd" else binPath
        if File.Exists(binPath) |> not then
            printfn "Path does not exist, please make sure you've restored npm dependencies: %s" binPath; -1
        else
            defaultArg args.commandArgs ""
            |> startServerWithProcess pkgJsonDir args.port binPath
    | None -> printfn "Command missing. Use `dotnet fable --help` to see available options."; -1
