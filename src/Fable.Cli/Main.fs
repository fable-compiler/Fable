module Fable.Cli.Main

open System
open Agent
open Fable

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

let setGlobalParams(args: string[]) =
    let verbosity =
        match tryFindArgValue "--verbose" args, tryFindArgValue "--silent" args with
        | Some _, _ -> Verbosity.Verbose
        | None, Some _ -> Verbosity.Silent
        | None, None -> Verbosity.Normal
    GlobalParams.Singleton.SetValues(
        verbosity = verbosity,
        forcePkgs = (tryFindArgValue "--force-pkgs" args |> Option.isSome),
        ?fableLibraryPath =
            (tryFindArgValue "--fable-library" args
             |> Option.map (fun path ->
                if path.StartsWith(Literals.FORCE)
                then path
                else Fable.Path.normalizeFullPath path))
    )

let printHelp() =
    Log.always """Usage: fable [watch] [.fsproj file or dir path] [arguments]

Commands:
  -h|--help           Show help
  --version           Print version
  watch               Run Fable in watch mode

Arguments:
  --verbose           Print more info during execution
  --force-pkgs        Force a new copy of package sources into `.fable` folder.

"""

let startCompilation watchMode fsprojDirOrFilePath args =
    fsprojDirOrFilePath
    |> Option.defaultWith IO.Directory.GetCurrentDirectory
    |> Path.normalizeFullPath
    |> fun path ->
        if IO.Directory.Exists(path) then
            IO.Directory.EnumerateFileSystemEntries(path)
            |> Seq.filter (fun file -> file.EndsWith(".fsproj"))
            |> Seq.toList
            |> function
                | [] -> Error("Cannot find .fsproj in dir: " + path)
                | [fsproj] -> Ok fsproj
                | _ -> Error("Found multiple .fsproj in dir: " + path)
        else
            Ok path
    |> function
        | Error msg -> printfn "%s" msg; 1
        | Ok projFile ->
            let msg = MessageHelper.Make()
            let config = AgentConfig.FromMessage(projFile, msg)
            let agent = AgentState.ParseProject(config)
            agent.Compile() |> Async.RunSynchronously
            0

let (|SplitCommandArgs|) (xs: string list) =
    xs |> List.splitWhile (fun x -> x.StartsWith("-") |> not)

[<EntryPoint>]
let main argv =
    Log.always("Fable: F# to JS compiler " + Literals.VERSION)
    setGlobalParams(argv)

    match Array.toList argv with
    | ("help"|"--help"|"-h")::_ -> printHelp(); 0
    | ("--version")::_ -> printfn "%s" Literals.VERSION; 0
    | SplitCommandArgs(commands, args) ->
        match commands with
        | ["watch"; path] -> startCompilation true (Some path) args
        | ["watch"] -> startCompilation true None args
        | [path] -> startCompilation false (Some path) args
        | [] -> startCompilation false None args
        | _ -> printfn "Unexpected arguments. Use `fable --help` to see available options."; 1
