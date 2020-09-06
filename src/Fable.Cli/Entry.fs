module Fable.Cli.Entry

open System
open Agent
open Fable

let hasFlag flag (args: string list) =
    List.contains flag args

let setGlobalParams(args: string list) =
    let verbosity =
        match hasFlag "--verbose" args, hasFlag "--silent" args with
        | true, _ -> Verbosity.Verbose
        | false, true -> Verbosity.Silent
        | false, false -> Verbosity.Normal
    GlobalParams.Singleton.SetValues(
        verbosity = verbosity,
        forcePkgs = (hasFlag "--force-pkgs" args)
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

let run watchMode fsprojDirOrFilePath args =
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
            let cliArgs =
                { ProjectFile = projFile
                  RootDir = IO.Directory.GetCurrentDirectory()
                  NoReferences = hasFlag "--no-references" args
                  NoRestore = hasFlag "--no-restore" args
                  Define = [||] // TODO
                  TypedArrays = false // TODO
                  Typescript = false } // TODO

            let watcher =
                if watchMode then
                    IO.Path.GetDirectoryName(projFile)
                    |> Watcher
                    |> Some
                else None

            { CliArgs = cliArgs
              ProjectCrackedAndParsed = None
              Watcher = watcher
              WatchDependencies = Map.empty }
            |> startCompilation []
            |> Async.RunSynchronously
            |> function
                | Ok _ -> 0
                | Error _ -> 1

let (|SplitCommandArgs|) (xs: string list) =
    xs |> List.splitWhile (fun x -> x.StartsWith("-") |> not)

[<EntryPoint>]
let main argv =
    Log.always("Fable: F# to JS compiler " + Literals.VERSION)

    let argv = List.ofArray argv
    setGlobalParams(argv)

    match argv with
    | ("help"|"--help"|"-h")::_ -> printHelp(); 0
    | ("--version")::_ -> printfn "%s" Literals.VERSION; 0
    | SplitCommandArgs(commands, args) ->
        match commands with
        | ["watch"; path] -> run true (Some path) args
        | ["watch"] -> run true None args
        | [path] -> run false (Some path) args
        | [] -> run false None args
        | _ -> printfn "Unexpected arguments. Use `fable --help` to see available options."; 1
