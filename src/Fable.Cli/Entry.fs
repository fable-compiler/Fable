module Fable.Cli.Entry

open System
open Agent
open Fable

let hasFlag flag (args: string list) =
    List.contains flag args

let argValue key (args: string list) =
    args
    |> List.windowed 2
    |> List.tryPick (function
        | [key2; value] when key = key2 -> Some value
        | _ -> None)

let argValues key (args: string list) =
    args
    |> List.windowed 2
    |> List.fold (fun acc window ->
        match window with
        | [key2; value] when key = key2 -> value::acc
        | _ -> acc) []
    |> List.rev

let printHelp() =
    Log.always """Usage: fable [watch] [.fsproj file or dir path] [arguments]

Commands:
  -h|--help         Show help
  --version         Print version
  watch             Run Fable in watch mode

Arguments:
  --define          Defines a symbol for use in conditional compilation
  --extension       Extension for generated JS files (default .fs.js)
  --verbose         Print more info during compilation
  --exclude         Skip Fable compilation for files containing the pattern
  --force-pkgs      Force a new copy of package sources into `.fable` folder

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
            let verbosity =
                if hasFlag "--verbose" args then
                    Log.makeVerbose()
                    Verbosity.Verbose
                else Verbosity.Normal

            let defines =
                argValues "--define" args
                |> List.append ["FABLE_COMPILER"]
                |> List.distinct
                |> List.toArray

            let compilerOptions =
                CompilerOptionsHelper.Make(typescript = hasFlag "--typescript" args,
                                           typedArrays = hasFlag "--typed-arrays" args,
                                           ?fileExtension = argValue "--extension" args,
                                           debugMode = Array.contains "DEBUG" defines,
                                           verbosity = verbosity)

            let cliArgs =
                { ProjectFile = projFile
                  FableLibraryPath = argValue "--fable-library" args
                  RootDir = IO.Directory.GetCurrentDirectory()
                  ForcePackages = hasFlag "--force-pkgs" args
                  Exclude = argValue "--exclude" args
                  Define = defines
                  CompilerOptions = compilerOptions }

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
            |> startCompilation Set.empty
            |> Async.RunSynchronously
            |> function
                | Ok _ -> 0
                | Error _ -> 1

let (|SplitCommandArgs|) (xs: string list) =
    xs |> List.splitWhile (fun x -> x.StartsWith("-") |> not)

[<EntryPoint>]
let main argv =
    Log.always("Fable: F# to JS compiler " + Literals.VERSION)

    match List.ofArray argv with
    | ("help"|"--help"|"-h")::_ -> printHelp(); 0
    | ("--version")::_ -> printfn "%s" Literals.VERSION; 0
    | SplitCommandArgs(commands, args) ->
        match commands with
        | ["watch"; path] -> run true (Some path) args
        | ["watch"] -> run true None args
        | [path] -> run false (Some path) args
        | [] -> run false None args
        | _ -> printfn "Unexpected arguments. Use `fable --help` to see available options."; 1
