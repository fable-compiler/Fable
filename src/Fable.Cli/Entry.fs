module Fable.Cli.Entry

open System
open Main
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
  clean             Clean generated JS files

Arguments:
  --outDir          Redirect compilation output files to a directory
  --define          Defines a symbol for use in conditional compilation
  --extension       Extension for generated JS files (default .fs.js)
  --verbose         Print more info during compilation
  --exclude         Skip Fable compilation for files containing the pattern
  --typed-arrays    Compile numeric arrays to JS typed arrays
  --force-pkgs      Force a new copy of package sources into `.fable` folder
  --optimize        Use optimized AST from F# compiler (experimental)
  --cwd             Working directory
"""

type Runner =
  static member Run(args: string list, rootDir: string, ?fsprojPath, ?watch, ?testInfo) =
    let watch = defaultArg watch false

    fsprojPath
    |> Option.defaultValue rootDir
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
        elif not(IO.File.Exists(path)) then
            Error("File does not exist: " + path)
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
                                           optimizeFSharpAst = hasFlag "--optimize" args,
                                           verbosity = verbosity)

            let cliArgs =
                { ProjectFile = projFile
                  FableLibraryPath = argValue "--fable-library" args
                  RootDir = rootDir
                  OutDir = argValue "--outDir" args
                  ForcePackages = hasFlag "--force-pkgs" args
                  Exclude = argValue "--exclude" args
                  Define = defines
                  CompilerOptions = compilerOptions }

            let watcher =
                if watch then
                    IO.Path.GetDirectoryName(projFile)
                    |> Watcher
                    |> Some
                else None

            { CliArgs = cliArgs
              ProjectCrackedAndParsed = None
              Watcher = watcher
              WatchDependencies = Map.empty
              TestInfo = testInfo }
            |> startCompilation Set.empty
            |> Async.RunSynchronously
            |> function
                | Ok _ -> 0
                | Error _ -> 1

let clean args dir =
    let ignoreDirs = set ["bin"; "obj"; "node_modules"]
    let ext =
        argValue "--extension" args
        |> Option.defaultValue CompilerOptionsHelper.DefaultFileExtension

    let rec recClean dir =
        IO.Directory.GetFiles(dir)
        |> Array.choose (fun file ->
            if file.EndsWith(".fs") then Some(file.[.. (file.Length - 4)])
            else None)
        |> Array.iter (fun filename ->
            let file = filename + ext
            if IO.File.Exists(file) then
                IO.File.Delete(file)
                Log.verbose(lazy ("Deleted " + file))
        )

        IO.Directory.GetDirectories(dir)
        |> Array.filter (fun subdir ->
            ignoreDirs.Contains(IO.Path.GetFileName(subdir)) |> not)
        |> Array.iter recClean

    recClean dir
    Log.always("Clean completed!")
    0

let (|SplitCommandArgs|) (xs: string list) =
    xs |> List.splitWhile (fun x -> x.StartsWith("-") |> not)

[<EntryPoint>]
let main argv =
    let argv = List.ofArray argv
    let rootDir =
        match argValue "--cwd" argv with
        | Some rootDir -> IO.Path.GetFullPath(rootDir)
        | None -> IO.Directory.GetCurrentDirectory()

    Log.always("Fable: F# to JS compiler " + Literals.VERSION)
    if hasFlag "--verbose" argv then
        Log.makeVerbose()

    match argv with
    | ("help"|"--help"|"-h")::_ -> printHelp(); 0
    | ("--version")::_ -> printfn "%s" Literals.VERSION; 0
    | SplitCommandArgs(commands, args) ->
        match commands with
        | ["clean"; dir] -> clean args dir
        | ["clean"] -> clean args rootDir
        | ["test"; path] -> Runner.Run(args, rootDir, fsprojPath=path, testInfo=TestInfo())
        | ["watch"; path] -> Runner.Run(args, rootDir, fsprojPath=path, watch=true)
        | ["watch"] -> Runner.Run(args, rootDir, watch=true)
        | [path] -> Runner.Run(args, rootDir, fsprojPath=path)
        | [] -> Runner.Run(args, rootDir)
        | _ -> printfn "Unexpected arguments. Use `fable --help` to see available options."; 1
