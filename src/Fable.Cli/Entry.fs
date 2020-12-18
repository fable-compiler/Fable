module Fable.Cli.Entry

open System
open Main
open Fable

let argValue key (args: string list) =
    args
    |> List.windowed 2
    |> List.tryPick (function
        | [key2; value] when not(value.StartsWith("-")) && key = key2 -> Some value
        | _ -> None)

let argValueMulti keys (args: string list) =
    args
    |> List.windowed 2
    |> List.tryPick (function
        | [key2; value] when not(value.StartsWith("-")) && List.exists ((=) key2) keys -> Some value
        | _ -> None)

let tryFlag flag (args: string list) =
    match argValue flag args with
    | Some flag ->
        match Boolean.TryParse(flag) with
        | true, flag -> Some flag
        | false, _ -> None
    // Flags can be activated without an explicit value
    | None when List.contains flag args -> Some true
    | None -> None

let flagEnabled flag (args: string list) =
    tryFlag flag args |> Option.defaultValue false

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
  clean             Remove .fable folders and files with specified extension (default .fs.js)

Arguments:
  --cwd             Working directory
  -o|--outDir       Redirect compilation output to a directory
  --extension       Extension for generated JS files (default .fs.js)

  --define          Defines a symbol for use in conditional compilation
  --verbose         Print more info during compilation
  --typedArrays     Compile numeric arrays as JS typed arrays (default true)

  --run             The command after the argument will be executed after compilation
  --runFast         The command after the argument will be executed BEFORE compilation
  --runWatch        Like run, but will execute after each watch compilation
  --runScript       Runs the generated script for last file with node
                    (Requires "esm" npm package)

  --yes             Automatically reply 'yes' (e.g. with `clean` command)
  --noRestore       Skip `dotnet restore`
  --forcePkgs       Force a new copy of package sources into `.fable` folder
  --exclude         Don't merge sources of referenced projects with specified pattern
                    (Intended for plugin development)

  --optimize        Compile with optimized F# AST (experimental)
  --typescript      Compile to TypeScript (experimental)
"""

let defaultFileExt isTypescript args =
    let fileExt =
        match argValueMulti ["-o"; "--outDir"] args with
        | Some _ -> ".js"
        | None -> CompilerOptionsHelper.DefaultExtension
    if isTypescript then Path.replaceExtension ".ts" fileExt else fileExt

type Runner =
  static member Run(args: string list, rootDir: string, runProc: RunProcess option, ?fsprojPath: string, ?watch, ?testInfo) =
    let normalizeAbsolutePath (path: string) =
        (if IO.Path.IsPathRooted(path) then path
         else IO.Path.Combine(rootDir, path))
        // Use getExactFullPath to remove things like: myrepo/./build/
        // and get proper casing (see `getExactFullPath` comment)
        |> File.getExactFullPath
        |> Path.normalizePath

    let watch = defaultArg watch false

    let fsprojPath =
        fsprojPath
        |> Option.map normalizeAbsolutePath
        |> Option.defaultValue rootDir

    if IO.Directory.Exists(fsprojPath) then
        IO.Directory.EnumerateFileSystemEntries(fsprojPath)
        |> Seq.filter (fun file -> file.EndsWith(".fsproj"))
        |> Seq.toList
        |> function
            | [] -> Error("Cannot find .fsproj in dir: " + fsprojPath)
            | [fsproj] -> Ok fsproj
            | _ -> Error("Found multiple .fsproj in dir: " + fsprojPath)
    elif not(IO.File.Exists(fsprojPath)) then
        Error("File does not exist: " + fsprojPath)
    else
        Ok fsprojPath

    // TODO: Remove this check when typed arrays are compatible with typescript
    |> Result.bind (fun projFile ->
        let typescript = flagEnabled "--typescript" args
        let typedArrays = tryFlag "--typedArrays" args |> Option.defaultValue true
        if typescript && typedArrays then
            Error("Typescript output is currently not compatible with typed arrays, pass: --typedArrays false")
        else
            Ok(projFile, typescript, typedArrays)
    )

    |> Result.bind (fun (projFile, typescript, typedArrays) ->
        let verbosity =
            if flagEnabled "--verbose" args then
                Log.makeVerbose()
                Verbosity.Verbose
            else Verbosity.Normal

        let define =
            argValues "--define" args
            |> List.append [
                "FABLE_COMPILER"
                "FABLE_COMPILER_3"
                if watch then "DEBUG"
            ]
            |> List.distinct

        let fileExt =
            argValue "--extension" args |> Option.defaultValue (defaultFileExt typescript args)

        let compilerOptions =
            CompilerOptionsHelper.Make(typescript = typescript,
                                       typedArrays = typedArrays,
                                       fileExtension = fileExt,
                                       define = define,
                                       optimizeFSharpAst = flagEnabled "--optimize" args,
                                       verbosity = verbosity)

        let cliArgs =
            { ProjectFile = Path.normalizeFullPath projFile
              FableLibraryPath = argValue "--fableLib" args
              RootDir = rootDir
              OutDir = argValueMulti ["-o"; "--outDir"] args |> Option.map normalizeAbsolutePath
              ForcePkgs = flagEnabled "--forcePkgs" args
              NoRestore = flagEnabled "--noRestore" args
              Exclude = argValue "--exclude" args
              Replace =
                argValues "--replace" args
                |> List.map (fun v ->
                    let v = v.Split(':')
                    v.[0], Path.normalizeFullPath v.[1])
                |> Map
              RunProcess = runProc
              CompilerOptions = compilerOptions }

        { CliArgs = cliArgs
          ProjectCrackedAndParsed = None
          WatchDependencies = Map.empty
          Watcher = if watch then Some(FsWatcher()) else None
          DeduplicateDic = Collections.Concurrent.ConcurrentDictionary()
          FableCompilationMs = 0L
          ErroredFiles = Set.empty
          TestInfo = testInfo }
        |> startFirstCompilation
        |> Async.RunSynchronously)


let clean args dir =
    let typescript = flagEnabled "--typescript" args
    let ignoreDirs = set ["bin"; "obj"; "node_modules"]
    let fileExt =
        argValue "--extension" args |> Option.defaultValue (defaultFileExt typescript args)

    // clean is a potentially destructive operation, we need a permission before proceeding
    Console.WriteLine("This will recursively delete all *{0} files in {1}", fileExt, dir)
    if not(flagEnabled "--yes" args) then
        Console.WriteLine("Please press 'Y' or 'y' if you want to continue: ")
        let keyInfo = Console.ReadKey()
        Console.WriteLine()
        if keyInfo.Key <> ConsoleKey.Y then
            Console.WriteLine("Clean was cancelled.")
            exit 0

    let mutable fileCount = 0
    let rec recClean dir =
        IO.Directory.GetFiles(dir, "*" + fileExt)
        |> Array.iter (fun file ->
            IO.File.Delete(file)
            fileCount <- fileCount + 1
            Log.verbose(lazy ("Deleted " + file)))

        IO.Directory.GetDirectories(dir)
        |> Array.filter (fun subdir ->
            ignoreDirs.Contains(IO.Path.GetFileName(subdir)) |> not)
        |> Array.iter (fun subdir ->
            if IO.Path.GetFileName(subdir) = Naming.fableHiddenDir then
                IO.Directory.Delete(subdir, true)
                Log.always("Deleted " + File.getRelativePathFromCwd subdir)
            else recClean subdir)

    recClean dir
    Log.always("Clean completed! Files deleted: " + string fileCount)

type ResultBuilder() =
    member _.Bind(v,f) = Result.bind f v
    member _.Return v = Ok v
    member _.ReturnFrom v = v

let result = ResultBuilder()

[<EntryPoint>]
let main argv =
    result {
        let! argv, runProc =
            argv
            |> List.ofArray
            |> List.splitWhile (fun a -> not(a.StartsWith("--run")))
            |> function
                | argv, flag::runArgs ->
                    match flag, runArgs with
                    | "--run", exeFile::args -> Ok(RunProcess(exeFile, args))
                    | "--runFast", exeFile::args -> Ok(RunProcess(exeFile, args, fast=true))
                    | "--runWatch", exeFile::args -> Ok(RunProcess(exeFile, args, watch=true))
                    | "--runScript", args -> Ok(RunProcess(Naming.placeholder, args, watch=true))
                    | _, [] -> Error("Missing command after "+ flag)
                    | _ -> Error("Unknown argument " + flag)
                    |> Result.map (fun runProc -> argv, Some runProc)
                | argv, [] -> Ok(argv, None)

        let rootDir =
            match argValue "--cwd" argv with
            | Some rootDir -> File.getExactFullPath rootDir
            | None -> IO.Directory.GetCurrentDirectory()

        do
            Log.always("Fable: F# to JS compiler " + Literals.VERSION)
            Log.always("Thanks to the contributor! @" + Contributors.getRandom())
            if flagEnabled "--verbose" argv then
                Log.makeVerbose()

        match argv with
        | ("help"|"--help"|"-h")::_ -> return printHelp()
        | ("--version")::_ -> return Log.always Literals.VERSION
        | argv ->
            let commands, args =
                argv |> List.splitWhile (fun x ->
                    x.StartsWith("-") |> not)

            match commands with
            | ["clean"; dir] -> return clean args dir
            | ["clean"] -> return clean args rootDir
            | ["test"; path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, testInfo=TestInfo())
            | ["watch"; path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, watch=true)
            | ["watch"] -> return! Runner.Run(args, rootDir, runProc, watch=true)
            | [path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, watch=flagEnabled "--watch" args)
            | [] -> return! Runner.Run(args, rootDir, runProc, watch=flagEnabled "--watch" args)
            | _ -> return Log.always "Unexpected arguments. Use `fable --help` to see available options."
    }
    |> function
        | Ok _ -> 0
        | Error msg -> Log.always msg; 1
