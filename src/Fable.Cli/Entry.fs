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
  -e|--extension    Extension for generated JS files (default .fs.js)
  -s|--sourceMaps   Enable source maps

  --define          Defines a symbol for use in conditional compilation
  --configuration   The configuration to use when parsing .fsproj with MSBuild,
                    default is 'Debug' in watch mode, or 'Release' otherwise
  --verbose         Print more info during compilation
  --typedArrays     Compile numeric arrays as JS typed arrays (default true)

  --run             The command after the argument will be executed after compilation
  --runFast         The command after the argument will be executed BEFORE compilation
  --runWatch        Like run, but will execute after each watch compilation
  --runScript       Runs the generated script for last file with node
                    (Requires "esm" npm package)

  --yes             Automatically reply 'yes' (e.g. with `clean` command)
  --noRestore       Skip `dotnet restore`
  --noCache         Recompile all files, including sources from packages
  --exclude         Don't merge sources of referenced projects with specified pattern
                    (Intended for plugin development)
  --sourceMapsRoot  Set the value of the `sourceRoot` property in generated source maps

  --optimize        Compile with optimized F# AST (experimental)
  --lang|--language Compile to JavaScript (default), TypeScript or Python.
                    Support for TypeScript and Python is experimental.

  Environment variables:
   DOTNET_USE_POLLING_FILE_WATCHER
   When set to '1' or 'true', Fable watch will poll the file system for
   changes. This is required for some file systems, such as network shares,
   Docker mounted volumes, and other virtual file systems.
"""

let defaultFileExt language args =
    let fileExt =
        match argValueMulti ["-o"; "--outDir"] args with
        | Some _ -> ".js"
        | None -> CompilerOptionsHelper.DefaultExtension
    match language with
    | TypeScript -> Path.replaceExtension ".ts" fileExt
    | Python -> Path.replaceExtension ".py" fileExt
    | _ -> fileExt

let argLanguage args =
    argValue "--lang" args
    |> Option.orElse (argValue "--language" args)
    |> Option.orElse (tryFlag "--typescript" args |> Option.map (fun _ -> "typescript")) // Compatibility with "--typescript".
    |> Option.defaultValue "JavaScript"
    |> (function
    | "ts" | "typescript" | "TypeScript" -> TypeScript
    | "py" | "python" | "Python" -> Python
    | _ -> JavaScript)

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
        let language = argLanguage args
<<<<<<< HEAD

=======
>>>>>>> 18447c3f5c2ee0ff68d5e610124bf9242417124a
        let typedArrays = tryFlag "--typedArrays" args |> Option.defaultValue true
        if language = TypeScript && typedArrays then
            Error("Typescript output is currently not compatible with typed arrays, pass: --typedArrays false")
        else
            Ok(projFile, language, typedArrays)
    )

    |> Result.bind (fun (projFile, language, typedArrays) ->
        let verbosity =
            if flagEnabled "--verbose" args then
                Log.makeVerbose()
                Verbosity.Verbose
            else Verbosity.Normal

        let configuration =
            let defaultConfiguration = if watch then "Debug" else "Release"
            let configurationArg = argValue "--configuration" args |> Option.defaultValue defaultConfiguration
            if String.IsNullOrWhiteSpace configurationArg then
                defaultConfiguration
            else
                configurationArg

        let define =
            argValues "--define" args
            |> List.append [
                "FABLE_COMPILER"
                "FABLE_COMPILER_3"
                configuration.ToUpperInvariant()
            ]
            |> List.distinct

        let fileExt =
            argValueMulti ["-e"; "--extension"] args
            |> Option.defaultValue (defaultFileExt language args)

        let compilerOptions =
            CompilerOptionsHelper.Make(language=language,
                                       typedArrays = typedArrays,
                                       fileExtension = fileExt,
                                       define = define,
                                       optimizeFSharpAst = flagEnabled "--optimize" args,
                                       verbosity = verbosity)

        let cliArgs =
            { ProjectFile = Path.normalizeFullPath projFile
              FableLibraryPath = argValue "--fableLib" args
              RootDir = rootDir
              Configuration = configuration
              OutDir = argValueMulti ["-o"; "--outDir"] args |> Option.map normalizeAbsolutePath
              SourceMaps = flagEnabled "-s" args || flagEnabled "--sourceMaps" args
              SourceMapsRoot = argValue "--sourceMapsRoot" args
              NoRestore = flagEnabled "--noRestore" args
              NoCache = flagEnabled "--noCache" args || flagEnabled "--forcePkgs" args // backwards compatibility
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
    let language = argLanguage args
    let ignoreDirs = set ["bin"; "obj"; "node_modules"]

    let fileExt =
        argValueMulti ["-e"; "--extension"] args
        |> Option.defaultValue (defaultFileExt language args)

    let dir =
        argValueMulti ["-o"; "--outDir"] args
        |> Option.defaultValue dir
        |> IO.Path.GetFullPath

    // clean is a potentially destructive operation, we need a permission before proceeding
    Console.WriteLine("This will recursively delete all *{0}[.map] files in {1}", fileExt, dir)
    if not(flagEnabled "--yes" args) then
        Console.WriteLine("Please press 'Y' or 'y' if you want to continue: ")
        let keyInfo = Console.ReadKey()
        Console.WriteLine()
        if keyInfo.Key <> ConsoleKey.Y then
            Console.WriteLine("Clean was cancelled.")
            exit 0

    let mutable fileCount = 0
    let rec recClean dir =
        seq {
            yield! IO.Directory.GetFiles(dir, "*" + fileExt)
            yield! IO.Directory.GetFiles(dir, "*" + fileExt + ".map")
        }
        |> Seq.iter (fun file ->
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
            | _ -> return! Error "Unexpected arguments. Use `fable --help` to see available options."
    }
    |> function
        | Ok _ -> 0
        | Error msg -> Log.error msg; 1
