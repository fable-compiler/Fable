module Fable.Cli.Entry

open System
open Main
open Fable

type CliArgs(args: string list) =
    let argsMap =
        let even = List.length args % 2 = 0
        // If arguments are odd, assume last has true value in case it's a flag
        let args = if even then args else args @ ["true"]
        (Map.empty, List.windowed 2 args) ||> List.fold (fun map pair ->
            match pair with
            | [key; value] when key.StartsWith("-") ->
                let key = key.ToLower()
                let value = if value.StartsWith("-") then "true" else value
                match Map.tryFind key map with
                | Some prev -> Map.add key (value::prev) map
                | None -> Map.add key [value] map
            | _ -> map)

    member _.LoweredKeys = argsMap |> Map.toList |> List.map fst

    member _.Values(key: string) =
        Map.tryFind (key.ToLower()) argsMap
        |> Option.defaultValue []

    member _.Value([<ParamArray>] keys: string array) =
        keys
        |> Array.map (fun k -> k.ToLower())
        |> Array.tryPick (fun k -> Map.tryFind k argsMap)
        |> Option.bind List.tryHead

    member this.FlagOr(flag: string, defaultValue: bool) =
        this.Value(flag) |> Option.bind (fun flag ->
            match Boolean.TryParse(flag) with
            | true, flag -> Some flag
            | false, _ -> None)
        |> Option.defaultValue defaultValue

    member this.FlagEnabled([<ParamArray>] flags: string array) =
        flags |> Array.exists (fun flag -> this.FlagOr(flag, false))

let knownCliArgs() = [
  ["--cwd"],             ["Working directory"]
  ["-o"; "--outDir"],    ["Redirect compilation output to a directory"]
  ["-e"; "--extension"], ["Extension for generated JS files (default .fs.js)"]
  ["-s"; "--sourceMaps"],["Enable source maps"]
  ["--sourceMapsRoot"],  ["Set the value of the `sourceRoot` property in generated source maps"]
  [], []
  ["--define"],          ["Defines a symbol for use in conditional compilation"]
  ["-c"; "--configuration"], ["The configuration to use when parsing .fsproj with MSBuild,"
                              "default is 'Debug' in watch mode, or 'Release' otherwise"]
  ["--verbose"],         ["Print more info during compilation"]
  ["--typedArrays"],     ["Compile numeric arrays as JS typed arrays (default true)"]
  ["--watch"],           ["Alias of watch command"]
  ["--watchDelay"],      ["Delay in ms before recompiling after a file changes (default 200)"]
  [], []
  ["--run"],             ["The command after the argument will be executed after compilation"]
  ["--runFast"],         ["The command after the argument will be executed BEFORE compilation"]
  ["--runWatch"],        ["Like run, but will execute after each watch compilation"]
  ["--runScript"],       ["Runs the generated script for last file with node"
                          """(Requires `"type": "module"` in package.json and at minimum Node.js 12.20, 14.14, or 16.0.0)"""]
  [], []
  ["--yes"],             ["Automatically reply 'yes' (e.g. with `clean` command)"]
  ["--noRestore"],       ["Skip `dotnet restore`"]
  ["--noCache"],         ["Recompile all files, including sources from packages"]
  ["--exclude"],         ["Don't merge sources of referenced projects with specified pattern"
                          "(Intended for plugin development)"]
  [], []
  ["--optimize"],        ["Compile with optimized F# AST (experimental)"]
  ["--lang"; "--language"], ["Compile to JavaScript (default), TypeScript, Php or Python."
                             "Support for TypeScript, Php and Python is experimental."]

  // Hidden args
  ["--precompiledLib"], []
  ["--typescript"], []
  ["--trimRootModule"], []
  ["--fableLib"], []
  ["--replace"], []
]

let printKnownCliArgs() =
    knownCliArgs() |> List.collect (function
        | [], _ -> [""] // Empty line
        | args, desc ->
            let args = String.concat "|" args
            match desc with
            | [] -> [] // Args without description are hidden
            | desc::extraLines -> [
                $"  %-18s{args}{desc}"
                yield! extraLines |> List.map (sprintf "%20s%s" "")
            ])

let sanitizeCliArgs (args: CliArgs) =
    let knownCliArgs =
        knownCliArgs()
        |> List.collect fst
        |> List.map (fun a -> a.ToLower())
        |> set
    (Ok args, args.LoweredKeys) ||> List.fold (fun res arg ->
        match res with
        | Error msg -> Error msg
        | Ok args ->
            if knownCliArgs.Contains(arg) then Ok args
            else Error $"Unknown argument: {arg}")

let parseCliArgs (args: string list) =
    CliArgs(args) |> sanitizeCliArgs

let printHelp() =
    Log.always $"""Usage: fable [watch] [.fsproj file or dir path] [arguments]

Commands:
  -h|--help         Show help
  --version         Print version
  watch             Run Fable in watch mode
  clean             Remove fable_modules folders and files with specified extension (default .fs.js)

Arguments:
{printKnownCliArgs() |> String.concat "\n"}

  Environment variables:
   DOTNET_USE_POLLING_FILE_WATCHER
   When set to '1' or 'true', Fable watch will poll the file system for
   changes. This is required for some file systems, such as network shares,
   Docker mounted volumes, and other virtual file systems.
"""

let defaultFileExt language (args: CliArgs) =
    let fileExt =
        match args.Value("-o", "--outDir") with
        | Some _ -> ".js"
        | None -> CompilerOptionsHelper.DefaultExtension
    match language with
    | TypeScript -> Path.replaceExtension ".ts" fileExt
    | Python -> Path.replaceExtension ".py" fileExt
    | Php -> ".php"
    | Dart -> ".dart"
    | Rust -> ".rs"
    | _ -> fileExt

let argLanguage (args: CliArgs) =
    args.Value("--lang", "--language")
    |> Option.orElseWith (fun () -> if args.FlagEnabled("--typescript") then Some "ts" else None) // Compatibility with "--typescript"
    |> Option.map (fun lang -> lang.ToLower())
    |> Option.defaultWith (fun () ->
        // Set default language based on name of executing process (fable tool).
        let proc = System.Reflection.Assembly.GetEntryAssembly().Location
        match Path.GetFileNameWithoutExtension(proc) with
        | "fable-py" -> Python
        | _ -> JavaScript
        |> string
    )
    |> (function
    | "js" | "javascript" | "JavaScript" -> JavaScript
    | "ts" | "typescript" | "TypeScript" -> TypeScript
    | "py" | "python" | "Python" -> Python
    | "php" | "Php" | "PHP" -> Php
    | "dart" -> Dart
    | "rust" | "Rust" -> Rust
    | _ -> JavaScript)

type Runner =
  static member Run(args: CliArgs, rootDir: string, runProc: RunProcess option, ?fsprojPath: string, ?watch, ?precompile) = result {
    let normalizeAbsolutePath (path: string) =
        (if IO.Path.IsPathRooted(path) then path
         else IO.Path.Combine(rootDir, path))
        // Use getExactFullPath to remove things like: myrepo/./build/
        // and get proper casing (see `getExactFullPath` comment)
        |> File.getExactFullPath
        |> Path.normalizePath

    let watch = defaultArg watch false
    let precompile = defaultArg precompile false

    let fsprojPath =
        fsprojPath
        |> Option.map normalizeAbsolutePath
        |> Option.defaultValue rootDir

    let! projFile =
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

    let language = argLanguage args
    Log.always($"Fable: F# to {language} compiler " + Literals.VERSION)
    Log.always("Thanks to the contributor! @" + Contributors.getRandom() + "\n")

    let typedArrays = args.FlagOr("--typedArrays", true)
    let outDir = args.Value("-o", "--outDir") |> Option.map normalizeAbsolutePath
    let precompiledLib = args.Value("--precompiledLib") |> Option.map normalizeAbsolutePath
    let fableLib = args.Value "--fableLib"

    do!
        match outDir, fableLib with
        | None, _ when precompile -> Error("outDir must be specified when precompiling")
        | _, Some _ when Option.isSome precompiledLib -> Error("Cannot set fableLib when setting precompiledLib")
        | _ -> Ok ()

    do!
        let reservedDirs = [Naming.fableModules; "obj"]
        let outDirLast = outDir |> Option.bind (fun outDir -> outDir.TrimEnd('/').Split('/') |> Array.tryLast) |> Option.defaultValue ""
        if List.contains outDirLast reservedDirs then
            Error($"{outDirLast} is a reserved directory, please use another output directory")
        // TODO: Remove this check when typed arrays are compatible with typescript
        elif language = TypeScript && typedArrays then
            Error("Typescript output is currently not compatible with typed arrays, pass: --typedArrays false")
        else
            Ok ()

    let verbosity =
        if args.FlagEnabled "--verbose" then
            Log.makeVerbose()
            Verbosity.Verbose
        else Verbosity.Normal

    let configuration =
        let defaultConfiguration = if watch then "Debug" else "Release"
        match args.Value("-c", "--configuration") with
        | None -> defaultConfiguration
        | Some c when String.IsNullOrWhiteSpace c -> defaultConfiguration
        | Some configurationArg -> configurationArg

    let define =
        args.Values "--define"
        |> List.append [
            "FABLE_COMPILER"
            "FABLE_COMPILER_4"
            "FABLE_COMPILER_4_OR_GREATER"
            match language with
            | Php -> "FABLE_COMPILER_PHP"
            | Rust -> "FABLE_COMPILER_RUST"
            | Dart -> "FABLE_COMPILER_DART"
            | Python -> "FABLE_COMPILER_PYTHON"
            | TypeScript -> "FABLE_COMPILER_TYPESCRIPT"
            | JavaScript -> "FABLE_COMPILER_JAVASCRIPT"
        ]
        |> List.distinct

    let fileExt =
        args.Value("-e", "--extension")
        |> Option.defaultValue (defaultFileExt language args)

    let compilerOptions =
        CompilerOptionsHelper.Make(language=language,
                                   typedArrays = typedArrays,
                                   fileExtension = fileExt,
                                   define = define,
                                   debugMode = (configuration = "Debug"),
                                   optimizeFSharpAst = args.FlagEnabled "--optimize",
                                   verbosity = verbosity)

    let cliArgs =
        { ProjectFile = Path.normalizeFullPath projFile
          FableLibraryPath = fableLib
          RootDir = rootDir
          Configuration = configuration
          OutDir = outDir
          IsWatch = watch
          Precompile = precompile
          PrecompiledLib = precompiledLib
          SourceMaps = args.FlagEnabled "-s" || args.FlagEnabled "--sourceMaps"
          SourceMapsRoot = args.Value "--sourceMapsRoot"
          NoRestore = args.FlagEnabled "--noRestore"
          NoCache = args.FlagEnabled "--noCache"
          Exclude = args.Value "--exclude"
          Replace =
            args.Values "--replace"
            |> List.map (fun v ->
                let v = v.Split(':')
                v.[0], normalizeAbsolutePath v.[1])
            |> Map
          RunProcess = runProc
          CompilerOptions = compilerOptions }

    let watchDelay =
        if watch then
            args.Value("--watchDelay")
            |> Option.map int
            |> Option.defaultValue 200
            |> Some
        else None

    return!
        State.Create(cliArgs, ?watchDelay=watchDelay)
        |> startCompilation
        |> Async.RunSynchronously
}

let clean (args: CliArgs) rootDir =
    let language = argLanguage args
    let ignoreDirs = set ["bin"; "obj"; "node_modules"]

    let fileExt =
        args.Value("-e", "--extension")
        |> Option.defaultValue (defaultFileExt language args)

    let cleanDir =
        args.Value("-o", "--outDir")
        |> Option.defaultValue rootDir
        |> IO.Path.GetFullPath

    // clean is a potentially destructive operation, we need a permission before proceeding
    Console.WriteLine("This will recursively delete all *{0}[.map] files in {1}", fileExt, cleanDir)
    if not(args.FlagEnabled "--yes") then
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
            if IO.Path.GetFileName(subdir) = Naming.fableModules then
                IO.Directory.Delete(subdir, true)
                Log.always $"Deleted {IO.Path.GetRelativePath(rootDir, subdir)}"
            else recClean subdir)

    recClean cleanDir
    Log.always("Clean completed! Files deleted: " + string fileCount)

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

        let commands, args =
            match argv with
            | ("help"|"--help"|"-h")::_ -> ["--help"], []
            | "--version"::_ -> ["--version"], []
            | argv -> argv |> List.splitWhile (fun x -> x.StartsWith("-") |> not)

        let! args = parseCliArgs args

        let rootDir =
            match args.Value "--cwd" with
            | Some rootDir -> File.getExactFullPath rootDir
            | None -> IO.Directory.GetCurrentDirectory()

        do
            match commands with
            | ["--version"] -> ()
            | _ ->
                if args.FlagEnabled "--verbose" then
                    Log.makeVerbose()

        match commands with
        | ["--help"] -> return printHelp()
        | ["--version"] -> return Log.always Literals.VERSION
        | ["clean"; dir] -> return clean args dir
        | ["clean"] -> return clean args rootDir
        | ["watch"; path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, watch=true)
        | ["watch"] -> return! Runner.Run(args, rootDir, runProc, watch=true)
        | ["precompile"; path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, precompile=true)
        | ["precompile"] -> return! Runner.Run(args, rootDir, runProc, precompile=true)
        | [path] -> return! Runner.Run(args, rootDir, runProc, fsprojPath=path, watch=args.FlagEnabled("--watch"))
        | [] -> return! Runner.Run(args, rootDir, runProc, watch=args.FlagEnabled("--watch"))
        | _ -> return! Error "Unexpected arguments. Use `fable --help` to see available options."
    }
    |> function
        | Ok _ -> 0
        | Error msg -> Log.error msg; 1
