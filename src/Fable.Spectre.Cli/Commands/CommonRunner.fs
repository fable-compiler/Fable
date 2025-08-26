module Fable.Spectre.Cli.Commands.CommonRunner

open Fable.Spectre.Cli.Settings.Spec
open Fable.Spectre.Cli.SpectreOutput
open Spectre.Console
open SpectreCoff
open System
open Fable.Cli.Main
open Fable.Cli
open Fable
open Fable.Compiler.Util
open Fable.Cli.CustomLogging
open Microsoft.Extensions.Logging
open Spectre.Console.Cli

// type Runner =
//     static member Run
//         (
//             args: CliArgs,
//             language: Language,
//             rootDir: string,
//             runProc: RunProcess option,
//             verbosity: Fable.Verbosity,
//             ?fsprojPath: string,
//             ?watch,
//             ?precompile
//         )
//         =
//         result {
//             let normalizeAbsolutePath (path: string) =
//                 (if IO.Path.IsPathRooted(path) then
//                      path
//                  else
//                      IO.Path.Combine(rootDir, path))
//                 // Use getExactFullPath to remove things like: myrepo/./build/
//                 // and get proper casing (see `getExactFullPath` comment)
//                 |> File.getExactFullPath
//                 |> Path.normalizePath
//
//             let watch = defaultArg watch false
//             let precompile = defaultArg precompile false
//
//             let fsprojPath =
//                 fsprojPath |> Option.map normalizeAbsolutePath |> Option.defaultValue rootDir
//
//             let! projFile =
//                 if IO.Directory.Exists(fsprojPath) then
//                     let files = IO.Directory.EnumerateFileSystemEntries(fsprojPath) |> Seq.toList
//
//                     files
//                     |> List.filter (fun file -> file.EndsWith(".fsproj", StringComparison.Ordinal))
//                     |> function
//                         | [] ->
//                             files
//                             |> List.filter (fun file -> file.EndsWith(".fsx", StringComparison.Ordinal))
//                         | candidates -> candidates
//                     |> function
//                         | [] -> Error("Cannot find .fsproj/.fsx in dir: " + fsprojPath)
//                         | [ fsproj ] -> Ok fsproj
//                         | _ -> Error("Found multiple .fsproj/.fsx in dir: " + fsprojPath)
//                 elif not (IO.File.Exists(fsprojPath)) then
//                     Error("File does not exist: " + fsprojPath)
//                 else
//                     Ok fsprojPath
//
//             let typedArrays = args.FlagOr("--typedArrays", not (language = TypeScript))
//
//             let outDir = args.Value("-o", "--outDir") |> Option.map normalizeAbsolutePath
//
//             let precompiledLib =
//                 args.Value("--precompiledLib") |> Option.map normalizeAbsolutePath
//
//             let fableLib = args.Value "--fableLib" |> Option.map Path.normalizePath
//             let useMSBuildForCracking = args.FlagOr("--legacyCracker", true)
//
//             do!
//                 match watch, outDir, fableLib with
//                 | true, _, _ when precompile -> Error("Cannot watch when precompiling")
//                 | _, None, _ when precompile -> Error("outDir must be specified when precompiling")
//                 | _, _, Some _ when Option.isSome precompiledLib ->
//                     Error("Cannot set fableLib when setting precompiledLib")
//                 | _ -> Ok()
//
//             do!
//                 let reservedDirs = [ Naming.fableModules; "obj" ]
//
//                 let outDirLast =
//                     outDir
//                     |> Option.bind (fun outDir -> outDir.TrimEnd('/').Split('/') |> Array.tryLast)
//                     |> Option.defaultValue ""
//
//                 if List.contains outDirLast reservedDirs then
//                     Error($"{outDirLast} is a reserved directory, please use another output directory")
//                 // TODO: Remove this check when typed arrays are compatible with typescript
//                 elif language = TypeScript && typedArrays then
//                     Error("Typescript output is currently not compatible with typed arrays, pass: --typedArrays false")
//                 else
//                     Ok()
//
//             let configuration =
//                 let defaultConfiguration =
//                     if watch then
//                         "Debug"
//                     else
//                         "Release"
//
//                 match args.Value("-c", "--configuration") with
//                 | None -> defaultConfiguration
//                 | Some c when String.IsNullOrWhiteSpace c -> defaultConfiguration
//                 | Some configurationArg -> configurationArg
//
//             let define =
//                 args.Values "--define"
//                 |> List.append
//                     [
//                         "FABLE_COMPILER"
//                         "FABLE_COMPILER_5"
//                         match language with
//                         | Php -> "FABLE_COMPILER_PHP"
//                         | Rust -> "FABLE_COMPILER_RUST"
//                         | Dart -> "FABLE_COMPILER_DART"
//                         | Python -> "FABLE_COMPILER_PYTHON"
//                         | TypeScript -> "FABLE_COMPILER_TYPESCRIPT"
//                         | JavaScript -> "FABLE_COMPILER_JAVASCRIPT"
//                     ]
//                 |> List.distinct
//
//             let fileExt =
//                 args.Value("-e", "--extension")
//                 |> Option.map (fun e ->
//                     if e.StartsWith('.') then
//                         e
//                     else
//                         "." + e
//                 )
//                 |> Option.defaultWith (fun () ->
//                     let usesOutDir = Option.isSome outDir
//                     File.defaultFileExt usesOutDir language
//                 )
//
//             let compilerOptions =
//                 CompilerOptionsHelper.Make(
//                     language = language,
//                     typedArrays = typedArrays,
//                     fileExtension = fileExt,
//                     define = define,
//                     debugMode = (configuration = "Debug"),
//                     optimizeFSharpAst = args.FlagEnabled "--optimize",
//                     noReflection = args.FlagEnabled "--noReflection",
//                     verbosity = verbosity
//                 )
//
//             let cliArgs =
//                 {
//                     ProjectFile = Path.normalizeFullPath projFile
//                     FableLibraryPath = fableLib
//                     RootDir = rootDir
//                     Configuration = configuration
//                     OutDir = outDir
//                     IsWatch = watch
//                     Precompile = precompile
//                     PrecompiledLib = precompiledLib
//                     PrintAst = args.FlagEnabled "--printAst"
//                     SourceMaps = args.FlagEnabled "-s" || args.FlagEnabled "--sourceMaps"
//                     SourceMapsRoot = args.Value "--sourceMapsRoot"
//                     NoRestore = args.FlagEnabled "--noRestore"
//                     NoCache = args.FlagEnabled "--noCache"
//                     // TODO: If we select optimize we cannot have F#/Fable parallelization
//                     NoParallelTypeCheck = args.FlagEnabled "--noParallelTypeCheck"
//                     Exclude = args.Values "--exclude"
//                     Replace =
//                         args.Values "--replace"
//                         |> List.map (fun v ->
//                             let v = v.Split(':')
//                             v.[0], normalizeAbsolutePath v.[1]
//                         )
//                         |> Map
//                     RunProcess = runProc
//                     CompilerOptions = compilerOptions
//                     Verbosity = verbosity
//                 }
//
//             let watchDelay =
//                 if watch then
//                     args.Value("--watchDelay") |> Option.map int |> Option.defaultValue 200 |> Some
//                 else
//                     None
//
//             let startCompilation () =
//                 State.Create(cliArgs, ?watchDelay = watchDelay, useMSBuildForCracking = useMSBuildForCracking)
//                 |> startCompilationAsync
//                 |> Async.RunSynchronously
//
//             return!
//                 // In CI builds, it may happen that two parallel Fable compilations try to precompile
//                 // the same library at the same time, use a lock file to prevent issues in that case.
//                 match outDir, precompile, watch with
//                 | Some outDir, true, false -> File.withLock outDir startCompilation
//                 | _ -> startCompilation ()
//                 |> Result.mapEither ignore fst
//         }
