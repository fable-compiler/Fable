module Fable.Cli.Main

open System
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

module private Util =
    let loadType (cliArgs: CliArgs) (r: PluginRef): Type =
        /// Prevent ReflectionTypeLoadException
        /// From http://stackoverflow.com/a/7889272
        let getTypes (asm: System.Reflection.Assembly) =
            let mutable types: Option<Type[]> = None
            try
                types <- Some(asm.GetTypes())
            with
            | :? System.Reflection.ReflectionTypeLoadException as e ->
                types <- Some e.Types
            match types with
            | None -> Seq.empty
            | Some types ->
                types |> Seq.filter ((<>) null)

        // The assembly may be already loaded, so use `LoadFrom` which takes
        // the copy in memory unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
        System.Reflection.Assembly.LoadFrom(r.DllPath)
        |> getTypes
        // Normalize type name
        |> Seq.tryFind (fun t -> t.FullName.Replace("+", ".") = r.TypeFullName)
        |> function
            | Some t ->
                $"Loaded %s{r.TypeFullName} from %s{IO.Path.GetRelativePath(cliArgs.RootDir, r.DllPath)}"
                |> Log.always; t
            | None -> failwithf "Cannot find %s in %s" r.TypeFullName r.DllPath

    let getSourceFiles (opts: FSharpProjectOptions) =
        opts.OtherOptions |> Array.filter (fun path -> path.StartsWith("-") |> not)

    let splitVersion (version: string) =
        match Version.TryParse(version) with
        | true, v -> v.Major, v.Minor, v.Revision
        | _ -> 0, 0, 0

    // let checkFableCoreVersion (checkedProject: FSharpCheckProjectResults) =
    //     for ref in checkedProject.ProjectContext.GetReferencedAssemblies() do
    //         if ref.SimpleName = "Fable.Core" then
    //             let version = System.Text.RegularExpressions.Regex.Match(ref.QualifiedName, @"Version=(\d+\.\d+\.\d+)")
    //             let expectedMajor, expectedMinor, _ = splitVersion Literals.CORE_VERSION
    //             let actualMajor, actualMinor, _ = splitVersion version.Groups.[1].Value
    //             if not(actualMajor = expectedMajor && actualMinor = expectedMinor) then
    //                 failwithf "Fable.Core v%i.%i detected, expecting v%i.%i" actualMajor actualMinor expectedMajor expectedMinor
    //             // else printfn "Fable.Core version matches"

    let measureTime (f: unit -> 'a) =
        let sw = Diagnostics.Stopwatch.StartNew()
        let res = f()
        sw.Stop()
        res, sw.ElapsedMilliseconds

    let measureTimeAsync (f: unit -> Async<'a>) = async {
        let sw = Diagnostics.Stopwatch.StartNew()
        let! res = f()
        sw.Stop()
        return res, sw.ElapsedMilliseconds
    }

    let formatException file ex =
        let rec innerStack (ex: Exception) =
            if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
        let stack = innerStack ex
        $"[ERROR] %s{file}\n%s{ex.Message}\n%s{stack}"

    let formatLog (cliArgs: CliArgs) (log: Log) =
        match log.FileName with
        | None -> log.Message
        | Some file ->
            let file = IO.Path.GetRelativePath(cliArgs.RootDir, file)
            let severity =
                match log.Severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"
            match log.Range with
            | Some r -> $"%s{file}(%i{r.start.line},%i{r.start.column}): (%i{r.``end``.line},%i{r.``end``.column}) %s{severity} %s{log.Tag}: %s{log.Message}"
            | None -> $"%s{file}(1,1): %s{severity} %s{log.Tag}: %s{log.Message}"

    let getFSharpErrorLogs (errors: FSharpDiagnostic array) =
        errors
        |> Array.map (fun er ->
            let severity =
                match er.Severity with
                | FSharpDiagnosticSeverity.Hidden
                | FSharpDiagnosticSeverity.Info -> Severity.Info
                | FSharpDiagnosticSeverity.Warning -> Severity.Warning
                | FSharpDiagnosticSeverity.Error -> Severity.Error

            let range =
                { start={ line=er.StartLine; column=er.StartColumn+1}
                  ``end``={ line=er.EndLine; column=er.EndColumn+1}
                  identifierName = None }

            let msg = $"%s{er.Message} (code %i{er.ErrorNumber})"

            Log.Make(severity, msg, fileName=er.FileName, range=range, tag="FSHARP")
        )

    let getOutJsPath (cliArgs: CliArgs) dedupTargetDir file =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let isInFableHiddenDir = Naming.isInFableHiddenDir file
        match cliArgs.OutDir with
        | Some outDir ->
            let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile
            let absPath = Imports.getTargetAbsolutePath dedupTargetDir file projDir outDir
            File.changeFsExtension isInFableHiddenDir absPath fileExt
        | None ->
            File.changeFsExtension isInFableHiddenDir file fileExt

    let compileFile isRecompile (cliArgs: CliArgs) dedupTargetDir (com: CompilerImpl) = async {
        try
            let outPath = getOutJsPath cliArgs dedupTargetDir com.CurrentFile

            // ensure directory exists
            let dir = IO.Path.GetDirectoryName outPath
            if not (IO.Directory.Exists dir) then IO.Directory.CreateDirectory dir |> ignore

            do! Pipeline.compileFile com cliArgs dedupTargetDir outPath

            $"Compiled {IO.Path.GetRelativePath(cliArgs.RootDir, com.CurrentFile)}"
            |> Log.verboseOrIf isRecompile

            return Ok {| File = com.CurrentFile
                         Logs = com.Logs
                         WatchDependencies = com.WatchDependencies |}
        with e ->
            return Error {| File = com.CurrentFile
                            Exception = e |}
    }

module FileWatcherUtil =
    let getCommonBaseDir (files: string list) =
        let withTrailingSep d = $"%s{d}%c{IO.Path.DirectorySeparatorChar}"
        files
        |> List.map IO.Path.GetDirectoryName
        |> List.distinct
        |> List.sortBy (fun f -> f.Length)
        |> function
            | [] -> failwith "Empty list passed to watcher"
            | [dir] -> dir
            | dir::restDirs ->
                let rec getCommonDir (dir: string) =
                    // it's important to include a trailing separator when comparing, otherwise things
                    // like ["a/b"; "a/b.c"] won't get handled right
                    // https://github.com/fable-compiler/Fable/issues/2332
                    let dir' = withTrailingSep dir
                    if restDirs |> List.forall (fun d -> (withTrailingSep d).StartsWith dir') then dir
                    else
                        match IO.Path.GetDirectoryName(dir) with
                        | null -> failwith "No common base dir"
                        | dir -> getCommonDir dir
                getCommonDir dir

open Util
open FileWatcher
open FileWatcherUtil

let caseInsensitiveSet(items: string seq): ISet<string> =
    let s = HashSet(items)
    for i in items do s.Add(i) |> ignore
    s :> _

type FsWatcher(delayMs: int) =
    let globFilters = [ "*.fs"; "*.fsi"; "*.fsx"; "*.fsproj" ]
    let createWatcher () =
        let usePolling =
            // This is the same variable used by dotnet watch
            let envVar = Environment.GetEnvironmentVariable("DOTNET_USE_POLLING_FILE_WATCHER")
            not (isNull envVar) &&
                (envVar.Equals("1", StringComparison.OrdinalIgnoreCase)
                || envVar.Equals("true", StringComparison.OrdinalIgnoreCase))

        let watcher: IFileSystemWatcher =
            if usePolling then
                Log.always("Using polling watcher.")
                // Ignored for performance reasons:
                let ignoredDirectoryNameRegexes = [ "(?i)node_modules"; "(?i)bin"; "(?i)obj"; "\..+" ]
                upcast new ResetablePollingFileWatcher(globFilters, ignoredDirectoryNameRegexes)
            else
                upcast new DotnetFileWatcher(globFilters)
        watcher

    let watcher = createWatcher ()
    let observable = Observable.SingleObservable(fun () ->
        watcher.EnableRaisingEvents <- false)

    do
        watcher.OnFileChange.Add(fun path -> observable.Trigger(path))
        watcher.OnError.Add(fun ev ->
            Log.always("Watcher found an error, some events may have been lost.")
            Log.verbose(lazy ev.GetException().Message)
        )

    member _.Observe(filesToWatch: string list) =
        let commonBaseDir = getCommonBaseDir filesToWatch

        // It may happen we get the same path with different case in case-insensitive file systems
        // https://github.com/fable-compiler/Fable/issues/2277#issuecomment-737748220
        let filePaths = caseInsensitiveSet filesToWatch
        watcher.BasePath <- commonBaseDir
        watcher.EnableRaisingEvents <- true

        observable
        |> Observable.choose (fun fullPath ->
            let fullPath = Path.normalizePath fullPath
            if filePaths.Contains(fullPath)
            then Some fullPath
            else None)
        |> Observable.throttle delayMs
        |> Observable.map caseInsensitiveSet

type ProjectCracked(projFile: string,
                    sourceFiles: string array,
                    cliArgs: CliArgs,
                    crackerResponse: CrackerResponse) =

    member _.CliArgs = cliArgs
    member _.ProjectFile = projFile
    member _.FableOptions = cliArgs.CompilerOptions
    member _.ProjectOptions = crackerResponse.ProjectOptions
    member _.References = crackerResponse.References
    member _.SourceFiles = sourceFiles

    member _.MakeCompiler(currentFile, project, outDir) =
        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir
        let common = Path.getCommonBaseDir([currentFile; crackerResponse.FableLibDir])
        let outputType =
            // Everything within the Fable hidden directory will be compiled as Library. We do this since the files there will be
            // compiled as part of the main project which might be a program (Exe) or library (Library).
            let fableHiddenDir =
                match cliArgs.CompilerOptions.Language with
                | Python -> PY.Naming.fableModulesDir
                | _ -> Naming.fableHiddenDir
            if common.EndsWith(fableHiddenDir) then
                Some "Library"
            else
                crackerResponse.OutputType
        CompilerImpl(currentFile, project, cliArgs.CompilerOptions, fableLibDir, ?outDir=outDir, ?outType=outputType)

    static member Init(cliArgs: CliArgs) =
        Log.always $"Parsing {IO.Path.GetRelativePath(cliArgs.RootDir, cliArgs.ProjectFile)}..."
        let result, ms = measureTime <| fun () ->
            CrackerOptions(fableOpts = cliArgs.CompilerOptions,
                           fableLib = cliArgs.FableLibraryPath,
                           outDir = cliArgs.OutDir,
                           configuration = cliArgs.Configuration,
                           exclude = cliArgs.Exclude,
                           replace = cliArgs.Replace,
                           noCache = cliArgs.NoCache,
                           noRestore = cliArgs.NoRestore,
                           projFile = cliArgs.ProjectFile)
            |> getFullProjectOpts

        // We display "parsed" because "cracked" may not be understood by users
        Log.always $"Project parsed in %i{ms}ms\n"
        Log.verbose(lazy
            let proj = IO.Path.GetRelativePath(cliArgs.RootDir, cliArgs.ProjectFile)
            let opts = result.ProjectOptions.OtherOptions |> String.concat "\n   "
            $"F# PROJECT: %s{proj}\n   %s{opts}")

        let sourceFiles = getSourceFiles result.ProjectOptions
        ProjectCracked(cliArgs.ProjectFile, sourceFiles, cliArgs, result)

type ProjectChecked(project: Project, checker: FSharpChecker, errors: FSharpDiagnostic array) =

    member _.Project = project
    member _.Checker = checker
    member _.Errors = errors

    static member Init(config: ProjectCracked) = async {
        let checker =
            FSharpChecker.Create(
                keepAssemblyContents=true,
                keepAllBackgroundResolutions=false,
                keepAllBackgroundSymbolUses=false)

        Log.always $"Compiling {IO.Path.GetRelativePath(config.CliArgs.RootDir, config.ProjectOptions.ProjectFileName)}..."
        let! checkResults, ms = measureTimeAsync <| fun () ->
            checker.ParseAndCheckProject(config.ProjectOptions)
        Log.always $"F# compilation finished in %i{ms}ms\n"

        let proj =
            Project.From(
                config.ProjectFile,
                (if config.FableOptions.OptimizeFSharpAst then
                    checkResults.GetOptimizedAssemblyContents().ImplementationFiles
                else
                    checkResults.AssemblyContents.ImplementationFiles),
                checkResults.ProjectContext.GetReferencedAssemblies(),
                getPlugin = loadType config.CliArgs,
                trimRootModule = config.FableOptions.TrimRootModule
            )
        return ProjectChecked(proj, checker, checkResults.Diagnostics)
    }

    member this.Update(config: ProjectCracked, files: string array) = async {

        Log.always $"Compiling {IO.Path.GetRelativePath(config.CliArgs.RootDir, config.ProjectOptions.ProjectFileName)}..."
        let! results, ms = measureTimeAsync <| fun () ->
            files
            |> Array.map (fun file -> async {
                let! sourceText = File.readAllTextNonBlocking file
                let sourceText = FSharp.Compiler.Text.SourceText.ofString sourceText
                // `fileVersion` parameter doesn't seem to be used, it ends up discarded here:
                // https://github.com/dotnet/fsharp/blob/38fd59027e4b8bb42a72c4d896cb3ef4e345f743/src/fsharp/service/service.fs#L452
                let! _parseResult, checkResult = checker.ParseAndCheckFileInProject(file, 0, sourceText, config.ProjectOptions)
                return
                    match checkResult with
                    | FSharpCheckFileAnswer.Succeeded result -> Some result
                    | FSharpCheckFileAnswer.Aborted ->
                        Log.always $"Aborted: {IO.Path.GetRelativePath(config.CliArgs.RootDir, file)}"
                        None
            })
            |> Async.Parallel
        Log.always $"F# compilation finished in %i{ms}ms\n"

        let results = results |> Array.choose id
        let proj = results |> Array.choose (fun r -> r.ImplementationFile) |> Array.toList |> this.Project.Update
        return ProjectChecked(proj, checker, results |> Array.collect (fun r -> r.Diagnostics))
    }

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndChecked: (ProjectCracked * ProjectChecked) option
      WatchDependencies: Map<string, string[]>
      PendingFiles: string[]
      DeduplicateDic: Collections.Concurrent.ConcurrentDictionary<string, string>
      Watcher: FsWatcher option
      HasCompiledOnce: bool }

    member this.RunProcessEnv =
        let nodeEnv =
            match this.CliArgs.Configuration with
            | "Release" -> "production"
            // | "Debug"
            | _ -> "development"
        [ "NODE_ENV", nodeEnv ]

    member this.GetOrAddDeduplicateTargetDir (importDir: string) addTargetDir =
        // importDir must be trimmed and normalized by now, but lower it just in case
        // as some OS use case insensitive paths
        let importDir = importDir.ToLower()
        this.DeduplicateDic.GetOrAdd(importDir, fun _ ->
            set this.DeduplicateDic.Values
            |> addTargetDir)

    static member Create(cliArgs, ?watchDelay) =
        { CliArgs = cliArgs
          ProjectCrackedAndChecked = None
          WatchDependencies = Map.empty
          Watcher = watchDelay |> Option.map FsWatcher
          DeduplicateDic = Collections.Concurrent.ConcurrentDictionary()
          PendingFiles = [||]
          HasCompiledOnce = false }

let rec startCompilation (changes: ISet<string>) (state: State) = async {
    let state =
        match state.CliArgs.RunProcess with
        | Some runProc when runProc.IsFast ->
            let workingDir = state.CliArgs.RootDir
            let exeFile =
                File.tryNodeModulesBin workingDir runProc.ExeFile
                |> Option.defaultValue runProc.ExeFile
            Process.startWithEnv state.RunProcessEnv workingDir exeFile runProc.Args
            { state with CliArgs = { state.CliArgs with RunProcess = None } }
        | _ -> state

    // TODO: Use Result here to fail more gracefully if FCS crashes
    let! projCracked, projChecked, filesToCompile = async {
        match state.ProjectCrackedAndChecked with
        | Some(projCracked, projChecked) ->
            let fsprojChanged, projCracked =
                // For performance reasons, don't crack .fsx scripts for every change
                if changes |> Seq.exists (fun c -> c.EndsWith(".fsproj")) then
                    true, ProjectCracked.Init(state.CliArgs)
                else false, projCracked

            if fsprojChanged then
                let! projChecked = ProjectChecked.Init(projCracked)
                return projCracked, projChecked, projCracked.SourceFiles
            else
                let hasWatchDependency (path: string) =
                    match Map.tryFind path state.WatchDependencies with
                    | None -> false
                    | Some watchDependencies -> watchDependencies |> Array.exists changes.Contains

                let pendingFiles = set state.PendingFiles

                let filesToCompile =
                    projCracked.SourceFiles |> Array.filter (fun path ->
                        changes.Contains path || pendingFiles.Contains path || hasWatchDependency path)

                let! projChecked = projChecked.Update(projCracked, filesToCompile)
                return projCracked, projChecked, filesToCompile

        | None ->
            let projCracked = ProjectCracked.Init(state.CliArgs)
            let! projChecked = ProjectChecked.Init(projCracked)
            let filesToCompile =
                if Option.isNone state.Watcher || state.CliArgs.NoCache
                then projCracked.SourceFiles
                else
                    // Skip files that have a more recent JS version
                    let filesToCompile =
                        projCracked.SourceFiles
                        |> Array.skipWhile (fun file ->
                            try
                                let jsFile = getOutJsPath state.CliArgs state.GetOrAddDeduplicateTargetDir file
                                IO.File.Exists(jsFile) && IO.File.GetLastWriteTime(jsFile) > IO.File.GetLastWriteTime(file)
                            with _ -> false)
                    if filesToCompile.Length < projCracked.SourceFiles.Length then
                        Log.always("Skipping Fable compilation of up-to-date JS files")
                    filesToCompile
            return projCracked, projChecked, filesToCompile
        }

    let logs = getFSharpErrorLogs projChecked.Errors
    let hasFSharpError = logs |> Array.exists (fun l -> l.Severity = Severity.Error)

    let! logs, state = async {
        // Skip Fable recompilation if there are F# errors, this prevents bundlers, dev servers, tests... from being triggered
        if hasFSharpError && state.HasCompiledOnce then
            return logs, { state with PendingFiles = filesToCompile }
        else
            let! results, ms = measureTimeAsync <| fun () ->
                filesToCompile
                |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
                |> Array.map (fun file ->
                    projCracked.MakeCompiler(file, projChecked.Project, state.CliArgs.OutDir)
                    |> compileFile state.HasCompiledOnce state.CliArgs state.GetOrAddDeduplicateTargetDir)
                |> Async.Parallel

            Log.always $"Fable compilation finished in %i{ms}ms\n"

            let logs, watchDependencies =
                ((logs, state.WatchDependencies), results)
                ||> Array.fold (fun (logs, deps) -> function
                    | Ok res ->
                        let logs = Array.append logs res.Logs
                        let deps = Map.add res.File res.WatchDependencies deps
                        logs, deps
                    | Error e ->
                        let log = Log.MakeError(e.Exception.Message, fileName=e.File, tag="EXCEPTION")
                        Log.verbose(lazy e.Exception.StackTrace)
                        Array.append logs [|log|], deps)

            return logs, { state with HasCompiledOnce = true
                                      PendingFiles = [||]
                                      WatchDependencies = watchDependencies }
    }

    // Sometimes errors are duplicated
    let logs = Array.distinct logs

    logs
    |> Array.filter (fun x -> x.Severity = Severity.Info)
    |> Array.iter (formatLog state.CliArgs >> Log.always)

    logs
    |> Array.filter (fun log ->
        match log.Severity, log.FileName with
        // Ignore warnings from packages in `fable_modules` folder
        | Severity.Warning, Some filename when Naming.isInFableHiddenDir(filename) -> false
        | Severity.Warning, _ -> true
        | _ -> false)
    |> Array.iter (formatLog state.CliArgs >> Log.warning)

    let erroredFiles =
        logs |> Array.choose (fun log ->
            if log.Severity = Severity.Error then
                Log.error(formatLog state.CliArgs log)
                log.FileName
            else None)

    let errorMsg =
        if Array.isEmpty erroredFiles then None
        else Some "Compilation failed"

    let errorMsg, state =
        match errorMsg, state.CliArgs.RunProcess with
        // Only run process if there are no errors
        | Some e, _ -> Some e, state
        | None, None -> None, state
        | None, Some runProc ->
            let workingDir = state.CliArgs.RootDir

            let exeFile, args =
                match runProc.ExeFile with
                | Naming.placeholder ->
                    let lastFile = Array.last projCracked.SourceFiles
                    let lastFilePath = getOutJsPath state.CliArgs state.GetOrAddDeduplicateTargetDir lastFile
                    // Fable's getRelativePath version ensures there's always a period in front of the path: ./
                    let lastFilePath = Path.getRelativeFileOrDirPath true workingDir false lastFilePath
                    "node", lastFilePath::runProc.Args
                | exeFile ->
                    File.tryNodeModulesBin workingDir exeFile
                    |> Option.defaultValue exeFile, runProc.Args

            if Option.isSome state.Watcher then
                Process.startWithEnv state.RunProcessEnv workingDir exeFile args
                let runProc = if runProc.IsWatch then Some runProc else None
                None, { state with CliArgs = { state.CliArgs with RunProcess = runProc } }
            else
                // TODO: When not in watch mode, we should run the process out of this scope
                // to free the memory used by Fable/F# compiler
                let exitCode = Process.runSyncWithEnv state.RunProcessEnv workingDir exeFile args
                (if exitCode = 0 then None else Some "Run process failed"), state

    match state.Watcher with
    | Some watcher ->
        let! changes =
            watcher.Observe [
                projCracked.ProjectOptions.ProjectFileName
                yield! projCracked.References
                yield! projCracked.SourceFiles
                    |> Array.choose (fun path ->
                        if Naming.isInFableHiddenDir(path) then None
                        else Some path)
            ]
            |> Async.AwaitObservable

        return!
            { state with ProjectCrackedAndChecked = Some(projCracked, projChecked)
                         PendingFiles = erroredFiles }
            |> startCompilation changes

    | None ->
        return match errorMsg with Some e -> Error e | None -> Ok()
}

let startFirstCompilation state =
    startCompilation (HashSet() :> ISet<_>) state
