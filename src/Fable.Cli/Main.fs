module Fable.Cli.Main

open System
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

module private Util =
    let newLine = Environment.NewLine

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
        $"[ERROR] %s{file}{newLine}%s{ex.Message}{newLine}%s{stack}"

    let formatLog (cliArgs: CliArgs) (log: Log) =
        match log.FileName with
        | None -> log.Message
        | Some file ->
            // Add ./ to make sure VS Code terminal recognises this as a clickable path
            let file = "." + IO.Path.DirectorySeparatorChar.ToString() + IO.Path.GetRelativePath(cliArgs.RootDir, file)
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

    // For Python we must have an outDir since all compiled files must be inside the same subdir, so if `outDir` is not
    // set we set `outDir` to the directory of the project file being compiled.
    let getOutPyPath (cliArgs: CliArgs) dedupTargetDir file =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let isInFableHiddenDir = Naming.isInFableHiddenDir file
        let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile
        let outDir =
            match cliArgs.OutDir with
            | Some outDir ->outDir
            | None -> IO.Path.GetDirectoryName cliArgs.ProjectFile
        let absPath = Imports.getTargetAbsolutePath dedupTargetDir file projDir outDir
        File.changeFsExtension isInFableHiddenDir absPath fileExt

    let compileFile isRecompile (cliArgs: CliArgs) dedupTargetDir (com: CompilerImpl) = async {
        try
            let outPath =
                match com.Options.Language with
                | Python -> getOutPyPath cliArgs dedupTargetDir com.CurrentFile
                | _ -> getOutJsPath cliArgs dedupTargetDir com.CurrentFile

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

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None
    member _.NormalizedFullPath = normalizedFullPath
    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy File.readAllTextNonBlocking normalizedFullPath
        | _ ->
            let source = File.readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

type ProjectCracked(cliArgs: CliArgs, crackerResponse: CrackerResponse, sourceFiles: File array) =

    member _.CliArgs = cliArgs
    member _.ProjectFile = cliArgs.ProjectFile
    member _.FableOptions = cliArgs.CompilerOptions
    member _.ProjectOptions = crackerResponse.ProjectOptions
    member _.References = crackerResponse.References
    member _.SourceFiles = sourceFiles
    member _.SourceFilePaths = sourceFiles |> Array.map (fun f -> f.NormalizedFullPath)

    member _.MakeCompiler(currentFile, project, ?triggeredByDependency) =
        let opts =
            match triggeredByDependency with
            | Some t -> { cliArgs.CompilerOptions with TriggeredByDependency = t }
            | None -> cliArgs.CompilerOptions
        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir
        let common = Path.getCommonBaseDir([currentFile; crackerResponse.FableLibDir])
        let outputType =
            // Everything within the Fable hidden directory will be compiled as Library. We do this since the files there will be
            // compiled as part of the main project which might be a program (Exe) or library (Library).
            let fableHiddenDir = Naming.fableHiddenDir
            if common.EndsWith(fableHiddenDir) then
                Some "Library"
            else
                crackerResponse.OutputType
        CompilerImpl(currentFile, project, opts, fableLibDir, ?outDir=cliArgs.OutDir, ?outType=outputType)

    member _.MapSourceFiles(f) =
        ProjectCracked(cliArgs, crackerResponse, Array.map f sourceFiles)

    static member Init(cliArgs: CliArgs) =
        Log.always $"Parsing {cliArgs.ProjectFileAsRelativePath}..."
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
        Log.always $"Project and references ({result.ProjectOptions.SourceFiles.Length} source files) parsed in %i{ms}ms{newLine}"
        Log.verbose(lazy $"""F# PROJECT: %s{cliArgs.ProjectFileAsRelativePath}
    %s{result.ProjectOptions.OtherOptions |> String.concat $"{newLine}   "}
    %s{result.ProjectOptions.SourceFiles |> String.concat $"{newLine}   "}""")
        let sourceFiles = result.ProjectOptions.SourceFiles |> Array.map File
        ProjectCracked(cliArgs, result, sourceFiles)

type ProjectChecked(checker: InteractiveChecker, errors: FSharpDiagnostic array, project: Project) =

    static let checkProject (config: ProjectCracked) (checker: InteractiveChecker) lastFile = async {
        Log.always "Started F# compilation..."

        let! checkResults, ms = measureTimeAsync <| fun () ->
            let fileDic =
                config.SourceFiles
                |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
            let sourceReader f = fileDic.[f].ReadSource()
            let filePaths = config.SourceFiles |> Array.map (fun file -> file.NormalizedFullPath)
            checker.ParseAndCheckProject(config.ProjectFile, filePaths, sourceReader, ?lastFile=lastFile)

        Log.always $"F# compilation finished in %i{ms}ms{newLine}"

        let implFiles =
            if config.FableOptions.OptimizeFSharpAst then checkResults.GetOptimizedAssemblyContents().ImplementationFiles
            else checkResults.AssemblyContents.ImplementationFiles

        return implFiles, checkResults.Diagnostics, lazy checkResults.ProjectContext.GetReferencedAssemblies()
    }

    member _.Project = project
    member _.Checker = checker
    member _.Errors = errors

    static member Init(projCracked: ProjectCracked) = async {
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
        let! implFiles, errors, assemblies = checkProject projCracked checker None

        return ProjectChecked(checker, errors, Project.From(
                                projCracked.ProjectFile,
                                implFiles,
                                assemblies.Value,
                                mkCompiler = (fun p f -> projCracked.MakeCompiler(f, p)),
                                getPlugin = loadType projCracked.CliArgs,
                                trimRootModule = projCracked.FableOptions.TrimRootModule))
    }

    member this.Update(projCracked: ProjectCracked, filesToCompile) = async {
        let! implFiles, errors, _ =
            Some(Array.last filesToCompile)
            |> checkProject projCracked this.Checker

        let filesToCompile = set filesToCompile
        let implFiles = implFiles |> List.filter (fun f -> filesToCompile.Contains(f.FileName))
        return ProjectChecked(checker, errors, this.Project.Update(implFiles, mkCompiler = (fun p f -> projCracked.MakeCompiler(f, p))))
    }

type Watcher =
    { Watcher: FsWatcher
      Subscription: IDisposable
      StartedAt: DateTime
      OnChange: ISet<string> -> unit }

    static member Create(watchDelay) =
        { Watcher = FsWatcher(watchDelay)
          Subscription = { new IDisposable with member _.Dispose() = () }
          StartedAt = DateTime.MinValue
          OnChange = ignore }

    member this.Watch(projCracked: ProjectCracked) =
        if this.StartedAt > projCracked.ProjectOptions.LoadTime then this
        else
            Log.verbose(lazy "Watcher started!")
            this.Subscription.Dispose()
            let subs =
                // TODO: watch also project.assets.json?
                this.Watcher.Observe [
                    projCracked.ProjectFile
                    yield! projCracked.References
                    yield! projCracked.SourceFiles |> Array.choose (fun f ->
                        let path = f.NormalizedFullPath
                        if Naming.isInFableHiddenDir(path) then None
                        else Some path)
                ]
                |> Observable.subscribe this.OnChange
            { this with Subscription = subs; StartedAt = DateTime.UtcNow }

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndChecked: (ProjectCracked * ProjectChecked) option
      WatchDependencies: Map<string, string[]>
      PendingFiles: string[]
      DeduplicateDic: Collections.Concurrent.ConcurrentDictionary<string, string>
      Watcher: Watcher option
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

    member this.TriggeredByDependency(path: string, changes: ISet<string>) =
        match Map.tryFind path this.WatchDependencies with
        | None -> false
        | Some watchDependencies -> watchDependencies |> Array.exists changes.Contains

    static member Create(cliArgs, ?watchDelay) =
        { CliArgs = cliArgs
          ProjectCrackedAndChecked = None
          WatchDependencies = Map.empty
          Watcher = watchDelay |> Option.map Watcher.Create
          DeduplicateDic = Collections.Concurrent.ConcurrentDictionary()
          PendingFiles = [||]
          HasCompiledOnce = false }

let private compilationCycle (state: State) (changes: ISet<string>) = async {
    let getFilesToCompile (oldFiles: IDictionary<string, File> option) (projCracked: ProjectCracked) =
        let pendingFiles = set state.PendingFiles

        // Clear the hash of files that have changed
        let projCracked = projCracked.MapSourceFiles(fun file ->
            if changes.Contains(file.NormalizedFullPath) then
                File(file.NormalizedFullPath)
            else file)

        let filesToCompile =
            projCracked.SourceFilePaths |> Array.filter (fun path ->
                changes.Contains path
                || pendingFiles.Contains path
                || state.TriggeredByDependency(path, changes)
                // TODO: If files have been deleted, we should likely recompile after first deletion
                || (match oldFiles with Some oldFiles -> not(oldFiles.ContainsKey(path)) | None -> false)
            )
        Log.verbose(lazy $"""Files to compile:{newLine}    {filesToCompile |> String.concat $"{newLine}    "}""")
        projCracked, filesToCompile

    let projCracked, projChecked, filesToCompile =
        match state.ProjectCrackedAndChecked with
        | None ->
            let projCracked = ProjectCracked.Init(state.CliArgs)
            projCracked, None, projCracked.SourceFilePaths

        | Some(projCracked, projChecked) ->
            // For performance reasons, don't crack .fsx scripts for every change
            let fsprojChanged = changes |> Seq.exists (fun c -> c.EndsWith(".fsproj"))

            if fsprojChanged then
                let oldProjCracked = projCracked
                let newProjCracked = ProjectCracked.Init(state.CliArgs)

                // If only source files have changed, keep the project checker to speed up recompilation
                let projChecked =
                    if oldProjCracked.ProjectOptions.OtherOptions = newProjCracked.ProjectOptions.OtherOptions
                    then Some projChecked
                    else None

                let oldFiles = oldProjCracked.SourceFiles |> Array.map (fun f -> f.NormalizedFullPath, f) |> dict
                let newProjCracked = newProjCracked.MapSourceFiles(fun f ->
                    match oldFiles.TryGetValue(f.NormalizedFullPath) with
                    | true, f -> f
                    | false, _ -> f)
                let newProjCracked, filesToCompile = getFilesToCompile (Some oldFiles) newProjCracked
                newProjCracked, projChecked, filesToCompile
            else
                let projCracked, filesToCompile = getFilesToCompile None projCracked
                projCracked, Some projChecked, filesToCompile

    // Update the watcher (it will restart if the fsproj has changed)
    // so changes while compiling get enqueued
    let watcher = state.Watcher |> Option.map (fun w -> w.Watch(projCracked))
    let state = { state with Watcher = watcher }

    // TODO: Use Result here to fail more gracefully if FCS crashes
    let! projChecked =
        match projChecked with
        | None -> ProjectChecked.Init(projCracked)
        | Some projChecked when Array.isEmpty filesToCompile -> async.Return projChecked
        | Some projChecked -> projChecked.Update(projCracked, filesToCompile)

    let logs = getFSharpErrorLogs projChecked.Errors
    let hasFSharpError = logs |> Array.exists (fun l -> l.Severity = Severity.Error)

    let! logs, state = async {
        // Skip Fable recompilation if there are F# errors, this prevents bundlers, dev servers, tests... from being triggered
        if hasFSharpError && state.HasCompiledOnce then
            return logs, { state with PendingFiles = filesToCompile }
        else
            Log.always "Started Fable compilation..."

            let! results, ms = measureTimeAsync <| fun () ->
                filesToCompile
                |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
                |> Array.map (fun file ->
                    projCracked.MakeCompiler(file, projChecked.Project, state.TriggeredByDependency(file, changes))
                    |> compileFile state.HasCompiledOnce state.CliArgs state.GetOrAddDeduplicateTargetDir)
                |> Async.Parallel

            Log.always $"Fable compilation finished in %i{ms}ms{newLine}"

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
    let filesToCompile = set filesToCompile

    for log in logs do
        match log.Severity with
        | Severity.Error -> () // We deal with errors below
        | Severity.Info | Severity.Warning ->
            // Ignore warnings from packages in `fable_modules` folder
            match log.FileName with
            | Some filename when Naming.isInFableHiddenDir(filename) || not(filesToCompile.Contains(filename)) -> ()
            | _ ->
                let formatted = formatLog state.CliArgs log
                if log.Severity = Severity.Warning then Log.warning formatted
                else Log.always formatted

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
                    let lastFilePath = getOutJsPath state.CliArgs state.GetOrAddDeduplicateTargetDir lastFile.NormalizedFullPath
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

    let state =
        { state with ProjectCrackedAndChecked = Some(projCracked, projChecked)
                     PendingFiles = if state.PendingFiles.Length = 0 then erroredFiles else state.PendingFiles }

    return
        match errorMsg with
        | Some e -> state, Error e
        | None -> state, Ok()
}

type Msg =
    | Changes of timeStamp: DateTime * changes: ISet<string>

let startCompilation state = async {
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

    // Initialize changes with an empty set
    let changes = HashSet() :> ISet<_>
    let! _state, result =
        match state.Watcher with
        | None -> compilationCycle state changes
        | Some watcher ->
            let agent =
                MailboxProcessor<Msg>.Start(fun agent ->
                    let rec loop state = async {
                        match! agent.Receive() with
                        | Changes(timestamp, changes) ->
                            match state.Watcher with
                            // Discard changes that may have happened before we restarted the watcher
                            | Some w when w.StartedAt < timestamp ->
                                // TODO: Get all messages until QueueLength is 0 before starting the compilation cycle?
                                Log.verbose(lazy $"""Changes:{newLine}    {changes |> String.concat $"{newLine}    "}""")
                                let! state, _result = compilationCycle state changes
                                Log.always "Watching..."
                                return! loop state
                            | _ -> return! loop state
                    }

                    let onChange changes =
                        Changes(DateTime.UtcNow, changes) |> agent.Post

                    loop { state with Watcher = Some { watcher with OnChange = onChange } })

            // The watcher will remain endless active so we don't really need the reply channel
            agent.PostAndAsyncReply(fun _ -> Changes(DateTime.UtcNow, changes))

    return result
}