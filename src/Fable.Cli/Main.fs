module Fable.Cli.Main

open System
open System.IO
open FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

type FileWriter(sourcePath: string, targetPath: string, projDir: string, outDir: string option) =
    let stream = new StreamWriter(targetPath)
    interface BabelPrinter.Writer with
        member _.Write(str) =
            stream.WriteAsync(str) |> Async.AwaitTask
        member _.EscapeJsStringLiteral(str) =
            Web.HttpUtility.JavaScriptStringEncode(str)
        member _.MakeImportPath(path) =
            Imports.getImportPath sourcePath targetPath projDir outDir path
        member _.Dispose() = stream.Dispose()

type SingleShotObservable<'T>() =
    let mutable subscriber: IObserver<'T> option = None
    member _.Trigger(value) =
        match subscriber with
        | None -> ()
        | Some w ->
            subscriber <- None
            w.OnNext(value)

    interface IObservable<'T> with
        member _.Subscribe(listener) =
            subscriber <- Some listener
            { new IDisposable with
                member _.Dispose() = () }

type Watcher(projDir: string) =
    let changes = ResizeArray()
    let observable = SingleShotObservable()
    let watcher = new FileSystemWatcher(projDir)
    let timer = new Timers.Timer(500., AutoReset=false)

    do
        watcher.Filters.Add("*.fs")
        watcher.Filters.Add("*.fsx")
        watcher.Filters.Add("*.fsproj")
        watcher.IncludeSubdirectories <- true
        // watcher.NotifyFilter <- NotifyFilters.LastWrite
        watcher.EnableRaisingEvents <- false

        timer.Elapsed.Add(fun _ ->
            changes
            |> Seq.map Path.normalizeFullPath
            |> set
            |> observable.Trigger
            changes.Clear())

        let onChange _changeType fullPath =
            if not timer.Enabled then
                timer.Start()
            changes.Add(fullPath)

        watcher.Changed.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Created.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Deleted.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Renamed.Add(fun ev -> onChange ev.ChangeType ev.FullPath)

    member _.Directory = projDir

    member _.AwaitChanges() =
        Async.FromContinuations(fun (onSuccess, _onError, _onCancel) ->
            observable.Add(fun changes ->
                watcher.EnableRaisingEvents <- false
                onSuccess changes)
            watcher.EnableRaisingEvents <- true
        )

module private Util =
    let getSourceFiles (opts: FSharpProjectOptions) =
        opts.OtherOptions |> Array.filter (fun path -> path.StartsWith("-") |> not)

    let splitVersion (version: string) =
        match Version.TryParse(version) with
        | true, v -> v.Major, v.Minor, v.Revision
        | _ -> 0, 0, 0

    let checkFableCoreVersion (checkedProject: FSharpCheckProjectResults) =
        for ref in checkedProject.ProjectContext.GetReferencedAssemblies() do
            if ref.SimpleName = "Fable.Core" then
                let version = System.Text.RegularExpressions.Regex.Match(ref.QualifiedName, @"Version=(\d+\.\d+\.\d+)")
                let expectedMajor, expectedMinor, _ = splitVersion Literals.CORE_VERSION
                let actualMajor, actualMinor, _ = splitVersion version.Groups.[1].Value
                if not(actualMajor = expectedMajor && actualMinor = expectedMinor) then
                    failwithf "Fable.Core v%i.%i detected, expecting v%i.%i" actualMajor actualMinor expectedMajor expectedMinor
                // else printfn "Fable.Core version matches"

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
        sprintf "[ERROR] %s\n%s\n%s" file ex.Message stack

    let formatLog (log: Log) =
        match log.FileName with
        | None -> log.Message
        | Some file ->
            let severity =
                match log.Severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"
            match log.Range with
            | Some r -> sprintf "%s(%i,%i): (%i,%i) %s %s: %s" file r.start.line r.start.column r.``end``.line r.``end``.column severity log.Tag log.Message
            | None -> sprintf "%s(1,1): %s %s: %s" file severity log.Tag log.Message

    let getFSharpErrorLogs (proj: Project) =
        proj.Errors
        |> Array.map (fun er ->
            let severity =
                match er.Severity with
                | FSharpErrorSeverity.Warning -> Severity.Warning
                | FSharpErrorSeverity.Error -> Severity.Error

            let range =
                { start={ line=er.StartLineAlternate; column=er.StartColumn+1}
                  ``end``={ line=er.EndLineAlternate; column=er.EndColumn+1}
                  identifierName = None }

            let msg = sprintf "%s (code %i)" er.Message er.ErrorNumber

            Log.Make(severity, msg, fileName=er.FileName, range=range, tag="FSHARP")
        )
        |> Array.distinct // Sometimes errors are duplicated

    let hasWatchDependency (path: string) (dirtyFiles: Set<string>) watchDependencies =
        match Map.tryFind path watchDependencies with
        | None -> false
        | Some watchDependencies ->
            watchDependencies |> Array.exists (fun p -> Set.contains p dirtyFiles)

    let compileFile (cliArgs: CliArgs) (com: CompilerImpl) = async {
        try
            let babel =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com

            // TODO: Dummy interface until we have a dotnet port of SourceMapGenerator
            // https://github.com/mozilla/source-map#with-sourcemapgenerator-low-level-api
            let map = { new BabelPrinter.SourceMapGenerator with
                            member _.AddMapping(_,_,_,_,_) = () }

            let newExtension =
                match com.Options.FileExtension with
                | "" when com.Options.Typescript -> ".ts"
                | "" -> ".js"
                | ext -> ext

            let projDir =
                cliArgs.ProjectFile |> Path.normalizeFullPath |> Path.GetDirectoryName
            let outPath =
                match cliArgs.OutDir with
                | None ->
                    Path.replaceExtension newExtension com.CurrentFile
                | Some outDir ->
                    let fileExt = if com.Options.Typescript then ".ts" else ".js"
                    let relPath = Imports.getRelativePath projDir com.CurrentFile |> Imports.trimPath
                    let relPath = Path.ChangeExtension(relPath, fileExt)
                    Path.Combine(outDir, relPath)

            // ensure directory exists
            let dir = Path.GetDirectoryName outPath
            if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore

            // write output to file
            let writer = new FileWriter(com.CurrentFile, outPath, projDir, cliArgs.OutDir)
            do! BabelPrinter.run writer map babel

            Log.always("Compiled " + File.getRelativePathFromCwd com.CurrentFile)

            return Ok {| File = com.CurrentFile
                         Logs = com.Logs
                         WatchDependencies = com.WatchDependencies |}
        with e ->
            return Error {| File = com.CurrentFile
                            Exception = e |}
    }

open Util

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None
    member _.NormalizedFullPath = normalizedFullPath
    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy File.readAllTextNonBlocking normalizedFullPath
        | None ->
            let source = File.readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

type ProjectCracked(sourceFiles: File array,
                    fsharpProjOptions: FSharpProjectOptions,
                    fableCompilerOptions: CompilerOptions,
                    fableLibraryDir: string) =

    member _.ProjectFile = fsharpProjOptions.ProjectFileName
    member _.ProjectOptions = fsharpProjOptions
    member _.SourceFiles = sourceFiles

    member _.MakeCompiler(currentFile, project) =
        let fableLibraryDir = Path.getRelativePath currentFile fableLibraryDir
        CompilerImpl(currentFile, project, fableCompilerOptions, fableLibraryDir)

    member _.MapSourceFiles(f) =
        ProjectCracked(Array.map f sourceFiles, fsharpProjOptions, fableCompilerOptions, fableLibraryDir)

    static member Init(msg: CliArgs) =
        let projectOptions, fableLibraryDir =
            getFullProjectOpts {
                fableLib = msg.FableLibraryPath
                define = msg.Define
                forcePkgs = msg.ForcePackages
                rootDir = msg.RootDir
                projFile = msg.ProjectFile
            }

        Log.verbose(lazy
            let proj = File.getRelativePathFromCwd msg.ProjectFile
            let opts = projectOptions.OtherOptions |> String.concat "\n   "
            sprintf "F# PROJECT: %s\n   %s" proj opts)

        let sourceFiles = getSourceFiles projectOptions |> Array.map File
        ProjectCracked(sourceFiles, projectOptions, msg.CompilerOptions, fableLibraryDir)

type ProjectParsed(project: Project,
                   checker: InteractiveChecker) =

    member _.Project = project
    member _.Checker = checker

    static member Init(cliArgs: CliArgs, config: ProjectCracked, ?checker) =
        let checker =
            match checker with
            | Some checker -> checker
            | None ->
                Log.always("Initializing F# compiler...")
                InteractiveChecker.Create(config.ProjectOptions)

        Log.always("Compiling " + File.getRelativePathFromCwd config.ProjectFile + "...")

        let checkedProject, ms = measureTime <| fun () ->
            let fileDic = config.SourceFiles |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
            let sourceReader f = fileDic.[f].ReadSource()
            let filePaths = config.SourceFiles |> Array.map (fun file -> file.NormalizedFullPath)
            checker.ParseAndCheckProject(config.ProjectFile, filePaths, sourceReader)

        Log.always(sprintf "F# compilation finished in %ims" ms)

        // checkFableCoreVersion checkedProject
        let implFiles =
            if cliArgs.CompilerOptions.OptimizeFSharpAst then
                checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
            else
                checkedProject.AssemblyContents.ImplementationFiles

        if List.isEmpty implFiles then
            Log.always "The list of files returned by F# compiler is empty"

        let implFilesMap =
            implFiles
            |> Seq.map (fun file -> Path.normalizePathAndEnsureFsExtension file.FileName, file)
            |> dict

        let proj = Project(config.ProjectOptions, implFilesMap, checkedProject.Errors)
        ProjectParsed(proj, checker)

type TestInfo private (current, iterations, times: int64 list) =
    new (?iterations) = TestInfo(0, defaultArg iterations 50, [])
    member _.LogFileName = "fable-perf-log.txt"
    member _.CurrentIteration: int = current
    member _.TotalIterations: int = iterations
    member _.AverageMilliseconds =
        let total = List.sum times
        total / int64(List.length times)
    member _.MedianMilliseconds =
        let times = List.sort times
        let len = List.length times
        List.item (len / 2 + len % 2) times
    member this.NextIteration(spentMs) =
        new TestInfo(this.CurrentIteration + 1,
                     this.TotalIterations,
                     spentMs::times)

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndParsed: (ProjectCracked * ProjectParsed) option
      Watcher: Watcher option
      WatchDependencies: Map<string, string[]>
      ErroredFiles: Set<string>
      TestInfo: TestInfo option }

let rec startCompilation (changes: Set<string>) (state: State) = async {
    let cracked, parsed, filesToCompile =
        match state.ProjectCrackedAndParsed with
        | Some(cracked, parsed) ->
            let cracked =
                if changes.Contains(state.CliArgs.ProjectFile)
                    // For performance reasons, don't crack .fsx scripts for every change
                    && not(state.CliArgs.ProjectFile.EndsWith(".fsx")) then
                    ProjectCracked.Init(state.CliArgs)
                else cracked

            cracked.SourceFiles
            |> Array.choose (fun file ->
                let path = file.NormalizedFullPath
                if Set.contains path changes then Some path
                else None)
            |> function
                | [||] -> cracked, parsed, [||]
                | dirtyFiles ->
                    let dirtyFiles = set dirtyFiles
                    let cracked = cracked.MapSourceFiles(fun file ->
                        if Set.contains file.NormalizedFullPath dirtyFiles then
                            File(file.NormalizedFullPath) // Clear the cached source hash
                        else file)
                    let parsed = ProjectParsed.Init(state.CliArgs, cracked, parsed.Checker)
                    let filesToCompile =
                        cracked.SourceFiles
                        |> Array.choose (fun file ->
                            let path = file.NormalizedFullPath
                            if Set.contains path dirtyFiles
                                || hasWatchDependency path dirtyFiles state.WatchDependencies then Some path
                            else None)
                    cracked, parsed, filesToCompile
        | None ->
            let cracked = ProjectCracked.Init(state.CliArgs)
            let parsed = ProjectParsed.Init(state.CliArgs, cracked)
            let filesToCompile =
                cracked.SourceFiles
                |> Array.map (fun f -> f.NormalizedFullPath)
                // Skip files that have a more recent JS version
                |> fun files ->
                    if Option.isSome state.CliArgs.OutDir then files
                    else
                        files |> Array.skipWhile (fun file ->
                            try
                                let jsFile = Path.replaceExtension state.CliArgs.CompilerOptions.FileExtension file
                                File.Exists(jsFile) && File.GetLastWriteTime(jsFile) > File.GetLastWriteTime(file)
                            with _ -> false)
            cracked, parsed, filesToCompile

    let filesToCompile =
        filesToCompile
        |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
        |> Array.append (Set.toArray state.ErroredFiles)
        |> Array.distinct
        |> fun filesToCompile ->
            match state.CliArgs.Exclude with
            | Some exclude -> filesToCompile |> Array.filter (fun x -> not(x.Contains(exclude))) // Use regex?
            | None -> filesToCompile

    let logs = getFSharpErrorLogs parsed.Project

    let! (logs, watchDependencies), ms = measureTimeAsync <| fun () ->
        filesToCompile
        |> Array.map (fun file ->
            cracked.MakeCompiler(file, parsed.Project)
            |> compileFile state.CliArgs)
        |> Async.Parallel
        |> Async.map (fun results ->
            ((logs, state.WatchDependencies), results) ||> Array.fold (fun (logs, deps) -> function
                | Ok res ->
                    let logs = Array.append logs res.Logs
                    let deps = Map.add res.File res.WatchDependencies deps
                    logs, deps
                | Error e ->
                    let log = Log.MakeError(e.Exception.Message, fileName=e.File, tag="EXCEPTION")
                    Array.append logs [|log|], deps))

    Log.always(sprintf "Fable compilation finished in %ims" ms)

    logs
    |> Array.filter (fun x -> x.Severity = Severity.Info)
    |> Array.iter (formatLog >> Log.always)

    logs
    |> Array.filter (fun log ->
        match log.Severity, log.FileName with
        // Ignore warnings from packages in `.fable` folder
        | Severity.Warning, Some filename when Naming.isInFableHiddenDir(filename) -> false
        | Severity.Warning, _ -> true
        | _ -> false)
    |> Array.iter (formatLog >> Log.always)

    let newErrors =
        logs
        |> Array.filter (fun x -> x.Severity = Severity.Error)
        |> Array.fold (fun errors log ->
            Log.always(formatLog log)
            match log.FileName with
            | Some file -> Set.add file errors
            | None -> errors) Set.empty

    let state =
        match state.CliArgs.RunArgs with
        | Some runArgs ->
            Process.fireAndForget state.CliArgs.RootDir runArgs.ExeFile runArgs.Args
            if runArgs.IsWatch then state
            else { state with CliArgs = { state.CliArgs with RunArgs = None } }
        | None -> state

    match state.Watcher, state.TestInfo with
    | Some watcher, _ ->
        let oldErrors =
            state.ErroredFiles
            |> Set.filter (fun file -> not(Array.contains file filesToCompile))

        Log.always("Watching " + File.getRelativePathFromCwd watcher.Directory)
        let! changes = watcher.AwaitChanges()
        return!
            { state with ProjectCrackedAndParsed = Some(cracked, parsed)
                         WatchDependencies = watchDependencies
                         ErroredFiles = Set.union oldErrors newErrors }
            |> startCompilation changes

    | None, None ->
        let hasError = not(Set.isEmpty newErrors)
        return if not hasError then Ok() else Error()

    | None, Some info ->
        let info = info.NextIteration(ms)
        if info.CurrentIteration < info.TotalIterations then
            return!
                { state with ProjectCrackedAndParsed = Some(cracked, parsed)
                             TestInfo = Some info }
                |> startCompilation (set filesToCompile)
        else
            let log = sprintf "Completed %i iterations, average: %ims, median: %ims"
                        info.TotalIterations
                        info.AverageMilliseconds
                        info.MedianMilliseconds
            Log.always(log)
            let perfLog = IO.Path.Combine(state.CliArgs.RootDir, info.LogFileName)
            File.AppendAllText(perfLog, log + "\n")
            return Ok()
}
