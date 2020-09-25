module Fable.Cli.Agent

open System
open FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

type FileWriter(path: string) =
    let stream = new IO.StreamWriter(path)
    interface BabelPrinter.Writer with
        member _.Write(str) =
            stream.WriteAsync(str) |> Async.AwaitTask
        member _.EscapeJsStringLiteral(str) =
            Web.HttpUtility.JavaScriptStringEncode(str)
        member _.MakeImportPath(path) = path // TODO: support outDir
        member _.Dispose() = stream.Dispose()

type SingleShotObservable<'T>() =
    let mutable subscriber: IObserver<'T> option = None
    member _.Trigger(value) =
        subscriber |> Option.iter (fun w -> w.OnNext(value))
        subscriber <- None

    interface IObservable<'T> with
        member _.Subscribe(listener) =
            subscriber <- Some listener
            { new IDisposable with
                member _.Dispose() = () }

type Watcher(projDir: string) =
    let changes = ResizeArray()
    let observable = SingleShotObservable()
    let watcher = new IO.FileSystemWatcher(projDir)
    let timer = new Timers.Timer(500., AutoReset=false)

    do
        watcher.Filters.Add("*.fs")
        watcher.Filters.Add("*.fsproj")
        watcher.IncludeSubdirectories <- true
        watcher.NotifyFilter <- IO.NotifyFilters.LastWrite
        watcher.EnableRaisingEvents <- false

        timer.Elapsed.Add(fun _ ->
            changes
            |> Seq.map Path.normalizeFullPath
            |> set
            |> observable.Trigger
            changes.Clear())

        watcher.Changed.Add(fun ev ->
            // TODO: Reset timer for every change?
            if not timer.Enabled then
                timer.Start()
            changes.Add(ev.FullPath))

    member _.AwaitChanges() =
        Async.FromContinuations(fun (onSuccess, _onError, _onCancel) ->
            watcher.EnableRaisingEvents <- true
            observable.Add(fun changes ->
                watcher.EnableRaisingEvents <- false
                onSuccess changes))

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

    let compileFile (com: CompilerImpl) = async {
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
            let writer = new FileWriter(Path.replaceExtension newExtension com.CurrentFile)

            do! BabelPrinter.run writer map babel

            Log.always(sprintf "Compiled %s" com.CurrentFile)
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
            let proj = Path.getRelativePath msg.RootDir msg.ProjectFile
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

        Log.always("Compiling " + Path.getRelativePath cliArgs.RootDir config.ProjectFile + "...")

        let checkedProject =
            let fileDic = config.SourceFiles |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
            let sourceReader f = fileDic.[f].ReadSource()
            let filePaths = config.SourceFiles |> Array.map (fun file -> file.NormalizedFullPath)
            checker.ParseAndCheckProject(config.ProjectFile, filePaths, sourceReader)

        // checkFableCoreVersion checkedProject
        let optimized = false // TODO: Pass through CompilerOptions
        let implFiles =
            if not optimized
            then checkedProject.AssemblyContents.ImplementationFiles
            else checkedProject.GetOptimizedAssemblyContents().ImplementationFiles

        if List.isEmpty implFiles then
            Log.always "The list of files returned by F# compiler is empty"

        let implFilesMap =
            implFiles
            |> Seq.map (fun file -> Path.normalizePathAndEnsureFsExtension file.FileName, file)
            |> dict

        let proj = Project(config.ProjectOptions, implFilesMap, checkedProject.Errors)
        ProjectParsed(proj, checker)

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndParsed: (ProjectCracked * ProjectParsed) option
      Watcher: Watcher option
      WatchDependencies: Map<string, string[]> }

let rec startCompilation (changes: Set<string>) (state: State) = async {
    let cracked, parsed, filesToCompile =
        match state.ProjectCrackedAndParsed with
        // TODO: If changes contain project file, crack and parse the project again
        | Some(cracked, parsed) ->
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
                    // TODO: We probably need to recompile errored files too even if they're not touched
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
            // TODO: If compiled file more recent than F# file already exists,
            // skip compilation if passing a --cache flag?
            let filesToCompile =
                cracked.SourceFiles
                |> Array.map (fun f -> f.NormalizedFullPath)
            cracked, parsed, filesToCompile

    // TODO: Fail already if F# compilation gives error?
    let logs = getFSharpErrorLogs parsed.Project

    let filesToCompile =
        filesToCompile
        |> Array.filter (fun x -> x.EndsWith(".fs"))
        |> fun filesToCompile ->
            // TODO: Log skipped files in verbose mode
            match state.CliArgs.Exclude with
            | Some exclude -> filesToCompile |> Array.filter (fun x -> not(x.Contains(exclude))) // Use regex?
            | None -> filesToCompile

    let! logs, watchDependencies =
        filesToCompile
        |> Array.map (fun file ->
            cracked.MakeCompiler(file, parsed.Project)
            |> compileFile)
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

    // TODO: Print first info, then warning and finally errors
    for log in logs do
        match log.Severity, log.FileName with
        // Ignore warnings from packages in `.fable` folder
        | Severity.Warning, Some filename when Naming.isInFableHiddenDir(filename) -> ()
        | _ -> Log.always(formatLog log)

    match state.Watcher with
    | None ->
        let hasError = logs |> Array.exists (fun log -> log.Severity = Severity.Error)
        return if not hasError then Ok() else Error()
    | Some watcher ->
        Log.always("Watching...")
        let! changes = watcher.AwaitChanges()
        return!
            { state with ProjectCrackedAndParsed = Some(cracked, parsed)
                         WatchDependencies = watchDependencies }
            |> startCompilation changes
}
