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
        member _.Dispose() =
            stream.Dispose()

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
            |> Seq.distinct
            |> Seq.toList
            |> observable.Trigger
            changes.Clear())

        watcher.Changed.Add(fun ev ->
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

    let addFSharpErrorLogs (com: Compiler) (proj: Project) (triggerFile: string) =
        proj.Errors |> Seq.filter (fun er ->
            // Report warnings always in the corresponding file
            // but ignore those from packages in `.fable` folder
            if er.Severity = FSharpErrorSeverity.Warning then
                com.CurrentFile = er.FileName && not(Naming.isInFableHiddenDir er.FileName)
            // For errors, if the trigger is the .fsproj (first compilation), report them in the corresponding file
            elif triggerFile.EndsWith(".fsproj") then
                com.CurrentFile = er.FileName
            // If another file triggers the compilation, report errors there so they don't go missing
            // But ignore errors from packages in `.fable` folder, as this is watch mode and users can't do anything
            // See https://github.com/fable-compiler/Fable/pull/1714#issuecomment-463137486
            else
                com.CurrentFile = triggerFile && not(Naming.isInFableHiddenDir er.FileName))
        |> Seq.map (fun er ->
            let severity =
                match er.Severity with
                | FSharpErrorSeverity.Warning -> Severity.Warning
                | FSharpErrorSeverity.Error -> Severity.Error
            let range =
                { start={ line=er.StartLineAlternate; column=er.StartColumn+1}
                  ``end``={ line=er.EndLineAlternate; column=er.EndColumn+1}
                  identifierName = None }
            (er.FileName, range, severity, sprintf "%s (code %i)" er.Message er.ErrorNumber))
        |> Seq.distinct // Sometimes errors are duplicated
        |> Seq.iter (fun (fileName, range, severity, msg) ->
            com.AddLog(msg, severity, range, fileName, "FSHARP"))

    let compileFile (com: CompilerImpl) = async {
        try
            let babel =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com

            let writer = new FileWriter(com.CurrentFile + ".js")
            // TODO: Dummy interface until we have a dotnet port of SourceMapGenerator
            // https://github.com/mozilla/source-map#with-sourcemapgenerator-low-level-api
            let map = { new BabelPrinter.SourceMapGenerator with
                            member _.AddMapping(_,_,_,_,_) = () }
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

    member _.WithSourceFiles(sourceFiles) =
        ProjectCracked(sourceFiles, fsharpProjOptions, fableCompilerOptions, fableLibraryDir)

    static member Init(msg: CliArgs) =
        let projectOptions, fableLibraryDir =
            getFullProjectOpts {
                define = msg.Define
                noReferences = msg.NoReferences
                noRestore = msg.NoRestore
                rootDir = msg.RootDir
                projFile = msg.ProjectFile
            }

        Log.verbose(lazy
            let proj = File.getRelativePath msg.ProjectFile
            let opts = projectOptions.OtherOptions |> String.concat "\n   "
            sprintf "F# PROJECT: %s\n   %s" proj opts)

        let sourceFiles = getSourceFiles projectOptions |> Array.map File
        let compilerOptions = CompilerOptionsHelper.Make(typedArrays = msg.TypedArrays,
                                                         typescript = msg.Typescript,
                                                         debugMode = Array.contains "DEBUG" msg.Define,
                                                         verbosity = GlobalParams.Singleton.Verbosity)

        ProjectCracked(sourceFiles, projectOptions, compilerOptions, fableLibraryDir)

type ProjectParsed(project: Project,
                   checker: InteractiveChecker) =

    member _.Project = project
    member _.Checker = checker

    static member Init(config: ProjectCracked, ?checker) =
        let checker =
            match checker with
            | Some checker -> checker
            | None ->
                Log.always("Initializing F# compiler...")
                InteractiveChecker.Create(config.ProjectOptions)

        Log.always("Compiling " + File.getRelativePath config.ProjectFile + "...")

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

//    member this.Update() =
//        // Do nothing if project is parsed less than a second ago or doesn't have dirty files
//        if DateTime.Now - timestamp < TimeSpan.FromSeconds(1.) then
//           this
//        else
//            let mutable someDirtyFiles = false
//            let sourceFiles =
//                this.Config.SourceFiles
//                |> Array.map (fun file ->
//                    let path = file.NormalizedFullPath
//                    // Assume files in .fable folder are stable
//                    if Naming.isInFableHiddenDir path then file
//                    else
//                        let isDirty = IO.File.GetLastWriteTime(path) > timestamp
//                        someDirtyFiles <- someDirtyFiles || isDirty
//                        if isDirty then File(path) // Clear the cached source hash
//                        else file)
//
//            if not someDirtyFiles then this
//            else AgentState.ParseProject(this.Config.WithSourceFiles(sourceFiles), checker)

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndParsed: (ProjectCracked * ProjectParsed) option
      Watcher: Watcher option
      WatchDependencies: Map<string, string[]> }

let rec startCompilation changes (state: State) = async {
    let cracked, parsed =
        match state.ProjectCrackedAndParsed with
        // TODO: If changes contain project file, crack and parse the project again
        // TODO: If not, only parse the project again resetting the dirty files (see Update above)
        | Some(cracked, parsed) -> cracked, parsed
        | None ->
            let cracked = ProjectCracked.Init(state.CliArgs)
            let parsed = ProjectParsed.Init(cracked)
            cracked, parsed

    let! logs, watchDependencies =
        cracked.SourceFiles
        |> Array.map (fun f ->
            cracked.MakeCompiler(f.NormalizedFullPath, parsed.Project)
            |> compileFile)
        |> Async.Parallel
        |> Async.map (fun results ->
            (([||], state.WatchDependencies), results) ||> Array.fold (fun (logs, deps) -> function
                | Ok res ->
                    let logs = Array.append logs res.Logs
                    let deps = Map.add res.File res.WatchDependencies deps
                    logs, deps
                | Error e ->
                    formatException e.File e.Exception
                    |> Log.always
                    logs, deps))

    // TODO: print logs including those from F# compilation

    match state.Watcher with
    // TODO: If there's a log with error severity and we're not in watch mode return error
    | None -> return Ok()
    | Some watcher ->
        let! changes = watcher.AwaitChanges()
        return!
            { state with ProjectCrackedAndParsed = Some(cracked, parsed)
                         WatchDependencies = watchDependencies }
            |> startCompilation changes
}
