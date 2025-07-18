module Fable.Cli.Main

open System
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open Fable.Compiler.ProjectCracker
open Fable.Compiler.Util

module private Util =
    type PathResolver with

        static member Dummy =
            { new PathResolver with
                member _.TryPrecompiledOutPath(_sourceDir, _relativePath) = None

                member _.GetOrAddDeduplicateTargetDir(_importDir, _addTargetDir) = ""
            }

    let isImplementationFile (fileName: string) =
        fileName.EndsWith(".fs", StringComparison.Ordinal)
        || fileName.EndsWith(".fsx", StringComparison.Ordinal)

    let caseInsensitiveSet (items: string seq) : ISet<string> =
        let s = HashSet(items)

        for i in items do
            s.Add(i) |> ignore

        s :> _

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

    let formatException file ex =
        let rec innerStack (ex: Exception) =
            if isNull ex.InnerException then
                ex.StackTrace
            else
                innerStack ex.InnerException

        let stack = innerStack ex
        $"[ERROR] %s{file}{Log.newLine}%s{ex.Message}{Log.newLine}%s{stack}"

    let formatLog rootDir (log: LogEntry) =
        match log.FileName with
        | None -> log.Message
        | Some file ->
            // Add ./ to make sure VS Code terminal recognises this as a clickable path
            let file =
                "."
                + IO.Path.DirectorySeparatorChar.ToString()
                + IO.Path.GetRelativePath(rootDir, file)

            let severity =
                match log.Severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"

            match log.Range with
            | Some r ->
                $"%s{file}(%i{r.start.line},%i{r.start.column}): (%i{r.``end``.line},%i{r.``end``.column}) %s{severity} %s{log.Tag}: %s{log.Message}"
            | None -> $"%s{file}(1,1): %s{severity} %s{log.Tag}: %s{log.Message}"

    let logErrors rootDir (logs: LogEntry seq) =
        logs
        |> Seq.filter (fun log -> log.Severity = Severity.Error)
        |> Seq.iter (fun log -> Log.error (formatLog rootDir log))

    let getFSharpDiagnostics (diagnostics: FSharpDiagnostic array) =
        diagnostics
        |> Array.map (fun er ->
            let severity =
                match er.Severity with
                | FSharpDiagnosticSeverity.Hidden
                | FSharpDiagnosticSeverity.Info -> Severity.Info
                | FSharpDiagnosticSeverity.Warning -> Severity.Warning
                | FSharpDiagnosticSeverity.Error -> Severity.Error

            let range =
                SourceLocation.Create(
                    start =
                        {
                            line = er.StartLine
                            column = er.StartColumn + 1
                        },
                    ``end`` =
                        {
                            line = er.EndLine
                            column = er.EndColumn + 1
                        }
                )

            let msg = $"%s{er.Message} (code %i{er.ErrorNumber})"

            LogEntry.Make(severity, msg, fileName = er.FileName, range = range, tag = "FSHARP")
        )

    let getOutPath (cliArgs: CliArgs) pathResolver file =
        match cliArgs.CompilerOptions.Language with
        // For Python we must have an outDir since all compiled files must be inside the same subdir, so if `outDir` is not
        // set we set `outDir` to the directory of the project file being compiled.
        | Python ->
            let fileExt = cliArgs.CompilerOptions.FileExtension
            let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile

            let outDir =
                match cliArgs.OutDir with
                | Some outDir -> outDir
                | None -> IO.Path.GetDirectoryName cliArgs.ProjectFile

            let absPath =
                let absPath = Imports.getTargetAbsolutePath pathResolver file projDir outDir

                let fileName = IO.Path.GetFileName(file)

                let modules =
                    absPath
                        .Substring(outDir.Length, absPath.Length - outDir.Length - fileName.Length)
                        .Trim([| '/'; '\\' |])
                        .Split([| '/'; '\\' |])
                    |> Array.map (fun m ->
                        match m with
                        | "." -> ""
                        | m -> m.Replace(".", "_")
                    )
                    |> IO.Path.Join

                let fileName = fileName |> Pipeline.Python.getTargetPath cliArgs

                IO.Path.Join(outDir, modules, fileName)

            Path.ChangeExtension(absPath, fileExt)
        | lang ->
            let changeExtension path fileExt =
                match lang with
                | JavaScript
                | TypeScript ->
                    let isInFableModules = Naming.isInFableModules file

                    File.changeExtensionButUseDefaultExtensionInFableModules lang isInFableModules path fileExt
                | _ -> Path.ChangeExtension(path, fileExt)

            let fileExt = cliArgs.CompilerOptions.FileExtension

            match cliArgs.OutDir with
            | Some outDir ->
                let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile

                let absPath = Imports.getTargetAbsolutePath pathResolver file projDir outDir

                changeExtension absPath fileExt
            | None -> changeExtension file fileExt

    let compileFile (com: CompilerImpl) (cliArgs: CliArgs) pathResolver isSilent =
        async {
            let fileName = (com :> Compiler).CurrentFile

            try
                let outPath = getOutPath cliArgs pathResolver fileName

                // ensure directory exists
                let dir = IO.Path.GetDirectoryName outPath

                if not (IO.Directory.Exists dir) then
                    IO.Directory.CreateDirectory dir |> ignore

                do! Pipeline.compileFile com cliArgs pathResolver isSilent outPath

                return
                    Ok
                        {|
                            File = fileName
                            OutPath = outPath
                            Logs = com.Logs
                            InlineExprs = Array.empty<string * InlineExpr>
                            WatchDependencies = com.WatchDependencies
                        |}
            with e ->
                return
                    Error
                        {|
                            File = fileName
                            Exception = e
                        |}
        }

module FileWatcherUtil =
    let getCommonBaseDir (files: string list) =
        let withTrailingSep d =
            $"%s{d}%c{IO.Path.DirectorySeparatorChar}"

        files
        // FCS may add files in temporary dirs to resolve nuget references in scripts
        // See https://github.com/fable-compiler/Fable/pull/2725#issuecomment-1015123642
        |> List.filter (fun file ->
            not (
                file.EndsWith(".fsproj.fsx", StringComparison.Ordinal)
                // It looks like latest F# compiler puts generated files for resolution of packages
                // in scripts in $HOME/.packagemanagement. See #3222
                || file.Contains(".packagemanagement")
            )
        )
        |> List.map IO.Path.GetDirectoryName
        |> List.distinct
        |> List.sortBy (fun f -> f.Length)
        |> function
            | [] -> failwith "Empty list passed to watcher"
            | [ dir ] -> dir
            | dir :: restDirs ->
                let rec getCommonDir (dir: string) =
                    // it's important to include a trailing separator when comparing, otherwise things
                    // like ["a/b"; "a/b.c"] won't get handled right
                    // https://github.com/fable-compiler/Fable/issues/2332
                    let dir' = withTrailingSep dir

                    if
                        restDirs
                        |> List.forall (fun d -> (withTrailingSep d).StartsWith(dir', StringComparison.Ordinal))
                    then
                        dir
                    else
                        match IO.Path.GetDirectoryName(dir) with // 'dir' is empty string ?
                        | null ->
                            let badPaths =
                                restDirs
                                |> List.filter (fun d ->
                                    not ((withTrailingSep d).StartsWith(dir', StringComparison.Ordinal))
                                )
                                |> List.truncate 20

                            [
                                "Fable is trying to find a common base directory for all files and projects referenced."
                                $"But '%s{dir'}' is not a common base directory for these source paths:"
                                for d in badPaths do
                                    $"    - {d}"
                                "If you think this is a bug, please run again with --verbose option and report."
                            ]
                            |> String.concat Environment.NewLine
                            |> failwith

                        | dir -> getCommonDir dir

                getCommonDir dir

open Util
open FileWatcher
open FileWatcherUtil

type FsWatcher(delayMs: int) =
    let globFilters = [ "*.fs"; "*.fsi"; "*.fsx"; "*.fsproj" ]

    let createWatcher () =
        let usePolling =
            // This is the same variable used by dotnet watch
            let envVar = Environment.GetEnvironmentVariable("DOTNET_USE_POLLING_FILE_WATCHER")

            not (isNull envVar)
            && (envVar.Equals("1", StringComparison.OrdinalIgnoreCase)
                || envVar.Equals("true", StringComparison.OrdinalIgnoreCase))

        let watcher: IFileSystemWatcher =
            if usePolling then
                Log.always ("Using polling watcher.")
                // Ignored for performance reasons:
                let ignoredDirectoryNameRegexes =
                    [ "(?i)node_modules"; "(?i)bin"; "(?i)obj"; "\..+" ]

                upcast new ResetablePollingFileWatcher(globFilters, ignoredDirectoryNameRegexes)
            else
                upcast new DotnetFileWatcher(globFilters)

        watcher

    let watcher = createWatcher ()

    let observable =
        Observable.SingleObservable(fun () -> watcher.EnableRaisingEvents <- false)

    do
        watcher.OnFileChange.Add(fun path -> observable.Trigger(path))

        watcher.OnError.Add(fun ev -> Log.verbose (lazy $"[WATCHER] {ev.GetException().Message}"))

    member _.BasePath = watcher.BasePath

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

            if filePaths.Contains(fullPath) then
                Some fullPath
            else
                None
        )
        |> Observable.throttle delayMs
        |> Observable.map caseInsensitiveSet

type ProjectCracked(cliArgs: CliArgs, crackerResponse: CrackerResponse, sourceFiles: Fable.Compiler.File array) =

    member _.CliArgs = cliArgs
    member _.ProjectFile = cliArgs.ProjectFile
    member _.FableOptions = cliArgs.CompilerOptions
    member _.ProjectOptions = crackerResponse.ProjectOptions
    member _.References = crackerResponse.References
    member _.PrecompiledInfo = crackerResponse.PrecompiledInfo
    member _.CanReuseCompiledFiles = crackerResponse.CanReuseCompiledFiles
    member _.SourceFiles = sourceFiles

    member _.SourceFilePaths = sourceFiles |> Array.map (fun f -> f.NormalizedFullPath)

    member _.FableLibDir = crackerResponse.FableLibDir
    member _.FableModulesDir = crackerResponse.FableModulesDir
    member _.TreatmentWarningsAsErrors = crackerResponse.TreatWarningsAsErrors

    member _.MakeCompiler(currentFile, project, ?triggeredByDependency) =
        let opts =
            match triggeredByDependency with
            | Some t -> { cliArgs.CompilerOptions with TriggeredByDependency = t }
            | None -> cliArgs.CompilerOptions

        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir

        let watchDependencies =
            if cliArgs.IsWatch then
                Some(HashSet())
            else
                None

        CompilerImpl(
            currentFile,
            project,
            opts,
            fableLibDir,
            crackerResponse.OutputType,
            ?outDir = cliArgs.OutDir,
            ?watchDependencies = watchDependencies
        )

    member _.MapSourceFiles(f) =
        ProjectCracked(cliArgs, crackerResponse, Array.map f sourceFiles)

    static member Init(cliArgs: CliArgs, useMSBuildForCracking, ?evaluateOnly: bool) =
        let evaluateOnly = defaultArg evaluateOnly false
        Log.always $"Parsing {cliArgs.ProjectFileAsRelativePath}..."

        let result, ms =
            Performance.measure
            <| fun () ->
                let resolver: ProjectCrackerResolver =
                    if useMSBuildForCracking then
                        Fable.Compiler.MSBuildCrackerResolver()
                    else
                        BuildalyzerCrackerResolver()

                CrackerOptions(cliArgs, evaluateOnly) |> getFullProjectOpts resolver

        // We display "parsed" because "cracked" may not be understood by users
        Log.always
            $"Project and references ({result.ProjectOptions.SourceFiles.Length} source files) parsed in %i{ms}ms{Log.newLine}"

        Log.verbose (
            lazy
                $"""F# PROJECT: %s{cliArgs.ProjectFileAsRelativePath}
FABLE LIBRARY: {result.FableLibDir}
TARGET FRAMEWORK: {result.TargetFramework}
OUTPUT TYPE: {result.OutputType}

    %s{result.ProjectOptions.OtherOptions |> String.concat $"{Log.newLine}    "}
    %s{result.ProjectOptions.SourceFiles |> String.concat $"{Log.newLine}    "}{Log.newLine}"""
        )

        // If targeting Python, make sure users are not compiling the project as library by mistake
        // (imports won't work when running the code)
        match cliArgs.CompilerOptions.Language, result.OutputType with
        | Python, OutputType.Library ->
            Log.always
                "Compiling project as Library. If you intend to run the code directly, please set OutputType to Exe."
        | _ -> ()

        let sourceFiles = result.ProjectOptions.SourceFiles |> Array.map Fable.Compiler.File

        ProjectCracked(cliArgs, result, sourceFiles)

type FableCompileResult =
    Result<
        {|
            File: string
            OutPath: string
            Logs: LogEntry[]
            InlineExprs: (string * InlineExpr)[]
            WatchDependencies: string[]
        |},
        {|
            File: string
            Exception: exn
        |}
     >

type ReplyChannel = AsyncReplyChannel<Result< (* fsharpLogs *) LogEntry[] * FableCompileResult list, exn>>

type FableCompilerMsg =
    | GetFableProject of replyChannel: AsyncReplyChannel<Project>
    | StartCompilation of
        sourceFiles: Fable.Compiler.File[] *
        filesToCompile: string[] *
        pathResolver: PathResolver *
        isSilent: bool *
        isTriggeredByDependency: (string -> bool) *
        ReplyChannel
    | FSharpFileTypeChecked of FSharpImplementationFileContents
    | FSharpCompilationFinished of FSharpCheckProjectResults
    | FableFileCompiled of string * FableCompileResult
    | UnexpectedError of exn

type FableCompilerState =
    {
        FableProj: Project
        PathResolver: PathResolver
        IsSilent: bool
        TriggeredByDependency: string -> bool
        FilesToCompile: string[]
        FilesToCompileSet: Set<string>
        FilesCheckedButNotCompiled: Set<string>
        FableFilesToCompileExpectedCount: int
        FableFilesCompiledCount: int
        FSharpLogs: LogEntry[]
        FableResults: FableCompileResult list
        HasFSharpCompilationFinished: bool
        ReplyChannel: ReplyChannel option
    }

    static member Create
        (fableProj, filesToCompile: string[], ?pathResolver, ?isSilent, ?triggeredByDependency, ?replyChannel)
        =
        {
            FableProj = fableProj
            PathResolver = defaultArg pathResolver PathResolver.Dummy
            IsSilent = defaultArg isSilent false
            TriggeredByDependency = defaultArg triggeredByDependency (fun _ -> false)
            FilesToCompile = filesToCompile
            FilesToCompileSet = set filesToCompile
            FilesCheckedButNotCompiled = Set.empty
            FableFilesToCompileExpectedCount = filesToCompile |> Array.filter isImplementationFile |> Array.length
            FableFilesCompiledCount = 0
            FSharpLogs = [||]
            FableResults = []
            HasFSharpCompilationFinished = false
            ReplyChannel = replyChannel
        }

and FableCompiler(checker: InteractiveChecker, projCracked: ProjectCracked, fableProj: Project) =
    let agent =
        MailboxProcessor<FableCompilerMsg>.Start(fun agent ->
            let postTo toMsg work =
                async {
                    try
                        let! result = work ()
                        toMsg result |> agent.Post
                    with e ->
                        UnexpectedError e |> agent.Post
                }

            let startInThreadPool toMsg work = postTo toMsg work |> Async.Start

            let fableCompile state fileName =
                let fableProj = state.FableProj

                startInThreadPool
                    FableFileCompiled
                    (fun () ->
                        async {
                            let com =
                                projCracked.MakeCompiler(
                                    fileName,
                                    fableProj,
                                    triggeredByDependency = state.TriggeredByDependency(fileName)
                                )

                            let! res = compileFile com projCracked.CliArgs state.PathResolver state.IsSilent

                            let res =
                                if not projCracked.CliArgs.Precompile then
                                    res
                                else
                                    res
                                    |> Result.map (fun res ->
                                        {| res with InlineExprs = fableProj.GetFileInlineExprs(com) |}
                                    )

                            return fileName, res
                        }
                    )

                { state with FilesCheckedButNotCompiled = Set.add fileName state.FilesCheckedButNotCompiled }

            let rec loop state =
                async {
                    match! agent.Receive() with
                    | UnexpectedError er -> state.ReplyChannel |> Option.iter (fun ch -> Error er |> ch.Reply)

                    | GetFableProject channel ->
                        channel.Reply(state.FableProj)
                        return! loop state

                    | StartCompilation(sourceFiles,
                                       filesToCompile,
                                       pathResolver,
                                       isSilent,
                                       isTriggeredByDependency,
                                       replyChannel) ->
                        let state =
                            FableCompilerState.Create(
                                state.FableProj,
                                filesToCompile,
                                pathResolver,
                                isSilent,
                                isTriggeredByDependency,
                                replyChannel
                            )

                        startInThreadPool
                            FSharpCompilationFinished
                            (fun () ->
                                let filePaths, sourceReader = Fable.Compiler.File.MakeSourceReader sourceFiles

                                let subscriber =
                                    if projCracked.CliArgs.NoParallelTypeCheck then
                                        None
                                    else
                                        Some(FSharpFileTypeChecked >> agent.Post)

                                checker.ParseAndCheckProject(
                                    projCracked.ProjectFile,
                                    filePaths,
                                    sourceReader,
                                    Array.last filesToCompile,
                                    ?subscriber = subscriber
                                )
                            )

                        return! loop state

                    | FSharpFileTypeChecked file ->
                        // It seems when there's a pair .fsi/.fs the F# compiler gives the .fsi extension to the implementation file
                        let fileName = file.FileName |> Path.normalizePath |> Path.ensureFsExtension

                        // For Rust, delay last file's compilation so other files can finish compiling
                        if
                            projCracked.CliArgs.CompilerOptions.Language = Rust
                            && fileName = Array.last state.FilesToCompile
                            && state.FableFilesCompiledCount < state.FableFilesToCompileExpectedCount - 1
                        then
                            do! Async.Sleep(1000)

                        Log.verbose (
                            lazy $"Type checked: {IO.Path.GetRelativePath(projCracked.CliArgs.RootDir, file.FileName)}"
                        )

                        // Print F# AST to file
                        if projCracked.CliArgs.PrintAst then
                            let outPath = getOutPath projCracked.CliArgs state.PathResolver file.FileName
                            let outDir = IO.Path.GetDirectoryName(outPath)
                            Printers.printAst outDir [ file ]

                        let state =
                            if not (state.FilesToCompileSet.Contains(fileName)) then
                                state
                            else
                                let state = { state with FableProj = state.FableProj.Update([ file ]) }
                                fableCompile state fileName

                        return! loop state

                    | FSharpCompilationFinished results ->
                        Log.verbose (lazy "Type check finished")

                        let state =
                            { state with
                                FSharpLogs = getFSharpDiagnostics results.Diagnostics
                                HasFSharpCompilationFinished = true
                            }

                        let state =
                            if projCracked.CliArgs.NoParallelTypeCheck then
                                let implFiles =
                                    if projCracked.CliArgs.CompilerOptions.OptimizeFSharpAst then
                                        results.GetOptimizedAssemblyContents().ImplementationFiles
                                    else
                                        results.AssemblyContents.ImplementationFiles

                                let state = { state with FableProj = state.FableProj.Update(implFiles) }

                                let filesToCompile =
                                    state.FilesToCompile
                                    |> Array.filter (fun file ->
                                        file.EndsWith(".fs", StringComparison.Ordinal)
                                        || file.EndsWith(".fsx", StringComparison.Ordinal)
                                    )

                                (state, filesToCompile) ||> Array.fold fableCompile
                            else
                                state

                        FableCompiler.CheckIfCompilationIsFinished(state)
                        return! loop state

                    | FableFileCompiled(fileName, result) ->
                        let state =
                            { state with
                                FableResults = result :: state.FableResults
                                FableFilesCompiledCount = state.FableFilesCompiledCount + 1
                                FilesCheckedButNotCompiled = Set.remove fileName state.FilesCheckedButNotCompiled
                            }

                        if not state.IsSilent then
                            let msg =
                                let fileName = IO.Path.GetRelativePath(projCracked.CliArgs.RootDir, fileName)

                                $"Compiled {state.FableFilesCompiledCount}/{state.FableFilesToCompileExpectedCount}: {fileName}"

                            if projCracked.CliArgs.NoParallelTypeCheck then
                                Log.always msg
                            else
                                let isCi = String.IsNullOrEmpty(Environment.GetEnvironmentVariable("CI")) |> not

                                match projCracked.CliArgs.Verbosity with
                                | Verbosity.Silent -> ()
                                | Verbosity.Verbose -> Console.Out.WriteLine(msg)
                                | Verbosity.Normal ->
                                    // Avoid log pollution in CI. Also, if output is redirected don't try to rewrite
                                    // the same line as it seems to cause problems, see #2727
                                    if not isCi && not Console.IsOutputRedirected then
                                        // If the message is longer than the terminal width it will jump to next line
                                        let msg =
                                            if msg.Length > 80 then
                                                msg.[..80] + "..."
                                            else
                                                msg

                                        let curCursorLeft = Console.CursorLeft

                                        Console.SetCursorPosition(0, Console.CursorTop)

                                        Console.Out.Write(msg)
                                        let diff = curCursorLeft - msg.Length

                                        if diff > 0 then
                                            Console.Out.Write(String.replicate diff " ")

                                            Console.SetCursorPosition(msg.Length, Console.CursorTop)

                        FableCompiler.CheckIfCompilationIsFinished(state)
                        return! loop state
                }

            FableCompilerState.Create(fableProj, [||]) |> loop
        )

    member _.GetFableProject() =
        agent.PostAndAsyncReply(GetFableProject)

    member _.StartCompilation(sourceFiles, filesToCompile, pathResolver, isSilent, isTriggeredByDependency) =
        async {
            if Array.isEmpty filesToCompile then
                return [||], []
            else
                if not isSilent then
                    Log.always "Started Fable compilation..."

                let! results, ms =
                    Performance.measureAsync
                    <| fun () ->
                        agent.PostAndAsyncReply(fun channel ->
                            StartCompilation(
                                sourceFiles,
                                filesToCompile,
                                pathResolver,
                                isSilent,
                                isTriggeredByDependency,
                                channel
                            )
                        )

                return
                    match results with
                    | Ok results ->
                        Log.always $"{Log.newLine}Fable compilation finished in %i{ms}ms{Log.newLine}"

                        results
                    | Error e -> e.Message + Log.newLine + e.StackTrace |> Fable.FableError |> raise
        }

    static member CheckIfCompilationIsFinished(state: FableCompilerState) =
        match state.HasFSharpCompilationFinished, Set.isEmpty state.FilesCheckedButNotCompiled, state.ReplyChannel with
        | true, true, Some channel ->
            // Fable results are not guaranteed to be in order but revert them to make them closer to the original order
            let fableResults = state.FableResults |> List.rev
            Ok(state.FSharpLogs, fableResults) |> channel.Reply
        | _ -> ()

    static member Init(projCracked: ProjectCracked) =
        async {
            let checker = InteractiveChecker.Create(projCracked.ProjectOptions)
            let! assemblies = checker.GetImportedAssemblies()

            let fableProj =
                Project.From(
                    projCracked.ProjectFile,
                    projCracked.ProjectOptions,
                    [],
                    assemblies,
                    Log.log,
                    ?precompiledInfo = (projCracked.PrecompiledInfo |> Option.map (fun i -> i :> _)),
                    getPlugin = Reflection.loadType projCracked.CliArgs
                )

            return FableCompiler(checker, projCracked, fableProj)
        }

    member _.CompileToFile(outFile: string) =
        let filePaths, sourceReader =
            Fable.Compiler.File.MakeSourceReader projCracked.SourceFiles

        checker.Compile(filePaths, sourceReader, outFile)

type Watcher =
    {
        Watcher: FsWatcher
        Subscription: IDisposable
        StartedAt: DateTime
        OnChange: ISet<string> -> unit
    }

    static member Create(watchDelay) =
        {
            Watcher = FsWatcher(watchDelay)
            Subscription =
                { new IDisposable with
                    member _.Dispose() = ()
                }
            StartedAt = DateTime.MinValue
            OnChange = ignore
        }

    member this.Watch(projCracked: ProjectCracked) =
        if this.StartedAt > projCracked.ProjectOptions.LoadTime then
            this
        else
            Log.verbose (lazy "Watcher started!")
            this.Subscription.Dispose()

            let subs =
                this.Watcher.Observe
                    [
                        projCracked.ProjectFile
                        yield! projCracked.References
                        yield!
                            projCracked.SourceFiles
                            |> Array.choose (fun f ->
                                let path = f.NormalizedFullPath

                                if Naming.isInFableModules (path) then
                                    None
                                else
                                    Some path
                            )
                    ]
                |> Observable.subscribe this.OnChange

            { this with
                Subscription = subs
                StartedAt = DateTime.UtcNow
            }

type State =
    {
        CliArgs: CliArgs
        ProjectCrackedAndFableCompiler: (ProjectCracked * FableCompiler) option
        WatchDependencies: Map<string, string[]>
        PendingFiles: string[]
        DeduplicateDic: ConcurrentDictionary<string, string>
        Watcher: Watcher option
        SilentCompilation: bool
        RecompileAllFiles: bool
        UseMSBuildForCracking: bool
    }

    member this.TriggeredByDependency(path: string, changes: ISet<string>) =
        match Map.tryFind path this.WatchDependencies with
        | None -> false
        | Some watchDependencies -> watchDependencies |> Array.exists changes.Contains

    member this.GetPathResolver(?precompiledInfo: PrecompiledInfoImpl) =
        { new PathResolver with
            member _.TryPrecompiledOutPath(sourceDir, relativePath) =
                match precompiledInfo with
                | None -> None
                | Some precompiledInfo ->
                    let fullPath = IO.Path.Combine(sourceDir, relativePath) |> Path.normalizeFullPath

                    precompiledInfo.TryPrecompiledOutPath(fullPath)

            member _.GetOrAddDeduplicateTargetDir(importDir: string, addTargetDir) =
                // importDir must be trimmed and normalized by now, but lower it just in case
                // as some OS use case insensitive paths
                let importDir = importDir.ToLower()

                this.DeduplicateDic.GetOrAdd(importDir, (fun _ -> set this.DeduplicateDic.Values |> addTargetDir))
        }

    static member Create(cliArgs, ?watchDelay, ?recompileAllFiles, ?useMSBuildForCracking) =
        {
            CliArgs = cliArgs
            ProjectCrackedAndFableCompiler = None
            WatchDependencies = Map.empty
            Watcher = watchDelay |> Option.map Watcher.Create
            DeduplicateDic = ConcurrentDictionary()
            PendingFiles = [||]
            SilentCompilation = false
            RecompileAllFiles = defaultArg recompileAllFiles false
            UseMSBuildForCracking = defaultArg useMSBuildForCracking false
        }

let private getFilesToCompile
    (state: State)
    (changes: ISet<string>)
    (oldFiles: IDictionary<string, Fable.Compiler.File> option)
    (projCracked: ProjectCracked)
    =
    let pendingFiles = set state.PendingFiles

    // Clear the hash of files that have changed
    let projCracked =
        projCracked.MapSourceFiles(fun file ->
            if changes.Contains(file.NormalizedFullPath) then
                Fable.Compiler.File(file.NormalizedFullPath)
            else
                file
        )

    let pathResolver = state.GetPathResolver()

    let filesToCompile =
        projCracked.SourceFilePaths
        |> Array.filter (fun path ->
            changes.Contains path
            || pendingFiles.Contains path
            || state.TriggeredByDependency(path, changes)
            || (
                match oldFiles with
                | Some oldFiles -> not (oldFiles.ContainsKey(path))
                | None -> false
            )
            || (
                // If files have been deleted, we should likely recompile after first deletion
                let outPath = getOutPath state.CliArgs pathResolver path

                let wasDeleted =
                    (path.EndsWith(".fs") || path.EndsWith(".fsx")) && not (IO.File.Exists outPath)

                wasDeleted)
        )

    Log.verbose (lazy $"""Files to compile:{Log.newLine}    {filesToCompile |> String.concat $"{Log.newLine}    "}""")

    projCracked, filesToCompile

let private areCompiledFilesUpToDate (state: State) (filesToCompile: string[]) =
    try
        let mutable foundCompiledFile = false
        let pathResolver = state.GetPathResolver()

        let upToDate =
            filesToCompile
            |> Array.filter (fun file ->
                file.EndsWith(".fs", StringComparison.Ordinal)
                || file.EndsWith(".fsx", StringComparison.Ordinal)
            )
            |> Array.forall (fun source ->
                let outPath = getOutPath state.CliArgs pathResolver source
                // Empty files are not written to disk so we only check date for existing files
                if IO.File.Exists(outPath) then
                    foundCompiledFile <- true

                    let upToDate = IO.File.GetLastWriteTime(source) < IO.File.GetLastWriteTime(outPath)

                    if not upToDate then
                        Log.verbose (
                            lazy
                                $"Output file {File.relPathToCurDir outPath} is older than {File.relPathToCurDir source}"
                        )

                    upToDate
                else
                    true
            )
        // If we don't find compiled files, assume we need recompilation
        upToDate && foundCompiledFile
    with er ->
        Log.warning ("Cannot check timestamp of compiled files: " + er.Message)
        false

let private runProcessAndForget (cliArgs: CliArgs) (runProc: RunProcess) =
    let workingDir = cliArgs.RootDir

    let exeFile =
        File.tryNodeModulesBin workingDir runProc.ExeFile
        |> Option.defaultValue runProc.ExeFile

    Process.startWithEnv cliArgs.RunProcessEnv workingDir exeFile runProc.Args
    { cliArgs with RunProcess = None }

let private checkRunProcess (state: State) (projCracked: ProjectCracked) (compilationExitCode: int) =
    let cliArgs = state.CliArgs

    match cliArgs.RunProcess with
    // Only run process if there are no errors
    | _ when compilationExitCode <> 0 -> compilationExitCode, state
    | None -> 0, state
    | Some runProc ->
        let workingDir = cliArgs.RootDir

        let findLastFileFullPath () =
            let pathResolver = state.GetPathResolver()
            let lastFile = Array.last projCracked.SourceFiles
            getOutPath cliArgs pathResolver lastFile.NormalizedFullPath

        // Fable's getRelativePath version ensures there's always a period in front of the path: ./
        let findLastFileRelativePath () =
            findLastFileFullPath () |> Path.getRelativeFileOrDirPath true workingDir false

        let exeFile, args =
            match cliArgs.CompilerOptions.Language, runProc.ExeFile with
            | Python, Naming.placeholder ->
                let lastFilePath = findLastFileRelativePath ()
                "python", lastFilePath :: runProc.Args
            | Rust, Naming.placeholder ->
                let lastFileDir = IO.Path.GetDirectoryName(findLastFileFullPath ())

                let args =
                    match File.tryFindUpwards "Cargo.toml" lastFileDir with
                    | Some path -> "--manifest-path" :: path :: runProc.Args
                    | None -> runProc.Args

                "cargo", "run" :: args
            | Dart, Naming.placeholder ->
                let lastFilePath = findLastFileRelativePath ()
                "dart", "run" :: lastFilePath :: runProc.Args
            | JavaScript, Naming.placeholder ->
                let lastFilePath = findLastFileRelativePath ()
                "node", lastFilePath :: runProc.Args
            | (JavaScript | TypeScript), exeFile ->
                File.tryNodeModulesBin workingDir exeFile |> Option.defaultValue exeFile, runProc.Args
            | _, exeFile -> exeFile, runProc.Args

        if Option.isSome state.Watcher then
            Process.startWithEnv cliArgs.RunProcessEnv workingDir exeFile args

            let runProc =
                if runProc.IsWatch then
                    Some runProc
                else
                    None

            0, { state with CliArgs = { cliArgs with RunProcess = runProc } }
        else
            // TODO: When not in watch mode, run process out of this scope to free memory used by Fable/F# compiler
            let exitCode = Process.runSyncWithEnv cliArgs.RunProcessEnv workingDir exeFile args

            exitCode, state

let private compilationCycle (state: State) (changes: ISet<string>) =
    async {
        let cliArgs = state.CliArgs

        let projCracked, fableCompiler, filesToCompile =
            match state.ProjectCrackedAndFableCompiler with
            | None ->
                // #if DEBUG
                // let evaluateOnly = true
                // #else
                let evaluateOnly = false
                // #endif
                let projCracked =
                    ProjectCracked.Init(cliArgs, state.UseMSBuildForCracking, evaluateOnly)

                projCracked, None, projCracked.SourceFilePaths

            | Some(projCracked, fableCompiler) ->
                // For performance reasons, don't crack .fsx scripts for every change
                let fsprojChanged =
                    changes |> Seq.exists (fun c -> c.EndsWith(".fsproj", StringComparison.Ordinal))

                if fsprojChanged then
                    let oldProjCracked = projCracked

                    let newProjCracked =
                        ProjectCracked.Init(
                            { cliArgs with NoCache = true },
                            state.UseMSBuildForCracking,
                            evaluateOnly = true
                        )

                    // If only source files have changed, keep the project checker to speed up recompilation
                    let fableCompiler =
                        if oldProjCracked.ProjectOptions.OtherOptions = newProjCracked.ProjectOptions.OtherOptions then
                            Some fableCompiler
                        else
                            None

                    let oldFiles =
                        oldProjCracked.SourceFiles
                        |> Array.map (fun f -> f.NormalizedFullPath, f)
                        |> dict

                    let newProjCracked =
                        newProjCracked.MapSourceFiles(fun f ->
                            match oldFiles.TryGetValue(f.NormalizedFullPath) with
                            | true, f -> f
                            | false, _ -> f
                        )

                    let newProjCracked, filesToCompile =
                        getFilesToCompile state changes (Some oldFiles) newProjCracked

                    newProjCracked, fableCompiler, filesToCompile
                else
                    let changes =
                        if state.RecompileAllFiles then
                            HashSet projCracked.SourceFilePaths :> ISet<_>
                        else
                            changes

                    let projCracked, filesToCompile = getFilesToCompile state changes None projCracked

                    projCracked, Some fableCompiler, filesToCompile

        // Update the watcher (it will restart if the fsproj has changed)
        // so changes while compiling get enqueued
        let state =
            { state with Watcher = state.Watcher |> Option.map (fun w -> w.Watch(projCracked)) }

        // If not in watch mode and projCracked.CanReuseCompiledFiles, skip compilation if compiled files are up-to-date
        // NOTE: Don't skip Fable compilation in watch mode because we need to calculate watch dependencies
        if
            Option.isNone state.Watcher
            && projCracked.CanReuseCompiledFiles
            && areCompiledFilesUpToDate state filesToCompile
        then
            Log.always "Skipped compilation because all generated files are up-to-date!"

            let exitCode, state = checkRunProcess state projCracked 0

            return
                {|
                    State = state
                    ErrorLogs = [||]
                    OtherLogs = [||]
                    ExitCode = exitCode
                |}
        else
            // Optimization for watch mode, if files haven't changed run the process as with --runFast
            let state, cliArgs =
                match cliArgs.RunProcess with
                | Some runProc when
                    Option.isSome state.Watcher
                    && projCracked.CanReuseCompiledFiles
                    && not runProc.IsWatch
                    && runProc.ExeFile <> Naming.placeholder
                    && areCompiledFilesUpToDate state filesToCompile
                    ->
                    let cliArgs = runProcessAndForget cliArgs runProc

                    { state with
                        CliArgs = cliArgs
                        SilentCompilation = true
                    },
                    cliArgs
                | _ -> state, cliArgs

            let! fableCompiler =
                match fableCompiler with
                | None -> FableCompiler.Init(projCracked)
                | Some fableCompiler -> async.Return fableCompiler

            let! fsharpLogs, fableResults =
                fableCompiler.StartCompilation(
                    projCracked.SourceFiles, // Make sure to pass the up-to-date source files (with cleared hashes for changed files)
                    filesToCompile,
                    state.GetPathResolver(?precompiledInfo = projCracked.PrecompiledInfo),
                    state.SilentCompilation,
                    fun f -> state.TriggeredByDependency(f, changes)
                )

            let logs, watchDependencies =
                ((fsharpLogs, state.WatchDependencies), fableResults)
                ||> List.fold (fun (logs, deps) ->
                    function
                    | Ok res ->
                        let logs = Array.append logs res.Logs
                        let deps = Map.add res.File res.WatchDependencies deps
                        logs, deps
                    | Error e ->
                        let log =
                            match e.Exception with
                            | Fable.FableError msg -> LogEntry.MakeError(msg, fileName = e.File)
                            | ex ->
                                let msg = ex.Message + Log.newLine + ex.StackTrace

                                LogEntry.MakeError(msg, fileName = e.File, tag = "EXCEPTION")

                        Array.append logs [| log |], deps
                )

            let state =
                { state with
                    PendingFiles = [||]
                    WatchDependencies = watchDependencies
                    SilentCompilation = false
                }

            let filesToCompile = set filesToCompile

            let errorLogs, otherLogs =
                // Sometimes errors are duplicated
                logs
                |> Array.distinct
                // Ignore warnings from packages in `fable_modules` folder
                |> Array.filter (fun log ->
                    match log.Severity with
                    // We deal with errors later
                    | Severity.Error -> true
                    | Severity.Info
                    | Severity.Warning ->
                        match log.FileName with
                        | Some filename when
                            Naming.isInFableModules (filename) || not (filesToCompile.Contains(filename))
                            ->
                            false
                        | _ -> true
                )
                // We can't forward TreatWarningsAsErrors to FCS because it would generate errors
                // in packages code (Fable recreate a single project with all packages source files)
                // For this reason, we need to handle the conversion ourselves here
                // It applies to all logs (Fable, F#, etc.)
                |> fun logs ->
                    if projCracked.TreatmentWarningsAsErrors then
                        logs
                        |> Array.map (fun log ->
                            match log.Severity with
                            | Severity.Warning -> { log with Severity = Severity.Error }
                            | _ -> log
                        )
                    else
                        logs
                |> Array.partition (fun log -> log.Severity = Severity.Error)

            otherLogs
            |> Array.iter (fun log ->
                match log.Severity with
                | Severity.Error -> () // In theory, we shouldn't have errors here
                | Severity.Info -> formatLog cliArgs.RootDir log |> Log.always
                | Severity.Warning -> formatLog cliArgs.RootDir log |> Log.warning
            )

            errorLogs |> Array.iter (formatLog cliArgs.RootDir >> Log.error)
            let hasError = Array.isEmpty errorLogs |> not

            // Generate assembly and serialize info if precompile is selected
            let! exitCode =
                async {
                    match hasError, cliArgs.Precompile with
                    | false, true ->
                        let outPathsAndInlineExprs =
                            (Some(Map.empty, []), fableResults)
                            ||> List.fold (fun acc res ->
                                match acc, res with
                                | Some(outPaths, inlineExprs), Ok res ->
                                    Some(Map.add res.File res.OutPath outPaths, res.InlineExprs :: inlineExprs)
                                | _ -> None
                            )

                        match outPathsAndInlineExprs with
                        | None -> return 1
                        | Some(outPaths, inlineExprs) ->
                            // Assembly generation is single threaded but I couldn't make it work in parallel with serialization
                            // (if I use Async.StartChild, assembly generation doesn't seem to start until serialization is finished)
                            let dllPath = PrecompiledInfoImpl.GetDllPath(projCracked.FableModulesDir)

                            Log.always ("Generating assembly...")

                            let! (diagnostics, result), ms =
                                Performance.measureAsync <| fun _ -> fableCompiler.CompileToFile(dllPath)

                            Log.always ($"Assembly generated in {ms}ms")

                            match result with
                            | Some ex ->
                                getFSharpDiagnostics diagnostics |> logErrors cliArgs.RootDir
                                Log.error (ex.Message)
                                return 1
                            | None ->
                                Log.always ($"Saving precompiled info...")
                                let! fableProj = fableCompiler.GetFableProject()

                                let _, ms =
                                    Performance.measure
                                    <| fun _ ->
                                        let inlineExprs = inlineExprs |> List.rev |> Array.concat

                                        let files =
                                            fableProj.ImplementationFiles
                                            |> Map.map (fun k v ->
                                                match Map.tryFind k outPaths with
                                                | Some outPath ->
                                                    {
                                                        RootModule = v.RootModule
                                                        OutPath = outPath
                                                    }
                                                | None ->
                                                    Fable.FableError($"Cannot find out path for precompiled file {k}")
                                                    |> raise
                                            )

                                        PrecompiledInfoImpl.Save(
                                            files = files,
                                            inlineExprs = inlineExprs,
                                            compilerOptions = cliArgs.CompilerOptions,
                                            fableModulesDir = projCracked.FableModulesDir,
                                            fableLibDir = projCracked.FableLibDir
                                        )

                                Log.always ($"Precompiled info saved in {ms}ms")
                                return 0
                    | _ -> return 0
                }

            // Run process
            let exitCode, state =
                if hasError then
                    1
                else
                    exitCode
                |> checkRunProcess state projCracked

            let state =
                { state with
                    ProjectCrackedAndFableCompiler = Some(projCracked, fableCompiler)
                    PendingFiles =
                        if state.PendingFiles.Length = 0 then
                            errorLogs |> Array.choose (fun l -> l.FileName) |> Array.distinct
                        else
                            state.PendingFiles
                }

            return
                {|
                    State = state
                    ErrorLogs = errorLogs
                    OtherLogs = otherLogs
                    ExitCode = exitCode
                |}
    }

type FileWatcherMsg = | Changes of timeStamp: DateTime * changes: ISet<string>

let startCompilationAsync state =
    async {
        try
            let state =
                match state.CliArgs.RunProcess with
                | Some runProc when runProc.IsFast -> { state with CliArgs = runProcessAndForget state.CliArgs runProc }
                | _ -> state

            // Initialize changes with an empty set
            let changes = HashSet() :> ISet<_>

            let! compilationResult =
                match state.Watcher with
                | None -> compilationCycle state changes
                | Some watcher ->
                    let agent =
                        MailboxProcessor<FileWatcherMsg>.Start(fun agent ->
                            let rec loop state =
                                async {
                                    match! agent.Receive() with
                                    | Changes(timestamp, changes) ->
                                        match state.Watcher with
                                        // Discard changes that may have happened before we restarted the watcher
                                        | Some w when w.StartedAt < timestamp ->
                                            // TODO: Get all messages until QueueLength is 0 before starting the compilation cycle?
                                            if changes.Count > 0 then
                                                Log.verbose (
                                                    lazy
                                                        $"""Changes:{Log.newLine}    {changes |> String.concat $"{Log.newLine}    "}"""
                                                )

                                            let! compilationResult = compilationCycle state changes

                                            Log.always $"Watching {File.relPathToCurDir w.Watcher.BasePath}"

                                            return! loop compilationResult.State
                                        | _ -> return! loop state
                                }

                            let onChange changes =
                                Changes(DateTime.UtcNow, changes) |> agent.Post

                            loop { state with Watcher = Some { watcher with OnChange = onChange } }
                        )

                    // The watcher will remain active so we don't really need the reply channel, but leave loop on fatal errors
                    agent.PostAndAsyncReply(fun _ -> Changes(DateTime.UtcNow, changes)) |> ignore

                    Async.FromContinuations(fun (_onSuccess, onError, _onCancel) -> agent.Error.Add(onError))

            // TODO: We should propably keep them separated and even use a DUs to represents
            // logs using one case per level
            // type LogEntry =
            //      | Error of LogData
            //      | Warning of LogData
            // but for now we are just rebuilding a single array because I don't know how to
            // adapt the integrations tests suits
            // Perhaps, we should make the integration tests code more simple
            let logs = Array.append compilationResult.ErrorLogs compilationResult.OtherLogs

            match compilationResult.ExitCode with
            | 0 -> return Ok(state, logs)
            | _ -> return Error("Compilation failed", logs)

        with
        | Fable.FableError e -> return Error(e, [||])
        | exn -> return raise exn
    }
