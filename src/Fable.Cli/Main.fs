module Fable.Cli.Main

open System
open FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

module private Util =
    let loadType (r: PluginRef): Type =
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
                sprintf "Loaded %s from %s" r.TypeFullName (File.getRelativePathFromCwd r.DllPath)
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

    let hasWatchDependency (path: string) (dirtyFiles: Set<string>) watchDependencies =
        match Map.tryFind path watchDependencies with
        | None -> false
        | Some watchDependencies ->
            watchDependencies |> Array.exists (fun p -> Set.contains p dirtyFiles)

    let changeFsExtension isInFableHiddenDir filePath fileExt =
        let fileExt =
            // Prevent conflicts in package sources as they may include
            // JS files with same name as the F# .fs file
            if fileExt = ".js" && isInFableHiddenDir then
                CompilerOptionsHelper.DefaultExtension
            else fileExt
        Path.replaceExtension fileExt filePath

    let getOutJsPath (cliArgs: CliArgs) file =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let isInFableHiddenDir = Naming.isInFableHiddenDir file
        match cliArgs.OutDir with
        | Some outDir ->
            let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile
            let absPath = Imports.getTargetAbsolutePath file projDir outDir
            changeFsExtension isInFableHiddenDir absPath fileExt
        | None ->
            changeFsExtension isInFableHiddenDir file fileExt

    type FileWriter(sourcePath: string, targetPath: string, projDir: string, outDir: string option, fileExt: string) =
        // In imports *.ts extensions have to be converted to *.js extensions instead
        let fileExt = if fileExt.EndsWith(".ts") then Path.replaceExtension ".js" fileExt else fileExt
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        interface BabelPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeJsStringLiteral(str) =
                Web.HttpUtility.JavaScriptStringEncode(str)
            member _.MakeImportPath(path) =
                let path = Imports.getImportPath sourcePath targetPath projDir outDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                    changeFsExtension isInFableHiddenDir path fileExt
                else path
            member _.Dispose() = stream.Dispose()

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

            let outPath = getOutJsPath cliArgs com.CurrentFile

            // ensure directory exists
            let dir = IO.Path.GetDirectoryName outPath
            if not (IO.Directory.Exists dir) then IO.Directory.CreateDirectory dir |> ignore

            // write output to file
            let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
            let fileExt = cliArgs.CompilerOptions.FileExtension
            let writer = new FileWriter(com.CurrentFile, outPath, projDir, cliArgs.OutDir, fileExt)
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

type Watcher() =
    static member AwaitChanges(filesToWatch: string list) =
        let commonBaseDir =
            filesToWatch
            |> List.map (IO.Path.GetDirectoryName)
            |> List.distinct
            |> List.sortBy (fun f -> f.Length)
            |> function
                | [] -> failwith "Empty list passed to watcher"
                | [dir] -> dir
                | dir::restDirs ->
                    let rec getCommonDir (dir: string) =
                        if restDirs |> List.forall (fun d -> d.StartsWith(dir)) then dir
                        else
                            match IO.Path.GetDirectoryName(dir) with
                            | null -> failwith "No common base dir"
                            | dir -> getCommonDir dir
                    getCommonDir dir

        let changes = ResizeArray()
        let timer = new Timers.Timer(200., AutoReset=false)
        Log.always("Watching " + File.getRelativePathFromCwd commonBaseDir)
        let watcher = new IO.FileSystemWatcher(EnableRaisingEvents=false)
        watcher.Filters.Add("*.fs")
        watcher.Filters.Add("*.fsx")
        watcher.Filters.Add("*.fsproj")
        watcher.Path <- commonBaseDir
        watcher.IncludeSubdirectories <- true
        // watcher.NotifyFilter <- NotifyFilters.LastWrite

        let filePaths = set filesToWatch
        let onChange _changeType fullPath =
            let fullPath = Path.normalizePath fullPath
            if Set.contains fullPath filePaths then
                changes.Add(fullPath)
                if timer.Enabled then
                    timer.Stop()
                timer.Start()

        watcher.Changed.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Created.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Deleted.Add(fun ev -> onChange ev.ChangeType ev.FullPath)
        watcher.Renamed.Add(fun ev -> onChange ev.ChangeType ev.FullPath)

        Async.FromContinuations(fun (onSuccess, _onError, _onCancel) ->
            timer.Elapsed.Add(fun _ ->
                watcher.EnableRaisingEvents <- false
                watcher.Dispose()
                timer.Dispose()

                changes
                |> set
                |> onSuccess)
            watcher.EnableRaisingEvents <- true
        )

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
                    fableCompilerOptions: CompilerOptions,
                    crackerResponse: CrackerResponse) =

    member _.FableOptions = fableCompilerOptions
    member _.ProjectOptions = crackerResponse.ProjectOptions
    member _.Packages = crackerResponse.Packages
    member _.SourceFiles = sourceFiles

    member _.MakeCompiler(currentFile, project) =
        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir
        CompilerImpl(currentFile, project, fableCompilerOptions, fableLibDir)

    member _.MapSourceFiles(f) =
        ProjectCracked(Array.map f sourceFiles, fableCompilerOptions, crackerResponse)

    static member Init(cliArgs: CliArgs) =
        let res =
            CrackerOptions(fableOpts = cliArgs.CompilerOptions,
                           fableLib = cliArgs.FableLibraryPath,
                           outDir = cliArgs.OutDir,
                           exclude = cliArgs.Exclude,
                           replace = cliArgs.Replace,
                           forcePkgs = cliArgs.ForcePkgs,
                           noRestore = cliArgs.NoRestore,
                           projFile = cliArgs.ProjectFile)
            |> getFullProjectOpts

        Log.verbose(lazy
            let proj = File.getRelativePathFromCwd cliArgs.ProjectFile
            let opts = res.ProjectOptions.OtherOptions |> String.concat "\n   "
            sprintf "F# PROJECT: %s\n   %s" proj opts)

        let sourceFiles = getSourceFiles res.ProjectOptions |> Array.map File
        ProjectCracked(sourceFiles, cliArgs.CompilerOptions, res)

type ProjectParsed(project: Project, checker: InteractiveChecker) =

    static let checkProject (config: ProjectCracked) (checker: InteractiveChecker) =
        Log.always("Compiling " + File.getRelativePathFromCwd config.ProjectOptions.ProjectFileName + "...")
        let result, ms = measureTime <| fun () ->
            let fileDic = config.SourceFiles |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
            let sourceReader f = fileDic.[f].ReadSource()
            let filePaths = config.SourceFiles |> Array.map (fun file -> file.NormalizedFullPath)
            checker.ParseAndCheckProject(config.ProjectOptions.ProjectFileName, filePaths, sourceReader)
        Log.always(sprintf "F# compilation finished in %ims" ms)
        result

    member _.Project = project
    member _.Checker = checker

    static member Init(config: ProjectCracked, ?checker) =
        let checker =
            match checker with
            | Some checker -> checker
            | None ->
                Log.always("Initializing F# compiler...")
                InteractiveChecker.Create(config.ProjectOptions)

        let checkResults = checkProject config checker
        let proj = Project(checkResults,
                           getPlugin=loadType,
                           optimizeFSharpAst=config.FableOptions.OptimizeFSharpAst)
        ProjectParsed(proj, checker)

    member this.Update(config: ProjectCracked) =
        let checkResults = checkProject config this.Checker
        let proj = this.Project.Update(checkResults)
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
        TestInfo(this.CurrentIteration + 1, this.TotalIterations, spentMs::times)

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndParsed: (ProjectCracked * ProjectParsed) option
      WatchDependencies: Map<string, string[]>
      ErroredFiles: Set<string>
      TestInfo: TestInfo option }

let rec startCompilation (changes: Set<string>) (state: State) = async {
    let state =
        match state.CliArgs.RunProcess with
        | Some runProc when runProc.IsFast ->
            let workingDir = state.CliArgs.RootDir
            let exeFile =
                File.tryNodeModulesBin workingDir runProc.ExeFile
                |> Option.defaultValue runProc.ExeFile
            Process.start workingDir exeFile runProc.Args
            { state with CliArgs = { state.CliArgs with RunProcess = None } }
        | _ -> state

    // TODO: Use Result here to fail more gracefully if FCS crashes
    let cracked, parsed, filesToCompile =
        match state.ProjectCrackedAndParsed with
        | Some(cracked, parsed) ->
            let init, cracked =
                if changes.Contains(state.CliArgs.ProjectFile)
                    // For performance reasons, don't crack .fsx scripts for every change
                    && not(state.CliArgs.ProjectFile.EndsWith(".fsx")) then
                    true, ProjectCracked.Init(state.CliArgs)
                else false, cracked

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
                    let parsed =
                        if init then ProjectParsed.Init(cracked, parsed.Checker)
                        else parsed.Update(cracked)
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
            let parsed = ProjectParsed.Init(cracked)
            let filesToCompile =
                cracked.SourceFiles
                |> Array.map (fun f -> f.NormalizedFullPath)
                |> fun files ->
                    if not state.CliArgs.CompilerOptions.DebugMode then files
                    else
                        // Skip files that have a more recent JS version
                        let skipped =
                            files |> Array.skipWhile (fun file ->
                                try
                                    let jsFile = getOutJsPath state.CliArgs file
                                    IO.File.Exists(jsFile) && IO.File.GetLastWriteTime(jsFile) > IO.File.GetLastWriteTime(file)
                                with _ -> false)
                        if skipped.Length < files.Length then
                            Log.always("Skipping Fable compilation of up-to-date JS files")
                        skipped
            cracked, parsed, filesToCompile

    let filesToCompile =
        filesToCompile
        |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
        |> Array.append (Set.toArray state.ErroredFiles)
        |> Array.distinct

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
    let logs = Array.distinct logs // Sometimes errors are duplicated

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

    let errorMsg =
        if Set.isEmpty newErrors then None
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
                    let lastFile = Array.last cracked.SourceFiles
                    let lastFilePath = getOutJsPath state.CliArgs lastFile.NormalizedFullPath
                    // Fable's getRelativePath version ensures there's always a period in front of the path: ./
                    let lastFilePath = Path.getRelativeFileOrDirPath true workingDir false lastFilePath
                    // Pass also the file name as argument, as when calling the script directly
                    "node", ["--eval"; "\"require('esm')(module)('" + lastFilePath + "')\""; lastFilePath] @ runProc.Args
                | exeFile ->
                    File.tryNodeModulesBin workingDir exeFile
                    |> Option.defaultValue exeFile, runProc.Args

            if state.CliArgs.WatchMode then
                Process.start workingDir exeFile args
                let runProc = if runProc.IsWatch then Some runProc else None
                None, { state with CliArgs = { state.CliArgs with RunProcess = runProc } }
            else
                let exitCode = Process.runSync workingDir exeFile args
                (if exitCode = 0 then None else Some "Run process failed"), state

    match state.CliArgs.WatchMode, state.TestInfo with
    | true, _ ->
        let oldErrors =
            state.ErroredFiles
            |> Set.filter (fun file -> not(Array.contains file filesToCompile))

        let! changes = Watcher.AwaitChanges [
            cracked.ProjectOptions.ProjectFileName
            yield! cracked.SourceFiles
                |> Array.choose (fun f ->
                    let path = f.NormalizedFullPath
                    if Naming.isInFableHiddenDir(path) then None
                    else Some path)
        ]
        return!
            { state with ProjectCrackedAndParsed = Some(cracked, parsed)
                         WatchDependencies = watchDependencies
                         ErroredFiles = Set.union oldErrors newErrors }
            |> startCompilation changes

    | false, None ->
        return match errorMsg with Some e -> Error e | None -> Ok()

    | false, Some info ->
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
            IO.File.AppendAllText(perfLog, log + "\n")
            return Ok()
}
