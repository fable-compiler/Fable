namespace Fable.Cli

open System
open System.Threading

type RunProcess(exeFile: string, args: string list, ?watch: bool, ?fast: bool) =
    member _.ExeFile = exeFile
    member _.Args = args
    member _.IsWatch = defaultArg watch false
    member _.IsFast = defaultArg fast false

type CliArgs =
    { ProjectFile: string
      RootDir: string
      OutDir: string option
      FableLibraryPath: string option
      Configuration: string
      NoRestore: bool
      NoCache: bool
      SourceMaps: bool
      SourceMapsRoot: string option
      Exclude: string option
      Replace: Map<string, string>
      RunProcess: RunProcess option
      CompilerOptions: Fable.CompilerOptions }
    member this.ProjectFileAsRelativePath = IO.Path.GetRelativePath(this.RootDir, this.ProjectFile)

type private TypeInThisAssembly = class end

type Agent<'T> private (mbox: MailboxProcessor<'T>, cts: CancellationTokenSource) =
  static member Start(f: 'T -> unit) =
    let cts = new CancellationTokenSource()
    new Agent<'T>(MailboxProcessor<'T>.Start((fun mb ->
        let rec loop () = async {
            let! msg = mb.Receive()
            f msg
            return! loop()
        }
        loop()), cancellationToken = cts.Token), cts)

  member _.Post msg = mbox.Post msg

  interface IDisposable with
    member _.Dispose() =
      (mbox :> IDisposable).Dispose()
      cts.Cancel()

[<RequireQualifiedAccess>]
module Log =
    let mutable private verbosity = Fable.Verbosity.Normal

    /// To be called only at the beginning of the app
    let makeVerbose() =
        verbosity <- Fable.Verbosity.Verbose

    let alwaysWithColor color (msg: string) =
        if verbosity <> Fable.Verbosity.Silent && not(String.IsNullOrEmpty(msg)) then
            Console.ForegroundColor <- color
            Console.Out.WriteLine(msg)
            Console.ResetColor()

    let always (msg: string) =
        if verbosity <> Fable.Verbosity.Silent && not(String.IsNullOrEmpty(msg)) then
            Console.Out.WriteLine(msg)

    let verbose (msg: Lazy<string>) =
        if verbosity = Fable.Verbosity.Verbose then
            always msg.Value

    let verboseOrIf condition (msg: string) =
        if condition || verbosity = Fable.Verbosity.Verbose then
            always msg

    let warning (msg: string) =
        Console.ForegroundColor <- ConsoleColor.DarkYellow
        Console.Out.WriteLine(msg)
        Console.ResetColor()

    let error (msg: string) =
        Console.ForegroundColor <- ConsoleColor.DarkRed
        Console.Error.WriteLine(msg)
        Console.ResetColor()

    let mutable private femtoMsgShown = false

    let showFemtoMsg (show: unit -> bool): unit =
        if not femtoMsgShown then
            if show() then
                femtoMsgShown <- true
                "Some Nuget packages contain information about NPM dependencies that can be managed by Femto: https://github.com/Zaid-Ajaj/Femto"
                |> alwaysWithColor ConsoleColor.Blue

module File =
    open System.IO

    /// File.ReadAllText fails with locked files. See https://stackoverflow.com/a/1389172
    let readAllTextNonBlocking (path: string) =
        if File.Exists(path) then
            use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use textReader = new StreamReader(fileStream)
            textReader.ReadToEnd()
        else
            Log.always("File does not exist: " + path)
            ""

    let readAllTextNonBlockingAsync (path: string) = async {
        if File.Exists(path) then
            use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use textReader = new StreamReader(fileStream)
            let! text = textReader.ReadToEndAsync() |> Async.AwaitTask
            return text
        else
            Log.always("File does not exist: " + path)
            return ""
    }

    let rec tryFindPackageJsonDir dir =
        if File.Exists(Path.Combine(dir, "package.json")) then Some dir
        else
            let parent = Directory.GetParent(dir)
            if isNull parent then None
            else tryFindPackageJsonDir parent.FullName

    let tryNodeModulesBin workingDir exeFile =
        tryFindPackageJsonDir workingDir
        |> Option.bind (fun pkgJsonDir ->
            let nodeModulesBin = Path.Join(pkgJsonDir, "node_modules", ".bin", exeFile)
            if File.Exists(nodeModulesBin) then Path.GetRelativePath(workingDir, nodeModulesBin) |> Some
            else None)

    /// System.IO.GetFullPath doesn't change the case of the argument in case insensitive file systems
    /// even if it doesn't match the actual path, causing unexpected issues when comparing files later.
    // From https://stackoverflow.com/a/326153
    // See https://github.com/fable-compiler/Fable/issues/2277#issuecomment-737748220
    // and https://github.com/fable-compiler/Fable/issues/2293#issuecomment-738134611
    let getExactFullPath (pathName: string) =
        let rec getExactPath (pathName: string) =
            if not(File.Exists pathName || Directory.Exists pathName) then pathName
            else
                let di = DirectoryInfo(pathName)
                if not(isNull di.Parent) then
                    Path.Combine(
                        getExactPath di.Parent.FullName,
                        di.Parent.GetFileSystemInfos(di.Name).[0].Name
                    )
                else
                    di.Name.ToUpper()
        Path.GetFullPath(pathName) |> getExactPath

    /// FAKE and other tools clean dirs but don't remove them, so check whether it doesn't exist or it's empty
    let isDirectoryEmpty dir =
        not(IO.Directory.Exists(dir)) || IO.Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty

[<RequireQualifiedAccess>]
module Process =
    open System.Runtime
    open System.Diagnostics

    let isWindows() =
        InteropServices.RuntimeInformation.IsOSPlatform(InteropServices.OSPlatform.Windows)

    // Adapted from https://stackoverflow.com/a/22210859
    let tryFindInPath (exec: string) =
        let isWindows = isWindows()
        let exec = if isWindows then exec + ".exe" else exec
        Environment.GetEnvironmentVariable("PATH")
            .Split(if isWindows then ';' else ':')
        |> Array.tryPick (fun dir ->
            let execPath = IO.Path.Combine(dir, exec)
            if IO.File.Exists execPath then Some execPath else None)

    let findInPath (exec: string) =
        match tryFindInPath exec with
        | Some exec -> exec
        | None -> failwith $"Cannot find {exec} in PATH"

    let getCurrentAssembly() =
        typeof<TypeInThisAssembly>.Assembly

    let addToPath (dir: string) =
        let currentPath = Environment.GetEnvironmentVariable("PATH")
        IO.Path.GetFullPath(dir) + (if isWindows() then ";" else ":") + currentPath

    // Adapted from https://github.com/enricosada/dotnet-proj-info/blob/1e6d0521f7f333df7eff3148465f7df6191e0201/src/dotnet-proj/Program.fs#L155
    let private startProcess (envVars: (string * string) list) workingDir exePath (args: string list) =
        let exePath, args =
            if isWindows() then "cmd", "/C"::exePath::args
            else exePath, args

        // TODO: We should use cliArgs.RootDir instead of Directory.GetCurrentDirectory here but it's only informative
        // so let's leave it as is for now to avoid having to pass the cliArgs through all the call sites
        Log.always $"""{IO.Path.GetRelativePath(IO.Directory.GetCurrentDirectory(), workingDir)}> {exePath} {String.concat " " args}"""

        let psi = ProcessStartInfo(exePath)
        for arg in args do
            psi.ArgumentList.Add(arg)
        for (key, value) in envVars do
            psi.EnvironmentVariables.[key] <- value
        psi.WorkingDirectory <- workingDir
        psi.CreateNoWindow <- false
        psi.UseShellExecute <- false

        Process.Start(psi)

    let kill(p: Process) =
        p.Refresh()
        if not p.HasExited then
            p.Kill(entireProcessTree=true)

    let startWithEnv envVars =
        let mutable runningProcess = None

        // In Windows, terminating the main process doesn't kill the spawned ones so we need
        // to listen for the Console.CancelKeyPress and AssemblyLoadContext.Unloading events
        if isWindows() then
            Console.CancelKeyPress.AddHandler(ConsoleCancelEventHandler(fun _ _ ->
                runningProcess |> Option.iter kill))
            let assemblyLoadContext =
                getCurrentAssembly()
                |> Loader.AssemblyLoadContext.GetLoadContext
            assemblyLoadContext.add_Unloading(fun _ ->
                runningProcess |> Option.iter kill)

        fun (workingDir: string) (exePath: string) (args: string list) ->
            try
                runningProcess |> Option.iter kill
                let p = startProcess envVars workingDir exePath args
                runningProcess <- Some p
            with ex ->
                Log.always("Cannot run: " + ex.Message)

    let start (workingDir: string) (exePath: string) (args: string list) =
        startWithEnv [] workingDir exePath args

    let runSyncWithEnv envVars (workingDir: string) (exePath: string) (args: string list) =
        try
            let p = startProcess envVars workingDir exePath args
            p.WaitForExit()
            p.ExitCode
        with ex ->
            Log.always("Cannot run: " + ex.Message)
            Log.always(ex.StackTrace)
            -1

    let runSync (workingDir: string) (exePath: string) (args: string list) =
        runSyncWithEnv [] workingDir exePath args

[<RequireQualifiedAccess>]
module Async =
    let fold f (state: 'State) (xs: 'T seq) = async {
        let mutable state = state
        for x in xs do
            let! result = f state x
            state <- result
        return state
    }

    let map f x = async {
        let! x = x
        return f x
    }

    let tryPick (f: 'T->Async<'Result option>) xs: Async<'Result option> = async {
        let mutable result: 'Result option = None
        for x in xs do
            match result with
            | Some _ -> ()
            | None ->
                let! r = f x
                result <- r
        return result
    }

    let orElse (f: unit->Async<'T>) (x: Async<'T option>): Async<'T> = async {
        let! x = x
        match x with
        | Some x -> return x
        | None -> return! f ()
    }

    let AwaitObservable (obs: IObservable<'T>) =
        Async.FromContinuations(fun (onSuccess, _onError, _onCancel) ->
            let mutable disp = Unchecked.defaultof<IDisposable>
            disp <- obs.Subscribe(fun v ->
                disp.Dispose()
                onSuccess(v)))

    let ignore (_: 'a) = async {
        return ()
    }

module Imports =
    open System.Text.RegularExpressions
    open Fable

    let trimPath (path: string) = path.Replace("../", "").Replace("./", "").Replace(":", "")
    let isRelativePath (path: string) = path.StartsWith("./") || path.StartsWith("../")
    let isAbsolutePath (path: string) = path.StartsWith('/') || path.IndexOf(':') = 1

    let getRelativePath (path: string) (pathTo: string) =
        let relPath = IO.Path.GetRelativePath(path, pathTo).Replace('\\', '/')
        if isRelativePath relPath then relPath else "./" + relPath

    let getTargetAbsolutePath getOrAddDeduplicateTargetDir importPath projDir outDir =
        let importPath = Path.normalizePath importPath
        let outDir = Path.normalizePath outDir
        // It may happen the importPath is already in outDir,
        // for example package sources in fable_modules folder
        if importPath.StartsWith(outDir) then importPath
        else
            let importDir = Path.GetDirectoryName(importPath)
            let targetDir = getOrAddDeduplicateTargetDir importDir (fun (currentTargetDirs: Set<string>) ->
                let relDir = getRelativePath projDir importDir |> trimPath
                Path.Combine(outDir, relDir)
                |> Naming.preventConflicts currentTargetDirs.Contains)
            let importFile = Path.GetFileName(importPath)
            Path.Combine(targetDir, importFile)

    let getTargetRelativePath getOrAddDeduplicateTargetDir (importPath: string) targetDir projDir (outDir: string) =
        let absPath = getTargetAbsolutePath getOrAddDeduplicateTargetDir importPath projDir outDir
        let relPath = getRelativePath targetDir absPath
        if isRelativePath relPath then relPath else "./" + relPath

    let getImportPath getOrAddDeduplicateTargetDir sourcePath targetPath projDir outDir (importPath: string) =
        let macro, importPath =
            let m = Regex.Match(importPath, @"^\${(\w+)}[\/\\]?")
            if m.Success then Some m.Groups.[1].Value, importPath.[m.Length..]
            else None, importPath
        match macro, outDir with
        | Some "outPath", _ -> "./" + importPath
        // Not entirely correct but not sure what to do with outDir macro if there's no outDir
        | Some "outDir", None -> "./" + importPath
        | Some "outDir", Some outDir ->
            let importPath = Path.Combine(outDir, importPath)
            let targetDir = Path.GetDirectoryName(targetPath)
            getRelativePath targetDir importPath
        | Some "entryDir", _ ->
            let importPath = Path.Combine(projDir, importPath)
            let targetDir = Path.GetDirectoryName(targetPath)
            getRelativePath targetDir importPath
        | Some macro, _ ->
            failwith $"Unknown import macro: {macro}"
        | None, None -> importPath
        | None, Some outDir ->
            let sourceDir = Path.GetDirectoryName(sourcePath)
            let targetDir = Path.GetDirectoryName(targetPath)
            let importPath =
                if isRelativePath importPath
                then Path.Combine(sourceDir, importPath) |> Path.normalizeFullPath
                else importPath
            if isAbsolutePath importPath then
                if importPath.EndsWith(".fs")
                then getTargetRelativePath getOrAddDeduplicateTargetDir importPath targetDir projDir outDir
                else getRelativePath targetDir importPath
            else importPath

module Observable =
    type SingleObservable<'T>(dispose: unit -> unit) =
        let mutable listener: IObserver<'T> option = None
        member _.Trigger v =
            match listener with
            | Some lis -> lis.OnNext v
            | None -> ()
        interface IObservable<'T> with
            member _.Subscribe w =
                listener <- Some w
                { new IDisposable with
                    member _.Dispose() = dispose() }

    let throttle (ms: int) (obs: IObservable<'T>) =
        { new IObservable<'T[]> with
            member _.Subscribe w =
                let events = ResizeArray()
                let timer = new Timers.Timer(float ms, AutoReset=false)
                timer.Elapsed.Add(fun _ ->
                    let evs = events.ToArray()
                    events.Clear()
                    w.OnNext(evs))
                let disp = obs.Subscribe(fun v ->
                    events.Add(v)
                    timer.Stop()
                    timer.Start())
                { new IDisposable with
                    member _.Dispose() =
                        timer.Dispose()
                        disp.Dispose() } }

[<AutoOpen>]
module ResultCE =
    type ResultBuilder() =
        member _.Zero = Ok()
        member _.Bind(v,f) = Result.bind f v
        member _.Return v = Ok v
        member _.ReturnFrom v = v

    let result = ResultBuilder()
