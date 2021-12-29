namespace Fable.Cli

#nowarn "3391"

open System
open System.Threading

exception FableError of string

type RunProcess(exeFile: string, args: string list, ?watch: bool, ?fast: bool) =
    member _.ExeFile = exeFile
    member _.Args = args
    member _.IsWatch = defaultArg watch false
    member _.IsFast = defaultArg fast false

type CliArgs =
    { ProjectFile: string
      RootDir: string
      OutDir: string option
      IsWatch: bool
      Precompile: bool
      PrecompiledLib: string option
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
    let newLine = Environment.NewLine
    let isCi = String.IsNullOrEmpty(Environment.GetEnvironmentVariable("CI")) |> not

    let mutable private verbosity = Fable.Verbosity.Normal

    /// To be called only at the beginning of the app
    let makeVerbose() =
        verbosity <- Fable.Verbosity.Verbose

    let isVerbose() =
        verbosity = Fable.Verbosity.Verbose

    let inSameLineIfNotCI (msg: string) =
        if not isCi then
            let curCursorLeft = Console.CursorLeft
            Console.SetCursorPosition(0, Console.CursorTop)
            Console.Out.Write(msg)
            let diff = curCursorLeft - msg.Length
            if diff > 0 then
                Console.Out.Write(String.replicate diff " ")
                Console.SetCursorPosition(msg.Length, Console.CursorTop)

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

    let changeFsExtension isInFableHiddenDir filePath fileExt =
        let fileExt =
            // Prevent conflicts in package sources as they may include
            // native files with same name as the F# .fs file
            if fileExt <> Fable.CompilerOptionsHelper.DefaultExtension
                && isInFableHiddenDir then
                    Fable.CompilerOptionsHelper.DefaultExtension
            else fileExt
        Fable.Path.replaceExtension fileExt filePath

    let existsAndIsOlderThan (dt: DateTime) (targetPath: string) =
        try
            File.Exists(targetPath) && dt > File.GetLastWriteTime(targetPath)
        with _ -> false

    let existsAndIsNewerThanSource (sourcePath: string) (targetPath: string) =
        try
            File.Exists(targetPath) && File.GetLastWriteTime(sourcePath) < File.GetLastWriteTime(targetPath)
        with _ -> false

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
        not(Directory.Exists(dir)) || Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty

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

type PathResolver =
    abstract TryPrecompiledOutPath: sourceDir: string * relativePath: string -> string option
    abstract GetOrAddDeduplicateTargetDir: importDir: string * addTargetDir: (Set<string> -> string) -> string

module Imports =
    open System.Text.RegularExpressions
    open Fable

    let trimPath (path: string) = path.Replace("../", "").Replace("./", "").Replace(":", "")
    let isRelativePath (path: string) = path.StartsWith("./") || path.StartsWith("../")
    let isAbsolutePath (path: string) = path.StartsWith('/') || path.IndexOf(':') = 1

    let getRelativePath (path: string) (pathTo: string) =
        let relPath = IO.Path.GetRelativePath(path, pathTo).Replace('\\', '/')
        if isRelativePath relPath then relPath else "./" + relPath

    let getTargetAbsolutePath (pathResolver: PathResolver) importPath projDir outDir =
        let importPath = Path.normalizePath importPath
        let outDir = Path.normalizePath outDir
        // It may happen the importPath is already in outDir,
        // for example package sources in fable_modules folder
        if importPath.StartsWith(outDir) then importPath
        else
            let importDir = Path.GetDirectoryName(importPath)
            let targetDir = pathResolver.GetOrAddDeduplicateTargetDir(importDir, fun currentTargetDirs ->
                let relDir = getRelativePath projDir importDir |> trimPath
                Path.Combine(outDir, relDir)
                |> Naming.preventConflicts currentTargetDirs.Contains)
            let importFile = Path.GetFileName(importPath)
            Path.Combine(targetDir, importFile)

    let getTargetRelativePath pathResolver (importPath: string) targetDir projDir (outDir: string) =
        let absPath = getTargetAbsolutePath pathResolver importPath projDir outDir
        let relPath = getRelativePath targetDir absPath
        if isRelativePath relPath then relPath else "./" + relPath

    let getImportPath pathResolver sourcePath targetPath projDir outDir (importPath: string) =
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
                then getTargetRelativePath pathResolver importPath targetDir projDir outDir
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

module Json =
    open System.IO
    open System.Text.Json
    open System.Text.Json.Serialization
    open System.Collections.Generic
    open Fable.AST

    // TODO: Check which other parameters are accepted by attributes (arrays?)
    type AttParam =
        | Int of int
        | Float of float
        | Bool of bool
        | String of string
        static member From(values: obj list) =
            (Ok [], values) ||> List.fold (fun res (v: obj) ->
                res |> Result.bind (fun acc ->
                    match v with
                    | :? int as v -> (Int v)::acc |> Ok
                    | :? float as v -> (Float v)::acc |> Ok
                    | :? bool as v -> (Bool v)::acc |> Ok
                    | :? string as v -> (String v)::acc |> Ok
                    | _ -> Error $"Cannot serialize attribute param of type %s{v.GetType().FullName}"
            ))
            |> function
                | Ok values -> List.rev values
                | Error msg -> Log.warning msg; []
        member this.Value =
            match this with
            | Int v -> box v
            | Float v -> box v
            | Bool v -> box v
            | String v -> box v

    type Attribute =
        { Entity: Fable.EntityRef; Params: AttParam list }
        static member FromFableAttribute(value: Fable.Attribute) =
            { Entity = value.Entity; Params = AttParam.From value.ConstructorArgs }
        static member ToFableAttribute(value: Attribute) =
            { new Fable.Attribute with
                  member _.ConstructorArgs = value.Params |> List.map (fun v -> v.Value)
                  member _.Entity = value.Entity }

    // We cannot change MemberInfo to avoid breaking plugins so we use a specific converter
    type MemberInfoConverter() =
        inherit JsonConverter<Fable.MemberInfo>()
        member _.ReadBoolean(reader: byref<Utf8JsonReader>) =
            let b = reader.GetBoolean()
            reader.Read() |> ignore
            b

        override this.Read(reader: byref<Utf8JsonReader>, _typeToConvert, options) =
            reader.Read() |> ignore // Skip start array
            reader.Read() |> ignore // Skip start array for attributes
            let attributes = ResizeArray<Fable.Attribute>()
            while reader.TokenType <> JsonTokenType.EndArray do
                JsonSerializer.Deserialize<Attribute>(&reader, options)
                |> Attribute.ToFableAttribute
                |> attributes.Add
            reader.Read() |> ignore // Skip end array for attributes
            let hasSpread = this.ReadBoolean(&reader)
            let isMangled = this.ReadBoolean(&reader)
            let isPublic = this.ReadBoolean(&reader)
            let isInstance = this.ReadBoolean(&reader)
            let isValue = this.ReadBoolean(&reader)
            let isMutable = this.ReadBoolean(&reader)
            let isGetter = this.ReadBoolean(&reader)
            let isSetter = this.ReadBoolean(&reader)
            let isEnumerator = this.ReadBoolean(&reader)
            reader.Read() |> ignore // Skip end array
            { new Fable.MemberInfo with
                member _.Attributes = attributes
                member _.HasSpread = hasSpread
                member _.IsMangled = isMangled
                member _.IsPublic = isPublic
                member _.IsInstance = isInstance
                member _.IsValue = isValue
                member _.IsMutable = isMutable
                member _.IsGetter = isGetter
                member _.IsSetter = isSetter
                member _.IsEnumerator = isEnumerator }

        override _.Write(writer, value, options) =
            writer.WriteStartArray()
            writer.WriteStartArray()
            value.Attributes
            |> Seq.map Attribute.FromFableAttribute
            |> Seq.iter (fun att -> JsonSerializer.Serialize(writer, att, options))
            writer.WriteEndArray()
            writer.WriteBooleanValue(value.HasSpread)
            writer.WriteBooleanValue(value.IsMangled)
            writer.WriteBooleanValue(value.IsPublic)
            writer.WriteBooleanValue(value.IsInstance)
            writer.WriteBooleanValue(value.IsValue)
            writer.WriteBooleanValue(value.IsMutable)
            writer.WriteBooleanValue(value.IsGetter)
            writer.WriteBooleanValue(value.IsSetter)
            writer.WriteBooleanValue(value.IsEnumerator)
            writer.WriteEndArray()

    type DoubleConverter() =
        inherit JsonConverter<float>()
        override _.Read(reader, _typeToConvert, _options) =
            if reader.TokenType = JsonTokenType.String then
                match reader.GetString() with
                | "+Infinity" -> Double.PositiveInfinity
                | "-Infinity" -> Double.NegativeInfinity
                | _ -> Double.NaN
            else
                reader.GetDouble()

        override _.Write(writer, value, _options) =
            if Double.IsPositiveInfinity(value) then
                writer.WriteStringValue("+Infinity")
            elif Double.IsNegativeInfinity(value) then
                writer.WriteStringValue("-Infinity")
            elif Double.IsNaN(value) then
                writer.WriteStringValue("NaN")
            else
                writer.WriteNumberValue(value)

    type StringPoolReader(pool: string[]) =
        inherit JsonConverter<string>()
        override _.Read(reader, _typeToConvert, _options) =
            let i = reader.GetInt32()
            pool.[i]

        override _.Write(writer, value, _options) =
            failwith "Read only"

    type StringPoolWriter() =
        inherit JsonConverter<string>()
        let pool = Dictionary<string, int>()

        member _.GetPool() =
            pool
            |> Seq.toArray
            |> Array.sortBy (fun kv -> kv.Value)
            |> Array.map (fun kv -> kv.Key)

        override _.Read(reader, _typeToConvert, _options) =
            failwith "Write only"

        override _.Write(writer, value, _options) =
            let i =
                match pool.TryGetValue(value) with
                | true, i -> i
                | false, _ ->
                    let i = pool.Count
                    pool.Add(value, i)
                    i
            writer.WriteNumberValue(i)

    // TODO: When upgrading to net6, check if we still need FSharp.SystemTextJson
    let private getOptions() =
        // The default depth (64) is not enough, using 1024 that hopefully
        // should still prevent StackOverflow exceptions
        let jsonOptions = JsonSerializerOptions(MaxDepth=1024)
        jsonOptions.Converters.Add(DoubleConverter())
        jsonOptions.Converters.Add(MemberInfoConverter())
        // JsonUnionEncoding.InternalTag serializes unions in a more compact way, as Thoth.Json
        jsonOptions.Converters.Add(JsonFSharpConverter(unionEncoding = JsonUnionEncoding.InternalTag))
        jsonOptions

    let read<'T> (path: string) =
        let jsonReadOnlySpan: ReadOnlySpan<byte> = File.ReadAllBytes(path)
        JsonSerializer.Deserialize<'T>(jsonReadOnlySpan, getOptions())

    let write (path: string) (data: 'T): unit =
        use fileStream = new FileStream(path, FileMode.Create)
        use writer = new Utf8JsonWriter(fileStream)
        JsonSerializer.Serialize(writer, data, getOptions())

    let readWithStringPool<'T> (path: string) =
        let strings =
            let ext = Path.GetExtension(path)
            let path = path.[0 .. path.Length - ext.Length - 1] + "_strings.json"
            let jsonReadOnlySpan: ReadOnlySpan<byte> = File.ReadAllBytes(path)
            JsonSerializer.Deserialize<string[]>(jsonReadOnlySpan)
        let options = getOptions()
        options.Converters.Add(StringPoolReader(strings))
        let jsonReadOnlySpan: ReadOnlySpan<byte> = File.ReadAllBytes(path)
        JsonSerializer.Deserialize<'T>(jsonReadOnlySpan, options)

    let writeWithStringPool (path: string) (data: 'T): unit =
        let pool = StringPoolWriter()
        do
            let options = getOptions()
            options.Converters.Add(pool)
            use fileStream = new FileStream(path, FileMode.Create)
            use writer = new Utf8JsonWriter(fileStream)
            JsonSerializer.Serialize(writer, data, options)
        do
            let pool = pool.GetPool()
            let ext = Path.GetExtension(path)
            let path = path.[0 .. path.Length - ext.Length - 1] + "_strings.json"
            use fileStream = new FileStream(path, FileMode.Create)
            use writer = new Utf8JsonWriter(fileStream)
            // Only serializing a string array, no need for special options here
            JsonSerializer.Serialize(writer, pool)

module Performance =
    let measure (f: unit -> 'a) =
        let sw = Diagnostics.Stopwatch.StartNew()
        let res = f()
        sw.Stop()
        res, sw.ElapsedMilliseconds

    let measureAsync (f: unit -> Async<'a>) = async {
        let sw = Diagnostics.Stopwatch.StartNew()
        let! res = f()
        sw.Stop()
        return res, sw.ElapsedMilliseconds
    }

// Make sure chunks are sorted the same way when serialized
// and in Array.BinarySearch below
type StringOrdinalComparer() =
    interface System.Collections.Generic.IComparer<string> with
        member _.Compare(x: string, y: string): int =
            String.CompareOrdinal(x, y)

type PrecompiledFileJson =
    { RootModule: string; OutPath: string }

type PrecompiledInfoJson =
    { CompilerVersion: string
      CompilerOptions: Fable.CompilerOptions
      FableLibDir: string
      Files: Map<string, PrecompiledFileJson>
      InlineExprHeaders: string[] }

type PrecompiledInfoImpl(fableModulesDir: string, info: PrecompiledInfoJson) =
    let dic = System.Collections.Concurrent.ConcurrentDictionary<int, Lazy<Map<string, Fable.InlineExpr>>>()
    let comparer = StringOrdinalComparer()
    let dllPath = PrecompiledInfoImpl.GetDllPath(fableModulesDir)

    member _.CompilerVersion = info.CompilerVersion
    member _.CompilerOptions = info.CompilerOptions
    member _.Files = info.Files
    member _.FableLibDir = info.FableLibDir
    member _.DllPath = dllPath

    member _.TryPrecompiledOutPath(normalizedFullPath: string) =
        Map.tryFind normalizedFullPath info.Files
        |> Option.map (fun f -> f.OutPath)

    static member GetDllPath(fableModulesDir: string): string =
        IO.Path.Combine(fableModulesDir, Fable.Naming.fablePrecompile + ".dll")
        |> Fable.Path.normalizeFullPath

    interface Fable.Transforms.State.PrecompiledInfo with
        member _.DllPath = dllPath

        member _.TryGetRootModule(normalizedFullPath) =
            Map.tryFind normalizedFullPath info.Files
            |> Option.map (fun f -> f.RootModule)

        member _.TryGetInlineExpr(memberUniqueName) =
            let index = Array.BinarySearch(info.InlineExprHeaders, memberUniqueName, comparer)
            let index = if index < 0 then ~~~index - 1 else index
            // We use lazy to prevent two threads from deserializing the inline expressions simultaneously
            // http://reedcopsey.com/2011/01/16/concurrentdictionarytkeytvalue-used-with-lazyt/
            let map = dic.GetOrAdd(index, fun _ ->
                lazy
                    PrecompiledInfoImpl.GetInlineExprsPath(fableModulesDir, index)
                    |> Json.readWithStringPool<(string * Fable.InlineExpr)[]>
                    |> Map)
            Map.tryFind memberUniqueName map.Value

    static member GetPath(dir) =
        IO.Path.Combine(dir, "precompiled_info.json")

    static member GetInlineExprsPath(fableModulesDir, index: int) =
        IO.Path.Combine(fableModulesDir, "inline_exprs", $"inline_exprs_{index}.json")

    static member Load(dir: string) =
        try
            let precompiledInfoPath = PrecompiledInfoImpl.GetPath(dir)
            let info = Json.read<PrecompiledInfoJson> precompiledInfoPath
            PrecompiledInfoImpl(dir, info)
        with
        | e -> FableError($"Cannot load precompiled info from %s{dir}: %s{e.Message}") |> raise

    static member Save(files, inlineExprs, compilerOptions, fableModulesDir, fableLibDir) =
        let comparer = StringOrdinalComparer() :> System.Collections.Generic.IComparer<string>

        let inlineExprs =
            inlineExprs
            |> Array.sortWith (fun (x,_) (y,_) -> comparer.Compare(x, y))
            |> Array.chunkBySize 500 // This number is taken a bit arbitrarily based on tests
            |> Array.mapi (fun i chunk -> i, chunk)

        do
            PrecompiledInfoImpl.GetInlineExprsPath(fableModulesDir, 0)
            |> IO.Path.GetDirectoryName
            |> IO.Directory.CreateDirectory
            |> ignore

        inlineExprs |> Array.Parallel.iter (fun (i, chunk) ->
            let path = PrecompiledInfoImpl.GetInlineExprsPath(fableModulesDir, i)
            Json.writeWithStringPool path chunk)

        let precompiledInfoPath = PrecompiledInfoImpl.GetPath(fableModulesDir)
        let inlineExprHeaders = inlineExprs |> Array.map (snd >> Array.head >> fst)

        { CompilerVersion = Fable.Literals.VERSION
          CompilerOptions = compilerOptions
          Files = files
          FableLibDir = fableLibDir
          InlineExprHeaders = inlineExprHeaders }
        |> Json.write precompiledInfoPath
