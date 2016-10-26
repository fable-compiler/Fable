module Fable.Client.Node.Main

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST

type CompilerMessage =
    | Error of message: string * stack: string
    | Log of message: string
    static member toDic = function
        | Error (msg, stack) ->
            dict [ ("type", "ERROR"); ("message", msg); ("stack", stack) ]
        | Log msg ->
            dict [ ("type", "LOG"); ("message", msg) ]

type PerfTimer(label) =
    let t = System.Diagnostics.Stopwatch()
    do t.Start()
    /// Stops timer and returns a log message with label and total seconds
    member x.Finish() =
        t.Stop()
        t.Elapsed.TotalSeconds
        |> sprintf "%s: %fs" label
        |> Fable.Info

type FSProjInfo = FSharp2Fable.Compiler.FSProjectInfo

let readOptions argv =
    let def opts key defArg f =
        defaultArg (Map.tryFind key opts |> Option.map f) defArg
    let rec readOpts opts = function
        | [] -> opts
        | (opt: string)::rest ->
            let k = opt.Substring(2)
            match Map.tryFind k opts with
            | None -> Map.add k (U2.Case1 rest.Head) opts
            | Some (U2.Case1 v) -> Map.add k (U2.Case2 [rest.Head;v]) opts
            | Some (U2.Case2 v) -> Map.add k (U2.Case2 (rest.Head::v)) opts
            |> readOpts <| rest.Tail
    let un f = function U2.Case1 v -> f v | U2.Case2 _ -> failwith "Unexpected multiple argument"
    let li f = function U2.Case1 v -> [f v] | U2.Case2 v -> List.map f v
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    let opts = {
        projFile = def opts "projFile" [] (li Path.GetFullPath) |> List.rev
        outDir = def opts "outDir" null (un Path.GetFullPath)
        coreLib = def opts "coreLib" "fable-core" (un (fun x -> x.TrimEnd('/')))
        watch = def opts "watch" false (un bool.Parse)
        dll = def opts "dll" false (un bool.Parse)
        clamp = def opts "clamp" false (un bool.Parse)
        noTypedArrays = def opts "noTypedArrays" false (un bool.Parse)
        declaration = def opts "declaration" false (un bool.Parse)
        symbols = def opts "symbols" [] (li id) |> List.append ["FABLE_COMPILER"] |> List.distinct
        plugins = def opts "plugins" [] (li id)
        refs = Map(def opts "refs" [] (li (fun (x: string) ->
            let xs = x.Split('=') in xs.[0], xs.[1])))
        extra = Map(def opts "extra" [] (li (fun (x: string) ->
            if x.Contains("=")
            then let xs = x.Split('=') in xs.[0], xs.[1]
            else x, "")))
    }
    opts

let loadPlugins (pluginPaths: string list) =
    pluginPaths
    |> Seq.collect (fun path ->
        try
            let filePath = Path.GetFullPath path
#if NETSTANDARD1_6 || NETCOREAPP1_0
            let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
            let assembly = globalLoadContext.LoadFromAssemblyPath(filePath)
#else
            let assembly = (filePath |> Assembly.LoadFrom)
#endif
            assembly.GetTypes()
            |> Seq.filter typeof<IPlugin>.IsAssignableFrom
            |> Seq.map (fun x ->
                Path.GetFileNameWithoutExtension path,
                Activator.CreateInstance x |> unbox<IPlugin>)
        with
        | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
    |> Seq.toList

type private TypeInThisAssembly = class end

let forgeGetProjectOptions (opts: CompilerOptions) projFile =
    let projDir = Path.GetDirectoryName(projFile)
    let projParsed = Forge.ProjectSystem.FsProject.load projFile
    let sourceFiles =
        projParsed.SourceFiles.AllFiles()
        |> Seq.filter (fun fileName -> fileName.EndsWith(".fs") || fileName.EndsWith(".fsx"))
        |> Seq.map (fun fileName -> Path.Combine(projDir, fileName))
        |> Seq.toArray
    let beforeComma (str: string) = match str.IndexOf(',', 0) with | -1 -> str | i -> str.Substring(0, i)
    let projReferences = projParsed.References |> Seq.map (fun x ->
        let include' = beforeComma x.Include
        if include'.StartsWith "."
        then include', Path.Combine(projDir, include') |> Some
        else include', x.HintPath |> Option.map (fun x -> Path.Combine(projDir, x)))
    let fscoreDir =
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows
            let PF =
                match Environment.GetEnvironmentVariable("ProgramFiles(x86)") with
                | null -> Environment.GetEnvironmentVariable("ProgramFiles")  // if PFx86 is null, then we are 32-bit and just get PF
                | s -> s
            PF + @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0"
        else
            System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
    let resolve refs =
        SimulatedMSBuildReferenceResolver.SimulatedMSBuildResolver.Resolve(
            ReferenceResolver.ResolutionEnvironment.CompileTimeLike,
            [| for a in refs -> (a, "") |],
            defaultArg projParsed.Settings.TargetFrameworkVersion.Data "v4.5.1",
            [SimulatedMSBuildReferenceResolver.SimulatedMSBuildResolver.DotNetFrameworkReferenceAssembliesRootDirectory + @"\v4.5.1" ],
            "",
            fscoreDir,
            [],
            projDir,
            ignore,
            (fun _ _ _ -> ())
        )
    let allFlags = [|
        yield "--simpleresolution"
        yield "--noframework"
        //yield "--debug:full"
        //yield "--define:DEBUG"
        //yield "--doc:test.xml"
        yield "--optimize-"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
        //yield "--targetprofile:netcore"

        for symbol in opts.symbols do
            yield "--define:" + symbol

        let coreReferences = [
            "FSharp.Core", None
            "mscorlib", None
            "System", None
            // "System.IO", None
            "System.Runtime", None
        ]
        // add distinct project references
        let resolvedRefs, unresolvedRefs =
            Seq.append coreReferences projReferences
            |> Seq.distinctBy fst |> Seq.toArray
            |> Array.partition (snd >> Option.isSome)
        let resolvedFiles =
            unresolvedRefs |> Array.map fst |> resolve
        for r in resolvedFiles do
            yield "-r:" + r.itemSpec
        for (_,r) in resolvedRefs do
            yield "-r:" + r.Value
    |]
    let projOptions: FSharpProjectOptions = {
        ProjectFileName = projFile
        ProjectFileNames = sourceFiles
        OtherOptions = allFlags
        ReferencedProjects = [| |] // TODO: read from projParsed.ProjectReferences
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = true
        LoadTime = DateTime.Now
        UnresolvedReferences = None
    }
    //printfn "Forge projOptions ===> %A" projOptions
    projOptions

type FileResolver() =
    let cache = System.Collections.Generic.Dictionary<string,string>()
    let mutable projDirs = Map.empty<string*string, (string*string) list>
    let getNonFSharpDir(filePath) =
        let rec getNonFSharpDir' dir =
            let parent = Path.GetDirectoryName dir
            if Directory.EnumerateFiles(parent, "*.fs*") |> Seq.isEmpty
            then dir
            else getNonFSharpDir' parent
        let dir = Path.GetDirectoryName filePath |> Path.GetFullPath
        match cache.TryGetValue dir with
        | true, value -> value
        | false, _ ->
            let value = getNonFSharpDir' dir
            cache.Add(dir, value)
            value
    // The strategy is as follows:
    // * Get the project directory:
    //    - If the file is in `node_modules` or not within the given projDir
    //      find the first parent directory that doesn't contain an *.fs file.
    //    - Otherwise, get the given projDir (which is the project file dir).
    // * Look for `projDir` in the stored map of `projDirs`:
    //    - If it is, add the file to the corresponding list of files.
    //    - If not, get the name of `projDir`, check it doesn't conflict with any
    //      other and add it to the key value pairs.
    // * When the final list of files is requested, put each file in the selected folder
    //   within `outDir`.
    member __.AddFile(projDir, srcFile: string) =
      let projDir =
        if srcFile.Contains "node_modules" || not(Fable.Path.isChildPath projDir srcFile)
        then getNonFSharpDir(srcFile)
        else projDir
      projDirs <-
        let trgFile = Fable.Path.getRelativeFileOrDirPath true projDir false srcFile
        projDirs |> Map.tryPick (fun (name, fullName) files ->
            if fullName = projDir
            then Some((name, fullName), files)
            else None)
        |> function
            | Some(k, files) -> Map.add k ((srcFile, trgFile)::files) projDirs
            | None ->
                let dirname =
                    Path.GetFileName projDir
                    |> Naming.preventConflicts (fun x ->
                        projDirs |> Map.exists (fun (name, _) _ -> name = x))
                Map.add (dirname, projDir) [srcFile, trgFile] projDirs
    member __.GetFinalFiles(outDir) =
        let ignoreProjDir =
            projDirs |> Seq.distinctBy (fun kv -> fst kv.Key) |> Seq.length |> (=) 1
        projDirs |> Seq.collect (fun kv ->
            let projDir = if ignoreProjDir then "" else fst kv.Key
            kv.Value |> Seq.map (fun (srcFile, trgFile) ->
            // Use GetFullPath to prevent things like "parentDir/./childDir"
            // which can cause problems when calculating relative paths
            srcFile, Path.GetFullPath <| Path.Combine(outDir, projDir, Path.ChangeExtension(trgFile, ".js"))))
        |> Map

let mergeProjectOpts (opts1: FSharpProjectOptions option, resolver: FileResolver)
                     (opts2: FSharpProjectOptions) =
    let projDir = Path.GetDirectoryName opts2.ProjectFileName
    for file in opts2.ProjectFileNames do
        resolver.AddFile(projDir, file)
    let projOptions: FSharpProjectOptions =
        match opts1 with
        | Some opts1 ->
          { ProjectFileName = opts2.ProjectFileName
            ProjectFileNames = Array.append opts1.ProjectFileNames opts2.ProjectFileNames
            OtherOptions = Array.append opts1.OtherOptions opts2.OtherOptions |> Array.distinct
            ReferencedProjects = [| |]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = true
            LoadTime = DateTime.Now
            UnresolvedReferences = None }
        | None -> opts2
    //printfn "Merged projOptions ===> %A" projOptions
    (Some projOptions, resolver)

let getProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        let otherFlags = [|
            yield "--target:library"
            for symbol in opts.symbols do yield "--define:" + symbol
        |]
        checker.GetProjectOptionsFromScript(projFile, File.ReadAllText projFile, otherFlags = otherFlags)
        |> Async.RunSynchronously
    | ".fsproj" ->
        forgeGetProjectOptions opts projFile
    | _ as s -> failwith (sprintf "Unsupported project type: %s" s)

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    let retryUntil = (DateTime.UtcNow + TimeSpan.FromSeconds 5.)
    let rec retry () =
        try
            getProjectOpts checker opts projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.UtcNow then
                System.Threading.Thread.Sleep 100
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | ex -> failwithf "Cannot read project options: %s" ex.Message
    retry()

let getFullProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) =
    opts.projFile
    |> Seq.map (retryGetProjectOpts checker opts)
    |> Seq.fold mergeProjectOpts (None, FileResolver())
    |> fun (projOpts, resolver) -> projOpts.Value, resolver.GetFinalFiles opts.outDir

/// Returns an (errors, warnings) tuple
let parseErrors errors =
    let parseError (er: FSharpErrorInfo) =
        let loc = sprintf " (L%i,%i-L%i,%i) (%s)"
                    er.StartLineAlternate er.StartColumn
                    er.EndLineAlternate er.EndColumn
                    (Path.GetFileName er.FileName)
        match er.Severity, er.ErrorNumber with
        | _, 40 -> true, "Recursive value definitions are not supported" + loc // See #237
        | FSharpErrorSeverity.Warning, _ -> false, er.Message + loc
        | FSharpErrorSeverity.Error, _ -> true, er.Message + loc
    errors
    |> Array.map parseError
    |> Array.partition fst
    |> fun (ers, wns) -> Array.map snd ers, Array.map snd wns

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                        (projOptions: FSharpProjectOptions) =
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors, warnings =
        parseErrors checkProjectResults.Errors
    if errors.Length = 0
    then warnings |> Array.map Warning, checkProjectResults
    else errors
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

let makeCompiler opts plugins =
    let id = ref 0
    let monitor = obj()
    let logs = ResizeArray()
    let projDir = Fable.Path.getCommonBaseDir opts.projFile
    { new ICompiler with
        member __.Options = opts
        member __.ProjDir = projDir
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetLogs() =
            let copy = logs.ToArray()
            logs.Clear()
            upcast copy
        member __.GetUniqueVar() =
            lock monitor (fun () ->
                id := !id + 1
                "$var" + string !id) }

let getMinimumFableCoreVersion() =
#if NETSTANDARD1_6 || NETCOREAPP1_0
    let assembly = typeof<CompilerOptions>.GetTypeInfo().Assembly
    assembly.GetName().Version |> Some
#else
    Assembly.GetExecutingAssembly()
            .GetCustomAttributes(typeof<AssemblyMetadataAttribute>, false)
    |> Seq.tryPick (fun att ->
        let att = att :?> AssemblyMetadataAttribute
        if att.Key = "fableCoreVersion"
        then Version att.Value |> Some
        else None)
#endif

let printFile =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore,
            StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (file: AST.Babel.Program) ->
        JsonConvert.SerializeObject (file, jsonSettings)
        |> Console.Out.WriteLine

let printMessages (msgs: #seq<CompilerMessage>) =
    msgs
    |> Seq.map (CompilerMessage.toDic >> JsonConvert.SerializeObject)
    |> Seq.iter Console.Out.WriteLine

let compileDll (checker: FSharpChecker) (comOpts: CompilerOptions) (projOpts: FSharpProjectOptions): unit =
    if Directory.Exists(comOpts.outDir) |> not then
        Directory.CreateDirectory(comOpts.outDir) |> ignore
    let projOut =
        let projName = Path.GetFileNameWithoutExtension(Seq.last comOpts.projFile)
        Path.GetFullPath(Path.Combine(comOpts.outDir, projName))
    let args = [|
        yield! projOpts.OtherOptions
        // Seems `--out` cannot come at the beginning
        // or the compiler will ignore it
        yield "--out:" + projOut + ".dll"
        yield "--doc:" + projOut + ".xml"
        yield! projOpts.ProjectFileNames
    |]
    let errors, warnings =
        let errors, _exitCode = checker.Compile(args)
        parseErrors errors
    if errors.Length > 0 then
        errors
        |> Seq.append ["Errors when generating dll assembly:"]
        |> String.concat "\n"
        |> failwith
    if warnings.Length > 0 then
        warnings
        |> Seq.append ["Warnings when generating dll assembly:"]
        |> String.concat "\n"
        |> Info |> string |> Log
        |> List.singleton |> printMessages

let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
    try
        // Reload project options if necessary
        // -----------------------------------
        let projInfo =
            match projInfo.FileMask with
            | Some file when com.Options.projFile |> List.exists ((=) file) ->
                let projOpts, filePairs = getFullProjectOpts checker com.Options
                FSProjInfo(projOpts, filePairs, ?fileMask=projInfo.FileMask, extra=projInfo.Extra)
            | _ -> projInfo

        // Print F# compiler options (verbose mode) on first compilation
        // (when projInfo.fileMask is None)
        if Option.isNone projInfo.FileMask then
            projInfo.ProjectOpts.OtherOptions
            |> String.concat "\n" |> sprintf "\nF# COMPILER OPTIONS:\n%s\n"
            |> Log |> List.singleton |> printMessages

        // Parse project (F# Compiler Services) and print diagnostic info
        // --------------------------------------------------------------
        //let timer = PerfTimer("Warmup") |> Some
        let warnings, parsedProj =
            parseFSharpProject com checker projInfo.ProjectOpts

        //let warnings = match timer with Some timer -> (timer.Finish())::warnings | None -> warnings
        warnings |> Seq.map (string >> Log) |> printMessages

        // Check Fable.Core version on first compilation (whe projInfo.fileMask is None)
        // -----------------------------------------------------------------------------
        #if NETSTANDARD1_6 || NETCOREAPP1_0
        // Skip this check in netcore for now
        #else
        if Option.isNone projInfo.FileMask
            && com.Options.extra |> Map.containsKey "noVersionCheck" |> not
        then
            parsedProj.ProjectContext.GetReferencedAssemblies()
            |> Seq.tryPick (fun asm ->
                if asm.SimpleName <> "Fable.Core"
                then None
                else Regex.Match(asm.QualifiedName, "Version=(.*?),").Groups.[1].Value |> Version |> Some)
            |> Option.iter (fun fableCoreVersion ->
                match getMinimumFableCoreVersion() with
                | Some minVersion when fableCoreVersion < minVersion ->
                    failwithf "Fable.Core %O required, please upgrade the project reference" minVersion
                | _ -> ())
        #endif

        // Compile project files, print them and get extra info
        // ----------------------------------------------------
        let rewrites =
            com.Plugins |> Seq.choose (function _, (:? IRewritePlugin as r) -> Some r | _ -> None)
        let applyRewrites (extra, input) =
            extra, rewrites |> Seq.fold (fun input rewrite -> rewrite.Rewrite input) input

        let extraInfo, files =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> applyRewrites
            |> Fable2Babel.Compiler.transformFiles com

        let files = Array.ofSeq files
        files
        |> Seq.iter printFile

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        Console.Out.WriteLine "[SIGSUCCESS]"
        true, FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs,
                            ?fileMask=projInfo.FileMask, extra=extraInfo)
    with ex ->
        let stackTrace =
            match ex.InnerException with
            | null -> ex.StackTrace
            | inner -> inner.StackTrace
        printMessages [Error(ex.Message, stackTrace)]
        Console.Out.WriteLine "[SIGFAIL]"
        false, projInfo

let rec awaitInput (com: ICompiler) checker fullCompileSuccess (projInfo: FSProjInfo) =
    match Console.In.ReadLine() with
    | "[SIGTERM]" -> ()
    | fileMask ->
        let projInfo =
            if fullCompileSuccess
            then FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs,
                            fileMask=fileMask, extra=projInfo.Extra)
            else
                let projectOpts, filePairs = getFullProjectOpts checker com.Options
                FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs)
        let success, projInfo = compile com checker projInfo
        awaitInput com checker (fullCompileSuccess || success) projInfo

[<EntryPoint>]
let main argv =
    try
        let opts = readOptions argv
        let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
        let projectOpts, filePairs = getFullProjectOpts checker opts
        let com = loadPlugins opts.plugins |> makeCompiler opts
        // Full compilation
        let success, projInfo =
            FSProjInfo(projectOpts, filePairs)
            |> compile com checker
        // Keep on watching if necessary
        if opts.watch then
            awaitInput com checker success projInfo
    with
    | ex -> printMessages [Error(ex.Message, ex.StackTrace)]
    0
