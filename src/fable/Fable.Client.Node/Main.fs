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
        projFile = def opts "projFile" [] (li Path.GetFullPath)
        outDir = def opts "outDir" null (un Path.GetFullPath)
        coreLib = def opts "coreLib" "fable-core" (un (fun x -> x.TrimEnd('/')))
        watch = def opts "watch" false (un bool.Parse)
        dll = def opts "dll" false (un bool.Parse)
        clamp = def opts "clamp" false (un bool.Parse)
        copyExt = def opts "copyExt" false (un bool.Parse)
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
    if opts.coreLib.StartsWith "."
    then { opts with coreLib =
                        Path.GetFullPath opts.coreLib
                        |> Fable.Path.getRelativePath opts.outDir
                        |> fun x -> x.TrimEnd('/') }
    else opts

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

let forgeGetProjectOptions projFile =
    let projPath = Path.GetDirectoryName(projFile)
    let projParsed = Forge.ProjectSystem.FsProject.load projFile
    let sourceFiles =
        projParsed.SourceFiles.AllFiles()
        |> Seq.filter (fun fileName -> fileName.EndsWith(".fs") || fileName.EndsWith(".fsx"))
        |> Seq.map (fun fileName -> Path.Combine(projPath, fileName))
        |> Seq.toArray
    let beforeComma (str: string) = match str.IndexOf(',', 0) with | -1 -> str | i -> str.Substring(0, i)
    let projReferences = projParsed.References |> Seq.map (fun x ->
        (beforeComma x.Include),
        (match x.HintPath with | Some path -> Path.Combine(projPath, path) |> Some | _ -> None))
    //NOTE: proper reference resolution ahead of time is necessary to avoid default FCS resolution
    let fsCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
    let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
    let sysPath = Path.GetDirectoryName(sysCoreLib)
    let sysLib name = Path.Combine(sysPath, name + ".dll")
    let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location)
    let localLib name = Path.Combine(localPath, name + ".dll")
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

        let coreReferences = [
            "FSharp.Core", Some fsCoreLib
            "CoreLib", Some sysCoreLib
            "mscorlib", None
            "System.IO", None
            "System.Runtime", None
        ]

        // add distinct project references
        let references = Seq.append coreReferences projReferences |> Seq.distinctBy fst
        for r in references do
            match r with
                | _, Some path -> // absolute paths
                    yield "-r:" + path
                | name, None -> // try to resolve path
                    if File.Exists (sysLib name) then
                        yield "-r:" + (sysLib name)
                    elif File.Exists (localLib name) then
                        yield "-r:" + (localLib name)
                    //TODO: check more paths?
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

let mergeProjectOpts (opts1: FSharpProjectOptions) (opts2: FSharpProjectOptions) =
    let projOptions: FSharpProjectOptions = {
        ProjectFileName = opts2.ProjectFileName
        ProjectFileNames = Array.append opts1.ProjectFileNames opts2.ProjectFileNames
        OtherOptions = Array.append opts1.OtherOptions opts2.OtherOptions |> Array.distinct
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = true
        LoadTime = DateTime.Now
        UnresolvedReferences = None
    }
    //printfn "Merged projOptions ===> %A" projOptions
    projOptions

let getProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
      let defines = [|for symbol in opts.symbols do yield "--define:" + symbol|]
      checker.GetProjectOptionsFromScript(projFile, File.ReadAllText projFile, otherFlags = defines)
      |> Async.RunSynchronously
    | ".fsproj" ->
      forgeGetProjectOptions projFile
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
    |> Seq.reduce mergeProjectOpts
    |> fun fullProjOpts ->
        let otherOpts =
            fullProjOpts.OtherOptions
            |> Array.append (List.map (sprintf "--define:%s") opts.symbols |> List.toArray)
        { fullProjOpts with OtherOptions = otherOpts }

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
    failwith "TODO" (*
    if Directory.Exists(comOpts.outDir) |> not then
        Directory.CreateDirectory(comOpts.outDir) |> ignore
    let projOut =
        let projName = Path.GetFileNameWithoutExtension(comOpts.projFile)
        Path.GetFullPath(Path.Combine(comOpts.outDir, projName))
    let args =
        if Path.GetExtension(comOpts.projFile).ToLower() = ".fsproj"
        then
            let projDir = Path.GetFullPath(Path.GetDirectoryName(comOpts.projFile))
            let projOpts =
                projOpts.OtherOptions
                |> Array.collect (function
                    | Naming.StartsWith "--doc:" _ -> [||]
                    | Naming.StartsWith "--out:" _ ->
                        [| "--out:" + projOut + ".dll"; "--doc:" + projOut + ".xml" |]
                    // Resolve relative references
                    | Naming.StartsWith "-r:." _ as x ->
                        [|"-r:" + Path.Combine(projDir, x.Substring(3))|]
                    | x -> [|x|])
            projOpts
        else
            [| comOpts.projFile
               "--out:" + projOut + ".dll"
               "--target:library"
               "--doc:" + projOut + ".xml" |]
    let errors, warnings =
        let scs = SimpleSourceCodeServices()
        if Path.GetExtension(comOpts.projFile).ToLower() = ".fsproj"
        then args
        // Project Options from a script don't contain file names
        else Array.append args projOpts.ProjectFileNames
        |> scs.Compile
        |> fun (errors, _exitCode) -> parseErrors errors
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
    *)

let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
    try
        // Reload project options if necessary
        // -----------------------------------
        let projInfo =
            match projInfo.FileMask with
            | Some file when com.Options.projFile |> List.exists ((=) file) ->
                let projOpts = getFullProjectOpts checker com.Options
                FSProjInfo(projOpts, ?fileMask=projInfo.FileMask, extra=projInfo.Extra)
            | _ -> projInfo

        // Print F# compiler options (verbose mode) on first compilation
        // (when projInfo.fileMask is None)
        if Option.isNone projInfo.FileMask then
            projInfo.ProjectOpts.OtherOptions
            |> String.concat "\n" |> sprintf "\nF# COMPILER OPTIONS:\n%s\n"
            |> Log |> List.singleton |> printMessages

        if com.Options.dll then
            compileDll checker com.Options projInfo.ProjectOpts

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

        files
        |> Seq.iter printFile

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        Console.Out.WriteLine "[SIGSUCCESS]"
        true, FSProjInfo(projInfo.ProjectOpts, ?fileMask=projInfo.FileMask, extra=extraInfo)
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
            then FSProjInfo(projInfo.ProjectOpts, fileMask=fileMask, extra=projInfo.Extra)
            else FSProjInfo(getFullProjectOpts checker com.Options)
        let success, projInfo = compile com checker projInfo
        awaitInput com checker (fullCompileSuccess || success) projInfo

[<EntryPoint>]
let main argv =
    try
        let opts = readOptions argv
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let projectOpts = getFullProjectOpts checker opts
        let com = loadPlugins opts.plugins |> makeCompiler opts
        // Full compilation
        let success, projInfo =
            FSProjInfo(projectOpts)
            |> compile com checker
        // Keep on watching if necessary
        if opts.watch then
            awaitInput com checker success projInfo
    with
    | ex -> printMessages [Error(ex.Message, ex.StackTrace)]
    0
