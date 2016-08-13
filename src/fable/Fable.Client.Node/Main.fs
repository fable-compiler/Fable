module Fable.Client.Node.Main

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
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
    {
        projFile = def opts "projFile" null (un Path.GetFullPath)
        coreLib = def opts "coreLib" "fable-core" (un id)
        watch = def opts "watch" false (un bool.Parse)
        clamp = def opts "clamp" false (un bool.Parse)
        copyExt = def opts "copyExt" false (un bool.Parse)
        declaration = def opts "declaration" false (un bool.Parse)
        symbols = def opts "symbols" ["FABLE_COMPILER"] (li id) |> List.distinct
        plugins = def opts "plugins" [] (li id)
        msbuild = def opts "msbuild" [] (li id)
        refs = Map(def opts "refs" [] (li (fun (x: string) ->
            let xs = x.Split('=') in xs.[0], xs.[1])))
        extra = Map(def opts "extra" [] (li (fun (x: string) ->
            let xs = x.Split('=') in xs.[0], xs.[1])))
    }

let loadPlugins (pluginPaths: string list) =
    pluginPaths
    |> Seq.collect (fun path ->
        try
            let filePath = Path.GetFullPath path
#if NETSTANDARD1_6 || NETCOREAPP1_0
            let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
            let assemblyName = System.Runtime.Loader.AssemblyLoadContext.GetAssemblyName(filePath)
            let assembly = globalLoadContext.LoadFromAssemblyName(assemblyName)
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

let getProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) =
    let rec addSymbols (symbols: string list) (opts: FSharpProjectOptions) =
        let addSymbols' (otherOpts: string[]) =
            otherOpts
            // |> Array.filter (fun s -> s.StartsWith "--define:" = false)
            |> Array.append (List.map (sprintf "--define:%s") symbols |> List.toArray)
        { opts with
            OtherOptions = addSymbols' opts.OtherOptions
            ReferencedProjects = opts.ReferencedProjects
                |> Array.map (fun (k,v) -> k, addSymbols symbols v) }
    try
        let projFile = Path.GetFullPath opts.projFile
        match (Path.GetExtension projFile).ToLower() with
        | ".fsx" ->
            checker.GetProjectOptionsFromScript(projFile, File.ReadAllText projFile)
            |> Async.RunSynchronously
        | _ -> // .fsproj
            let props = opts.msbuild |> List.choose (fun x ->
                match x.Split('=') with
                | [|key;value|] -> Some(key,value)
                | _ -> None)
            // NOTE: .NET Core MSBuild can't successfully build .fsproj (yet)
            // see https://github.com/Microsoft/msbuild/issues/709, 711, 713
            ProjectCracker.GetProjectOptionsFromProjectFile(projFile, props)
        |> addSymbols opts.symbols
    with
    | ex -> failwithf "Cannot read project options: %s" ex.Message

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                        (projOptions: FSharpProjectOptions) =
    let parseError (er: FSharpErrorInfo) =
        let loc = sprintf " (L%i,%i-L%i,%i) (%s)"
                    er.StartLineAlternate er.StartColumn
                    er.EndLineAlternate er.EndColumn
                    (Path.GetFileName er.FileName)
        match er.Severity, er.ErrorNumber with
        | _, 40 -> true, "Recursive value definitions are not supported" + loc // See #237
        | FSharpErrorSeverity.Warning, _ -> false, er.Message + loc
        | FSharpErrorSeverity.Error, _ -> true, er.Message + loc
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors, warnings =
        checkProjectResults.Errors
        |> Array.map parseError
        |> Array.partition fst
    if errors.Length = 0
    then warnings |> Array.map (snd >> Warning), checkProjectResults
    else errors
        |> Seq.map (snd >> (+) "> ")
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

let makeCompiler opts plugins =
    let logs = System.Collections.Concurrent.ConcurrentBag()
    { new ICompiler with
        member __.Options = opts
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetLogs() = logs :> seq<_> }

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
    |> Seq.map CompilerMessage.toDic
    |> Seq.map JsonConvert.SerializeObject
    |> Seq.iter Console.Out.WriteLine

let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
    try
        // Reload project options if necessary
        // -----------------------------------
        let projInfo =
            match projInfo.fileMask with
            | Some file when com.Options.projFile = file ->
                getProjectOpts checker com.Options
                |> fun projOpts -> { projInfo with projectOpts = projOpts }
            | _ -> projInfo

        // TODO: Find a way to check if the project is empty
        // (Unfortunately it seems `ProjectFileNames` is not reliable)

        // Parse project (F# Compiler Services) and print diagnostic info
        // --------------------------------------------------------------
        //let timer = PerfTimer("Warmup") |> Some
        let warnings, parsedProj =
            parseFSharpProject com checker projInfo.projectOpts
        //let warnings = match timer with Some timer -> (timer.Finish())::warnings | None -> warnings
        warnings |> Seq.map (string >> Log) |> printMessages

        // Check Fable.Core version on first compilation (whe projInfo.fileMask is None)
        // -----------------------------------------------------------------------------
        if Option.isNone projInfo.fileMask then
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

        // Compile project files, print them and get the dependencies
        // ----------------------------------------------------------
        let dependencies =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> Fable2Babel.Compiler.transformFile com
            |> Seq.fold (fun accDeps (babelFile, fileDeps) ->
                printFile babelFile
                match babelFile.originalFileName with
                | Some fileName -> Map.add fileName fileDeps accDeps
                | None -> accDeps
            ) projInfo.dependencies

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        // Send empty string to signal end of compilation and return
        // ---------------------------------------------------------
        Console.Out.WriteLine()
        true, { projInfo with dependencies = dependencies }
    with ex ->
        let stackTrace =
            match ex.InnerException with
            | null -> ex.StackTrace
            | inner -> inner.StackTrace
        printMessages [Error(ex.Message, stackTrace)]
        false, projInfo

let rec awaitInput (com: ICompiler) checker (projInfo: FSProjInfo) =
    match Console.In.ReadLine() with
    | "[SIGTERM]" -> ()
    | fileMask ->
        { projInfo with fileMask = Some fileMask }
        |> compile com checker
        |> snd
        |> awaitInput com checker

[<EntryPoint>]
let main argv =
    try
        let opts = readOptions argv
        let checker = FSharpChecker.Create(keepAssemblyContents=true)
        let projectOpts = getProjectOpts checker opts
        let com = loadPlugins opts.plugins |> makeCompiler opts
        // Full compilation
        let success, projInfo =
            { FSProjInfo.projectOpts = projectOpts
              FSProjInfo.fileMask = None
              FSProjInfo.dependencies = Map.empty }
            |> compile com checker
        // Keep on watching if necessary
        if success && opts.watch then
            awaitInput com checker projInfo
    with
    | ex -> printMessages [Error(ex.Message, ex.StackTrace)]
    0
