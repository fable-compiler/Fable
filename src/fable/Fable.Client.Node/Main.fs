module Fable.Client.Node.Main

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST

type CompilerMessageType =
    | Error | Log
    override x.ToString() =
        match x with Error -> "ERROR" | Log -> "LOG"

type CompilerMessage(typ: CompilerMessageType, msg) =
    member x.``type`` = string typ
    member x.message: string = msg

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
        symbols = def opts "symbols" [] (li id)
        plugins = def opts "plugins" [] (li id)
        msbuild = def opts "msbuild" [] (li id)
        refs = Map(def opts "refs" [] (li (fun (x: string) ->
            let xs = x.Split('=') in xs.[0], xs.[1])))
        extra = Map.empty // TODO: Read extra options
    }

let loadPlugins (pluginPaths: string list) (assemblies: FSharpAssembly list) =
    let optPlugins =
        pluginPaths
        |> Seq.collect (fun path ->
            try
                (Path.GetFullPath path |> Assembly.LoadFrom).GetTypes()
                |> Seq.filter typeof<IPlugin>.IsAssignableFrom
                |> Seq.map (fun x ->
                    Path.GetFileNameWithoutExtension path,
                    Activator.CreateInstance x |> unbox<IPlugin>)
            with
            | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
        |> Seq.toList
    // Type providers may contain also plugins
    let tpPlugins =
        try
            assemblies
            |> Seq.filter (fun asm ->
                asm.Contents.Attributes
                |> Seq.exists (fun x -> x.AttributeType.DisplayName = "TypeProviderAssemblyAttribute"))
            |> Seq.collect (fun asm ->
                let asm = Assembly.Load(asm.QualifiedName)
                asm.GetTypes()
                |> Seq.filter typeof<IPlugin>.IsAssignableFrom
                |> Seq.map (fun x ->
                    asm.FullName, Activator.CreateInstance x |> unbox<IPlugin>))
            |> Seq.toList
        with _ -> [] // TODO: React to errors?
    optPlugins@tpPlugins    

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                        (projOptions: FSharpProjectOptions) =
    let errorToString (er: FSharpErrorInfo) =
        sprintf "%s (L%i,%i-L%i,%i) (%s)"
                er.Message
                er.StartLineAlternate er.StartColumn
                er.EndLineAlternate er.EndColumn
                (Path.GetFileName er.FileName)
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let warnings, errors =
        checkProjectResults.Errors
        |> Array.partition (fun x -> x.Severity = FSharpErrorSeverity.Warning)
    let warnings =
        warnings |> Seq.map (errorToString >> Warning) |> Seq.toList
    if errors.Length = 0
    then warnings, checkProjectResults
    else errors
        |> Seq.map (errorToString >> (+) "> ")
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
    
let start argv checker getProjectOpts projectOpts =

    let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
        let printFile =
            let jsonSettings =
                JsonSerializerSettings(
                    Converters=[|Json.ErasedUnionConverter()|],
                    StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
            fun (file: AST.Babel.Program) ->
                JsonConvert.SerializeObject (file, jsonSettings)
                |> Console.Out.WriteLine
        let printMessages msgs =
            for (typ, msg) in msgs do
                CompilerMessage(typ, msg)
                |> JsonConvert.SerializeObject
                |> Console.Out.WriteLine
        try
            // Reload project options if necessary
            // -----------------------------------
            let projInfo =
                match projInfo.fileMask with
                | Some file when com.Options.projFile = file ->
                    (com.Options.projFile, com.Options.symbols, com.Options.msbuild)
                    |||> getProjectOpts checker
                    |> fun projOpts -> { projInfo with projectOpts = projOpts }
                | _ -> projInfo

            if projInfo.projectOpts.ProjectFileNames.Length = 0 then
                failwith "Project doesn't contain any file"

            // Parse project (F# Compiler Services) and print diagnostic info
            // --------------------------------------------------------------
            //let timer = PerfTimer("Warmup") |> Some
            let warnings, parsedProj =
                parseFSharpProject com checker projInfo.projectOpts
            //let warnings = match timer with Some timer -> (timer.Finish())::warnings | None -> warnings
            warnings |> List.map (fun log -> Log, string log) |> printMessages

            // Load plugins after parsing the project, so type provider
            // assemblies can be found in the parsed references
            // ------------------------------------------------
            let com =
                parsedProj.ProjectContext.GetReferencedAssemblies()
                |> loadPlugins com.Options.plugins
                |> makeCompiler com.Options

            // Compile project files, print them and get the dependencies
            // ----------------------------------------------------------
            let dependencies =
                FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
                |> Fable2Babel.Compiler.transformFile com
                |> Seq.fold (fun accDeps (babelFile, fileDeps) ->
                    printFile babelFile
                    Map.add babelFile.originalFileName fileDeps accDeps
                ) projInfo.dependencies

            // Print logs
            // ----------
            com.GetLogs()
            |> Seq.map (fun log -> Log, string log)
            |> Seq.toList
            |> printMessages

            // Send empty string to signal end of compilation and return
            // ---------------------------------------------------------
            Console.Out.WriteLine()
            true, { projInfo with dependencies = dependencies }
        with ex ->
            printMessages [Error, ex.Message]
            false, projInfo

    let rec awaitInput (com: ICompiler) checker (projInfo: FSProjInfo) =
        { projInfo with fileMask = Console.In.ReadLine() |> Some }
        |> compile com checker
        |> snd
        |> awaitInput com checker

    // Startup code
    let opts = readOptions argv
    let com = makeCompiler opts []
    // Full compilation
    let success, projInfo =
        { FSProjInfo.projectOpts = projectOpts
          FSProjInfo.fileMask = None
          FSProjInfo.dependencies = Map.empty }
        |> compile com checker

    // Keep on watching if necessary
    if success && opts.watch then
        awaitInput com checker projInfo
