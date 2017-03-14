module Fable.Client.Webpack.State

open Fable
open Fable.AST
open Newtonsoft.Json
open System
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open ProjectCracker
open Parser

let fableCoreJsDir =
    #if NO_PACKAGE
    let fableCoreDirRelativePath = "../fable-core"
    #else // Nuget package
    let fableCoreDirRelativePath = "../../fable-core"
    #endif
    let coreDir = Path.Combine(Path.GetDirectoryName(ProjectCracker.fableCoreLib), fableCoreDirRelativePath)
    let cwd = System.IO.Directory.GetCurrentDirectory()
    Path.getRelativeFileOrDirPath true cwd true coreDir

type Command = string * (string -> unit)

type State(projectOptions: FSharpProjectOptions, checkedProject: FSharpCheckProjectResults) =
    let entities = ConcurrentDictionary<string, Fable.Entity>()
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    let compiledFiles =
        let dic = Dictionary()
        for file in projectOptions.ProjectFileNames do
            dic.Add(Path.normalizeFullPath file, false)
        dic
    let rootModules =
        checkedProject.AssemblyContents.ImplementationFiles
        |> Seq.map (fun file -> file.FileName, FSharp2Fable.Compiler.getRootModuleFullName file)
        |> Map
    member __.CheckedProject = checkedProject
    member __.ProjectOptions = projectOptions
    member __.ProjectFile = projectOptions.ProjectFileName
    member __.CompiledFiles = compiledFiles
    interface ICompilerState with
        member this.GetRootModule(fileName) =
            match Map.tryFind fileName rootModules with
            | Some rootModule -> rootModule
            | None -> FableError("Cannot find root module for " + fileName) |> raise
        member this.GetOrAddEntity(fullName, generate) =
            entities.GetOrAdd(fullName, fun _ -> generate())
        member this.GetOrAddInlineExpr(fullName, generate) =
            inlineExprs.GetOrAdd(fullName, fun _ -> generate())

let loadAssembly path =
#if NETFX
    Assembly.LoadFrom(path)
#else
    let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
    globalLoadContext.LoadFromAssemblyPath(path)
#endif

type Compiler(options, plugins) =
    let mutable id = 0
    let logs = ResizeArray()
    member __.Logs = logs
    interface ICompiler with
        member __.CoreLib = fableCoreJsDir
        member __.Options = options
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetUniqueVar() =
            "$var" + (System.Threading.Interlocked.Increment(&id).ToString())

let loadPlugins pluginPaths (loadedPlugins: PluginInfo list) =
    let loadedPlugins =
        loadedPlugins |> Seq.groupBy (fun p -> p.path) |> Map
    pluginPaths
    |> Seq.collect (fun path ->
        let path = Path.normalizeFullPath path
        match Map.tryFind path loadedPlugins with
        | Some pluginInfos -> pluginInfos
        | None ->
            try
                let assembly = loadAssembly path
                assembly.GetTypes()
                |> Seq.filter typeof<IPlugin>.IsAssignableFrom
                |> Seq.map (fun x ->
                    { path = path
                    ; plugin = Activator.CreateInstance x :?> IPlugin })
            with
            | :? FableError as er -> raise er
            | ex -> FableError(sprintf "Cannot load plugin %s: %s" path ex.Message) |> raise)
    |> Seq.toList

/// Returns an (errors, warnings) tuple
let parseErrors errors =
    let parseError (er: FSharpErrorInfo) =
        let isError, severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> false, "warning"
            | FSharpErrorSeverity.Error -> true, "error"
        isError, sprintf "%s(L%i,%i) : %s FSHARP: %s"
            er.FileName er.StartLineAlternate er.StartColumn
            severity er.Message
    errors
    |> Array.map parseError
    |> Array.partition fst
    |> fun (ers, wns) -> Array.map snd ers, Array.map snd wns

let parseFSharpProject (checker: FSharpChecker) (projOptions: FSharpProjectOptions) =
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
        |> FableError |> raise

let createState checker projectOptions (com: ICompiler) (msg: Message) projFile =
    let com =
        let plugins = loadPlugins msg.plugins com.Plugins
        Compiler(msg.options, plugins)
    let projectOptions =
        match projectOptions with
        | Some projectOptions -> projectOptions
        | None -> getFullProjectOpts checker msg.define projFile
    // printfn "F# Project: %s" projectOptions.ProjectFileName
    // for option in projectOptions.OtherOptions do
    //     printfn "%s" option
    // for file in projectOptions.ProjectFileNames do
    //     printfn "%s" file
    let logs, checkedProject =
        // TODO: Do this asynchronously?
        parseFSharpProject checker projectOptions
    for log in logs do
        (com :> ICompiler).AddLog(log)
    com, State(projectOptions, checkedProject)

let toJsonAndReply =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore,
            StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (replyChannel: string->unit) fileName (value: obj) ->
        fileName |> Option.iter (printfn "Fable server sent: %s")
        JsonConvert.SerializeObject(value, jsonSettings) |> replyChannel

let sendError replyChannel (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    match ex with
    | :? FableError as err ->
        let errMessage = err.FormattedMessage
        printfn "%s" errMessage
        // Don't print stack trace for known Fable errors
        ["error", dict ["message", errMessage]]
        |> dict |> toJsonAndReply replyChannel None
    | ex ->
        let stack = innerStack ex
        printfn "ERROR: %s\n%s" ex.Message stack
        ["error", dict ["message", ex.Message; "stack", stack]]
        |> dict |> toJsonAndReply replyChannel None

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let addLogs (com: Compiler) (file: Babel.Program) =
        let loc = defaultArg file.loc SourceLocation.Empty
        let infos, warnings =
            com.Logs
            |> Seq.toArray
            |> Array.partition (function Info _ -> true | Warning _ -> false)
            |> fun (infos, warnings) ->
               Array.map Log.message infos,
               Array.map Log.message warnings
        Babel.Program(file.fileName, loc, file.body, file.directives, infos, warnings)
    let rec loop (checker: FSharpChecker) (com: Compiler) (state: State option) = async {
        let! (Parse msg), replyChannel = agent.Receive()
        try
            printfn "Fable server received: %s" msg.path
            let com, state =
                match state with
                | Some state ->
                    if msg.path.EndsWith(".fsproj") || msg.path = state.ProjectFile // Must be an .fsx script
                    then createState checker None com msg msg.path
                    else
                        match state.CompiledFiles.TryGetValue(msg.path) with
                        // Watch compilation, restart state
                        | true, true ->
                            createState checker (Some state.ProjectOptions) com msg state.ProjectFile
                        | true, false ->
                            let com = com :> ICompiler
                            Compiler(com.Options, com.Plugins), state
                        | false, _ when msg.path.EndsWith(".fsx") ->
                            createState checker None com msg msg.path
                        | false, _ ->
                            sprintf "%s doesn't belong to project %s" msg.path state.ProjectFile
                            |> FableError |> raise
                | None ->
                    createState checker None com msg msg.path
            let fileName = msg.path
            if fileName.EndsWith(".fsproj")
            then
                let lastFile = Array.last state.ProjectOptions.ProjectFileNames
                Fable2Babel.Compiler.createFacade fileName lastFile
                |> addLogs com
                |> toJsonAndReply replyChannel (Some fileName)
            else
                // Set file as already compiled
                state.CompiledFiles.[fileName] <- true
                Async.Start(async {
                    try
                        let fableFile = FSharp2Fable.Compiler.transformFile com state state.CheckedProject fileName
                        // Print AST
                        // state.CheckedProject.AssemblyContents.ImplementationFiles
                        // |> Seq.tryFind (fun f -> Path.normalizeFullPath f.FileName = fileName)
                        // |> Option.iter (fun f ->
                        //     let name = System.IO.Path.GetFileNameWithoutExtension(fileName)
                        //     Printers.printFSharpDecls "" f.Declarations
                        //     |> fun lines -> System.IO.File.WriteAllLines(name + ".fs.ast", lines)
                        //     Printers.printFableDecls fableFile.Declarations
                        //     |> fun lines -> System.IO.File.WriteAllLines(name + ".fable.ast", lines)
                        // )
                        Fable2Babel.Compiler.transformFile com state fableFile
                        |> addLogs com
                        |> toJsonAndReply replyChannel (Some fileName)
                    with ex ->
                        sendError replyChannel ex
                })
            return! loop checker com (Some state)
        with ex ->
            sendError replyChannel ex
            return! loop checker com state
    }
    let compiler = Compiler(getDefaultOptions(), [])
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    loop checker compiler None
  )
