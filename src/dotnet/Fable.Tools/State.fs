module Fable.Tools.State

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

type Compiler() =
    let mutable id = 0
    let mutable options' = getDefaultOptions()
    let mutable plugins': PluginInfo list = []
    let logs = ResizeArray()
    member __.Logs: Log seq = upcast logs
    member __.Reset(?options, ?plugins) =
        id <- 0
        options' <- defaultArg options options'
        plugins' <- defaultArg plugins plugins'
        logs.Clear()
    interface ICompiler with
        member __.Options = options'
        member __.Plugins = plugins'
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
    printfn "Parsing F# project..."
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors, warnings =
        parseErrors checkProjectResults.Errors
    if errors.Length = 0
    then
        printfn "F# project parsed successfully"
        warnings |> Array.map Warning, checkProjectResults
    else errors
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> FableError |> raise

let createState checker projectOptions (com: ICompiler) (define: string[]) projFile =
    let projectOptions =
        match projectOptions with
        | Some projectOptions -> projectOptions
        | None ->
            let projectOptions = getFullProjectOpts checker define projFile
            if projFile.EndsWith(".fsproj") then
                printfn "F# PROJECT: %s" projectOptions.ProjectFileName
                // for option in projectOptions.OtherOptions do
                //     printfn "%s" option
                for file in projectOptions.ProjectFileNames do
                    printfn "   %s" file
            projectOptions
    let logs, checkedProject =
        parseFSharpProject checker projectOptions
    for log in logs do
        com.AddLog(log)
    State(projectOptions, checkedProject)

let toJsonAndReply =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore,
            StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (replyChannel: string->unit) (value: obj) ->
        JsonConvert.SerializeObject(value, jsonSettings) |> replyChannel

let sendError replyChannel (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    match ex with
    | :? FableError as err ->
        let errMessage = err.FormattedMessage
        // Don't print stack trace for known Fable errors
        printfn "%s" errMessage
        ["error", errMessage] |> dict |> toJsonAndReply replyChannel
    | ex ->
        let stack = innerStack ex
        printfn "ERROR: %s\n%s" ex.Message stack
        ["error", ex.Message] |> dict |> toJsonAndReply replyChannel

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

let updateState (checker: FSharpChecker) (com: Compiler) (state: State option)
                astOutDir (define: string[]) (fileName: string) =
    let createFromScratch () =
        let state = createState checker None com define fileName
        astOutDir |> Option.iter (fun dir -> Printers.printAst dir state.CheckedProject)
        state
    match state with
    | Some state ->
        if fileName.EndsWith(".fsproj") || fileName = state.ProjectFile // Must be an .fsx script
        then createFromScratch()
        else
            match state.CompiledFiles.TryGetValue(fileName) with
            // Watch compilation, restart state
            | true, true ->
                createState checker (Some state.ProjectOptions) com define state.ProjectFile
            | true, false ->
                // Set file as already compiled
                state.CompiledFiles.[fileName] <- true
                state
            | false, _ when fileName.EndsWith(".fsx") ->
                createState checker None com define fileName
            | false, _ ->
                sprintf "%s doesn't belong to project %s" fileName state.ProjectFile
                |> FableError |> raise
    | None -> createFromScratch()

let compile (com: Compiler) (state: State) (fileName: string) =
    if fileName.EndsWith(".fsproj")
    then
        let lastFile = Array.last state.ProjectOptions.ProjectFileNames
        Fable2Babel.Compiler.createFacade fileName lastFile
    else
        printfn "Compile %s" fileName
        FSharp2Fable.Compiler.transformFile com state state.CheckedProject fileName
        |> Fable2Babel.Compiler.transformFile com state
    |> addLogs com

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let rec loop (checker: FSharpChecker) (com: Compiler) (state: State option) = async {
        let! msg, replyChannel = agent.Receive()
        try
            let msg = Parser.parse msg
            let astOutDir =
                match msg.extra.TryGetValue("saveAst") with
                | true, value -> Some value
                | _ -> None
            com.Reset(msg.options, loadPlugins msg.plugins (com :> ICompiler).Plugins)
            let state = updateState checker com state astOutDir msg.define msg.path
            async {
                try
                    compile com state msg.path
                    |> toJsonAndReply replyChannel
                with ex ->
                    sendError replyChannel ex
            } |> Async.Start
            return! loop checker com (Some state)
        with ex ->
            sendError replyChannel ex
            return! loop checker com state
    }
    let compiler = Compiler()
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    loop checker compiler None
  )
