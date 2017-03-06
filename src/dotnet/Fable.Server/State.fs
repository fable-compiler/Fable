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
#if DOTNETCORE
    let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
    globalLoadContext.LoadFromAssemblyPath(path)
#else
    Assembly.LoadFrom(path)
#endif

type Compiler(options, plugins, ?loadedPlugins: PluginInfo list) =
    let mutable id = 0
    let logs = ConcurrentBag()
    let plugins =
        let loadedPlugins =
            defaultArg loadedPlugins []
            |> Seq.groupBy (fun p -> p.path) |> Map
        plugins
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
    interface ICompiler with
        member __.CoreLib = "fable-core" // TODO
        member __.Options = options
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetUniqueVar() =
            "$var" + (System.Threading.Interlocked.Increment(&id).ToString())

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

let createState checker projectOptions com (msg: Message) projFile =
    let com = Compiler(msg.options, msg.plugins, (com :> ICompiler).Plugins)
    let projectOptions =
        match projectOptions with
        | Some projectOptions -> projectOptions
        | None -> getFullProjectOpts checker msg.define projFile
    let logs, checkedProject =
        // TODO: Do this asynchronously?
        parseFSharpProject checker projectOptions
    // TODO: Add warnings to compiler
    com, State(projectOptions, checkedProject)

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let toJsonAndReply =
        let jsonSettings =
            JsonSerializerSettings(
                Converters=[|Json.ErasedUnionConverter()|],
                NullValueHandling=NullValueHandling.Ignore,
                StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
        fun (replyChannel: string->unit) (file: Babel.Program) ->
            JsonConvert.SerializeObject (file, jsonSettings) |> replyChannel
    let rec loop (checker: FSharpChecker) (com: Compiler) (state: State option) = async {
        let! (Parse msg), replyChannel = agent.Receive()
        let state =
            try
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
                                com, state
                            | false, _ when msg.path.EndsWith(".fsx") ->
                                createState checker None com msg msg.path
                            | false, _ ->
                                sprintf "%s doesn't belong to project %s" msg.path state.ProjectFile
                                |> FableError |> raise
                    | None ->
                        createState checker None com msg msg.path
                // TODO: Start this in a background task to receive other messages?
                // If path is an .fsproj change it to the last file in project
                let fileName =
                    if msg.path.EndsWith(".fsproj")
                    then Array.last state.ProjectOptions.ProjectFileNames
                    else msg.path
                // Set file as already compiled
                state.CompiledFiles.[fileName] <- true
                FSharp2Fable.Compiler.transformFile com state state.CheckedProject fileName
                |> Fable2Babel.Compiler.transformFile com state
                |> toJsonAndReply replyChannel
                Some state
            with
            | ex ->
                printfn "%s %s" ex.Message ex.StackTrace
                state
        do! loop checker com state
    }
    let compiler = Compiler(getDefaultOptions(), [||])
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    loop checker compiler None
  )
