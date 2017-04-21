module Fable.Tools.State

open Fable
open Fable.AST
open System
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

#if !FABLE_COMPILER
open Newtonsoft.Json
open ProjectCracker
open Parser
#endif

#if FABLE_COMPILER
type Dictionary<'TKey, 'TValue> with
    member x.GetOrAdd (key, valueFactory) =
        match x.TryGetValue key with
        | true, v -> v
        | false, _ -> let v = valueFactory(key) in x.[key] <- v; v

type ConcurrentDictionary<'TKey, 'TValue> = Dictionary<'TKey, 'TValue>
#endif

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
        member this.ProjectFile = projectOptions.ProjectFileName
        member this.GetRootModule(fileName) =
            match Map.tryFind fileName rootModules with
            | Some rootModule -> rootModule
            | None -> failwithf "Cannot find root module for %s" fileName
        member this.GetOrAddEntity(fullName, generate) =
            entities.GetOrAdd(fullName, fun _ -> generate())
        member this.GetOrAddInlineExpr(fullName, generate) =
            inlineExprs.GetOrAdd(fullName, fun _ -> generate())

let getDefaultOptions() =
    { fableCore = "fable-core"
    ; declaration = false
    ; typedArrays = true
    ; clampByteArrays = false }

type Compiler() =
    let mutable id = 0
    let mutable options' = getDefaultOptions()
    let mutable plugins': PluginInfo list = []
    let logs = Dictionary<string,ResizeArray<string>>()
    member __.Logs: IDictionary<string,string[]> =
        logs |> Seq.map (fun kv -> kv.Key, Seq.toArray kv.Value) |> dict
    member __.Reset(?options, ?plugins) =
        id <- 0
        options' <- defaultArg options options'
        plugins' <- defaultArg plugins plugins'
        logs.Clear()
    interface ICompiler with
        member __.Options = options'
        member __.Plugins = plugins'
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            let tag = defaultArg tag "FABLE"
            let severity =
                match severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"
            let formattedMsg =
                match fileName with
                | Some file ->
                    let range =
                        match range with
                        | Some r -> sprintf "%i,%i,%i,%i" r.start.line r.start.column r.``end``.line r.``end``.column
                        | None -> "1,1,1,1"
                    sprintf "%s(%s) : %s %s: %s" file range severity tag msg
                | None -> msg
            match logs.TryGetValue(severity) with
            | true, v -> v.Add(formattedMsg)
            | false, _ -> logs.Add(severity, ResizeArray[|formattedMsg|])
        member __.GetUniqueVar() =
#if FABLE_COMPILER
            id <- id + 1; "$var" + (string id)
#else
            "$var" + (System.Threading.Interlocked.Increment(&id).ToString())
#endif

#if !FABLE_COMPILER

let loadAssembly path =
#if NETFX
    Assembly.LoadFrom(path)
#else
    let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
    globalLoadContext.LoadFromAssemblyPath(path)
#endif

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
            | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
    |> Seq.toList

// --- perhaps not used anymore? ---
// /// Returns an (errors, warnings) tuple
// let parseErrors errors =
//     let parseError (er: FSharpErrorInfo) =
//         let isError, severity =
//             match er.Severity with
//             | FSharpErrorSeverity.Warning -> false, "warning"
//             | FSharpErrorSeverity.Error -> true, "error"
//         isError, sprintf "%s(L%i,%i) : %s FSHARP: %s"
//             er.FileName er.StartLineAlternate er.StartColumn
//             severity er.Message
//     errors
//     |> Array.map parseError
//     |> Array.partition fst
//     |> fun (ers, wns) -> Array.map snd ers, Array.map snd wns

let parseFSharpProject (checker: FSharpChecker) (projOptions: FSharpProjectOptions) (com: ICompiler) =
    printfn "Parsing F# project..."
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    for er in checkProjectResults.Errors do
        let severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> Severity.Warning
            | FSharpErrorSeverity.Error -> Severity.Error
        let range =
            { start={ line=er.StartLineAlternate; column=er.StartColumn}
            ; ``end``={ line=er.EndLineAlternate; column=er.EndColumn} }
        com.AddLog(er.Message, severity, range, er.FileName, "FSHARP")
    checkProjectResults

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
    let checkedProject = parseFSharpProject checker projectOptions com
    State(projectOptions, checkedProject)

let toJsonAndReply =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore)
            // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (replyChannel: string->unit) (value: obj) ->
        JsonConvert.SerializeObject(value, jsonSettings) |> replyChannel

let sendError replyChannel (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let stack = innerStack ex
    printfn "ERROR: %s\n%s" ex.Message stack
    ["error", ex.Message] |> dict |> toJsonAndReply replyChannel

let addLogs (com: Compiler) (file: Babel.Program) =
    let loc = defaultArg file.loc SourceLocation.Empty
    Babel.Program(file.fileName, loc, file.body, file.directives, com.Logs)

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
            | true, alreadyCompiled ->
                // Watch compilation, restart state
                let state =
                    if alreadyCompiled
                    then createState checker (Some state.ProjectOptions) com define state.ProjectFile
                    else state
                // Set file as already compiled
                state.CompiledFiles.[fileName] <- true
                state
            | false, _ when fileName.EndsWith(".fsx") ->
                createState checker None com define fileName
            | false, _ ->
                failwithf "%s doesn't belong to project %s" fileName state.ProjectFile
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

type Command = string * (string -> unit)

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

#endif //!FABLE_COMPILER
