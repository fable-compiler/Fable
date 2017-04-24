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
        | false, _ -> let v = valueFactory(key) in x.Add(key, v); v
    member x.AddOrUpdate (key, valueFactory, updateFactory) =
        if x.ContainsKey(key)
        then let v = updateFactory key x.[key] in x.[key] <- v; v
        else let v = valueFactory(key) in x.Add(key, v); v

type ConcurrentDictionary<'TKey, 'TValue> = Dictionary<'TKey, 'TValue>
#endif

type Project(projectOptions: FSharpProjectOptions, checkedProject: FSharpCheckProjectResults) =
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

type State() =
    let projects = ConcurrentDictionary<string, Project>()
    let mutable active = Unchecked.defaultof<Project>
    member __.Projects = projects.Values
    member __.ActiveProject = active
    member __.IsLoadedProject(projectFileName) =
        projects.ContainsKey(projectFileName)
    member self.UpdateProject(project: Project) =
        projects.AddOrUpdate(project.ProjectFile, (fun _ -> project), (fun _ _ -> project)) |> ignore
        active <- project
        self

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

let createProject checker projectOptions (com: ICompiler) (define: string[]) projFile =
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
    Project(projectOptions, checkedProject)

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

let getExtension (fileName: string) =
    let i = fileName.LastIndexOf(".")
    fileName.Substring(i).ToLower()

let updateState (checker: FSharpChecker) (com: Compiler) (state: State) astOutDir (define: string[]) (fileName: string): State =
    let createProject options projectFile =
        let project = createProject checker options com define projectFile
        astOutDir |> Option.iter (fun dir -> Printers.printAst dir project.CheckedProject)
        project
    let tryFindAndUpdateProject ext sourceFile =
        // TODO: Optimize so the ActiveProject is searched first
        state.Projects |> Seq.tryPick (fun project ->
            match project.CompiledFiles.TryGetValue(sourceFile) with
            | true, alreadyCompiled ->
                let project =
                    // When a script is modified, restart the project with new options
                    // (to check for new references, loaded projects, etc.)
                    if ext = ".fsx"
                    then createProject None project.ProjectFile
                    // Watch compilation of an .fs file, restart project with old options
                    elif alreadyCompiled
                    then createProject (Some project.ProjectOptions) project.ProjectFile
                    else project
                // Set file as already compiled
                project.CompiledFiles.[sourceFile] <- true
                Some project
            | false, _ -> None)
    match getExtension fileName with
    | ".fsproj" ->
        state.UpdateProject(createProject None fileName)
    | ".fsx" as ext ->
        if state.IsLoadedProject(fileName)
        then state.UpdateProject(createProject None fileName)
        else
            match tryFindAndUpdateProject ext fileName with
            | Some project -> state.UpdateProject(project)
            | None -> state.UpdateProject(createProject None fileName)        
    | ".fs" as ext ->        
            match tryFindAndUpdateProject ext fileName with
            | Some project -> state.UpdateProject(project)
            | None ->
                state.Projects |> Seq.map (fun x -> x.ProjectFile) |> Seq.toList
                |> failwithf "%s doesn't belong to any of loaded projects %A" fileName
    | ".fsi" -> failwithf "Signature files cannot be compiled to JS: %s" fileName
    | _ -> failwithf "Not an F# source file: %s" fileName

let compile (com: Compiler) (project: Project) (fileName: string) =
    if fileName.EndsWith(".fsproj")
    then
        let lastFile = Array.last project.ProjectOptions.ProjectFileNames
        Fable2Babel.Compiler.createFacade fileName lastFile
    else
        printfn "Compile %s" fileName
        FSharp2Fable.Compiler.transformFile com project project.CheckedProject fileName
        |> Fable2Babel.Compiler.transformFile com project
    |> addLogs com

type Command = string * (string -> unit)

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let rec loop (checker: FSharpChecker) (com: Compiler) (state: State) = async {
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
                    compile com state.ActiveProject msg.path
                    |> toJsonAndReply replyChannel
                with ex ->
                    sendError replyChannel ex
            } |> Async.Start
            return! loop checker com state
        with ex ->
            sendError replyChannel ex
            return! loop checker com state
    }
    let compiler = Compiler()
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    loop checker compiler <| State()
  )

#endif //!FABLE_COMPILER
