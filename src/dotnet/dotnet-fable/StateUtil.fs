module Fable.CLI.StateUtil

open Fable
open Fable.AST
open Fable.State
open System
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open ProjectCracker

module private Cache =
    let fableCore = Dictionary<string,string>()
    let plugins = Dictionary<string,PluginInfo list>()
    let add (cache: Dictionary<'K,'V>) key value =
        cache.Add(key, value)
        value

let loadAssembly path =
#if NETFX
    Assembly.LoadFrom(path)
#else
    let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
    globalLoadContext.LoadFromAssemblyPath(path)
#endif

let loadPlugins pluginPaths =
    pluginPaths
    |> Seq.collect (fun path ->
        let path = Path.normalizeFullPath path
        match Cache.plugins.TryGetValue(path) with
        | true, pluginInfos -> pluginInfos
        | false, _ ->
            try
                loadAssembly path
                |> Reflection.getTypes
                |> Seq.filter typeof<IPlugin>.IsAssignableFrom
                |> Seq.map (fun x ->
                    { path = path
                    ; plugin = Activator.CreateInstance x :?> IPlugin })
                |> Seq.toList
                |> Cache.add Cache.plugins path
            with
            | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
    |> Seq.toList

let getRelativePath path =
    Path.getRelativePath (IO.Directory.GetCurrentDirectory()) path

let hasFlag flagName (opts: IDictionary<string, string>) =
    match opts.TryGetValue(flagName) with
    | true, value ->
        match bool.TryParse(value) with
        | true, value -> value
        | _ -> false
    | _ -> false

let tryGetOption name (opts: IDictionary<string, string>) =
    match opts.TryGetValue(name) with
    | true, value -> Some value
    | _ -> None

let createProject checker (prevOptions: FSharpProjectOptions option) (msg: Parser.Message) projFile =
    let projectOptions =
        match prevOptions with
        | Some projectOptions -> projectOptions
        | None ->
            let projectOptions = getFullProjectOpts checker msg.define projFile
            if projFile.EndsWith(".fsproj") then
                Log.logVerbose("F# PROJECT: " + projectOptions.ProjectFileName)
                for option in projectOptions.OtherOptions do
                     Log.logVerbose("   " + option)
                for file in projectOptions.ProjectFileNames do
                    Log.logVerbose("   " + file)
            projectOptions
    Log.logAlways(sprintf "Parsing %s..." (getRelativePath projectOptions.ProjectFileName))
    let checkedProject =
        projectOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    tryGetOption "saveAst" msg.extra |> Option.iter (fun dir ->
        Printers.printAst dir checkedProject)
    Project(projectOptions, checkedProject)

let toJson =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            NullValueHandling=NullValueHandling.Ignore)
            // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
    fun (value: obj) ->
        JsonConvert.SerializeObject(value, jsonSettings)

let sendError replyChannel (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let stack = innerStack ex
    Log.logAlways(sprintf "ERROR: %s\n%s" ex.Message stack)
    ["error", ex.Message] |> dict |> toJson |> replyChannel

let updateState (checker: FSharpChecker) (state: State) (msg: Parser.Message): State * Project =
    let addOrUpdateProject state (project: Project) =
        let state = Map.add project.ProjectFile project state
        state, project
    let tryFindAndUpdateProject state ext sourceFile =
        state |> Map.tryPick (fun _ (project: Project) ->
            if List.contains sourceFile project.NormalizedFiles then
                // (getRelativePath sourceFile, getRelativePath project.ProjectFile)
                // ||> sprintf "File %s belongs to project %s" |> Log.logVerbose
                // When a script is modified, restart the project with new options
                // (to check for new references, loaded projects, etc.)
                if ext = ".fsx" then
                    createProject checker None msg project.ProjectFile
                    |> addOrUpdateProject state |> Some
                // Watch compilation of an .fs file, restart project with old options
                elif IO.File.GetLastWriteTimeUtc(sourceFile) > project.TimeStamp then
                    let prevOptions = Some project.ProjectOptions
                    createProject checker prevOptions msg project.ProjectFile
                    |> addOrUpdateProject state |> Some
                else Some(state, project)
            else None)
    match IO.Path.GetExtension(msg.path).ToLower() with
    | ".fsproj" ->
        createProject checker None msg msg.path
        |> addOrUpdateProject state
    | ".fsx" as ext ->
        match Map.tryFind msg.path state with
        | Some project ->
            createProject checker None msg msg.path
            |> addOrUpdateProject state
        | None ->
            match tryFindAndUpdateProject state ext msg.path with
            | Some stateAndProject -> stateAndProject
            | None ->
                createProject checker None msg msg.path
                |> addOrUpdateProject state
    | ".fs" as ext ->
        match tryFindAndUpdateProject state ext msg.path with
        | Some stateAndProject -> stateAndProject
        | None ->
            state |> Map.map (fun _ p -> p.ProjectFile) |> Seq.toList
            |> failwithf "%s doesn't belong to any of loaded projects %A" msg.path
    | ".fsi" -> failwithf "Signature files cannot be compiled to JS: %s" msg.path
    | _ -> failwithf "Not an F# source file: %s" msg.path

let resolveFableCoreDir fableCoreDir projectFile currentFile =
    let trim (s: string) = s.TrimEnd('/')
    match fableCoreDir with
    | "fable-core" ->
        // Resolve the fable-core location if not defined by user
        match Cache.fableCore.TryGetValue(projectFile) with
        | true, fableCoreDir -> fableCoreDir
        | false, _ ->
            match ProjectCracker.tryGetFableCoreJsDir projectFile with
            | Some fableCoreDir -> Cache.add Cache.fableCore projectFile fableCoreDir
            | None -> failwith "Cannot find fable-core directory"
    | fableCoreDir -> fableCoreDir
    |> Path.getRelativePath currentFile |> trim

let compile (com: Compiler) (project: Project) (filePath: string) =
    let babel =
        if filePath.EndsWith(".fsproj") then
            let lastFile = Array.last project.ProjectOptions.ProjectFileNames
            Fable2Babel.Compiler.createFacade filePath lastFile
        else
            FSharp2Fable.Compiler.transformFile com project project.CheckedProject filePath
            |> Fable2Babel.Compiler.transformFile com project
    // Add logs and convert to JSON
    for er in project.CheckedProject.Errors do
        if (Path.normalizeFullPath er.FileName) = filePath then
            let severity =
                match er.Severity with
                | FSharpErrorSeverity.Warning -> Severity.Warning
                | FSharpErrorSeverity.Error -> Severity.Error
            let range =
                { start={ line=er.StartLineAlternate; column=er.StartColumn}
                ; ``end``={ line=er.EndLineAlternate; column=er.EndColumn} }
            (com :> ICompiler).AddLog(er.Message, severity, range, er.FileName, "FSHARP")
    let loc = defaultArg babel.loc SourceLocation.Empty
    Babel.Program(babel.fileName, loc, babel.body, babel.directives, com.ReadAllLogs())
    |> toJson

type Command = string * (string -> unit)

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let rec loop (checker: FSharpChecker) (state: State) = async {
        let! msg, replyChannel = agent.Receive()
        let newState =
            try
                let msg = Parser.parse msg
                // sprintf "Received message %A" msg |> Log.logVerbose
                let state, activeProject = updateState checker state msg
                // Resolve fableCore location and create new compiler
                let fableCoreDir = resolveFableCoreDir msg.options.fableCore activeProject.ProjectFile msg.path
                let com = Compiler({msg.options with fableCore=fableCoreDir}, loadPlugins msg.plugins)
                Some(com, state, activeProject, msg.path)
            with ex ->
                sendError replyChannel ex
                None
        match newState with
        | Some(com, state, activeProject, filePath) ->
            async {
                try
                    compile com activeProject filePath |> replyChannel
                with ex ->
                    sendError replyChannel ex
            } |> Async.Start
            return! loop checker state
        | None ->
            return! loop checker state
    }
    let checker = FSharpChecker.Create(keepAssemblyContents=true, msbuildEnabled=false)
    loop checker Map.empty
  )
