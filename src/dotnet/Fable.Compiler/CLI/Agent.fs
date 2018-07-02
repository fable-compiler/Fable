module Fable.CLI.Agent

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open ProjectCracker

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
    | false, _ -> None

let createProject checker dirtyFiles (prevProject: Project option) (msg: Parser.Message) projFile =
    let isWatchCompile = Array.length dirtyFiles > 0
    let projectOptions, fableCore, deps =
        match prevProject with
        | Some prevProject ->
            prevProject.ProjectOptions, prevProject.FableCore, prevProject.GetDependencies()
        | None ->
            let projectOptions, fableCore =
                getFullProjectOpts checker msg.define msg.rootDir projFile
            Log.logVerbose(lazy
                let proj = getRelativePath projectOptions.ProjectFileName
                let opts = projectOptions.OtherOptions |> String.concat "\n   "
                let files = projectOptions.SourceFiles |> String.concat "\n   "
                sprintf "F# PROJECT: %s\n   %s\n   %s" proj opts files)
            projectOptions, fableCore, Map.empty
    let implFiles, errors =
        match prevProject with
        | Some prevProject when isWatchCompile ->
            ((prevProject.ImplementationFiles, [||]), dirtyFiles) ||> Array.fold (fun (implFiles, errors) dirtyFile ->
                let relativePath = getRelativePath dirtyFile
                Log.logAlways(sprintf "Parsing %s..." relativePath)
                let source = IO.File.ReadAllText(dirtyFile)
                // About this parameter, see https://github.com/fsharp/FSharp.Compiler.Service/issues/796#issuecomment-333094956
                let version = IO.File.GetLastWriteTime(dirtyFile).Ticks |> int
                // TODO: results.Errors are different from res.Errors below?
                let _results, answer =
                    checker.ParseAndCheckFileInProject(dirtyFile, version, source, projectOptions)
                    |> Async.RunSynchronously
                match answer with
                | FSharpCheckFileAnswer.Aborted ->
                    Log.logAlways(sprintf "Compilation of file %s aborted!" relativePath)
                    implFiles, errors
                | FSharpCheckFileAnswer.Succeeded res ->
                    match res.ImplementationFile with
                    | None -> implFiles, errors // TODO: Log a message?
                    | Some newImplFile ->
                        let errors = Array.append errors res.Errors
                        let fileName = Path.normalizePath newImplFile.FileName
                        Map.add fileName newImplFile implFiles, errors
            )
        | _ ->
            Log.logAlways(sprintf "Parsing %s..." (getRelativePath projectOptions.ProjectFileName))
            let checkedProject =
                // There's a performance penalty if LoadTime is not updated
                // when parsing the full project (ParseAndCheckProject)
                { projectOptions with LoadTime = DateTime.Now }
                |> checker.ParseAndCheckProject
                |> Async.RunSynchronously
            tryGetOption "saveAst" msg.extra |> Option.iter (fun dir ->
                Printers.printAst dir checkedProject)
            // let optimized = false // TODO: from compiler option
            let implFiles =
                // if optimized then checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
                // else
                checkedProject.AssemblyContents.ImplementationFiles
                |> Seq.map (fun file -> Path.normalizePath file.FileName, file) |> Map
            implFiles, checkedProject.Errors
    Project(projectOptions, implFiles, errors, deps, fableCore, isWatchCompile)

let toJson =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            ContractResolver=Serialization.CamelCasePropertyNamesContractResolver(),
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

let updateState (checker: FSharpChecker) (state: Map<string,Project>) (msg: Parser.Message) =
    let getDirtyFiles (project: Project) sourceFile =
        let isDirty = IO.File.GetLastWriteTime(sourceFile) > project.TimeStamp
        // In watch compilations, always recompile the requested file in case it has non-solved errors
        if isDirty || project.HasSent(sourceFile) then
            project.ProjectOptions.SourceFiles
            |> Array.filter (fun file ->
                let file = Path.normalizePath file
                file = sourceFile || IO.File.GetLastWriteTime(file) > project.TimeStamp)
            |> project.GetFilesAndDependent
        else [||]
    let addOrUpdateProject state (project: Project) =
        let state = Map.add project.ProjectFile project state
        true, state, project
    let tryFindAndUpdateProject state sourceFile =
        let checkWatchCompilation project sourceFile =
            let dirtyFiles = getDirtyFiles project sourceFile
            if Array.length dirtyFiles > 0 then
                createProject checker dirtyFiles (Some project) msg project.ProjectFile
                |> addOrUpdateProject state |> Some
            else
                Some(false, state, project)
        match msg.extra.TryGetValue("projectFile") with
        | true, projFile ->
            let projFile = Path.normalizeFullPath projFile
            match Map.tryFind projFile state with
            | Some project -> checkWatchCompilation project sourceFile
            | None -> createProject checker [||] None msg projFile
                      |> addOrUpdateProject state |> Some
        | false, _ ->
            state |> Map.tryPick (fun _ (project: Project) ->
                if project.ContainsFile(sourceFile)
                then checkWatchCompilation project sourceFile
                else None)
    match IO.Path.GetExtension(msg.path).ToLower() with
    | ".fsproj" ->
        createProject checker [||] None msg msg.path
        |> addOrUpdateProject state
    | ".fsx" ->
        if Map.containsKey msg.path state then
            // When a script is modified, restart the project with new options
            // (to check for new references, loaded projects, etc.)
            createProject checker [||] None msg msg.path
            |> addOrUpdateProject state
        else
            match tryFindAndUpdateProject state msg.path with
            | Some stateAndProject -> stateAndProject
            | None ->
                createProject checker [||] None msg msg.path
                |> addOrUpdateProject state
    | ".fs" ->
        match tryFindAndUpdateProject state msg.path with
        | Some stateAndProject -> stateAndProject
        | None ->
            state |> Map.map (fun _ p -> p.ProjectFile) |> Seq.toList
            |> failwithf "%s doesn't belong to any of loaded projects %A" msg.path
    | ".fsi" -> failwithf "Signature files cannot be compiled to JS: %s" msg.path
    | _ -> failwithf "Not an F# source file: %s" msg.path

let addFSharpErrorLogs (com: ICompiler) (errors: FSharpErrorInfo array) (fileFilter: string option) =
    let errors =
        match fileFilter with
        | Some file -> errors |> Array.filter (fun er -> (Path.normalizePath er.FileName) = file)
        | None -> errors
    errors |> Seq.map (fun er ->
        let severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> Severity.Warning
            | FSharpErrorSeverity.Error -> Severity.Error
        let range =
            { start={ line=er.StartLineAlternate; column=er.StartColumn}
              ``end``={ line=er.EndLineAlternate; column=er.EndColumn} }
        er.FileName, range, severity, er.Message)
    |> Seq.distinct // Sometimes errors are duplicated
    |> Seq.iter (fun (fileName, range, severity, msg) ->
        com.AddLog(msg, severity, range, fileName, "FSHARP"))

let compile (com: Compiler) (project: Project) =
    if com.CurrentFile.EndsWith(".fsproj") then
        // If we compile the last file here, Webpack watcher will ignore changes in it
        Fable2Babel.Compiler.createFacade project.ProjectOptions.SourceFiles com.CurrentFile
        |> toJson
    else
        let babel =
            FSharp2Fable.Compiler.transformFile com project.ImplementationFiles
            |> FableTransforms.optimizeFile com
            |> Fable2Babel.Compiler.transformFile com
        // If this is the first compilation, add errors to each respective file
        if not project.IsWatchCompile then
            addFSharpErrorLogs com project.Errors (Some com.CurrentFile)
        project.MarkSent(com.CurrentFile)
        // Don't send dependencies to JS client (see #1241)
        project.AddDependencies(com.CurrentFile, babel.Dependencies)
        Babel.Program(babel.FileName, babel.Body, babel.Directives, com.ReadAllLogs())
        |> toJson

type Command = string * (string -> unit)

let startAgent () = MailboxProcessor<Command>.Start(fun agent ->
    let rec loop (checker: FSharpChecker) (state: Map<string,Project>) = async {
        let! msg, replyChannel = agent.Receive()
        let newState =
            try
                let msg = Parser.parse msg
                // lazy sprintf "Received message %A" msg |> Log.logVerbose
                let isUpdated, state, activeProject = updateState checker state msg
                let com = Compiler(msg.path, activeProject, Parser.toCompilerOptions msg)
                // If the project has been updated and this is a watch compilation, add
                // F# errors/warnings here so they're not skipped if they affect another file
                if isUpdated && activeProject.IsWatchCompile then
                    addFSharpErrorLogs com activeProject.Errors None
                Some(state, activeProject, com)
            with ex ->
                sendError replyChannel ex
                None
        match newState with
        | Some(state, activeProject, com) ->
            async {
                try
                    compile com activeProject |> replyChannel
                with ex ->
                    sendError replyChannel ex
            } |> Async.Start
            return! loop checker state
        | None ->
            return! loop checker state
    }
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    loop checker Map.empty
  )
