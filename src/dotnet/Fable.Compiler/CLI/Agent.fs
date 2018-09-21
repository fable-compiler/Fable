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

/// File.ReadAllText fails with locked files. See https://stackoverflow.com/a/1389172
let readAllText path =
    use fileStream = new IO.FileStream(path, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
    use textReader = new IO.StreamReader(fileStream)
    textReader.ReadToEnd()

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

let checkFableCoreVersion (checkedProject: FSharpCheckProjectResults) =
    for ref in checkedProject.ProjectContext.GetReferencedAssemblies() do
        if ref.SimpleName = "Fable.Core" then
            let version = System.Text.RegularExpressions.Regex.Match(ref.QualifiedName, @"Version=(\d+\.\d+\.\d+)")
            if version.Groups.[1].Value <> Literals.CORE_VERSION then
                failwithf "Fable.Core v%s detected, expecting v%s" version.Groups.[1].Value Literals.CORE_VERSION
            // else printfn "Fable.Core version matches"

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
    match prevProject with
    | Some prevProject when isWatchCompile ->
        ((prevProject.ImplementationFiles, [||]), dirtyFiles) ||> Async.fold (fun (implFiles, errors) dirtyFile ->
            let relativePath = getRelativePath dirtyFile
            Log.logAlways(sprintf "Parsing %s..." relativePath)
            let source = readAllText(dirtyFile)
            // About this parameter, see https://github.com/fsharp/FSharp.Compiler.Service/issues/796#issuecomment-333094956
            let version = IO.File.GetLastWriteTime(dirtyFile).Ticks |> int
            // TODO: results.Errors are different from res.Errors below?
            checker.ParseAndCheckFileInProject(dirtyFile, version, source, projectOptions)
            |> Async.map (fun (_results, answer) ->
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
            ))
    | _ ->
        Log.logAlways(sprintf "Parsing %s..." (getRelativePath projectOptions.ProjectFileName))
        // There's a performance penalty if LoadTime is not updated
        // when parsing the full project (ParseAndCheckProject)
        { projectOptions with LoadTime = DateTime.Now }
        |> checker.ParseAndCheckProject
        |> Async.map (fun checkedProject ->
            checkFableCoreVersion checkedProject
            let optimized = GlobalParams.Singleton.Experimental.Contains("optimize-fcs")
            let implFiles =
                if not optimized
                then checkedProject.AssemblyContents.ImplementationFiles
                else checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
            let implFilesMap =
                implFiles
                |> Seq.map (fun file -> Path.normalizePath file.FileName, file) |> Map
            tryGetOption "saveAst" msg.extra |> Option.iter (fun outDir ->
                Printers.printAst outDir implFiles)
            implFilesMap, checkedProject.Errors)
    |> Async.map (fun (implFiles, errors) ->
        Project(projectOptions, implFiles, errors, deps, fableCore, isWatchCompile))

let jsonSettings =
    JsonSerializerSettings(
        Converters=[|Json.ErasedUnionConverter()|],
        ContractResolver=Serialization.CamelCasePropertyNamesContractResolver(),
        NullValueHandling=NullValueHandling.Ignore)
        // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)

let toJson (msgHandler: Server.MessageHandler) (value: obj) =
        use writer = msgHandler.ResponseStream
        use jsonWriter = new JsonTextWriter(writer)
        let serializer = JsonSerializer.Create(jsonSettings)
        serializer.Serialize(jsonWriter, value)
        jsonWriter.Flush()

let sendError msgHandler (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let stack = innerStack ex
    Log.logAlways(sprintf "ERROR: %s\n%s" ex.Message stack)
    ["error", ex.Message] |> dict |> toJson msgHandler

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
                |> Async.map (addOrUpdateProject state >> Some)
            else
                Some(false, state, project) |> async.Return
        match msg.extra.TryGetValue("projectFile") with
        | true, projFile ->
            let projFile = Path.normalizeFullPath projFile
            match Map.tryFind projFile state with
            | Some project -> checkWatchCompilation project sourceFile
            | None -> createProject checker [||] None msg projFile
                      |> Async.map (addOrUpdateProject state >> Some)
        | false, _ ->
            state |> Async.tryPick (fun (KeyValue(_, project: Project)) ->
                if project.ContainsFile(sourceFile)
                then checkWatchCompilation project sourceFile
                else async.Return None)
    match IO.Path.GetExtension(msg.path).ToLower() with
    | ".fsproj" ->
        createProject checker [||] None msg msg.path
        |> Async.map (addOrUpdateProject state)
    | ".fsx" ->
        if Map.containsKey msg.path state then
            // When a script is modified, restart the project with new options
            // (to check for new references, loaded projects, etc.)
            createProject checker [||] None msg msg.path
            |> Async.map (addOrUpdateProject state)
        else
            tryFindAndUpdateProject state msg.path
            |> Async.orElse (fun () ->
                createProject checker [||] None msg msg.path
                |> Async.map (addOrUpdateProject state))
    | ".fs" ->
        tryFindAndUpdateProject state msg.path
        |> Async.orElse (fun () ->
            state |> Map.map (fun _ p -> p.ProjectFile) |> Seq.toList
            |> failwithf "%s doesn't belong to any of loaded projects %A" msg.path)
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

/// Don't await file compilation to let the agent receive more requests to implement files.
let startCompilation (msgHandler: Server.MessageHandler) (com: Compiler) (project: Project) =
    async {
        try
            if com.CurrentFile.EndsWith(".fsproj") then
                // If we compile the last file here, Webpack watcher will ignore changes in it
                Fable2Babel.Compiler.createFacade project.ProjectOptions.SourceFiles com.CurrentFile
                |> toJson msgHandler
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
                Babel.Program(babel.FileName, babel.Body, babel.Directives, com.GetFormattedLogs())
                |> toJson msgHandler
        with ex ->
            sendError msgHandler ex
    } |> Async.Start

let startAgent () = MailboxProcessor<Server.MessageHandler>.Start(fun agent ->
    let rec loop (checker: FSharpChecker) (state: Map<string,Project>) = async {
        use! msgHandler = agent.Receive()
        try
            let msg = Parser.parse msgHandler.Message
            // lazy sprintf "Received message %A" msg |> Log.logVerbose
            let! isUpdated, newState, activeProject = updateState checker state msg
            let com = Compiler(msg.path, activeProject, Parser.toCompilerOptions msg)
            // If the project has been updated and this is a watch compilation, add
            // F# errors/warnings here so they're not skipped if they affect another file
            if isUpdated && activeProject.IsWatchCompile then
                addFSharpErrorLogs com activeProject.Errors None
            startCompilation msgHandler com activeProject
            return! loop checker newState
        with ex ->
            sendError msgHandler ex
            return! loop checker state
    }
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    loop checker Map.empty
  )
