module rec Fable.Cli.Agent

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open System
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open ProjectCracker

let agentBody (agent: MailboxProcessor<AgentMsg>) =
    let jsonSettings =
        JsonSerializerSettings(
            Converters=[|Json.ErasedUnionConverter()|],
            ContractResolver=Serialization.CamelCasePropertyNamesContractResolver(),
            NullValueHandling=NullValueHandling.Ignore)
            // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)

    let rec loop (state: Map<string, ProjectWrapper>) = async {
      match! agent.Receive() with
      | Parsed(projFile, proj, checker) ->
        match Map.tryFind projFile state with
        | None ->
            Log.always("Project parsed but proj file not found in state: " + projFile)
            return! loop state
        | Some projWrapper ->
            let stackedMessages, projWrapper = projWrapper.WithParsedProject(proj, checker)
            for msg in List.rev stackedMessages do
                Received msg |> agent.Post
            return! addOrUpdateProject state projWrapper |> fst |> loop

      | Respond(value, msgHandler) ->
        msgHandler.Respond(fun writer ->
            // CloseOutput=false is necessary to prevent closing the underlying stream
            use jsonWriter = new JsonTextWriter(writer, CloseOutput=false)
            let serializer = JsonSerializer.Create(jsonSettings)
            serializer.Serialize(jsonWriter, value))
        return! loop state

      | Received msgHandler ->
        let respond(res: obj) =
            Respond(res, msgHandler) |> agent.Post
        try
            let msg = Parser.parse msgHandler.Message
            Log.verbose(lazy
                if msg.path.EndsWith(".fsproj") then sprintf "Received %A" msg
                else "")
            let state, (projWrapper: ProjectWrapper) = updateState state msg
            match projWrapper.ProjectStatus with
            | ParsedProject(proj,_,_) ->
                let com = projWrapper.MakeCompiler(msg.path, proj)
                addFSharpErrorLogs com proj msg.path
                startCompilation respond com proj
                return! loop state
            | ParsingProject _ ->
                if msg.path.EndsWith(".fsproj") then
                    // Quickly respond with a façade file so we don't block the client and cache can be activated
                    let sourceFiles = getSourceFiles projWrapper.ProjectOptions
                    Fable2Babel.Compiler.createFacade sourceFiles msg.path |> respond
                    return! loop state
                else
                    return! projWrapper.StackMessage(msgHandler)
                            |> addOrUpdateProject state
                            |> fst |> loop
        with ex ->
            sendError respond ex
            return! loop state
    }

    loop Map.empty

let private agent = new MailboxProcessor<AgentMsg>(agentBody)

let startAgent() = agent.Start(); agent

let getRelativePath path =
    Path.getRelativePath (IO.Directory.GetCurrentDirectory()) path

let getSourceFiles (opts: FSharpProjectOptions) =
    opts.OtherOptions |> Array.filter (fun path -> path.StartsWith("-") |> not)

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

let splitVersion (version: string) =
    match System.Version.TryParse(version) with
    | true, v -> v.Major, v.Minor, v.Revision
    | _ -> 0, 0, 0

let checkFableCoreVersion (checkedProject: FSharpCheckProjectResults) =
    for ref in checkedProject.ProjectContext.GetReferencedAssemblies() do
        if ref.SimpleName = "Fable.Core" then
            let version = System.Text.RegularExpressions.Regex.Match(ref.QualifiedName, @"Version=(\d+\.\d+\.\d+)")
            let expectedMajor, expectedMinor, _ = splitVersion Literals.CORE_VERSION
            let actualMajor, actualMinor, _ = splitVersion version.Groups.[1].Value
            if not(actualMajor = expectedMajor && actualMinor = expectedMinor) then
                failwithf "Fable.Core v%i.%i detected, expecting v%i.%i" actualMajor actualMinor expectedMajor expectedMinor
            // else printfn "Fable.Core version matches"

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None
    member _.NormalizedFullPath = normalizedFullPath
    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy File.readAllTextNonBlocking normalizedFullPath
        | None ->
            let source = File.readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

type ProjectStatus =
    | ParsedProject of Project * InteractiveChecker * DateTime
    | ParsingProject of msgStack: IMessageHandler list

type CompilerFactory(compilerOptions, fableLibraryDir) =
    member _.Make(currentFile, project) =
        let fableLibraryDir = Path.getRelativePath currentFile fableLibraryDir
        Compiler(currentFile, project, compilerOptions, fableLibraryDir)

type ProjectWrapper(sourceFiles: File array,
                    fsharpProjOptions: FSharpProjectOptions,
                    compilerFactory: CompilerFactory,
                    projectStatus: ProjectStatus) =
    member _.ProjectStatus = projectStatus
    member _.ProjectFile = fsharpProjOptions.ProjectFileName
    member _.ProjectOptions = fsharpProjOptions
    member _.SourceFiles = sourceFiles

    member _.MakeCompiler(currentFile, project) =
        compilerFactory.Make(currentFile, project)

    member _.ContainsFile(file) =
        sourceFiles |> Array.exists (fun file2 ->
            file = file2.NormalizedFullPath)

    member _.ResetSourceFiles(sourceFiles) =
        ProjectWrapper(sourceFiles, fsharpProjOptions, compilerFactory, ParsingProject [])

    member _.WithParsedProject(project, checker) =
        let stackedMessages =
            match projectStatus with
            | ParsingProject stack -> stack
            | _ -> []
        let proj = ParsedProject(project, checker, DateTime.Now)
        stackedMessages, ProjectWrapper(sourceFiles, fsharpProjOptions, compilerFactory, proj)

    member this.StackMessage(msgHandler) =
        match projectStatus with
        | ParsingProject stack ->
            let proj = ParsingProject(msgHandler::stack)
            ProjectWrapper(sourceFiles, fsharpProjOptions, compilerFactory, proj)
        | _ -> this

    member this.ParseProject(?checker, ?saveAstDir: string) =
        Log.always(sprintf "Parsing %s..." (getRelativePath this.ProjectFile))
        Async.Start <| async {
            let checker =
                match checker with
                | Some checker -> checker
                | None -> InteractiveChecker.Create(this.ProjectOptions)
            let checkedProject =
                let fileDic = this.SourceFiles |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
                let sourceReader f = fileDic.[f].ReadSource()
                let filePaths = this.SourceFiles |> Array.map (fun file -> file.NormalizedFullPath)
                checker.ParseAndCheckProject(this.ProjectFile, filePaths, sourceReader)
            // checkFableCoreVersion checkedProject
            let optimized = GlobalParams.Singleton.Experimental.Contains("optimize-fcs")
            let implFiles =
                if not optimized
                then checkedProject.AssemblyContents.ImplementationFiles
                else checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
            match implFiles, saveAstDir with
            | [], _ -> Log.always "The list of files returned by F# compiler is empty"
            | _, Some saveAstDir -> Printers.printAst saveAstDir implFiles
            | _ -> ()
            let implFilesMap =
                implFiles
                |> Seq.map (fun file -> Path.normalizePathAndEnsureFsExtension file.FileName, file)
                |> dict
            let proj = Project(this.ProjectOptions, implFilesMap, checkedProject.Errors)
            Parsed(this.ProjectFile, proj, checker) |> agent.Post
        }
        this

    static member FromMessage(projFile: string, msg: Parser.Message) =
        let projectOptions, fableLibraryDir =
            getFullProjectOpts {
                define = msg.define
                noReferences = msg.noReferences
                noRestore = msg.noRestore
                rootDir = msg.rootDir
                projFile = projFile
            }
        Log.verbose(lazy
            let proj = getRelativePath projFile
            let opts = projectOptions.OtherOptions |> String.concat "\n   "
            sprintf "F# PROJECT: %s\n   %s" proj opts)
        let sourceFiles = getSourceFiles projectOptions |> Array.map File
        let compilerFactory = CompilerFactory(Parser.toCompilerOptions msg, fableLibraryDir)
        ProjectWrapper(sourceFiles, projectOptions, compilerFactory, ParsingProject [])

let createProject (msg: Parser.Message) projFile (prevProject: ProjectWrapper option) =
    match prevProject with
    | Some projWrapper ->
        match projWrapper.ProjectStatus with
        // Ignore requests when the project is still parsing, parsed less than a second ago or doesn't have dirty files
        | ParsedProject(_, checker, timestamp) when DateTime.Now - timestamp >= TimeSpan.FromSeconds(1.) ->
            let mutable someDirtyFiles = false
            let sourceFiles =
                projWrapper.SourceFiles
                |> Array.map (fun file ->
                    let path = file.NormalizedFullPath
                    // Assume files in .fable folder are stable
                    if Naming.isInFableHiddenDir path then file
                    else
                        let isDirty = IO.File.GetLastWriteTime(path) > timestamp
                        someDirtyFiles <- someDirtyFiles || isDirty
                        if isDirty then File(path) // Clear the cached source hash
                        else file)
            if someDirtyFiles then
                projWrapper.ResetSourceFiles(sourceFiles)
                    .ParseProject(checker)
            else projWrapper
        | _ -> projWrapper
    | None ->
        ProjectWrapper.FromMessage(projFile, msg)
            .ParseProject(?saveAstDir=tryGetOption "saveAst" msg.extra)

let sendError (respond: obj->unit) (ex: Exception) =
    let rec innerStack (ex: Exception) =
        if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
    let stack = innerStack ex
    Log.always(sprintf "ERROR: %s\n%s" ex.Message stack)
    ["error", ex.Message] |> dict |> respond

let findFsprojUpwards originalFile =
    let rec innerLoop dir =
        match IO.Directory.GetFiles(dir, "*.fsproj") with
        | [||] ->
            let parentDir = IO.Path.GetDirectoryName(dir)
            if isNull parentDir
            then failwithf "Cannot find project file for %s. Do you need symlinks:false in your webpack.config?" originalFile
            else innerLoop parentDir
        | [|projFile|] -> projFile
        | _ -> failwithf "Found more than one project file for %s, please disambiguate." originalFile
    IO.Path.GetDirectoryName(originalFile) |> innerLoop

let addOrUpdateProject state (project: ProjectWrapper) =
    let state = Map.add project.ProjectFile project state
    state, project

let checkIfProjectIsAlreadyInState state msg projFile =
    let projFile = Path.normalizeFullPath projFile
    Map.tryFind projFile state
    |> createProject msg projFile
    |> addOrUpdateProject state

let tryFindAndUpdateProject onNotFound state (msg: Parser.Message) sourceFile =
    // Check for the `extra.projectFile` option. This is used to
    // disambiguate files referenced by several projects, see #1116
    match msg.extra.TryGetValue("projectFile") with
    | true, projFile ->
        checkIfProjectIsAlreadyInState state msg projFile
    | false, _ ->
        state |> Map.tryPick (fun _ (project: ProjectWrapper) ->
            if project.ContainsFile(sourceFile)
            then Some project
            else None)
        |> function Some project ->
                        Some project
                        |> createProject msg project.ProjectFile
                        |> addOrUpdateProject state
                    | None -> onNotFound()

let updateState (state: Map<string,ProjectWrapper>) (msg: Parser.Message) =
    match IO.Path.GetExtension(msg.path).ToLower() with
    | ".fsproj" ->
        createProject msg msg.path None
        |> addOrUpdateProject state
    | ".fsx" ->
        // For .fsx compiled as project we should recreate the project
        // in case #load or #r directives have changed, but this makes
        // watch compilation slower. Ignore it for now.
        // match Map.tryFind msg.path state with
        // | Some _ ->
        //     createProject msg msg.path None
        //     |> addOrUpdateProject state
        // | None ->
            (state, msg, msg.path) |||> tryFindAndUpdateProject (fun () ->
                createProject msg msg.path None
                |> addOrUpdateProject state)
    | ".fs" ->
        (state, msg, msg.path) |||> tryFindAndUpdateProject (fun () ->
            findFsprojUpwards msg.path
            |> checkIfProjectIsAlreadyInState state msg)
    | ".fsi" ->
        failwithf "Signature files cannot be compiled to JS: %s" msg.path
    | _ -> failwithf "Not an F# source file: %s" msg.path

let addFSharpErrorLogs (com: ICompiler) (proj: Project) (triggerFile: string) =
    proj.Errors |> Seq.filter (fun er ->
        // Report warnings always in the corresponding file
        // but ignore those from packages in `.fable` folder
        if er.Severity = FSharpErrorSeverity.Warning then
            com.CurrentFile = er.FileName && not(Naming.isInFableHiddenDir er.FileName)
        // For errors, if the trigger is the .fsproj (first compilation), report them in the corresponding file
        elif triggerFile.EndsWith(".fsproj") then
            com.CurrentFile = er.FileName
        // If another file triggers the compilation, report errors there so they don't go missing
        // But ignore errors from packages in `.fable` folder, as this is watch mode and users can't do anything
        // See https://github.com/fable-compiler/Fable/pull/1714#issuecomment-463137486
        else
            com.CurrentFile = triggerFile && not(Naming.isInFableHiddenDir er.FileName))
    |> Seq.map (fun er ->
        let severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> Severity.Warning
            | FSharpErrorSeverity.Error -> Severity.Error
        let range =
            { start={ line=er.StartLineAlternate; column=er.StartColumn+1}
              ``end``={ line=er.EndLineAlternate; column=er.EndColumn+1}
              identifierName = None }
        (er.FileName, range, severity, sprintf "%s (code %i)" er.Message er.ErrorNumber))
    |> Seq.distinct // Sometimes errors are duplicated
    |> Seq.iter (fun (fileName, range, severity, msg) ->
        com.AddLog(msg, severity, range, fileName, "FSHARP"))

/// Don't await file compilation to let the agent receive more requests to implement files.
let startCompilation (respond: obj->unit) (com: Compiler) (project: Project)  =
    async {
        try
            let babel =
                FSharp2Fable.Compiler.transformFile com project.ImplementationFiles
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com
            Babel.Program(babel.FileName, babel.Body, babel.Directives, com.GetFormattedLogs(), babel.Dependencies)
            |> respond
        with ex ->
            sendError respond ex
    } |> Async.Start
