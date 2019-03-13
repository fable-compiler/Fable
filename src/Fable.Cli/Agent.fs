module Fable.Cli.Agent

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open System
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open ProjectCracker

type File(normalizedFullPath: string) =
    let mutable sourceHash = None
    member __.NormalizedFullPath = normalizedFullPath
    member __.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy File.readAllTextNonBlocking normalizedFullPath
        | None ->
            let source = File.readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

/// Fable.Transforms.State.Project plus some properties only used here
type ProjectExtra(project: Project, checker: InteractiveChecker,
                  sourceFiles: File array, triggerFile: string,
                  fableLibraryDir: string) =
    let timestamp = DateTime.Now
    member __.TimeStamp = timestamp
    member __.Checker = checker
    member __.Project = project
    member __.ProjectOptions = project.ProjectOptions
    member __.ImplementationFiles = project.ImplementationFiles
    member __.Errors = project.Errors
    member __.ProjectFile = project.ProjectFile
    member __.SourceFiles = sourceFiles
    member __.TriggerFile = triggerFile
    member __.LibraryDir = fableLibraryDir
    member __.ContainsFile(file) =
        sourceFiles |> Array.exists (fun file2 ->
            file = file2.NormalizedFullPath)
    static member Create checker sourceFiles triggerFile fableLibraryDir project =
        ProjectExtra(project, checker, sourceFiles, triggerFile, fableLibraryDir)

let getSourceFiles (opts: FSharpProjectOptions) =
    opts.OtherOptions |> Array.choose (fun path ->
        if not(path.StartsWith("-")) then
            // These should be already normalized, but just in case
            // TODO: We should add a NormalizedFullPath type so we don't need normalize everywhere
            Path.normalizeFullPath path |> Some
        else None)

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

let checkProject (msg: Parser.Message)
                 (opts: FSharpProjectOptions)
                 (fableLibraryDir: string)
                 (triggerFile: string)
                 (srcFiles: File[])
                 (checker: InteractiveChecker) =
    Log.always(sprintf "Parsing %s..." (getRelativePath opts.ProjectFileName))
    let checkedProject =
        let fileDic = srcFiles |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
        let sourceReader f = fileDic.[f].ReadSource()
        let filePaths = srcFiles |> Array.map (fun file -> file.NormalizedFullPath)
        checker.ParseAndCheckProject(opts.ProjectFileName, filePaths, sourceReader)
    // checkFableCoreVersion checkedProject
    let optimized = GlobalParams.Singleton.Experimental.Contains("optimize-fcs")
    let implFiles =
        if not optimized
        then checkedProject.AssemblyContents.ImplementationFiles
        else checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
    if List.isEmpty implFiles then
        Log.always "The list of files returned by F# compiler is empty"
    let implFilesMap =
        implFiles |> Seq.map (fun file -> Path.normalizePathAndEnsureFsExtension file.FileName, file) |> dict
    tryGetOption "saveAst" msg.extra |> Option.iter (fun outDir ->
        Printers.printAst outDir implFiles)
    Project(opts, implFilesMap, checkedProject.Errors)
    |> ProjectExtra.Create checker srcFiles triggerFile fableLibraryDir

let createProject (msg: Parser.Message) projFile (prevProject: ProjectExtra option) =
    match prevProject with
    | Some proj ->
        let mutable someDirtyFiles = false
        // If now - proj.TimeStamp < 1s skip checking the lastwritetime for performance
        if DateTime.Now - proj.TimeStamp < TimeSpan.FromSeconds(1.) then
            proj
        else
            let sourceFiles =
                proj.SourceFiles
                |> Array.map (fun file ->
                    let path = file.NormalizedFullPath
                    // Assume files in .fable folder are stable
                    if path.Contains(".fable/") then file
                    else
                        let isDirty = IO.File.GetLastWriteTime(path) > proj.TimeStamp
                        someDirtyFiles <- someDirtyFiles || isDirty
                        if isDirty then File(path) // Clear the cached source hash
                        else file)
            if someDirtyFiles then
                checkProject msg proj.ProjectOptions proj.LibraryDir msg.path sourceFiles proj.Checker
            else proj
    | None ->
        let projectOptions, fableLibraryDir =
            getFullProjectOpts msg.define msg.rootDir projFile
        Log.verbose(lazy
            let proj = getRelativePath projectOptions.ProjectFileName
            let opts = projectOptions.OtherOptions |> String.concat "\n   "
            sprintf "F# PROJECT: %s\n   %s" proj opts)
        let sourceFiles = getSourceFiles projectOptions |> Array.map File
        InteractiveChecker.Create(projectOptions)
        |> checkProject msg projectOptions fableLibraryDir projFile sourceFiles

let jsonSettings =
    JsonSerializerSettings(
        Converters=[|Json.ErasedUnionConverter()|],
        ContractResolver=Serialization.CamelCasePropertyNamesContractResolver(),
        NullValueHandling=NullValueHandling.Ignore)
        // StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)

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

let addOrUpdateProject state (project: ProjectExtra) =
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
        let projFile = Path.normalizeFullPath projFile
        checkIfProjectIsAlreadyInState state msg projFile
    | false, _ ->
        state |> Map.tryPick (fun _ (project: ProjectExtra) ->
            if project.ContainsFile(sourceFile)
            then Some project
            else None)
        |> function Some project ->
                        Some project
                        |> createProject msg project.ProjectFile
                        |> addOrUpdateProject state
                    | None -> onNotFound()

let updateState (state: Map<string,ProjectExtra>) (msg: Parser.Message) =
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

let addFSharpErrorLogs (com: ICompiler) (proj: ProjectExtra) =
    proj.Errors |> Seq.filter (fun er ->
        // Report warnings always in the corresponding file
        // but ignore those from packages in `.fable` folder
        if er.Severity = FSharpErrorSeverity.Warning then
            com.CurrentFile = er.FileName && not(Naming.isInFableHiddenDir er.FileName)
        // For errors, if the trigger is the .fsproj (first compilation), report them in the corresponding file
        elif proj.TriggerFile.EndsWith(".fsproj") then
            com.CurrentFile = er.FileName
        // If another file triggers the compilation report errors there so they don't go missing
        // But ignore errors from packages in `.fable` folder, as this is watch mode and users can't do anything
        // See https://github.com/fable-compiler/Fable/pull/1714#issuecomment-463137486
        else
            com.CurrentFile = proj.TriggerFile && not(Naming.isInFableHiddenDir er.FileName))
    |> Seq.map (fun er ->
        let severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> Severity.Warning
            | FSharpErrorSeverity.Error -> Severity.Error
        let range =
            { start={ line=er.StartLineAlternate; column=er.StartColumn}
              ``end``={ line=er.EndLineAlternate; column=er.EndColumn}
              identifierName = None }
        (er.FileName, range, severity, sprintf "%s (code %i)" er.Message er.ErrorNumber))
    |> Seq.distinct // Sometimes errors are duplicated
    |> Seq.iter (fun (fileName, range, severity, msg) ->
        com.AddLog(msg, severity, range, fileName, "FSHARP"))

/// Don't await file compilation to let the agent receive more requests to implement files.
let startCompilation (respond: obj->unit) (com: Compiler) (project: ProjectExtra) =
    async {
        try
            if com.CurrentFile.EndsWith(".fsproj") then
                // If we compile the last file here, Webpack watcher will ignore changes in it
                Fable2Babel.Compiler.createFacade (getSourceFiles project.ProjectOptions) com.CurrentFile
                |> respond
            else
                let babel =
                    FSharp2Fable.Compiler.transformFile com project.ImplementationFiles
                    |> FableTransforms.optimizeFile com
                    |> Fable2Babel.Compiler.transformFile com
                Babel.Program(babel.FileName, babel.Body, babel.Directives, com.GetFormattedLogs(), babel.Dependencies)
                |> respond
        with ex ->
            sendError respond ex
    } |> Async.Start

let startAgent () = MailboxProcessor<AgentMsg>.Start(fun agent ->
    let rec loop (state: Map<string, ProjectExtra>) = async {
      match! agent.Receive() with
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
            // lazy sprintf "Received message %A" msg |> Log.logVerbose
            let newState, activeProject = updateState state msg
            let libDir = Path.getRelativePath msg.path activeProject.LibraryDir
            let com = Compiler(msg.path, activeProject.Project, Parser.toCompilerOptions msg, libDir)
            addFSharpErrorLogs com activeProject
            startCompilation respond com activeProject
            return! loop newState
        with ex ->
            sendError respond ex
            return! loop state
    }
    loop Map.empty
  )
