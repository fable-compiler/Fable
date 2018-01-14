/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
/// using Paket .paket.resolved file
module Fable.CLI.ProjectCracker

open System
open System.IO
open System.Xml.Linq
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.State

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("Microsoft.")
        || pkgName.StartsWith("runtime.")
        || pkgName.StartsWith("System.")
        || pkgName = "FSharp.Core"
        || pkgName = "Fable.Core"

let logWarningAndReturn (v:'T) str =
    Log.logAlways("[WARNING] " + str); v

/// Reads references from .paket.resolved file. Paket sorts the references in proper
/// dependency order which is necessary to later merge sources from Fable libraries
/// into a single project
let readPaketResolved (targetFramework: string) (projFile: string) =
    let projDir = Path.GetDirectoryName(projFile)
    let projFileName = Path.GetFileName(projFile)
    let paketRefs1 = IO.Path.Combine(projDir, "obj", projFileName + "." + targetFramework + ".paket.resolved")
    if File.Exists(paketRefs1)
    then File.ReadAllLines(paketRefs1)
    else
        let paketRefs2 = IO.Path.Combine(projDir, "obj", projFileName + ".references")
        if File.Exists(paketRefs2)
        then File.ReadAllLines(paketRefs2)
        else
            Path.GetFileName(projFile)
            |> sprintf "Cannot find PAKET info for project %s, it won't be possible to resolve Fable libraries."
            |> logWarningAndReturn [||]

type ProjectReference =
    { ProjectFile: string
      /// Project references from Nuget packages won't be considered direct references.
      /// The main difference is for non-direct packages we'll only do a simple XML
      /// parsing to get source files, as we cannot run `dotnet restore` on .fsproj files
      /// embedded in Nuget packages.
      IsDirectReference: bool }
    static member MakeDirect projFile =
        { ProjectFile = projFile; IsDirectReference = true }

type CrackedFsproj =
    { ProjectFile: string
      SourceFiles: string list
      ProjectReferences: ProjectReference list
      DllReferences: string list }

let makeProjectOptions project sources otherOptions =
    { ProjectFileName = project
      SourceFiles = sources
      OtherOptions = otherOptions
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None
      Stamp = None }

let getProjectOptionsFromScript (checker: FSharpChecker) (define: string[]) scriptFile =
    let otherFlags = [|
        yield "--target:library"
#if !NETFX
        yield "--targetprofile:netcore"
#endif
        for constant in define do yield "--define:" + constant
    |]
    checker.GetProjectOptionsFromScript(scriptFile, File.ReadAllText scriptFile,
                                        assumeDotNetFramework=false, otherFlags=otherFlags)
    |> Async.RunSynchronously
    |> fun (opts, errors) ->
        // TODO: Check errors
        opts.OtherOptions
        |> makeProjectOptions scriptFile opts.SourceFiles

let getBasicCompilerArgs (define: string[]) =
    [|
        // yield "--debug"
        // yield "--debug:portable"
        yield "--noframework"
        yield "--nologo"
        yield "--simpleresolution"
        yield "--nocopyfsharpcore"
        // yield "--define:DEBUG"
        for constant in define do
            yield "--define:" + constant
        yield "--optimize-"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
#if !NETFX
        yield "--targetprofile:netcore"
#endif
    |]

/// Simplistic XML-parsing of .fsproj to get source files, as we cannot
/// run `dotnet restore` on .fsproj files embedded in Nuget packages.
let getFsprojSourceFiles (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let xmlDoc = XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile) |> Path.normalizePath
    let sourceFiles =
        xmlDoc.Root.Elements()
        |> withName "ItemGroup"
        |> Seq.map (fun item ->
            (item.Elements(), [])
            ||> Seq.foldBack (fun el src ->
                if el.Name.LocalName = "Compile" then
                    el.Elements() |> withName "Link"
                    |> Seq.tryHead |> function
                    | Some link when link.Value.StartsWith(".") ->
                        link.Value::src
                    | _ ->
                        match el.Attribute(XName.Get "Include") with
                        | null -> src
                        | att -> att.Value::src
                else src))
        |> List.concat
    { ProjectFile = projFile
      SourceFiles =
        sourceFiles
        |> List.map (fun fileName -> Path.Combine(projDir, fileName) |> Path.normalizeFullPath)
      ProjectReferences = []
      DllReferences = [] }

let private getDllName (dllFullPath: string) =
    let i = dllFullPath.LastIndexOf('/')
    dllFullPath.[(i + 1) .. (dllFullPath.Length - 5)] // -5 removes the .dll extension

let private getFsprojName (fsprojFullPath: string) =
    let i = fsprojFullPath.LastIndexOf('/')
    fsprojFullPath.[(i + 1) .. (fsprojFullPath.Length - 8)]

/// Use Dotnet.ProjInfo (through ProjectCoreCracker) to invoke MSBuild
/// and get F# compiler args from an .fsproj file. As we'll merge this
/// later with other projects we'll only take the sources and the references,
/// checking if some .dlls correspond to Fable libraries (in which case,
/// we replace them with a non-direct project reference)
let fullyCrackFsproj (projFile: string): CrackedFsproj =
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.InvariantCultureIgnoreCase)
    let projOpts, directProjRefs, msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile projFile
    let targetFramework =
        match Map.tryFind "TargetFramework" msbuildProps with
        | Some targetFramework -> targetFramework
        | None -> failwithf "Cannot find TargetFramework for project %s" projFile
    let sourceFiles =
        (projOpts.OtherOptions, []) ||> Array.foldBack (fun line src ->
            if line.StartsWith("-r:") then
                let line = Path.normalizePath (line.[3..])
                let dllName = getDllName line
                dllRefs.Add(dllName, line)
                src
            elif line.StartsWith("-") then
                src
            else
                (Path.normalizeFullPath line)::src)
    let directProjRefs =
        directProjRefs |> List.map (fun projRef ->
            // Remove dllRefs corresponding to project references
            let projName = getFsprojName projRef
            dllRefs.Remove(projName) |> ignore
            Path.normalizeFullPath projRef |> ProjectReference.MakeDirect)
    let fableProjRefs =
        // Use references from .paket.resolved as they come in proper dependency order
        readPaketResolved targetFramework projFile
        |> Seq.choose (fun line ->
            let pkgName = line.Split(',').[0]
            if not(isSystemPackage pkgName) then
                match dllRefs.TryGetValue(pkgName) with
                | true, dllRef ->
                    // We confirm if the dll reference corresponds to a Fable library by
                    // checking if there exists a fable folder with an .fsproj in the package
                    let dllDir = Path.GetDirectoryName(dllRef)
                    let fableProj = IO.Path.Combine(dllDir, "..", "..", "fable", pkgName + ".fsproj")
                    if File.Exists(fableProj)
                    then
                        { ProjectFile = Path.normalizeFullPath(fableProj)
                          IsDirectReference = false } |> Some
                    else None
                | false, _ -> None
            else None)
        |> Seq.toList
    // directProjRefs |> List.map (fun x -> x.ProjectFile) |> printfn "Direct references %A"
    // fableProjRefs |> List.map (fun x -> x.ProjectFile) |> printfn "Fable references %A"
    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = fableProjRefs @ directProjRefs
      DllReferences = dllRefs.Values |> Seq.toList }

let crackFsproj (isFullCrack: bool) (projFile: string): CrackedFsproj =
    if isFullCrack
    then fullyCrackFsproj projFile
    else getFsprojSourceFiles projFile

let getProjectOptionsFromFsproj (msg: Parser.Message) (projFile: string) =
    let mainProj = fullyCrackFsproj projFile
    let fableCoreJsDir: PathRef =
        match msg.fableCore with
        | Some path when path.StartsWith(".") -> Path.normalizeFullPath path |> FilePath
        | Some path when Path.IsPathRooted(path) -> Path.normalizePath path |> FilePath
        | Some path -> NonFilePath path
        | None ->
            mainProj.DllReferences
            |> List.tryFind (fun x -> x.EndsWith("Fable.Core.dll"))
            |> Option.bind (fun dllRef ->
                let dllDir = Path.GetDirectoryName(dllRef)
                let path = IO.Path.Combine(dllDir, "..", "..", "fable-core")
                if Directory.Exists(path)
                then Path.normalizeFullPath path |> FilePath |> Some
                else None)
            |> Option.defaultWith (fun () ->
                sprintf "Cannot find Fable.Core JS files location for project %s, using `fable-core`. %s"
                    (Path.GetFileName(projFile)) "Add a reference to Fable.Core package or set the fableCore option in the JS client."
                |> logWarningAndReturn (NonFilePath "fable-core"))
    let rec crackProjects (acc: CrackedFsproj list) (projFile: Choice<ProjectReference, CrackedFsproj>) =
        let crackedFsproj =
            match projFile with
            | Choice1Of2 projRef ->
                let projFile = projRef.ProjectFile
                match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
                | None -> crackFsproj projRef.IsDirectReference projFile
                | Some crackedFsproj -> crackedFsproj
            | Choice2Of2 crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        (crackedFsproj.ProjectReferences, crackedFsproj::acc)
        ||> Seq.foldBack (fun projRef acc ->
            crackProjects acc (Choice1Of2 projRef))
    let crackedFsprojs =
        crackProjects [] (Choice2Of2 mainProj)
        |> List.distinctBy (fun x -> x.ProjectFile)
    let sourceFiles =
        crackedFsprojs |> Seq.collect (fun x -> x.SourceFiles) |> Seq.toArray
    let otherOptions =
        let projRefsSet = crackedFsprojs |> Seq.map (fun x -> getFsprojName x.ProjectFile) |> Set
        // We only keep dllRefs for the main project
        //crackedFsprojs |> Seq.collect (fun x -> x.DllReferences) |> Seq.distinct
        mainProj.DllReferences
        // Remove dllRefs that have turned into projRefs
        |> Seq.filter (fun dllRef -> Set.contains (getDllName dllRef) projRefsSet |> not)
        |> Seq.map ((+) "-r:") |> Seq.toArray
    makeProjectOptions projFile sourceFiles otherOptions, fableCoreJsDir

let getProjectOpts (checker: FSharpChecker) (msg: Parser.Message) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        // getProjectOptionsFromScript checker define projFile
        failwith "Parsing .fsx scripts is not currently possible, please use a .fsproj project"
    | ".fsproj" ->
        getProjectOptionsFromFsproj msg projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (msg: Parser.Message) (projFile: string) =
    let retryUntil = (DateTime.Now + TimeSpan.FromSeconds 2.)
    let rec retry () =
        try
            getProjectOpts checker msg projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.Now then
                System.Threading.Thread.Sleep 500
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | _ -> reraise()
    retry()

let getFullProjectOpts (checker: FSharpChecker) (msg: Parser.Message) (projFile: string) =
    let projFile = Path.GetFullPath(projFile)
    if not(File.Exists(projFile)) then
        failwith ("File does not exist: " + projFile)
    let projOpts, fableCoreJsDir = retryGetProjectOpts checker msg projFile
    let projOpts =
        Array.append (getBasicCompilerArgs msg.define) projOpts.OtherOptions
        |> makeProjectOptions projOpts.ProjectFileName projOpts.SourceFiles
    projOpts, fableCoreJsDir

