module Fable.CLI.ProjectCracker

open System
open System.IO
open System.Xml.Linq
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Core

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("Microsoft.")
        || pkgName.StartsWith("runtime.")
        || pkgName.StartsWith("System.")
        || pkgName = "FSharp.Core"
        || pkgName = "Fable.Core"

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
            sprintf "\nWARNING: Cannot find PAKET info for project %s\n%s\n"
                projFile "It won't be possible to resolve Fable libraries."
            |> Log.logAlways
            [||]

type ProjectReference =
    { ProjectFile: string
      /// Project references from Nuget packages
      /// won't be considered direct references
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
      ProjectFileNames = sources
      OtherOptions = otherOptions
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None }

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
        |> makeProjectOptions scriptFile opts.ProjectFileNames

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

/// Simplistic resolution of .fsproj to get source files
let getFsprojSourceFiles (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let (|SourceFile|Another|) (el: XElement) =
        match el.Name.LocalName with
        | "Compile" ->
            el.Elements() |> withName "Link"
            |> Seq.tryHead |> function
            | Some link when link.Value.StartsWith(".") ->
                SourceFile link.Value
            | _ ->
                match el.Attribute(XName.Get "Include") with
                | null -> Another
                | att -> SourceFile att.Value
        | _ -> Another
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

let partitionMap (f: 'T->Choice<'T1,'T2>) (xs: 'T list): 'T1 list * 'T2 list =
    (xs, ([], []))
    ||> List.foldBack (fun item (list1, list2) ->
        match f item with
        | Choice1Of2 item -> item::list1, list2
        | Choice2Of2 item -> list1, item::list2)

let fullyCrackFsproj (projFile: string): CrackedFsproj =
    let dllRefs = Dictionary()
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
                let i = line.LastIndexOf('/')
                let dllName = line.[(i + 1) .. (line.Length - 5)] // -5 removes the .dll extension
                dllRefs.Add(dllName, line)
                src
            elif line.StartsWith("-") then
                src
            else
                (Path.normalizeFullPath line)::src)
    let directProjRefs =
        directProjRefs |> List.map (fun projRef ->
            // Remove dllRefs corresponding to project references
            let i = projRef.LastIndexOf('/')
            let projName = projRef.[(i + 1) .. (projRef.Length - 8)]
            dllRefs.Remove(projName) |> ignore
            Path.normalizeFullPath projRef |> ProjectReference.MakeDirect)
    let fableProjRefs =
        readPaketResolved targetFramework projFile
        |> Seq.choose (fun line ->
            let pkgName = line.Split(',').[0]
            if not(isSystemPackage pkgName) then
                match dllRefs.TryGetValue(pkgName) with
                | true, dllRef ->
                    let dllDir = Path.GetDirectoryName(dllRef)
                    let fableProj = IO.Path.Combine(dllDir, "..", "..", "fable", pkgName + ".fsproj")
                    if File.Exists(fableProj)
                    then
                        dllRefs.Remove(pkgName) |> ignore
                        { ProjectFile = Path.normalizeFullPath(fableProj); IsDirectReference = false } |> Some
                    else None
                | false, _ -> None
            else None)
        |> Seq.toList
    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = fableProjRefs @ directProjRefs
      DllReferences = dllRefs.Values |> Seq.toList }

let crackFsproj (isFullCrack: bool) (projFile: string): CrackedFsproj =
    if isFullCrack
    then fullyCrackFsproj projFile
    else getFsprojSourceFiles projFile

let getProjectOptionsFromFsproj (projFile: string) =
    let mainProj = fullyCrackFsproj projFile
    let fableCoreJsDir: string =
        match mainProj.DllReferences |> List.tryFind (fun x -> x.EndsWith("Fable.Core.dll")) with
        | Some dllRef ->
            let dllDir = Path.GetDirectoryName(dllRef)
            IO.Path.Combine(dllDir, "..", "..", "fable-core") |> Path.normalizeFullPath
        | None -> failwithf "Cannot find Fable.Core reference in project %s" projFile
    let rec crackProjects (acc: CrackedFsproj list) (projFile: U2<ProjectReference, CrackedFsproj>) =
        let crackedFsproj =
            match projFile with
            | U2.Case1 projRef ->
                let projFile = projRef.ProjectFile
                match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
                | None -> crackFsproj projRef.IsDirectReference projFile
                | Some crackedFsproj -> crackedFsproj
            | U2.Case2 crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        (crackedFsproj.ProjectReferences, crackedFsproj::acc)
        ||> Seq.foldBack (fun projRef acc ->
            crackProjects acc (U2.Case1 projRef))
    let crackedFsprojs =
        crackProjects [] (U2.Case2 mainProj)
        |> List.distinctBy (fun x -> x.ProjectFile)
    let sourceFiles =
        crackedFsprojs |> Seq.collect (fun x -> x.SourceFiles) |> Seq.toArray
    let otherOptions =
        crackedFsprojs |> Seq.collect (fun x -> x.DllReferences) |> Seq.distinct |> Seq.map ((+) "-r:") |> Seq.toArray
    makeProjectOptions projFile sourceFiles otherOptions, fableCoreJsDir

let getProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        // getProjectOptionsFromScript checker define projFile
        failwith "Parsing .fsx scripts is not currently possible, please use a .fsproj project"
    | ".fsproj" ->
        getProjectOptionsFromFsproj projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    let retryUntil = (DateTime.Now + TimeSpan.FromSeconds 2.)
    let rec retry () =
        try
            getProjectOpts checker define projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.Now then
                System.Threading.Thread.Sleep 500
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | _ -> reraise()
    retry()

let getFullProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    let projFile = Path.GetFullPath(projFile)
    if not(File.Exists(projFile)) then
        failwith ("File does not exist: " + projFile)
    let projOpts, fableCoreJsDir = retryGetProjectOpts checker define projFile
    let projOpts =
        Array.append (getBasicCompilerArgs define) projOpts.OtherOptions
        |> makeProjectOptions projOpts.ProjectFileName projOpts.ProjectFileNames
    projOpts, fableCoreJsDir

