module Fable.CLI.ProjectCracker

open System
open System.IO
open System.Xml.Linq
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST

type TargetFramework =
    { Framework: string; Version: string }
    member this.Full = this.Framework + this.Version

// PAKET -----------------------------------------------------
[<RequireQualifiedAccess>]
type PaketRef =
    | Project of string
    | Dll of string

let rec findPaketDependenciesDir dir searchedDirs =
    let path = Path.Combine(dir, "paket.dependencies")
    if File.Exists(path) then
        Log.logVerbose(lazy sprintf "Found paket.dependencies inside %s" dir)
        dir
    else
        match Directory.GetParent(dir) with
        | null ->
            searchedDirs
            |> String.concat "\n"
            |> failwithf "Couldn't find paket.dependencies directory, searched in: \n%s"
        | parent ->
            let searched = dir :: searchedDirs
            findPaketDependenciesDir parent.FullName searched

let tryFindPaketDirFromProject projFile =
    let projDir = Path.GetDirectoryName(projFile)
    if File.Exists(Path.Combine(projDir, "paket.references"))
    then findPaketDependenciesDir projDir [] |> Some
    else None

let private tryGetPaketRefNameAndDir paketDir (filter: string->bool) (refLine: string) =
    if filter refLine then
        let parts = refLine.Split(',')
        let refName, refGroup =
            if parts.Length = 4 then
                match parts.[3].ToLower() with
                | "main" -> parts.[0], None
                | group -> parts.[0], Some group
            else parts.[0], None
        match refGroup with
        | Some group -> Some(refName, IO.Path.Combine(paketDir, "packages", group, refName))
        | None -> Some(refName, IO.Path.Combine(paketDir, "packages", refName))
    else None

let tryGetPaketRef paketDir (targetFramework: TargetFramework) (refLine: string): PaketRef option =
    tryGetPaketRefNameAndDir paketDir (fun l -> l.StartsWith("Fable.") && not(l.StartsWith("Fable.Core"))) refLine
    |> Option.map (fun (refName, pkgDir) ->
        let fableProj = IO.Path.Combine(pkgDir, "fable", refName + ".fsproj")
        if File.Exists(fableProj) then
            PaketRef.Project fableProj
        else
            Directory.GetDirectories(IO.Path.Combine(pkgDir, "lib"))
            |> Seq.map Path.GetFileName
            |> Seq.sortDescending
            |> Seq.tryFind (fun framework ->
                framework.StartsWith(targetFramework.Framework) && framework <= targetFramework.Full)
            |> function
                | Some framework -> IO.Path.Combine(pkgDir, "lib", framework, refName + ".dll") |> PaketRef.Dll
                | None -> failwithf "Cannot match target %s for library %s" targetFramework.Full refName)

let private readPaketProjectRefLines projFile =
    let projDir = Path.GetDirectoryName(projFile)
    let projFileName = Path.GetFileName(projFile)
    let paketRefs = IO.Path.Combine(projDir, "obj", projFileName + ".references")
    if File.Exists(paketRefs)
    then File.ReadLines(paketRefs)
    else upcast [||] // TODO: Fail if .fsproj.references file not found?

let getPaketRefs paketDir targetFramework projFile: PaketRef list =
    match paketDir with
    | None -> []
    | Some paketDir ->
        let paketRefs =
            readPaketProjectRefLines projFile
            |> Seq.choose (tryGetPaketRef paketDir targetFramework)
            |> Seq.toList
        // paketRefs
        // |> Seq.map (sprintf "> %A")
        // |> String.concat "\n"
        // |> sprintf "Paket refs for %s\n%s" (Path.GetFileName(projFile))
        // |> Log.logVerbose
        paketRefs

let private tryGetFableCorePkgDir paketDir projFile =
    match paketDir with
    | Some paketDir ->
        readPaketProjectRefLines projFile
        |> Seq.choose (tryGetPaketRefNameAndDir paketDir (fun l -> l.StartsWith("Fable.Core")))
        |> Seq.tryHead
        |> Option.map snd
    | None -> // fallback to Fable.Core NuGet package folder
        let fableCoreDir =
            typeof<Fable.Core.EraseAttribute>.GetTypeInfo().Assembly.Location
            |> IO.Path.GetDirectoryName
        IO.Path.Combine(fableCoreDir, "../..") |> Some

let tryGetFableCoreJsDir projFile =
    let paketDir = tryFindPaketDirFromProject projFile
    tryGetFableCorePkgDir paketDir projFile
    |> Option.map (fun corePkgDir -> IO.Path.Combine(corePkgDir, "fable-core"))

// let checkFableCoreVersion paketDir projFile =
//     if Flags.checkCoreVersion then
//         match tryGetFableCorePkgDir paketDir projFile with
//         | Some corePkgDir ->
//             let nuspec = IO.Path.Combine(corePkgDir, "Fable.Core.nuspec")
//             if File.Exists(nuspec) then
//                 let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
//                 File.ReadLines(nuspec)
//                 |> Seq.tryPick (fun line ->
//                     let m = versionRegex.Match(line)
//                     if m.Success then Some m.Groups.[1].Value else None)
//                 |> function
//                     | Some fableCoreVersion ->
//                         sprintf "Fable.Core > actual: %s - expected: %s" fableCoreVersion Constants.CORE_VERSION
//                         |> Log.logVerbose
//                         if fableCoreVersion <> Constants.CORE_VERSION then
//                             String.Format("Expecting Fable.Core {0} but got {1}. " +
//                                 "Pin Fable.Core version in paket.dependencies " +
//                                 "or update dotnet-fable in .fsproj to latest version " +
//                                 "(check https://www.nuget.org/packages/dotnet-fable)",
//                                 Constants.CORE_VERSION, fableCoreVersion)
//                             |> failwith
//                     | None -> failwithf "Cannot find version in %s" nuspec
//             else Log.logAlways(sprintf "Fable.Core: Missing %s, cannot verify version" nuspec)
//         | None -> failwith "Cannot find Fable.Core package location"

type CrackedFsproj = {
    projectFile: string
    sourceFiles: string list
    projectReferences: string list
    dllReferences: string list
}

type private TypeInThisAssembly = class end

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
        |> Array.filter (fun x ->
            // Keep only relative references
            x.StartsWith("-r:") && (x.Contains("./") || x.Contains(".\\")) && not(x.EndsWith("Fable.Core.dll")))
        |> makeProjectOptions scriptFile opts.ProjectFileNames

let fsCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
let fableCoreLib = typeof<Fable.Core.EraseAttribute>.GetTypeInfo().Assembly.Location
let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
let sysUriLib = typeof<System.Uri>.GetTypeInfo().Assembly.Location
let sysPath = Path.GetDirectoryName(sysCoreLib)
let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location)

let getBasicCompilerArgs (define: string[]) optimize =
    let (|FileExists|_|) f path =
        let path = f path
        if File.Exists path then Some path else None
    let sysLib name = Path.Combine(sysPath, name + ".dll")
    let localLib name = Path.Combine(localPath, name + ".dll")
    let resolve ref =
        match ref with
        | FileExists sysLib path -> path
        | FileExists localLib path -> path
        | ref -> failwithf "Cannot locate reference %s" ref
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
        yield "--optimize" + (if optimize then "+" else "-")
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
#if NETFX
        yield "-r:" + resolve "System"
        yield "-r:" + resolve "System.Core"
        yield "-r:" + resolve "System.Numerics"
#else
        yield "--targetprofile:netcore"
        yield "-r:" + sysCoreLib // "CoreLib"
        yield "-r:" + sysUriLib //required for System.Uri types
        yield "-r:" + resolve "netstandard"
        yield "-r:" + resolve "System.Console"
        yield "-r:" + resolve "System.Runtime.Extensions" //required for System.Environment
#endif
        yield "-r:" + resolve "mscorlib"
        yield "-r:" + resolve "System.Collections"
        yield "-r:" + resolve "System.Diagnostics.Debug"
        yield "-r:" + resolve "System.IO"
        yield "-r:" + resolve "System.Reflection"
        yield "-r:" + resolve "System.Runtime"
        yield "-r:" + resolve "System.Runtime.Numerics"
        yield "-r:" + resolve "System.Threading"
        yield "-r:" + resolve "System.Threading.Tasks"
        yield "-r:" + resolve "System.Text.RegularExpressions"
        yield "-r:" + fsCoreLib // "FSharp.Core"
        yield "-r:" + fableCoreLib // "FSharp.Core"
    |]

let tryGetTargetFramework (xmlDoc: XDocument) =
    xmlDoc.Root.Elements()
    |> Seq.tryPick (fun el ->
        if el.Name.LocalName = "PropertyGroup" then
            el.Elements() |> Seq.tryPick (fun el ->
                if el.Name.LocalName = "TargetFramework"
                then Some el.Value
                else None)
        else None)

/// Ultra-simplistic resolution of .fsproj files
let crackFsproj (projFile: string) (xmlDoc: XDocument option) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let (|BeforeComma|) (att: XAttribute) =
        let str = att.Value
        match str.IndexOf(',', 0) with
        | -1 -> str
        | i -> str.Substring(0, i)
    let (|SourceFile|ProjectReference|RelativeDllReference|Another|) (el: XElement) =
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
        | "ProjectReference" ->
            match el.Attribute(XName.Get "Include") with
            | null -> Another
            | att -> ProjectReference att.Value
        | "Reference" ->
            match el.Attribute(XName.Get "Include") with
            | null -> Another
            | BeforeComma att when att.StartsWith(".") -> RelativeDllReference att
            | _ ->
                el.Elements() |> withName "HintPath"
                |> Seq.tryHead |> function
                | Some x -> RelativeDllReference x.Value
                | None -> Another
        | _ -> Another
    let xmlDoc =
        match xmlDoc with
        | Some doc -> doc
        | None ->
            if not(File.Exists(projFile)) then
                failwith ("File does not exist: " + projFile)
            XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile) |> Path.normalizePath
    let sourceFiles, projectReferences, relativeDllReferences =
        xmlDoc.Root.Elements()
        |> withName "ItemGroup"
        |> Seq.map (fun item ->
            (item.Elements(), ([], [], []))
            ||> Seq.foldBack (fun item (src, prj, dll) ->
                match item with
                | SourceFile s -> s::src, prj, dll
                | ProjectReference p -> src, p::prj, dll
                | RelativeDllReference d -> src, prj, d::dll
                | Another -> src, prj, dll))
        |> Seq.reduce (fun (src1, prj1, dll1) (src2, prj2, dll2) ->
            src1@src2, prj1@prj2, dll1@dll2)
    let reg = Regex("\\.fs[ix]?$")
    { projectFile = projFile
    ; sourceFiles =
        sourceFiles
        |> List.filter (fun fileName -> reg.IsMatch(fileName))
        |> List.map (fun fileName -> Path.Combine(projDir, Path.normalizePath fileName) |> Path.GetFullPath)
    ; projectReferences =
        projectReferences
        |> List.map (fun projRef ->
            Path.Combine(projDir, Path.normalizePath projRef) |> Path.GetFullPath)
    ; dllReferences =
        relativeDllReferences
        |> List.filter (fun x -> not(x.EndsWith("Fable.Core.dll")))
        |> List.map (fun x -> Path.Combine(projDir, Path.normalizePath x) |> Path.GetFullPath) }

let partitionMap (f: 'T->Choice<'T1,'T2>) (xs: 'T list): 'T1 list * 'T2 list =
    (xs, ([], []))
    ||> List.foldBack (fun item (list1, list2) ->
        match f item with
        | Choice1Of2 item -> item::list1, list2
        | Choice2Of2 item -> list1, item::list2)

let getProjectOptionsFromFsproj (projFile: string) =
    let xmlDoc = XDocument.Load(projFile)
    let targetFramework =
        match tryGetTargetFramework xmlDoc with
        | Some framework ->
            let framework, version =
                if framework.StartsWith("netcoreapp1") then "netstandard", "1.6"
                elif framework.StartsWith("netcoreapp2") then "netstandard", "2.0"
                else
                    let m = Regex.Match(framework, "(.*?)(\\d.*)")
                    m.Groups.[1].Value, m.Groups.[2].Value
            { Framework = framework; Version = version }
        | None -> failwithf "Cannot find TargetFramework in %s" projFile
    let paketDir = tryFindPaketDirFromProject projFile
    // checkFableCoreVersion paketDir projFile
    let rec crackProjects (acc: CrackedFsproj list) projFile xmlDoc =
        let crackedFsproj =
            acc |> List.tryFind (fun x ->
                String.Equals(x.projectFile, projFile, StringComparison.OrdinalIgnoreCase))
            |> function
            | Some crackedFsproj -> crackedFsproj
            | None ->
                let crackedFsproj = crackFsproj projFile xmlDoc
                let paketProjRefs, paketDllRefs =
                    getPaketRefs paketDir targetFramework projFile
                    |> partitionMap (function
                        | PaketRef.Project r -> Choice1Of2 r
                        | PaketRef.Dll r -> Choice2Of2 r)
                { crackedFsproj with
                    projectReferences = crackedFsproj.projectReferences @ paketProjRefs
                    dllReferences = crackedFsproj.dllReferences @ paketDllRefs }
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        (crackedFsproj.projectReferences, crackedFsproj::acc)
        ||> Seq.foldBack (fun projFile acc ->
            crackProjects acc projFile None)
    let crackedFsprojs =
        crackProjects [] projFile (Some xmlDoc)
        |> List.distinctBy (fun x -> x.projectFile.ToLower())
    let sourceFiles =
        crackedFsprojs |> Seq.collect (fun x -> x.sourceFiles) |> Seq.toArray
    let otherOptions =
        crackedFsprojs |> Seq.collect (fun x -> x.dllReferences)
        |> Seq.distinct |> Seq.map ((+) "-r:") |> Seq.toArray
    makeProjectOptions projFile sourceFiles otherOptions

let getProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        getProjectOptionsFromScript checker define projFile
    | ".fsproj" ->
        getProjectOptionsFromFsproj projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    let retryUntil = (DateTime.UtcNow + TimeSpan.FromSeconds 2.)
    let rec retry () =
        try
            getProjectOpts checker define projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.UtcNow then
                System.Threading.Thread.Sleep 500
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | _ -> reraise()
    retry()

let getFullProjectOpts (checker: FSharpChecker) (define: string[]) (projFile: string) =
    let define = Array.append define [|"FABLE_COMPILER"|]
    let projFile = Path.GetFullPath(projFile)
    if not(File.Exists(projFile)) then
        failwith ("File does not exist: " + projFile)
    let projOpts = retryGetProjectOpts checker define projFile
    Array.append (getBasicCompilerArgs define false) projOpts.OtherOptions
    |> makeProjectOptions projOpts.ProjectFileName projOpts.ProjectFileNames

