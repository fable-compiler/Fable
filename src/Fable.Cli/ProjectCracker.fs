/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
/// using Paket .paket.resolved file
module Fable.Cli.ProjectCracker

open System
open System.Xml.Linq
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices
open Fable
open Globbing.Operators

type Options = {
    define: string[]
    noReferences: bool
    noRestore: bool
    rootDir: string
    projFile: string
}

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.")
        || pkgName.StartsWith("Microsoft.")
        || pkgName.StartsWith("runtime.")
        || pkgName = "NETStandard.Library"
        || pkgName = "FSharp.Core"
        || pkgName = "Fable.Core"

let logWarningAndReturn (v:'T) str =
    Log.always("[WARNING] " + str); v

type FablePackage =
    { Id: string
      Version: string
      FsprojPath: string
      Dependencies: Set<string> }

type CrackedFsproj =
    { ProjectFile: string
      SourceFiles: string list
      ProjectReferences: string list
      DllReferences: string list
      PackageReferences: FablePackage list
      OtherCompilerOptions: string list }

let makeProjectOptions project sources otherOptions: FSharpProjectOptions =
    { ProjectId = None
      ProjectFileName = project
      SourceFiles = [||]
      OtherOptions = Array.distinct sources |> Array.append otherOptions
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.MaxValue
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None
      Stamp = None }

let tryGetFablePackage (dllPath: string) =
    let tryFileWithPattern dir pattern =
        try
            let files = IO.Directory.GetFiles(dir, pattern)
            match files.Length with
            | 0 -> None
            | 1 -> Some files.[0]
            | _ -> Log.always("More than one file found in " + dir + " with pattern " + pattern)
                   None
        with _ -> None
    let firstWithName localName (els: XElement seq) =
        els |> Seq.find (fun x -> x.Name.LocalName = localName)
    let tryFirstWithName localName (els: XElement seq) =
        els |> Seq.tryFind (fun x -> x.Name.LocalName = localName)
    let elements (el: XElement) =
        el.Elements()
    let attr name (el: XElement) =
        el.Attribute(XName.Get name).Value
    let child localName (el: XElement) =
        let child = el.Elements() |> firstWithName localName
        child.Value
    let firstGroupOrAllDependencies (dependencies: XElement seq) =
        match tryFirstWithName "group" dependencies with
        | Some firstGroup -> elements firstGroup
        | None -> dependencies
    if Path.GetFileNameWithoutExtension(dllPath) |> isSystemPackage
    then None
    else
        let rootDir = IO.Path.Combine(IO.Path.GetDirectoryName(dllPath), "..", "..")
        let fableDir = IO.Path.Combine(rootDir, "fable")
        match tryFileWithPattern rootDir "*.nuspec",
              tryFileWithPattern fableDir "*.fsproj" with
        | Some nuspecPath, Some fsprojPath ->
            let xmlDoc = XDocument.Load(nuspecPath)
            let metadata =
                xmlDoc.Root.Elements()
                |> firstWithName "metadata"
            { Id = metadata |> child "id"
              Version = metadata |> child "version"
              FsprojPath = fsprojPath
              Dependencies =
                metadata.Elements()
                |> firstWithName "dependencies" |> elements
                // We don't consider different frameworks
                |> firstGroupOrAllDependencies
                |> Seq.map (attr "id")
                |> Seq.filter (isSystemPackage >> not)
                |> Set
            } |> Some
        | _ -> None

let sortFablePackages (pkgs: FablePackage list) =
    ([], pkgs) ||> List.fold (fun acc pkg ->
        match List.tryFindIndexBack (fun x -> pkg.Dependencies.Contains(x.Id)) acc with
        | None -> pkg::acc
        | Some targetIdx ->
            let rec insertAfter x targetIdx i before after =
                match after with
                | justBefore::after ->
                    if i = targetIdx then
                        if i > 0 then
                            let dependent, nonDependent =
                                List.rev before |> List.partition (fun x ->
                                    x.Dependencies.Contains(pkg.Id))
                            nonDependent @ justBefore::x::dependent @ after
                        else
                            (justBefore::before |> List.rev) @ x::after
                    else
                        insertAfter x targetIdx (i + 1) (justBefore::before) after
                | [] -> failwith "Unexpected empty list in insertAfter"
            insertAfter pkg targetIdx 0 [] acc
    )

let getProjectOptionsFromScript (define: string[]) scriptFile: CrackedFsproj list * CrackedFsproj =
    let otherFlags = [|
        yield "--target:library"
#if !NETFX
        yield "--targetprofile:netcore"
#endif
        for constant in define do yield "--define:" + constant
    |]
    let checker = FSharpChecker.Create()
    checker.GetProjectOptionsFromScript(scriptFile,
                                        File.readAllTextNonBlocking scriptFile |> FSharp.Compiler.Text.SourceText.ofString,
                                        assumeDotNetFramework=false, otherFlags=otherFlags)
    |> Async.RunSynchronously
    |> fun (opts, _errors) -> // TODO: Check errors
        let dllRefs =
            opts.OtherOptions
            |> Array.filter (fun r -> r.StartsWith("-r:"))
            |> Array.map (fun r -> r.[3..])
            |> List.ofArray

        let fablePkgs =
            opts.OtherOptions
            |> List.ofArray
            |> List.map (fun line ->
                if line.StartsWith("-r:") then
                    let dllPath = line.Substring(3)
                    tryGetFablePackage dllPath
                else
                    None
            )
            |> List.choose id
            |> sortFablePackages

        [], { ProjectFile = opts.ProjectFileName
              SourceFiles = opts.SourceFiles |> Array.mapToList Path.normalizeFullPath
              ProjectReferences = []
              DllReferences = dllRefs
              PackageReferences = fablePkgs
              OtherCompilerOptions = [] }

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
        // yield "--nowarn:NU1603,NU1604,NU1605,NU1608"
        // yield "--warnaserror:76"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
#if !NETFX
        yield "--targetprofile:netstandard"
#endif
    |]

/// Simplistic XML-parsing of .fsproj to get source files, as we cannot
/// run `dotnet restore` on .fsproj files embedded in Nuget packages.
let getSourcesFromFsproj (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let xmlDoc = XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile)
    xmlDoc.Root.Elements()
    |> withName "ItemGroup"
    |> Seq.map (fun item ->
        (item.Elements(), [])
        ||> Seq.foldBack (fun el src ->
            if el.Name.LocalName = "Compile" then
                el.Elements() |> withName "Link"
                |> Seq.tryHead |> function
                | Some link when Path.isRelativePath link.Value ->
                    link.Value::src
                | _ ->
                    match el.Attribute(XName.Get "Include") with
                    | null -> src
                    | att -> att.Value::src
            else src))
    |> List.concat
    |> List.collect (fun fileName ->
        Path.Combine(projDir, fileName)
        |> Path.normalizeFullPath
        |> function
        | path when (path.Contains("*") || path.Contains("?")) ->
            match !! path |> List.ofSeq with
            | [] -> [ path ]
            | globResults -> globResults
        | path -> [ path ])

let private getDllName (dllFullPath: string) =
    let i = dllFullPath.LastIndexOf('/')
    dllFullPath.[(i + 1) .. (dllFullPath.Length - 5)] // -5 removes the .dll extension

let private isUsefulOption (opt : string) =
    [ "--nowarn"
      "--warnon"
      "--warnaserror"
      "--langversion" ]
    |> List.exists opt.StartsWith

/// Use Dotnet.ProjInfo (through ProjectCoreCracker) to invoke MSBuild
/// and get F# compiler args from an .fsproj file. As we'll merge this
/// later with other projects we'll only take the sources and the references,
/// checking if some .dlls correspond to Fable libraries
let fullCrack (opts: Options): CrackedFsproj =
    let projFile = opts.projFile
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.OrdinalIgnoreCase)

    // Try restoring project
    if opts.noRestore then
        Log.always "Skipping restore..."
    else
        Process.runCmd Log.always
            (IO.Path.GetDirectoryName projFile)
            "dotnet" ["restore"; IO.Path.GetFileName projFile]
        |> ignore

    Log.always("Parsing " + File.getRelativePath projFile + "...")
    let projOpts, projRefs, _msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile projFile

    // let targetFramework =
    //     match Map.tryFind "TargetFramework" msbuildProps with
    //     | Some targetFramework -> targetFramework
    //     | None -> failwithf "Cannot find TargetFramework for project %s" projFile

    let sourceFiles, otherOpts =
        (projOpts.OtherOptions, ([], []))
        ||> Array.foldBack (fun line (src, otherOpts) ->
            if line.StartsWith("-r:") then
                let line = Path.normalizePath (line.[3..])
                let dllName = getDllName line
                dllRefs.Add(dllName, line)
                src, otherOpts
            elif isUsefulOption line then
                src, line::otherOpts
            elif line.StartsWith("-") then
                src, otherOpts
            else
                (Path.normalizeFullPath line)::src, otherOpts)

    let projRefs =
        if opts.noReferences then []
        else
            projRefs |> List.choose (fun projRef ->
                // Remove dllRefs corresponding to project references
                let projName = Path.GetFileNameWithoutExtension(projRef)
                let removed = dllRefs.Remove(projName)
                if not removed then
                    Log.always("Couldn't remove project reference " + projName + " from dll references")
                Path.normalizeFullPath projRef |> Some)

    let fablePkgs =
        let dllRefs' = dllRefs |> Seq.map (fun (KeyValue(k,v)) -> k,v) |> Seq.toArray
        dllRefs' |> Seq.choose (fun (dllName, dllPath) ->
            match tryGetFablePackage dllPath with
            | Some pkg ->
                dllRefs.Remove(dllName) |> ignore
                Some pkg
            | None -> None)
        |> Seq.toList
        |> sortFablePackages

    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = projRefs
      DllReferences = dllRefs.Values |> Seq.toList
      PackageReferences = fablePkgs
      OtherCompilerOptions = otherOpts }

/// For project references of main project, ignore dll and package references
let easyCrack (projFile: string): CrackedFsproj =
    let projOpts, projRefs, _msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile projFile

    let sourceFiles =
        (projOpts.OtherOptions, []) ||> Array.foldBack (fun line src ->
            if line.StartsWith("-")
            then src
            else (Path.normalizeFullPath line)::src)

    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = projRefs |> List.map Path.normalizeFullPath
      DllReferences = []
      PackageReferences = []
      OtherCompilerOptions = [] }

let getCrackedProjectsFromMainFsproj (opts: Options) =
    let rec crackProjects (acc: CrackedFsproj list) (projFile: string) =
        let crackedFsproj =
            match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
            | None -> easyCrack projFile
            | Some crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        List.fold crackProjects (crackedFsproj::acc) crackedFsproj.ProjectReferences
    let mainProj = fullCrack opts
    let refProjs =
        List.fold crackProjects [] mainProj.ProjectReferences
        |> List.distinctBy (fun x -> x.ProjectFile)
    refProjs, mainProj

let getCrackedProjects (opts: Options) =
    match (Path.GetExtension opts.projFile).ToLower() with
    | ".fsx" ->
        getProjectOptionsFromScript opts.define opts.projFile
    | ".fsproj" ->
        getCrackedProjectsFromMainFsproj opts
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetCrackedProjects opts =
    let retryUntil = (DateTime.Now + TimeSpan.FromSeconds 2.)
    let rec retry () =
        try
            getCrackedProjects opts
        with
        | :? IO.IOException as ioex ->
            if retryUntil > DateTime.Now then
                System.Threading.Thread.Sleep 500
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | _ -> reraise()
    retry()

/// FAKE and other tools clean dirs but don't remove them, so check whether it doesn't exist or it's empty
let isDirectoryEmpty dir =
    not(IO.Directory.Exists(dir)) || IO.Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty

let createFableDir rootDir =
    let fableDir = IO.Path.Combine(rootDir, Naming.fableHiddenDir)
    if isDirectoryEmpty fableDir then
        IO.Directory.CreateDirectory(fableDir) |> ignore
        IO.File.WriteAllText(IO.Path.Combine(fableDir, ".gitignore"), "**/*")
    fableDir

let copyDirIfDoesNotExist (source: string) (target: string) =
    if GlobalParams.Singleton.ForcePkgs || isDirectoryEmpty target then
        IO.Directory.CreateDirectory(target) |> ignore
        if IO.Directory.Exists source |> not then
            failwith ("Source directory is missing: " + source)
        let source = source.TrimEnd('/', '\\')
        let target = target.TrimEnd('/', '\\')
        for dirPath in IO.Directory.GetDirectories(source, "*", IO.SearchOption.AllDirectories) do
            IO.Directory.CreateDirectory(dirPath.Replace(source, target)) |> ignore
        for newPath in IO.Directory.GetFiles(source, "*.*", IO.SearchOption.AllDirectories) do
            IO.File.Copy(newPath, newPath.Replace(source, target), true)

let copyFableLibraryAndPackageSources rootDir (pkgs: FablePackage list) =
    let fableDir = createFableDir rootDir
    let fableLibrarySource = GlobalParams.Singleton.FableLibraryPath
    let fableLibraryPath =
        if fableLibrarySource.StartsWith(Literals.FORCE)
        then fableLibrarySource.Replace(Literals.FORCE, "")
        else
            if isDirectoryEmpty fableLibrarySource then
                failwithf "fable-library directory is empty, please build FableLibrary: %s" fableLibrarySource
            Log.verbose(lazy ("fable-library: " + fableLibrarySource))
            let fableLibraryTarget = IO.Path.Combine(fableDir, "fable-library" + "." + Literals.VERSION)
            copyDirIfDoesNotExist fableLibrarySource fableLibraryTarget
            fableLibraryTarget
    let pkgRefs =
        pkgs |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)
            let targetDir = IO.Path.Combine(fableDir, pkg.Id + "." + pkg.Version)
            copyDirIfDoesNotExist sourceDir targetDir
            IO.Path.Combine(targetDir, IO.Path.GetFileName(pkg.FsprojPath)))
    fableLibraryPath, pkgRefs

// See #1455: F# compiler generates *.AssemblyInfo.fs in obj folder, but we don't need it
let removeFilesInObjFolder sourceFiles =
    let reg = System.Text.RegularExpressions.Regex(@"[\\\/]obj[\\\/]")
    sourceFiles |> Array.filter (reg.IsMatch >> not)

let getFullProjectOpts (opts: Options) =
    if not(IO.File.Exists(opts.projFile)) then
        failwith ("File does not exist: " + opts.projFile)
    let projRefs, mainProj = retryGetCrackedProjects opts
    let fableLibraryPath, pkgRefs =
        copyFableLibraryAndPackageSources opts.rootDir mainProj.PackageReferences
    let projOpts =
        let sourceFiles =
            let pkgSources = pkgRefs |> List.collect getSourcesFromFsproj
            let refSources = projRefs |> List.collect (fun x -> x.SourceFiles)
            pkgSources @ refSources @ mainProj.SourceFiles |> List.toArray |> removeFilesInObjFolder
        for file in sourceFiles do
            if file.EndsWith(".fs") && not(IO.File.Exists(file)) then
                failwithf "File does not exist: %s" file
        let otherOptions =
            let dllRefs =
                // We only keep dllRefs for the main project
                mainProj.DllReferences
                |> List.mapToArray (fun r -> "-r:" + r)
            let otherOpts = mainProj.OtherCompilerOptions |> Array.ofList
            [ getBasicCompilerArgs opts.define
              otherOpts
              dllRefs ]
            |> Array.concat
        makeProjectOptions opts.projFile sourceFiles otherOptions
    projOpts, fableLibraryPath
