/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Cli.ProjectCracker

open System
open System.Xml.Linq
open System.Text.Json
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open Fable
open Globbing.Operators

type FablePackage =
    { Id: string
      Version: string
      FsprojPath: string
      DllPath: string
      SourcePaths: string list
      Dependencies: Set<string> }

type CacheInfo =
    {
        Version: string
        FableOptions: CompilerOptions
        ProjectPath: string
        SourcePaths: string array
        FSharpOptions: string array
        References: string list
        FableLibDir: string
        OutputType: string option
        TimestampUTC: DateTime
    }
    static member GetPath(fableModulesPath: string) =
        IO.Path.Combine(fableModulesPath, "cache_info.json")

    static member TryRead(fableModulesPath: string): CacheInfo option =
        try
            // TODO: When upgrading to net6 we shouldn't need FSharp.SystemTextJson, see also Write
            let jsonOptions = JsonSerializerOptions()
            jsonOptions.Converters.Add(Serialization.JsonFSharpConverter())
            let cacheInfoJson = IO.File.ReadAllText(CacheInfo.GetPath(fableModulesPath))
            JsonSerializer.Deserialize<CacheInfo>(cacheInfoJson, jsonOptions) |> Some
        with _ -> None

    member this.Write(fableModulesPath: string) =
        let jsonOptions = JsonSerializerOptions()
        jsonOptions.Converters.Add(Serialization.JsonFSharpConverter())
        IO.File.WriteAllText(CacheInfo.GetPath(fableModulesPath), JsonSerializer.Serialize(this, jsonOptions))

type CrackerOptions(fableOpts: CompilerOptions, fableLib, outDir, configuration, exclude, replace, noCache, noRestore, projFile) =
    let builtDlls = HashSet()
    let fableModulesDir = CrackerOptions.GetFableModulesDir(projFile, outDir, fableOpts.Language)
    let cacheInfo =
        if noCache then None
        else CacheInfo.TryRead(fableModulesDir)

    member _.CacheInfo = cacheInfo
    member _.FableModulesDir = fableModulesDir
    member _.FableOptions: CompilerOptions = fableOpts
    member _.FableLib: string option = fableLib
    member _.OutDir: string option = outDir
    member _.Configuration: string = configuration
    member _.Exclude: string option = exclude
    member _.Replace: Map<string, string> = replace
    member _.NoRestore: bool = noRestore
    member _.ProjFile: string = projFile
    member _.BuildDll(normalizedDllPath: string) =
        if not(builtDlls.Contains(normalizedDllPath)) then
            let projDir =
                normalizedDllPath.Split('/')
                |> Array.rev
                |> Array.skipWhile (fun part -> part <> "bin")
                |> Array.skip 1
                |> Array.rev

             |> String.concat "/"
            Process.runSync projDir "dotnet" ["build"; "-c"; configuration] |> ignore
            builtDlls.Add(normalizedDllPath) |> ignore

    static member GetFableModulesDir(projFile: string, outDir: string option, lang) =
        let fableModulesDir =
            let baseDir = outDir |> Option.defaultWith (fun () -> IO.Path.GetDirectoryName(projFile))
            let dirName =
                // TODO: Now PY.Naming.fableModulesDir and Naming.fableHiddenDir are the same,
                // should we still keep them separated?
                match lang with
                | Python -> PY.Naming.fableModulesDir
                | _ -> Naming.fableHiddenDir
            IO.Path.Combine(baseDir, dirName)

        if File.isDirectoryEmpty fableModulesDir then
            IO.Directory.CreateDirectory(fableModulesDir) |> ignore
            IO.File.WriteAllText(IO.Path.Combine(fableModulesDir, ".gitignore"), "**/*")

        fableModulesDir

type CrackerResponse =
    { FableLibDir: string
      References: string list
      ProjectOptions: FSharpProjectOptions
      OutputType: string option }

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.")
        || pkgName.StartsWith("Microsoft.")
        || pkgName.StartsWith("runtime.")
        || pkgName = "NETStandard.Library"
        || pkgName = "FSharp.Core"
        || pkgName = "Fable.Core"

type CrackedFsproj =
    { ProjectFile: string
      SourceFiles: string list
      ProjectReferences: string list
      DllReferences: IDictionary<string, string>
      PackageReferences: FablePackage list
      OtherCompilerOptions: string list
      OutputType: string option }

let makeProjectOptions project sources otherOptions: FSharpProjectOptions =
    { ProjectId = None
      ProjectFileName = project
      SourceFiles = Array.distinct sources
      OtherOptions = otherOptions
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.UtcNow
      UnresolvedReferences = None
      OriginalLoadReferences = []
      Stamp = None }

let tryGetFablePackage (opts: CrackerOptions) (dllPath: string) =
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
            let pkgId = metadata |> child "id"
            let fsprojPath =
                match Map.tryFind pkgId opts.Replace with
                | Some fsprojPath -> fsprojPath
                | None -> fsprojPath
            { Id = pkgId
              Version = metadata |> child "version"
              FsprojPath = fsprojPath
              DllPath = dllPath
              SourcePaths = []
              Dependencies =
                metadata.Elements()
                |> firstWithName "dependencies" |> elements
                // We don't consider different frameworks
                |> firstGroupOrAllDependencies
                |> Seq.map (attr "id")
                |> Seq.filter (isSystemPackage >> not)
                |> Set
            }: FablePackage |> Some
        | _ -> None

let sortFablePackages (pkgs: FablePackage list) =
    ([], pkgs) ||> List.fold (fun acc pkg ->
        match List.tryFindIndexBack (fun (x: FablePackage) -> pkg.Dependencies.Contains(x.Id)) acc with
        | None -> pkg::acc
        | Some targetIdx ->
            let rec insertAfter x targetIdx i before after =
                match after with
                | justBefore::after ->
                    if i = targetIdx then
                        if i > 0 then
                            let dependent, nonDependent =
                                List.rev before |> List.partition (fun (x: FablePackage) ->
                                    x.Dependencies.Contains(pkg.Id))
                            nonDependent @ justBefore::x::dependent @ after
                        else
                            (justBefore::before |> List.rev) @ x::after
                    else
                        insertAfter x targetIdx (i + 1) (justBefore::before) after
                | [] -> failwith "Unexpected empty list in insertAfter"
            insertAfter pkg targetIdx 0 [] acc
    )

let private getDllName (dllFullPath: string) =
    let i = dllFullPath.LastIndexOf('/')
    dllFullPath.[(i + 1) .. (dllFullPath.Length - 5)] // -5 removes the .dll extension

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some [for x in m.Groups -> x.Value]
    else None

// GetProjectOptionsFromScript doesn't work with latest FCS
// This is adapted from fable-compiler-js ProjectParser, maybe we should try to unify code
let getProjectOptionsFromScript (opts: CrackerOptions): CrackedFsproj list * CrackedFsproj =

    let projectFilePath = opts.ProjFile
    let projectDir = IO.Path.GetDirectoryName projectFilePath

    let dllRefs, srcFiles =
        (([], []), IO.File.ReadLines(projectFilePath))
        ||> Seq.fold (fun (dllRefs, srcFiles) line ->
            match line.Trim() with
            // TODO: Check nuget references
            | Regex @"^#r\s*""(.*?)""$" [_;path] ->
                path::dllRefs, srcFiles
            | Regex @"^#load\s*""(.*?)""$" [_;path] ->
                dllRefs, path::srcFiles
            | _ -> dllRefs, srcFiles)

    let coreDllDir = IO.Path.GetDirectoryName(typeof<Array>.Assembly.Location) |> Path.normalizePath
    let fsharpCoreDll = typeof<obj list>.Assembly.Location |> Path.normalizePath

    let coreDlls =
        Metadata.coreAssemblies
        |> Array.filter (function
            | "FSharp.Core" | "Fable.Core" -> false
            | _ -> true)
        |> Array.append [|"System.Private.CoreLib"|]
        |> Array.map (fun dll -> IO.Path.Combine(coreDllDir, dll + ".dll"))

    let dllRefs =
        [
            yield! coreDlls
            yield fsharpCoreDll
            yield! List.rev dllRefs
        ]
        |> List.map (fun dllRef ->
            let dllRef = IO.Path.Combine(projectDir, dllRef) |> Path.normalizeFullPath
            getDllName dllRef, dllRef)
        |> dict

    let srcFiles =
        srcFiles
        |> List.map (fun srcFile -> IO.Path.Combine(projectDir, srcFile) |> Path.normalizeFullPath)

    let srcFiles =
        projectFilePath::srcFiles
        |> List.rev

    [], { ProjectFile = projectFilePath
          SourceFiles = srcFiles
          ProjectReferences = []
          DllReferences = dllRefs
          PackageReferences = []
          OtherCompilerOptions = []
          OutputType = None }

let getBasicCompilerArgs (opts: CrackerOptions) =
    [|
        // yield "--debug"
        // yield "--debug:portable"
        yield "--noframework"
        yield "--nologo"
        yield "--simpleresolution"
        yield "--nocopyfsharpcore"
        // yield "--define:DEBUG"
        for constant in opts.FableOptions.Define do
            yield "--define:" + constant
        yield "--optimize" + if opts.FableOptions.OptimizeFSharpAst then "+" else "-"
        // yield "--nowarn:NU1603,NU1604,NU1605,NU1608"
        // yield "--warnaserror:76"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        // Since net5.0 there's no difference between app/library
        // yield "--target:library"
    |]

/// Simplistic XML-parsing of .fsproj to get source files, as we cannot
/// run `dotnet restore` on .fsproj files embedded in Nuget packages.
let getSourcesFromFablePkg (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)

    let xmlDoc = XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile)

    Log.showFemtoMsg (fun () ->
        xmlDoc.Root.Elements()
        |> withName "PropertyGroup"
        |> Seq.exists (fun propGroup ->
            propGroup.Elements()
            |> withName "NpmDependencies"
            |> Seq.isEmpty
            |> not))

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

let private isUsefulOption (opt : string) =
    [ "--define"
      "--nowarn"
      "--warnon"
    //   "--warnaserror" // Disable for now to prevent unexpected errors, see #2288
    //   "--langversion" // See getBasicCompilerArgs
    ]
    |> List.exists opt.StartsWith

let excludeProjRef (opts: CrackerOptions) (dllRefs: IDictionary<string,string>) (projRef: string) =
    let projName = Path.GetFileNameWithoutExtension(projRef)
    match opts.Exclude with
    | Some e when projRef.Contains(e) ->
        try
            opts.BuildDll(dllRefs.[projName])
        with e ->
            Log.always("Couldn't build " + projName + ": " + e.Message)
        None
    | _ ->
        let removed = dllRefs.Remove(projName)
        // if not removed then
        //     Log.always("Couldn't remove project reference " + projName + " from dll references")
        Path.normalizeFullPath projRef |> Some

/// Use Dotnet.ProjInfo (through ProjectCoreCracker) to invoke MSBuild
/// and get F# compiler args from an .fsproj file. As we'll merge this
/// later with other projects we'll only take the sources and the references,
/// checking if some .dlls correspond to Fable libraries
let fullCrack (opts: CrackerOptions): CrackedFsproj =
    let projFile = opts.ProjFile
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.OrdinalIgnoreCase)

    // Try restoring project
    let projDir = IO.Path.GetDirectoryName projFile
    let projName = IO.Path.GetFileName projFile

    if not opts.NoRestore then
        Process.runSync projDir "dotnet" ["restore"; projName] |> ignore

    let projOpts, projRefs, _msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile opts.Configuration projFile

    // let targetFramework =
    //     match Map.tryFind "TargetFramework" msbuildProps with
    //     | Some targetFramework -> targetFramework
    //     | None -> failwithf "Cannot find TargetFramework for project %s" projFile

    let outputType = Map.tryFind "OutputType" _msbuildProps

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

    let fablePkgs =
        let dllRefs' = dllRefs |> Seq.map (fun (KeyValue(k,v)) -> k,v) |> Seq.toArray
        dllRefs' |> Seq.choose (fun (dllName, dllPath) ->
            match tryGetFablePackage opts dllPath with
            | Some pkg ->
                dllRefs.Remove(dllName) |> ignore
                Some pkg
            | None -> None)
        |> Seq.toList
        |> sortFablePackages

    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = List.choose (excludeProjRef opts dllRefs) projRefs
      DllReferences = dllRefs
      PackageReferences = fablePkgs
      OtherCompilerOptions = otherOpts
      OutputType = outputType }

/// For project references of main project, ignore dll and package references
let easyCrack (opts: CrackerOptions) dllRefs (projFile: string): CrackedFsproj =
    let projOpts, projRefs, _msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile opts.Configuration projFile

    let outputType = Map.tryFind "OutputType" _msbuildProps
    let sourceFiles, otherOpts =
        (projOpts.OtherOptions, ([], []))
        ||> Array.foldBack (fun line (src, otherOpts) ->
            if isUsefulOption line then
                src, line::otherOpts
            elif line.StartsWith("-") then
                src, otherOpts
            else
                (Path.normalizeFullPath line)::src, otherOpts)

    { ProjectFile = projFile
      SourceFiles = sourceFiles
      ProjectReferences = List.choose (excludeProjRef opts dllRefs) projRefs
      DllReferences = Dictionary()
      PackageReferences = []
      OtherCompilerOptions = otherOpts
      OutputType = outputType}

let getCrackedProjectsFromMainFsproj (opts: CrackerOptions) =
    let mainProj = fullCrack opts
    let rec crackProjects (acc: CrackedFsproj list) (projFile: string) =
        let crackedFsproj =
            match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
            | None -> easyCrack opts mainProj.DllReferences projFile
            | Some crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        List.fold crackProjects (crackedFsproj::acc) crackedFsproj.ProjectReferences
    let refProjs =
        List.fold crackProjects [] mainProj.ProjectReferences
        |> List.distinctBy (fun x -> x.ProjectFile)
    refProjs, mainProj

let getCrackedProjects (opts: CrackerOptions) =
    match (Path.GetExtension opts.ProjFile).ToLower() with
    | ".fsx" ->
        getProjectOptionsFromScript opts
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

// Replace the .fsproj extension with .fableproj for files in fable_modules
// We do this to avoid conflicts with other F# tooling that scan for .fsproj files
let changeFsprojToFableproj (path: string) =
    if path.EndsWith(".fsproj") then
        IO.Path.ChangeExtension(path, Naming.fableProjExt)
    else path

let copyDirIfDoesNotExist replaceFsprojExt (source: string) (target: string) =
    if File.isDirectoryEmpty target then
        IO.Directory.CreateDirectory(target) |> ignore
        if IO.Directory.Exists source |> not then
            failwith ("Source directory is missing: " + source)
        let source = source.TrimEnd('/', '\\')
        let target = target.TrimEnd('/', '\\')
        for dirPath in IO.Directory.GetDirectories(source, "*", IO.SearchOption.AllDirectories) do
            IO.Directory.CreateDirectory(dirPath.Replace(source, target)) |> ignore
        for fromPath in IO.Directory.GetFiles(source, "*.*", IO.SearchOption.AllDirectories) do
            let toPath = fromPath.Replace(source, target)
            let toPath = if replaceFsprojExt then changeFsprojToFableproj toPath else toPath
            IO.File.Copy(fromPath, toPath, true)

let getFableLibraryPath (opts: CrackerOptions) =
    match opts.FableLib with
    | Some path -> Path.normalizeFullPath path
    | None ->
        let buildDir, libDir =
            match opts.FableOptions.Language with
            | Python -> "fable-library-py/fable_library", "fable_library"
            | _ -> "fable-library", "fable-library" + "." + Literals.VERSION

        let assemblyDir =
            Process.getCurrentAssembly().Location
            |> Path.GetDirectoryName

        let defaultFableLibraryPaths =
            [ $"../../../{buildDir}/"               // running from nuget tools package
              $"../../../../../build/{buildDir}/" ] // running from bin/Release/net5
            |> List.map (fun x -> Path.GetFullPath(Path.Combine(assemblyDir, x)))

        let fableLibrarySource =
            defaultFableLibraryPaths
            |> List.tryFind IO.Directory.Exists
            |> Option.defaultValue (List.last defaultFableLibraryPaths)

        if File.isDirectoryEmpty fableLibrarySource then
            failwithf "fable-library directory is empty, please build FableLibrary: %s" fableLibrarySource

        Log.verbose(lazy ("fable-library: " + fableLibrarySource))
        let fableLibraryTarget = IO.Path.Combine(opts.FableModulesDir, libDir)
        copyDirIfDoesNotExist false fableLibrarySource fableLibraryTarget
        fableLibraryTarget

let copyFableLibraryAndPackageSources (opts: CrackerOptions) (pkgs: FablePackage list) =
    let pkgRefs =
        pkgs |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)
            let targetDir = IO.Path.Combine(opts.FableModulesDir, pkg.Id + "." + pkg.Version)
            copyDirIfDoesNotExist true sourceDir targetDir
            let fsprojFile = IO.Path.GetFileName(pkg.FsprojPath) |> changeFsprojToFableproj
            { pkg with FsprojPath = IO.Path.Combine(targetDir, fsprojFile) })

    getFableLibraryPath opts, pkgRefs

// Separate handling for Python. Use plain lowercase package names without dots or version info.
let copyFableLibraryAndPackageSourcesPy (opts: CrackerOptions) (pkgs: FablePackage list) =
    let pkgRefs =
        pkgs |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)
            let targetDir =
                let name = Naming.applyCaseRule Core.CaseRules.SnakeCase pkg.Id
                IO.Path.Combine(opts.FableModulesDir, name.Replace(".", "_"))
            copyDirIfDoesNotExist false sourceDir targetDir
            { pkg with FsprojPath = IO.Path.Combine(targetDir, IO.Path.GetFileName(pkg.FsprojPath)) })

    getFableLibraryPath opts, pkgRefs

// See #1455: F# compiler generates *.AssemblyInfo.fs in obj folder, but we don't need it
let removeFilesInObjFolder sourceFiles =
    let reg = System.Text.RegularExpressions.Regex(@"[\\\/]obj[\\\/]")
    sourceFiles |> Array.filter (reg.IsMatch >> not)

let getFullProjectOpts (opts: CrackerOptions) =
    if not(IO.File.Exists(opts.ProjFile)) then
        failwith ("File does not exist: " + opts.ProjFile)

    let isCacheInfoOutdated (cacheInfo: CacheInfo) =
        let isFileOld (path: string) =
            IO.File.Exists(path)
            && cacheInfo.TimestampUTC > IO.File.GetLastWriteTime(path).ToUniversalTime()
        [
            cacheInfo.ProjectPath
            yield! cacheInfo.References
        ]
        |> List.forall (fun fsproj ->
            isFileOld(fsproj)
            // If project uses Paket, dependencies may have updated without touching the .fsproj
            && IO.Path.Combine(IO.Path.GetDirectoryName(fsproj), "obj", "project.assets.json") |> isFileOld)
        |> not

    // TODO: Check fable_modules contains all packages
    let cacheInfo =
        opts.CacheInfo |> Option.bind (fun cacheInfo ->
            if cacheInfo.Version <> Literals.VERSION
                || cacheInfo.FableOptions <> opts.FableOptions
                || isCacheInfoOutdated cacheInfo
            then None
            else Some cacheInfo)

    match cacheInfo with
    | Some cacheInfo ->
        Log.always $"Retrieving project options from cache, in case of issues run `dotnet fable clean` or try `--noCache` option."
        { ProjectOptions = makeProjectOptions opts.ProjFile cacheInfo.SourcePaths cacheInfo.FSharpOptions
          References = cacheInfo.References
          FableLibDir = cacheInfo.FableLibDir
          OutputType = cacheInfo.OutputType }

    | None ->
        let projRefs, mainProj = retryGetCrackedProjects opts

        let fableLibDir, pkgRefs =
            match opts.FableOptions.Language with
            | Python ->
                copyFableLibraryAndPackageSourcesPy opts mainProj.PackageReferences
            | _ ->
                copyFableLibraryAndPackageSources opts mainProj.PackageReferences

        let pkgRefs =
            pkgRefs |> List.map (fun pkg ->
                { pkg with SourcePaths = getSourcesFromFablePkg pkg.FsprojPath })

        let sourceFiles =
            let pkgSources = pkgRefs |> List.collect (fun x -> x.SourcePaths)
            let refSources = projRefs |> List.collect (fun x -> x.SourceFiles)
            pkgSources @ refSources @ mainProj.SourceFiles |> List.toArray |> removeFilesInObjFolder

        let refOptions =
            projRefs
            |> List.collect (fun x -> x.OtherCompilerOptions)
            |> List.toArray

        let otherOptions =
            [|
                yield! refOptions // merged options from all referenced projects
                yield! mainProj.OtherCompilerOptions // main project compiler options
                yield! getBasicCompilerArgs opts // options from compiler args
                yield "--optimize" + (if opts.FableOptions.OptimizeFSharpAst then "+" else "-")
            |]
            |> Array.distinct

        let dllRefs =
            let coreRefs = HashSet Metadata.coreAssemblies
            coreRefs.Add("System.Private.CoreLib") |> ignore
            let ignoredRefs = HashSet [
                "WindowsBase"
                "Microsoft.Win32.Primitives"
                "Microsoft.VisualBasic"
                "Microsoft.VisualBasic.Core"
                "Microsoft.CSharp"
            ]
            [|
                // We only keep dllRefs for the main project
                yield! mainProj.DllReferences.Values
                        // Remove unneeded System dll references
                        |> Seq.choose (fun r ->
                            let name = getDllName r
                            if ignoredRefs.Contains(name) ||
                                (name.StartsWith("System.") && not(coreRefs.Contains(name))) then None
                            else Some("-r:" + r))
            |]

        let outputType = mainProj.OutputType
        let projRefs = projRefs |> List.map (fun p -> p.ProjectFile)
        let otherOptions = Array.append otherOptions dllRefs

        let cacheInfo: CacheInfo =
            {
                Version = Literals.VERSION
                FableLibDir = fableLibDir
                FableOptions = opts.FableOptions
                ProjectPath = opts.ProjFile
                FSharpOptions = otherOptions
                SourcePaths = sourceFiles
                References = projRefs
                OutputType = outputType
                TimestampUTC = DateTime.UtcNow
            }

        cacheInfo.Write(opts.FableModulesDir)

        { ProjectOptions = makeProjectOptions opts.ProjFile sourceFiles otherOptions
          References = projRefs
          FableLibDir = fableLibDir
          OutputType = outputType }
