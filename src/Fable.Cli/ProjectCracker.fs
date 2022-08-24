/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Cli.ProjectCracker

open System
open System.Xml.Linq
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Fable
open Fable.AST
open Globbing.Operators

open Ionide.ProjInfo
open Ionide.ProjInfo.Types

// This gets the job done -- more data/fields can be added as needed.
// Otherwise Ionide.ProjInfo.FCS could be useful, as this directly gives a FSharpProjectOptions
type RawProjectInfo =
    { ProjectFile: string
      OtherOptions: string list
      ReferencedProjects: string list
      SourceFiles: string list
      OutputType: OutputType }

let private logProjectLoad = function
    | WorkspaceProjectState.Loading projFile -> Log.verbose (lazy $"Loading project '{projFile}'.")
    | WorkspaceProjectState.Loaded (projFile, _, fromCache) ->
        Log.verbose (lazy sprintf "Successfully loaded project '%s'%s." projFile.ProjectFileName (if fromCache then " from cache" else ""))
    | WorkspaceProjectState.Failed (_project, reason) ->
        // TODO see if the error was due to a locked file and
        // raise a different exception to be caught by retryGetCrackedProjects
        let projFile, message =
            match reason with
            | GenericError (projFile, msg) -> projFile, $"'{msg}'"
            | ProjectNotRestored projFile -> projFile, "the project is not restored."
            | ProjectNotFound projFile -> projFile, "the project file was not found."
            // it seems like the following cases are currently unused by Ionide.ProjInfo
            | LanguageNotSupported projFile -> projFile, "the project language is not supported."
            | ProjectNotLoaded projFile -> projFile, "the project is not loaded."
            | MissingExtraProjectInfos projFile -> projFile, "there is missing extra info."
            | InvalidExtraProjectInfos (projFile, error) -> projFile, $"there is invalid extra info: '{error}'."
            | ReferencesNotLoaded (projFile, referenceErrors) ->
                let refs = referenceErrors |> Seq.map fst |> String.concat "\n  "
                projFile, $"the following references are not loaded/have errors:\n  {refs}"
        Fable.FableError $"Failed to load project '{projFile}' because {message}" |> raise

let private rawProjInfo additionalMSBuildProps (projFile: string) =
    let toolsPath = Init.init (IO.DirectoryInfo <| Path.GetDirectoryName projFile) None
    let loader = WorkspaceLoader.Create(toolsPath, additionalMSBuildProps)
    loader.Notifications.Add logProjectLoad
    let proj = loader.LoadProjects [projFile] |> Seq.find (fun p -> p.ProjectFileName = projFile)

    // TODO cache projects info of p2p ref
    //   let p2pProjects =
    //       p2ps
    //       // do not follow others lang project, is not supported by FCS anyway
    //       |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
    //       |> List.map (fun p2p -> p2p.ProjectReferenceFullPath |> projInfo ["TargetFramework", p2p.TargetFramework] )

    // p2p.ProjectFileName is absolute, p2p.RelativePath is relative
    let projRefs = proj.ReferencedProjects |> List.map (fun p2p -> p2p.ProjectFileName)

    let outputType =
        match proj.ProjectOutputType with
        | Library -> OutputType.Library
        | Exe -> OutputType.Exe
        | Custom output ->
            Log.warning $"Unknown output type '{output}' for '{projFile}'. Defaulting to output type 'Library'."
            OutputType.Library

    // All paths including ones in OtherOptions are absolute but unnormalized (except ProjectFile which is normalized)
    { ProjectFile = projFile
      OtherOptions = proj.OtherOptions
      ReferencedProjects = projRefs
      SourceFiles = proj.SourceFiles
      OutputType = outputType }

let getRawProjectOptionsFromProjectFile configuration (projFile : string) =
    rawProjInfo ["Configuration", configuration] projFile

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
        OutDir: string option
        FableLibDir: string
        FableModulesDir: string
        OutputType: OutputType
        Exclude: string option
        SourceMaps: bool
        SourceMapsRoot: string option
    }
    static member GetPath(fableModulesDir: string, isDebug: bool) =
        IO.Path.Combine(fableModulesDir, $"""project_cracked{if isDebug then "_debug" else ""}.json""")

    member this.GetTimestamp() =
        CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)
        |> IO.File.GetLastWriteTime

    static member TryRead(fableModulesDir: string, isDebug): CacheInfo option =
        try
            CacheInfo.GetPath(fableModulesDir, isDebug) |> Json.read<CacheInfo> |> Some
        with _ -> None

    member this.Write() =
        let path = CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)
        Json.write path this

    /// Checks if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
    member this.IsMostRecent =
        match CacheInfo.TryRead(this.FableModulesDir, not this.FableOptions.DebugMode) with
        | None -> true
        | Some other -> this.GetTimestamp() > other.GetTimestamp()

type CrackerOptions(cliArgs: CliArgs) =
    let builtDlls = HashSet()
    let fableModulesDir = CrackerOptions.GetFableModulesFromProject(cliArgs.ProjectFile, cliArgs.OutDir, cliArgs.NoCache)
    let cacheInfo =
        if cliArgs.NoCache then None
        else CacheInfo.TryRead(fableModulesDir, cliArgs.CompilerOptions.DebugMode)

    member _.NoCache = cliArgs.NoCache
    member _.CacheInfo = cacheInfo
    member _.FableModulesDir = fableModulesDir
    member _.FableOptions: CompilerOptions = cliArgs.CompilerOptions
    member _.FableLib: string option = cliArgs.FableLibraryPath
    member _.OutDir: string option = cliArgs.OutDir
    member _.Configuration: string = cliArgs.Configuration
    member _.Exclude: string option = cliArgs.Exclude
    member _.Replace: Map<string, string> = cliArgs.Replace
    member _.PrecompiledLib: string option = cliArgs.PrecompiledLib
    member _.NoRestore: bool = cliArgs.NoRestore
    member _.ProjFile: string = cliArgs.ProjectFile
    member _.SourceMaps: bool = cliArgs.SourceMaps
    member _.SourceMapsRoot: string option = cliArgs.SourceMapsRoot

    member _.BuildDll(normalizedDllPath: string) =
        if not(builtDlls.Contains(normalizedDllPath)) then
            let projDir =
                normalizedDllPath.Split('/')
                |> Array.rev
                |> Array.skipWhile (fun part -> part <> "bin")
                |> Array.skip 1
                |> Array.rev
                |> String.concat "/"
            Process.runSync projDir "dotnet" ["build"; "-c"; cliArgs.Configuration] |> ignore
            builtDlls.Add(normalizedDllPath) |> ignore

    static member GetFableModulesFromDir(baseDir: string): string =
        IO.Path.Combine(baseDir, Naming.fableModules)
        |> Path.normalizePath

    static member GetFableModulesFromProject(projFile: string, outDir: string option, noCache: bool): string =
        let fableModulesDir =
            outDir
            |> Option.defaultWith (fun () -> IO.Path.GetDirectoryName(projFile))
            |> CrackerOptions.GetFableModulesFromDir

        if noCache then
            try
                IO.Directory.Delete(fableModulesDir, recursive=true)
            with _ -> ()

        if File.isDirectoryEmpty fableModulesDir then
            IO.Directory.CreateDirectory(fableModulesDir) |> ignore
            IO.File.WriteAllText(IO.Path.Combine(fableModulesDir, ".gitignore"), "**/*")

        fableModulesDir

type CrackerResponse =
    { FableLibDir: string
      FableModulesDir: string
      References: string list
      ProjectOptions: FSharpProjectOptions
      OutputType: OutputType
      PrecompiledInfo: PrecompiledInfoImpl option
      CanReuseCompiledFiles: bool }

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
      OutputType: OutputType }

let makeProjectOptions (opts: CrackerOptions) otherOptions sources: FSharpProjectOptions =
    let otherOptions = [|
        yield! otherOptions
        for constant in opts.FableOptions.Define do
            yield "--define:" + constant
        yield "--optimize" + if opts.FableOptions.OptimizeFSharpAst then "+" else "-"
    |]
    { ProjectId = None
      ProjectFileName = opts.ProjFile
      OtherOptions = otherOptions
      SourceFiles = Array.distinct sources
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
                | Some replaced ->
                    if replaced.EndsWith(".fsproj") then replaced
                    else tryFileWithPattern replaced "*.fsproj" |> Option.defaultValue fsprojPath
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

let getBasicCompilerArgs () =
    [|
        // yield "--debug"
        // yield "--debug:portable"
        yield "--noframework"
        yield "--nologo"
        yield "--simpleresolution"
        yield "--nocopyfsharpcore"
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
        |> function
        | path when (path.Contains("*") || path.Contains("?")) ->
            match !! path |> List.ofSeq with
            | [] -> [ path ]
            | globResults -> globResults
        | path -> [ path ]
        |> List.map Path.normalizeFullPath)

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
        let _removed = dllRefs.Remove(projName)
        // if not removed then
        //     Log.always("Couldn't remove project reference " + projName + " from dll references")
        Path.normalizeFullPath projRef |> Some

let getCrackedFsproj (opts: CrackerOptions) (projInfo: RawProjectInfo) =
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.OrdinalIgnoreCase)

    // let targetFramework =
    //     match Map.tryFind "TargetFramework" msbuildProps with
    //     | Some targetFramework -> targetFramework
    //     | None -> failwithf "Cannot find TargetFramework for project %s" projFile

    let sourceFiles = projInfo.SourceFiles |> List.map Path.normalizePath

    let otherOpts =
        projInfo.OtherOptions |> List.choose (fun (line: string) ->
            if line.StartsWith("-r:") then
                let line = Path.normalizePath (line.[3..])
                let dllName = getDllName line
                dllRefs.Add(dllName, line)
                None
            elif isUsefulOption line then
                Some line
            else
                None)

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

    { ProjectFile = projInfo.ProjectFile
      SourceFiles = sourceFiles
      ProjectReferences = List.choose (excludeProjRef opts dllRefs) projInfo.ReferencedProjects
      DllReferences = dllRefs
      PackageReferences = fablePkgs
      OtherCompilerOptions = otherOpts
      OutputType = projInfo.OutputType }

let getProjectOptionsFromScript (opts: CrackerOptions): CrackedFsproj =
    let projectFilePath = opts.ProjFile

    let projOpts, _diagnostics = // TODO: Check diagnostics
        let checker = FSharpChecker.Create()
        let text = File.readAllTextNonBlocking(projectFilePath) |> SourceText.ofString
        checker.GetProjectOptionsFromScript(projectFilePath, text, useSdkRefs=true, assumeDotNetFramework=false)
        |> Async.RunSynchronously

    getCrackedFsproj
        opts
        { ProjectFile = projectFilePath
          OtherOptions = List.ofArray projOpts.OtherOptions
          ReferencedProjects = []
          SourceFiles = List.ofArray projOpts.SourceFiles
          OutputType = OutputType.Exe }

/// Use Ionide.ProjInfo to invoke MSBuild
/// and get F# compiler args from an .fsproj file. As we'll merge this
/// later with other projects we'll only take the sources and the references,
/// checking if some .dlls correspond to Fable libraries.
let fullCrack (opts: CrackerOptions): CrackedFsproj =
    let projFile = opts.ProjFile
    let projDir = IO.Path.GetDirectoryName projFile
    let projName = IO.Path.GetFileName projFile

    if not opts.NoRestore then
        Process.runSync projDir "dotnet" ["restore"; projName] |> ignore

    // let targetFramework =
    //     match Map.tryFind "TargetFramework" msbuildProps with
    //     | Some targetFramework -> targetFramework
    //     | None -> failwithf "Cannot find TargetFramework for project %s" projFile

    getRawProjectOptionsFromProjectFile opts.Configuration projFile |> getCrackedFsproj opts

/// For project references of main project, ignore dll and package references
let easyCrack (opts: CrackerOptions) dllRefs (projFile: string): CrackedFsproj =
    let projInfo = getRawProjectOptionsFromProjectFile opts.Configuration projFile

    { ProjectFile = projFile
      SourceFiles = projInfo.SourceFiles |> List.map Path.normalizePath
      ProjectReferences = List.choose (excludeProjRef opts dllRefs) projInfo.ReferencedProjects
      DllReferences = Dictionary()
      PackageReferences = []
      OtherCompilerOptions = projInfo.OtherOptions |> List.filter isUsefulOption
      OutputType = projInfo.OutputType }

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
        [], getProjectOptionsFromScript opts
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
            | Python ->
                match opts.FableLib with
                | Some PY.Naming.sitePackages -> "fable-library-py", "fable-library"
                | _ -> "fable-library-py/fable_library", "fable_library"
            | Dart -> "fable-library-dart", "fable_library"
            | Rust -> "fable-library-rust", "fable-library-rust"
            | _ -> "fable-library", "fable-library" + "." + Literals.VERSION

        let fableLibrarySource =
            AppContext.BaseDirectory
            |> File.tryFindNonEmptyDirectoryUpwards {| matches = [buildDir; "build/" + buildDir]; exclude = ["src"] |}
            |> Option.defaultWith (fun () -> Fable.FableError "Cannot find fable-library" |> raise)

        Log.verbose(lazy ("fable-library: " + fableLibrarySource))
        let fableLibraryTarget = IO.Path.Combine(opts.FableModulesDir, libDir)
        copyDirIfDoesNotExist false fableLibrarySource fableLibraryTarget
        Path.normalizeFullPath fableLibraryTarget

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
                match opts.FableLib with
                | Some PY.Naming.sitePackages ->
                    let name = Naming.applyCaseRule Core.CaseRules.KebabCase pkg.Id
                    IO.Path.Combine(opts.FableModulesDir, name.Replace(".", "-"))
                | _ ->
                    let name = Naming.applyCaseRule Core.CaseRules.SnakeCase pkg.Id
                    IO.Path.Combine(opts.FableModulesDir, name.Replace(".", "_"))
            copyDirIfDoesNotExist false sourceDir targetDir
            { pkg with FsprojPath = IO.Path.Combine(targetDir, IO.Path.GetFileName(pkg.FsprojPath)) })

    getFableLibraryPath opts, pkgRefs

// See #1455: F# compiler generates *.AssemblyInfo.fs in obj folder, but we don't need it
let removeFilesInObjFolder sourceFiles =
    let reg = System.Text.RegularExpressions.Regex(@"[\\\/]obj[\\\/]")
    sourceFiles |> Array.filter (reg.IsMatch >> not)

let loadPrecompiledInfo (opts: CrackerOptions) otherOptions sourceFiles =
    // Sources in fable_modules correspond to packages and they're qualified with the version
    // (e.g. fable_modules/Fable.Promise.2.1.0/Promise.fs) so we assume they're the same wherever they are
    // TODO: Check if this holds true also for Python which may not include the version number in the path
    let normalizePath (path: string) =
        let i = path.IndexOf(Naming.fableModules)
        if i >= 0 then path.[i..] else path

    match opts.PrecompiledLib with
    | Some precompiledLib ->
        // Load PrecompiledInfo
        let info = CrackerOptions.GetFableModulesFromDir(precompiledLib) |> PrecompiledInfoImpl.Load

        // Check if precompiled compiler version and options match
        if info.CompilerVersion <> Literals.VERSION then
            Fable.FableError($"Library was precompiled using Fable v{info.CompilerVersion} but you're using v{Literals.VERSION}. Please use same version.") |> raise

        // Sometimes it may be necessary to use different options for the precompiled lib so don't throw an error here
        //if info.CompilerOptions <> opts.FableOptions then
        //    FableError($"Library was precompiled using different compiler options. Please use same options.") |> raise

        // Check if precompiled files are up-to-date
        info.Files |> Seq.choose (fun (KeyValue(file, { OutPath = outPath })) ->
            if File.existsAndIsNewerThanSource file outPath then None else Some file)
        |> Seq.toList
        |> function
            | [] -> ()
            | outdated ->
                let outdated = outdated |> List.map (fun f -> "    " + File.relPathToCurDir f) |> String.concat Log.newLine
                // TODO: This should likely be an error but make it a warning for now
                Log.warning($"Detected outdated files in precompiled lib:{Log.newLine}{outdated}")

        // Remove precompiled files from sources and add reference to precompiled .dll to other options
        let otherOptions = Array.append otherOptions [|"-r:" + info.DllPath|]
        let precompiledFiles = Map.keys info.Files |> Seq.map normalizePath |> set
        let sourceFiles = sourceFiles |> Array.filter (fun path ->
            normalizePath path |> precompiledFiles.Contains |> not)

        Some info, otherOptions, sourceFiles
    | None ->
        None, otherOptions, sourceFiles

let getFullProjectOpts (opts: CrackerOptions) =
    if not(IO.File.Exists(opts.ProjFile)) then
        Fable.FableError("Project file does not exist: " + opts.ProjFile) |> raise

    // Make sure cache info corresponds to same compiler version and is not outdated
    let cacheInfo =
        opts.CacheInfo |> Option.filter (fun cacheInfo ->
            let cacheTimestamp = cacheInfo.GetTimestamp()
            let isOlderThanCache filePath =
                let fileTimestamp = IO.File.GetLastWriteTime(filePath)
                let isOlder = fileTimestamp < cacheTimestamp
                if not isOlder then
                    Log.verbose(lazy $"Cached project info ({cacheTimestamp}) will be discarded because {File.relPathToCurDir filePath} ({fileTimestamp}) is newer")
                isOlder

            cacheInfo.Version = Literals.VERSION
            && cacheInfo.Exclude = opts.Exclude
            && (
                [
                    cacheInfo.ProjectPath
                    yield! cacheInfo.References
                ]
                |> List.forall (fun fsproj ->
                    if IO.File.Exists(fsproj) && isOlderThanCache fsproj then
                        // Check if the project uses Paket
                        let fsprojDir = IO.Path.GetDirectoryName(fsproj)
                        let paketReferences = IO.Path.Combine(fsprojDir, "paket.references")
                        if not(IO.File.Exists(paketReferences)) then true
                        else
                            if isOlderThanCache paketReferences then
                                // Only check paket.lock for main project and assume it's the same for references
                                if fsproj <> cacheInfo.ProjectPath then true
                                else
                                    match File.tryFindUpwards "paket.lock" fsprojDir with
                                    | Some paketLock -> isOlderThanCache paketLock
                                    | None -> false
                            else false
                    else false
                )
            )
        )

    match cacheInfo with
    | Some cacheInfo ->
        Log.always $"Retrieving project options from cache, in case of issues run `dotnet fable clean` or try `--noCache` option."

        // Check if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
        // (this means the last compilation was done for another build mode so we cannot reuse the files)
        let canReuseCompiledFiles =
            let sameOptions =
                cacheInfo.FableOptions = opts.FableOptions
                && cacheInfo.SourceMaps = opts.SourceMaps
                && cacheInfo.SourceMapsRoot = opts.SourceMapsRoot

            if not sameOptions then
                Log.verbose(lazy "Won't reuse compiled files because last compilation used different options")
                false
            else
                let isMostRecent = cacheInfo.IsMostRecent
                if not isMostRecent then
                    Log.verbose(lazy
                        let otherMode = if cacheInfo.FableOptions.DebugMode then "Release" else "Debug"
                        $"Won't reuse compiled files because last compilation was for {otherMode} mode")
                isMostRecent

        // Update cached info with current options and the timestamp for the corresponding build mode (Debug/Release)
        { cacheInfo with FableOptions = opts.FableOptions }.Write()

        let precompiledInfo, otherOptions, sourcePaths =

            loadPrecompiledInfo opts cacheInfo.FSharpOptions cacheInfo.SourcePaths

        { ProjectOptions = makeProjectOptions opts otherOptions sourcePaths
          References = cacheInfo.References
          FableLibDir = match precompiledInfo with Some i -> i.FableLibDir | None -> cacheInfo.FableLibDir
          FableModulesDir = opts.FableModulesDir
          OutputType = cacheInfo.OutputType
          PrecompiledInfo = precompiledInfo
          CanReuseCompiledFiles = canReuseCompiledFiles }

    | None ->
        let projRefs, mainProj = retryGetCrackedProjects opts

        let fableLibDir, pkgRefs =
            match opts.FableOptions.Language with
            | Python -> copyFableLibraryAndPackageSourcesPy opts mainProj.PackageReferences
            | _ -> copyFableLibraryAndPackageSources opts mainProj.PackageReferences

        let pkgRefs =
            pkgRefs |> List.map (fun pkg ->
                { pkg with SourcePaths = getSourcesFromFablePkg pkg.FsprojPath })

        let sourcePaths =
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
                yield! getBasicCompilerArgs() // options from compiler args
                yield "--optimize" + (if opts.FableOptions.OptimizeFSharpAst then "+" else "-")
            |]
            |> Array.distinct

        let dllRefs =
            let coreRefs = HashSet Metadata.coreAssemblies
            // TODO: Not sure if we still need this
            coreRefs.Add("System.Private.CoreLib") |> ignore
            let ignoredRefs = HashSet [
                "WindowsBase"
                "Microsoft.Win32.Primitives"
                "Microsoft.VisualBasic"
                "Microsoft.VisualBasic.Core"
                "Microsoft.CSharp"
            ]
            // We only keep dllRefs for the main project
            mainProj.DllReferences.Values
            // Remove unneeded System dll references
            |> Seq.choose (fun r ->
                let name = getDllName r
                if ignoredRefs.Contains(name) ||
                    (name.StartsWith("System.") && not(coreRefs.Contains(name))) then None
                else Some("-r:" + r))
            |> Seq.toArray

        let outputType = mainProj.OutputType
        let projRefs = projRefs |> List.map (fun p -> p.ProjectFile)
        let otherOptions = Array.append otherOptions dllRefs

        let cacheInfo: CacheInfo =
            {
                Version = Literals.VERSION
                OutDir = opts.OutDir
                FableLibDir = fableLibDir
                FableModulesDir = opts.FableModulesDir
                FableOptions = opts.FableOptions
                ProjectPath = opts.ProjFile
                FSharpOptions = otherOptions
                SourcePaths = sourcePaths
                References = projRefs
                OutputType = outputType
                Exclude = opts.Exclude
                SourceMaps = opts.SourceMaps
                SourceMapsRoot = opts.SourceMapsRoot
            }

        if not opts.NoCache then
            cacheInfo.Write()

        let precompiledInfo, otherOptions, sourcePaths =
            loadPrecompiledInfo opts otherOptions sourcePaths

        { ProjectOptions = makeProjectOptions opts otherOptions sourcePaths
          References = projRefs
          FableLibDir = match precompiledInfo with Some i -> i.FableLibDir | None -> fableLibDir
          FableModulesDir = opts.FableModulesDir
          OutputType = outputType
          PrecompiledInfo = precompiledInfo
          CanReuseCompiledFiles = false }
