/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Cli.ProjectCracker

open System
open System.Xml.Linq
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
        FableModulesDir: string
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

type CrackerOptions(fableOpts: CompilerOptions, fableLib, outDir, configuration, exclude, replace, precompiledLib, noCache, noRestore, projFile) =
    let builtDlls = HashSet()
    let fableModulesDir = CrackerOptions.GetFableModulesFromProject(projFile, outDir)
    let cacheInfo =
        if noCache then None
        else CacheInfo.TryRead(fableModulesDir, fableOpts.DebugMode)

    member _.NoCache = noCache
    member _.CacheInfo = cacheInfo
    member _.FableModulesDir = fableModulesDir
    member _.FableOptions: CompilerOptions = fableOpts
    member _.FableLib: string option = fableLib
    member _.OutDir: string option = outDir
    member _.Configuration: string = configuration
    member _.Exclude: string option = exclude
    member _.Replace: Map<string, string> = replace
    member _.PrecompiledLib: string option = precompiledLib
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

    static member GetFableModulesFromDir(baseDir: string): string =
        IO.Path.Combine(baseDir, Naming.fableModules)

    static member GetFableModulesFromProject(projFile: string, outDir: string option): string =
        let fableModulesDir =
            outDir
            |> Option.defaultWith (fun () -> IO.Path.GetDirectoryName(projFile))
            |> CrackerOptions.GetFableModulesFromDir

        if File.isDirectoryEmpty fableModulesDir then
            IO.Directory.CreateDirectory(fableModulesDir) |> ignore
            IO.File.WriteAllText(IO.Path.Combine(fableModulesDir, ".gitignore"), "**/*")

        fableModulesDir

type CrackerResponse =
    { FableLibDir: string
      FableModulesDir: string
      References: string list
      ProjectOptions: FSharpProjectOptions
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
      OtherCompilerOptions: string list }

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
          OtherCompilerOptions = [] }

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
      OtherCompilerOptions = otherOpts }

/// For project references of main project, ignore dll and package references
let easyCrack (opts: CrackerOptions) dllRefs (projFile: string): CrackedFsproj =
    let projOpts, projRefs, _msbuildProps =
        ProjectCoreCracker.GetProjectOptionsFromProjectFile opts.Configuration projFile

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
      OtherCompilerOptions = otherOpts }

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

let copyFableLibraryAndPackageSources (opts: CrackerOptions) (pkgs: FablePackage list) =
    let fableModulesDir = opts.FableModulesDir

    let fableLibraryPath =
        match opts.PrecompiledLib, opts.FableLib with
        | Some _, _ -> "" // Fable Library path will be taken from the precompiled info
        | None, Some path -> Path.normalizeFullPath path
        | None, None ->
            let assemblyDir =
                Process.getCurrentAssembly().Location
                |> Path.GetDirectoryName

            let defaultFableLibraryPaths =
                [ "../../../fable-library/"               // running from nuget tools package
                  "../../../../../build/fable-library/" ] // running from bin/Release/net5
                |> List.map (fun x -> Path.GetFullPath(Path.Combine(assemblyDir, x)))

            let fableLibrarySource =
                defaultFableLibraryPaths
                |> List.tryFind IO.Directory.Exists
                |> Option.defaultValue (List.last defaultFableLibraryPaths)

            if File.isDirectoryEmpty fableLibrarySource then
                FableError $"fable-library directory is empty, please build FableLibrary: %s{fableLibrarySource}" |> raise

            Log.verbose(lazy ("fable-library: " + fableLibrarySource))
            let fableLibraryTarget = IO.Path.Combine(fableModulesDir, "fable-library" + "." + Literals.VERSION)
            copyDirIfDoesNotExist false fableLibrarySource fableLibraryTarget
            Path.normalizeFullPath fableLibraryTarget

    let pkgRefs =
        pkgs |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)
            let targetDir = IO.Path.Combine(fableModulesDir, pkg.Id + "." + pkg.Version)
            copyDirIfDoesNotExist true sourceDir targetDir
            let fsprojFile = IO.Path.GetFileName(pkg.FsprojPath) |> changeFsprojToFableproj
            { pkg with FsprojPath = IO.Path.Combine(targetDir, fsprojFile) })

    fableLibraryPath, pkgRefs

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
            FableError($"Library was precompiled using Fable v{info.CompilerVersion} but you're using v{Literals.VERSION}. Please use same version.") |> raise

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
        FableError("Project file does not exist: " + opts.ProjFile) |> raise

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

            cacheInfo.Version = Literals.VERSION && (
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
        )))

    match cacheInfo with
    | Some cacheInfo ->
        Log.always $"Retrieving project options from cache, in case of issues run `dotnet fable clean` or try `--noCache` option."

        // Check if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
        // (this means the last compilation was done for another build mode so we cannot reuse the files)
        let canReuseCompiledFiles =
            let sameOptions = cacheInfo.FableOptions = opts.FableOptions
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
          PrecompiledInfo = precompiledInfo
          CanReuseCompiledFiles = canReuseCompiledFiles }

    | None ->
        let projRefs, mainProj = retryGetCrackedProjects opts
        let fableLibDir, pkgRefs = copyFableLibraryAndPackageSources opts mainProj.PackageReferences

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

        let projRefs = projRefs |> List.map (fun p -> p.ProjectFile)
        let otherOptions = Array.append otherOptions dllRefs

        let cacheInfo: CacheInfo =
            {
                Version = Literals.VERSION
                FableLibDir = fableLibDir
                FableModulesDir = opts.FableModulesDir
                FableOptions = opts.FableOptions
                ProjectPath = opts.ProjFile
                FSharpOptions = otherOptions
                SourcePaths = sourcePaths
                References = projRefs
            }

        if not opts.NoCache then
            cacheInfo.Write()

        let precompiledInfo, otherOptions, sourcePaths =
            loadPrecompiledInfo opts otherOptions sourcePaths

        { ProjectOptions = makeProjectOptions opts otherOptions sourcePaths
          References = projRefs
          FableLibDir = match precompiledInfo with Some i -> i.FableLibDir | None -> fableLibDir
          FableModulesDir = opts.FableModulesDir
          PrecompiledInfo = precompiledInfo
          CanReuseCompiledFiles = false }
