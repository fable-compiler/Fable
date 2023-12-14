/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Compiler.ProjectCracker

open System
open System.Xml.Linq
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Fable
open Fable.AST
open Fable.Compiler.Util
open Globbing.Operators
open Buildalyzer

type FablePackage =
    {
        Id: string
        Version: string
        FsprojPath: string
        DllPath: string
        SourcePaths: string list
        Dependencies: Set<string>
    }

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
        TargetFramework: string
        Exclude: string list
        SourceMaps: bool
        SourceMapsRoot: string option
    }

    static member GetPath(fableModulesDir: string, isDebug: bool) =
        IO.Path.Combine(
            fableModulesDir,
            $"""project_cracked{if isDebug then
                                    "_debug"
                                else
                                    ""}.json"""
        )

    member this.GetTimestamp() =
        CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)
        |> IO.File.GetLastWriteTime

    static member TryRead(fableModulesDir: string, isDebug) : CacheInfo option =
        try
            CacheInfo.GetPath(fableModulesDir, isDebug)
            |> Json.read<CacheInfo>
            |> Some
        with _ ->
            None

    member this.Write() =
        let path =
            CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)

        // Ensure the destination folder exists
        if not (IO.File.Exists path) then
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName path)
            |> ignore

        Json.write path this

    /// Checks if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
    member this.IsMostRecent =
        match
            CacheInfo.TryRead(
                this.FableModulesDir,
                not this.FableOptions.DebugMode
            )
        with
        | None -> true
        | Some other -> this.GetTimestamp() > other.GetTimestamp()

type CrackerOptions(cliArgs: CliArgs) =
    let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile

    let fableModulesDir =
        CrackerOptions.GetFableModulesFromProject(
            projDir,
            cliArgs.OutDir,
            cliArgs.NoCache
        )

    let builtDlls = HashSet()

    let cacheInfo =
        if cliArgs.NoCache then
            None
        else
            CacheInfo.TryRead(
                fableModulesDir,
                cliArgs.CompilerOptions.DebugMode
            )

    member _.NoCache = cliArgs.NoCache
    member _.CacheInfo = cacheInfo
    member _.FableModulesDir = fableModulesDir
    member _.FableOptions: CompilerOptions = cliArgs.CompilerOptions
    member _.FableLib: string option = cliArgs.FableLibraryPath
    member _.OutDir: string option = cliArgs.OutDir
    member _.Configuration: string = cliArgs.Configuration
    member _.Exclude: string list = cliArgs.Exclude
    member _.Replace: Map<string, string> = cliArgs.Replace
    member _.PrecompiledLib: string option = cliArgs.PrecompiledLib
    member _.NoRestore: bool = cliArgs.NoRestore
    member _.ProjFile: string = cliArgs.ProjectFile
    member _.SourceMaps: bool = cliArgs.SourceMaps
    member _.SourceMapsRoot: string option = cliArgs.SourceMapsRoot

    member _.BuildDll(normalizedDllPath: string) =
        if not (builtDlls.Contains(normalizedDllPath)) then
            let projDir =
                normalizedDllPath.Split('/')
                |> Array.rev
                |> Array.skipWhile (fun part -> part <> "bin")
                |> Array.skip 1
                |> Array.rev
                |> String.concat "/"

            Process.runSync
                projDir
                "dotnet"
                [
                    "build"
                    "-c"
                    cliArgs.Configuration
                ]
            |> ignore

            builtDlls.Add(normalizedDllPath) |> ignore

    static member GetFableModulesFromDir(baseDir: string) : string =
        IO.Path.Combine(baseDir, Naming.fableModules) |> Path.normalizePath

    static member GetFableModulesFromProject
        (
            projDir: string,
            outDir: string option,
            noCache: bool
        )
        : string
        =
        let fableModulesDir =
            outDir
            |> Option.defaultWith (fun () -> projDir)
            |> CrackerOptions.GetFableModulesFromDir

        if noCache then
            if IO.Directory.Exists(fableModulesDir) then
                IO.Directory.Delete(fableModulesDir, recursive = true)

        if File.isDirectoryEmpty fableModulesDir then
            IO.Directory.CreateDirectory(fableModulesDir) |> ignore

            IO.File.WriteAllText(
                IO.Path.Combine(fableModulesDir, ".gitignore"),
                "**/*"
            )

        fableModulesDir

type CrackerResponse =
    {
        FableLibDir: string
        FableModulesDir: string
        References: string list
        ProjectOptions: FSharpProjectOptions
        OutputType: OutputType
        TargetFramework: string
        PrecompiledInfo: PrecompiledInfoImpl option
        CanReuseCompiledFiles: bool
    }

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.", StringComparison.Ordinal)
    || pkgName.StartsWith("Microsoft.", StringComparison.Ordinal)
    || pkgName.StartsWith("runtime.", StringComparison.Ordinal)
    || pkgName = "NETStandard.Library"
    || pkgName = "FSharp.Core"
    || pkgName = "Fable.Core"

type CrackedFsproj =
    {
        ProjectFile: string
        SourceFiles: string list
        ProjectReferences: string list
        DllReferences: IDictionary<string, string>
        PackageReferences: FablePackage list
        OtherCompilerOptions: string list
        OutputType: string option
        TargetFramework: string
    }

let makeProjectOptions
    (opts: CrackerOptions)
    otherOptions
    sources
    : FSharpProjectOptions
    =
    let otherOptions =
        [|
            yield! otherOptions
            for constant in opts.FableOptions.Define do
                yield "--define:" + constant
            yield
                "--optimize"
                + if opts.FableOptions.OptimizeFSharpAst then
                      "+"
                  else
                      "-"
        |]

    {
        ProjectId = None
        ProjectFileName = opts.ProjFile
        OtherOptions = otherOptions
        SourceFiles = Array.distinct sources
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.UtcNow
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

let tryGetFablePackage (opts: CrackerOptions) (dllPath: string) =
    let tryFileWithPattern dir pattern =
        try
            let files = IO.Directory.GetFiles(dir, pattern)

            match files.Length with
            | 0 -> None
            | 1 -> Some files[0]
            | _ ->
                Log.always (
                    "More than one file found in "
                    + dir
                    + " with pattern "
                    + pattern
                )

                None
        with _ ->
            None

    let firstWithName localName (els: XElement seq) =
        els |> Seq.find (fun x -> x.Name.LocalName = localName)

    let tryFirstWithName localName (els: XElement seq) =
        els |> Seq.tryFind (fun x -> x.Name.LocalName = localName)

    let elements (el: XElement) = el.Elements()
    let attr name (el: XElement) = el.Attribute(XName.Get name).Value

    let child localName (el: XElement) =
        let child = el.Elements() |> firstWithName localName
        child.Value

    let firstGroupOrAllDependencies (dependencies: XElement seq) =
        match tryFirstWithName "group" dependencies with
        | Some firstGroup -> elements firstGroup
        | None -> dependencies

    if Path.GetFileNameWithoutExtension(dllPath) |> isSystemPackage then
        None
    else
        let rootDir =
            IO.Path.Combine(IO.Path.GetDirectoryName(dllPath), "..", "..")

        let fableDir = IO.Path.Combine(rootDir, "fable")

        match
            tryFileWithPattern rootDir "*.nuspec",
            tryFileWithPattern fableDir "*.fsproj"
        with
        | Some nuspecPath, Some fsprojPath ->
            let xmlDoc = XDocument.Load(nuspecPath)
            let metadata = xmlDoc.Root.Elements() |> firstWithName "metadata"
            let pkgId = metadata |> child "id"

            let fsprojPath =
                match Map.tryFind pkgId opts.Replace with
                | Some replaced ->
                    if
                        replaced.EndsWith(".fsproj", StringComparison.Ordinal)
                    then
                        replaced
                    else
                        tryFileWithPattern replaced "*.fsproj"
                        |> Option.defaultValue fsprojPath
                | None -> fsprojPath

            {
                Id = pkgId
                Version = metadata |> child "version"
                FsprojPath = fsprojPath
                DllPath = dllPath
                SourcePaths = []
                Dependencies =
                    metadata.Elements()
                    |> firstWithName "dependencies"
                    |> elements
                    // We don't consider different frameworks
                    |> firstGroupOrAllDependencies
                    |> Seq.map (attr "id")
                    |> Seq.filter (isSystemPackage >> not)
                    |> Set
            }
            : FablePackage
            |> Some
        | _ -> None

let sortFablePackages (pkgs: FablePackage list) =
    ([], pkgs)
    ||> List.fold (fun acc pkg ->
        match
            List.tryFindIndexBack
                (fun (x: FablePackage) -> pkg.Dependencies.Contains(x.Id))
                acc
        with
        | None -> pkg :: acc
        | Some targetIdx ->
            let rec insertAfter x targetIdx i before after =
                match after with
                | justBefore :: after ->
                    if i = targetIdx then
                        if i > 0 then
                            let dependent, nonDependent =
                                List.rev before
                                |> List.partition (fun (x: FablePackage) ->
                                    x.Dependencies.Contains(pkg.Id)
                                )

                            nonDependent @ justBefore :: x :: dependent @ after
                        else
                            (justBefore :: before |> List.rev) @ x :: after
                    else
                        insertAfter
                            x
                            targetIdx
                            (i + 1)
                            (justBefore :: before)
                            after
                | [] -> failwith "Unexpected empty list in insertAfter"

            insertAfter pkg targetIdx 0 [] acc
    )

let private getDllName (dllFullPath: string) =
    let i = dllFullPath.LastIndexOf('/')
    dllFullPath[(i + 1) .. (dllFullPath.Length - 5)] // -5 removes the .dll extension

let getBasicCompilerArgs () =
    [|
        // "--debug"
        // "--debug:portable"
        "--noframework"
        "--nologo"
        "--simpleresolution"
        "--nocopyfsharpcore"
        "--nowin32manifest"
        // "--nowarn:NU1603,NU1604,NU1605,NU1608"
        // "--warnaserror:76"
        "--warn:3"
        "--fullpaths"
        "--flaterrors"
    // Since net5.0 there's no difference between app/library
    // yield "--target:library"
    |]

let MSBUILD_CONDITION =
    Regex(
        @"^\s*'\$\((\w+)\)'\s*([!=]=)\s*'(true|false)'\s*$",
        RegexOptions.IgnoreCase
    )

/// Simplistic XML-parsing of .fsproj to get source files, as we cannot
/// run `dotnet restore` on .fsproj files embedded in Nuget packages.
let getSourcesFromFablePkg (opts: CrackerOptions) (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)

    let checkCondition (el: XElement) =
        match el.Attribute(XName.Get "Condition") with
        | null -> true
        | attr ->
            match attr.Value with
            | Naming.Regex MSBUILD_CONDITION [ _; prop; op; bval ] ->
                let bval = Boolean.Parse bval
                let isTrue = (op = "==") = bval // (op = "==" && bval) || (op = "!=" && not bval)

                let isDefined =
                    opts.FableOptions.Define
                    |> List.exists (fun d ->
                        String.Equals(
                            d,
                            prop,
                            StringComparison.InvariantCultureIgnoreCase
                        )
                    )
                // printfn $"CONDITION: {prop} ({isDefined}) {op} {bval} ({isTrue})"
                isTrue = isDefined
            | _ -> false

    let withNameAndCondition s (xs: XElement seq) =
        xs |> Seq.filter (fun el -> el.Name.LocalName = s && checkCondition el)

    let xmlDoc = XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile)

    Log.showFemtoMsg (fun () ->
        xmlDoc.Root.Elements()
        |> withName "PropertyGroup"
        |> Seq.exists (fun propGroup ->
            propGroup.Elements()
            |> withName "NpmDependencies"
            |> Seq.isEmpty
            |> not
        )
    )

    xmlDoc.Root.Elements()
    |> withNameAndCondition "ItemGroup"
    |> Seq.map (fun item ->
        (item.Elements(), [])
        ||> Seq.foldBack (fun el src ->
            if el.Name.LocalName = "Compile" && checkCondition el then
                el.Elements()
                |> withName "Link"
                |> Seq.tryHead
                |> function
                    | Some link when Path.isRelativePath link.Value ->
                        link.Value :: src
                    | _ ->
                        match el.Attribute(XName.Get "Include") with
                        | null -> src
                        | att -> att.Value :: src
            else
                src
        )
    )
    |> List.concat
    |> List.collect (fun fileName ->
        Path.Combine(projDir, fileName)
        |> function
            | path when (path.Contains("*") || path.Contains("?")) ->
                match !!path |> List.ofSeq with
                | [] -> [ path ]
                | globResults -> globResults
            | path -> [ path ]
        |> List.map Path.normalizeFullPath
    )

let private extractUsefulOptionsAndSources
    isMainProj
    (line: string)
    (accSources: string list, accOptions: string list)
    =
    if line.StartsWith('-') then
        //   "--warnaserror" // Disable for now to prevent unexpected errors, see #2288
        if
            line.StartsWith("--langversion:", StringComparison.Ordinal)
            && isMainProj
        then
            let v = line.Substring("--langversion:".Length).ToLowerInvariant()

            if v = "preview" then
                accSources, line :: accOptions
            else
                accSources, accOptions
        elif
            line.StartsWith("--nowarn", StringComparison.Ordinal)
            || line.StartsWith("--warnon", StringComparison.Ordinal)
        then
            accSources, line :: accOptions
        elif line.StartsWith("--define:", StringComparison.Ordinal) then
            // When parsing the project as .csproj there will be multiple defines in the same line,
            // but the F# compiler seems to accept only one per line
            let defines =
                line.Substring(9).Split(';')
                |> Array.mapToList (fun d -> "--define:" + d)

            accSources, defines @ accOptions
        else
            accSources, accOptions
    else
        (Path.normalizeFullPath line) :: accSources, accOptions

let excludeProjRef
    (opts: CrackerOptions)
    (dllRefs: IDictionary<string, string>)
    (projRef: string)
    =
    let projName = Path.GetFileNameWithoutExtension(projRef)

    let isExcluded =
        opts.Exclude
        |> List.exists (fun e ->
            String.Equals(
                e,
                Path.GetFileNameWithoutExtension(projRef),
                StringComparison.OrdinalIgnoreCase
            )
        )

    if isExcluded then
        try
            opts.BuildDll(dllRefs[projName])
        with e ->
            Log.always ("Couldn't build " + projName + ": " + e.Message)

        None
    else
        let _removed = dllRefs.Remove(projName)
        // if not removed then
        //     Log.always("Couldn't remove project reference " + projName + " from dll references")
        Path.normalizeFullPath projRef |> Some

let getCrackedMainFsproj
    (opts: CrackerOptions)
    (projOpts: string[], projRefs, msbuildProps, targetFramework)
    =
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.OrdinalIgnoreCase)

    let sourceFiles, otherOpts =
        (projOpts, ([], []))
        ||> Array.foldBack (fun line (src, otherOpts) ->
            if line.StartsWith("-r:", StringComparison.Ordinal) then
                let line = Path.normalizePath (line[3..])
                let dllName = getDllName line
                dllRefs.Add(dllName, line)
                src, otherOpts
            else
                extractUsefulOptionsAndSources true line (src, otherOpts)
        )

    let fablePkgs =
        let dllRefs' =
            dllRefs |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Seq.toArray

        dllRefs'
        |> Seq.choose (fun (dllName, dllPath) ->
            match tryGetFablePackage opts dllPath with
            | Some pkg ->
                dllRefs.Remove(dllName) |> ignore
                Some pkg
            | None -> None
        )
        |> Seq.toList
        |> sortFablePackages

    {
        ProjectFile = opts.ProjFile
        SourceFiles = sourceFiles
        ProjectReferences =
            projRefs
            |> Array.choose (excludeProjRef opts dllRefs)
            |> Array.toList
        DllReferences = dllRefs
        PackageReferences = fablePkgs
        OtherCompilerOptions = otherOpts
        OutputType = ReadOnlyDictionary.tryFind "OutputType" msbuildProps
        TargetFramework = targetFramework
    }

let getProjectOptionsFromScript (opts: CrackerOptions) : CrackedFsproj =
    let projectFilePath = opts.ProjFile

    let projOpts, _diagnostics = // TODO: Check diagnostics
        let checker = FSharpChecker.Create()

        let text =
            File.readAllTextNonBlocking (projectFilePath) |> SourceText.ofString

        checker.GetProjectOptionsFromScript(
            projectFilePath,
            text,
            useSdkRefs = true,
            assumeDotNetFramework = false
        )
        |> Async.RunSynchronously

    let projOpts = Array.append projOpts.OtherOptions projOpts.SourceFiles
    getCrackedMainFsproj opts (projOpts, [||], Dictionary(), "")

let getProjectOptionsFromProjectFile =
    let mutable manager = None

    let tryGetResult
        (isMain: bool)
        (opts: CrackerOptions)
        (manager: AnalyzerManager)
        (maybeCsprojFile: string)
        =
        if isMain && not opts.NoRestore then
            Process.runSync
                (IO.Path.GetDirectoryName opts.ProjFile)
                "dotnet"
                [
                    "restore"
                    IO.Path.GetFileName maybeCsprojFile
                    // $"-p:TargetFramework={opts.TargetFramework}"
                    for constant in opts.FableOptions.Define do
                        $"-p:{constant}=true"
                ]
            |> ignore

        let analyzer = manager.GetProject(maybeCsprojFile)

        let env =
            analyzer.EnvironmentFactory.GetBuildEnvironment(
                Environment.EnvironmentOptions(
                    DesignTime = true,
                    Restore = false
                )
            )
        // If the project targets multiple frameworks, multiple results will be returned
        // For now we just take the first one with non-empty command
        let results = analyzer.Build(env)
        results |> Seq.tryFind (fun r -> String.IsNullOrEmpty(r.Command) |> not)

    fun (isMain: bool) (opts: CrackerOptions) (projFile: string) ->
        let manager =
            match manager with
            | Some m -> m
            | None ->
                let log = new System.IO.StringWriter()
                let options = AnalyzerManagerOptions(LogWriter = log)
                let m = AnalyzerManager(options)
                m.SetGlobalProperty("Configuration", opts.Configuration)
                // m.SetGlobalProperty("TargetFramework", opts.TargetFramework)
                for define in opts.FableOptions.Define do
                    m.SetGlobalProperty(define, "true")

                manager <- Some m
                m

        // Because Buildalyzer works better with .csproj, we first "dress up" the project as if it were a C# one
        // and try to adapt the results. If it doesn't work, we try again to analyze the .fsproj directly
        let csprojResult =
            let csprojFile = projFile.Replace(".fsproj", ".csproj")

            if IO.File.Exists(csprojFile) then
                None
            else
                try
                    IO.File.Copy(projFile, csprojFile)

                    tryGetResult isMain opts manager csprojFile
                    |> Option.map (fun (r: IAnalyzerResult) ->
                        // Careful, options for .csproj start with / but so do root paths in unix
                        let reg = Regex(@"^\/[^\/]+?(:?:|$)")

                        let comArgs =
                            r.CompilerArguments
                            |> Array.map (fun line ->
                                if reg.IsMatch(line) then
                                    if
                                        line.StartsWith(
                                            "/reference",
                                            StringComparison.Ordinal
                                        )
                                    then
                                        "-r" + line.Substring(10)
                                    else
                                        "--" + line.Substring(1)
                                else
                                    line
                            )

                        let comArgs =
                            match r.Properties.TryGetValue("OtherFlags") with
                            | false, _ -> comArgs
                            | true, otherFlags ->
                                let otherFlags =
                                    otherFlags.Split(
                                        ' ',
                                        StringSplitOptions.RemoveEmptyEntries
                                    )

                                Array.append otherFlags comArgs

                        comArgs, r
                    )
                finally
                    File.safeDelete csprojFile

        let compilerArgs, result =
            csprojResult
            |> Option.orElseWith (fun () ->
                tryGetResult isMain opts manager projFile
                |> Option.map (fun r ->
                    // result.CompilerArguments doesn't seem to work well in Linux
                    let comArgs = Regex.Split(r.Command, @"\r?\n")
                    comArgs, r
                )
            )
            |> function
                | Some result -> result
                // TODO: Get Buildalyzer errors from the log
                | None ->
                    $"Cannot parse {projFile}" |> Fable.FableError |> raise

        let projDir = IO.Path.GetDirectoryName(projFile)

        let projOpts =
            compilerArgs
            |> Array.skipWhile (fun line -> not (line.StartsWith('-')))
            |> Array.map (fun f ->
                if
                    f.EndsWith(".fs", StringComparison.Ordinal)
                    || f.EndsWith(".fsi", StringComparison.Ordinal)
                then
                    if Path.IsPathRooted f then
                        f
                    else
                        Path.Combine(projDir, f)
                else
                    f
            )

        projOpts,
        Seq.toArray result.ProjectReferences,
        result.Properties,
        result.TargetFramework

/// Use Buildalyzer to invoke MSBuild and get F# compiler args from an .fsproj file.
/// As we'll merge this later with other projects we'll only take the sources and
/// the references, checking if some .dlls correspond to Fable libraries
let crackMainProject (opts: CrackerOptions) : CrackedFsproj =
    getProjectOptionsFromProjectFile true opts opts.ProjFile
    |> getCrackedMainFsproj opts

/// For project references of main project, ignore dll and package references
let crackReferenceProject
    (opts: CrackerOptions)
    dllRefs
    (projFile: string)
    : CrackedFsproj
    =
    let projOpts, projRefs, msbuildProps, targetFramework =
        getProjectOptionsFromProjectFile false opts projFile

    let sourceFiles, otherOpts =
        Array.foldBack (extractUsefulOptionsAndSources false) projOpts ([], [])

    {
        ProjectFile = projFile
        SourceFiles = sourceFiles
        ProjectReferences =
            projRefs
            |> Array.choose (excludeProjRef opts dllRefs)
            |> Array.toList
        DllReferences = Dictionary()
        PackageReferences = []
        OtherCompilerOptions = otherOpts
        OutputType = ReadOnlyDictionary.tryFind "OutputType" msbuildProps
        TargetFramework = targetFramework
    }

let getCrackedProjectsFromMainFsproj (opts: CrackerOptions) =
    let mainProj = crackMainProject opts

    let rec crackProjects (acc: CrackedFsproj list) (projFile: string) =
        let crackedFsproj =
            match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
            | None -> crackReferenceProject opts mainProj.DllReferences projFile
            | Some crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        List.fold
            crackProjects
            (crackedFsproj :: acc)
            crackedFsproj.ProjectReferences

    let refProjs =
        List.fold crackProjects [] mainProj.ProjectReferences
        |> List.distinctBy (fun x -> x.ProjectFile)

    refProjs, mainProj

let getCrackedProjects (opts: CrackerOptions) =
    match (Path.GetExtension opts.ProjFile).ToLower() with
    | ".fsx" -> [], getProjectOptionsFromScript opts
    | ".fsproj" -> getCrackedProjectsFromMainFsproj opts
    | s -> failwith $"Unsupported project type: %s{s}"

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
                retry ()
            else
                failwith
                    $"IO Error trying read project options: %s{ioex.Message} "
        | _ -> reraise ()

    retry ()

// Replace the .fsproj extension with .fableproj for files in fable_modules
// We do this to avoid conflicts with other F# tooling that scan for .fsproj files
let changeFsprojToFableproj (path: string) =
    if path.EndsWith(".fsproj", StringComparison.Ordinal) then
        IO.Path.ChangeExtension(path, Naming.fableProjExt)
    else
        path

let copyDir replaceFsprojExt (source: string) (target: string) =
    IO.Directory.CreateDirectory(target) |> ignore

    if IO.Directory.Exists source |> not then
        failwith ("Source directory is missing: " + source)

    let source = source.TrimEnd('/', '\\')
    let target = target.TrimEnd('/', '\\')

    for dirPath in
        IO.Directory.GetDirectories(source, "*", IO.SearchOption.AllDirectories) do
        IO.Directory.CreateDirectory(dirPath.Replace(source, target)) |> ignore

    for fromPath in
        IO.Directory.GetFiles(source, "*.*", IO.SearchOption.AllDirectories) do
        let toPath = fromPath.Replace(source, target)

        let toPath =
            if replaceFsprojExt then
                changeFsprojToFableproj toPath
            else
                toPath

        IO.File.Copy(fromPath, toPath, true)

let copyDirIfDoesNotExist replaceFsprojExt (source: string) (target: string) =
    if File.isDirectoryEmpty target then
        copyDir replaceFsprojExt source target

let getFableLibraryPath (opts: CrackerOptions) =
    let buildDir, libDir =
        match opts.FableOptions.Language, opts.FableLib with
        | Dart, None -> "fable-library-dart", "fable_library"
        | Rust, None -> "fable-library-rust", "fable-library-rust"
        | TypeScript, None -> "fable-library-ts", "fable-library-ts"
        | Php, None -> "fable-library-php", "fable-library-php"
        | JavaScript, None ->
            "fable-library", "fable-library" + "." + Literals.VERSION
        | Python, None -> "fable-library-py/fable_library", "fable_library"
        | Python, Some Py.Naming.sitePackages ->
            "fable-library-py", "fable-library"
        | _, Some path ->
            if path.StartsWith("./", StringComparison.Ordinal) then
                "", Path.normalizeFullPath path
            elif IO.Path.IsPathRooted(path) then
                "", Path.normalizePath path
            else
                "", path

    if String.IsNullOrEmpty(buildDir) then
        libDir
    else
        let fableLibrarySource =
            let baseDir = AppContext.BaseDirectory

            baseDir
            |> File.tryFindNonEmptyDirectoryUpwards
                {|
                    matches =
                        [
                            buildDir
                            "temp/" + buildDir
                        ]
                    exclude = [ "src" ]
                |}
            |> Option.defaultWith (fun () ->
                Fable.FableError
                    $"Cannot find [temp/]{buildDir} from {baseDir}.\nPlease, make sure you build {buildDir}"
                |> raise
            )

        let fableLibraryTarget = IO.Path.Combine(opts.FableModulesDir, libDir)
        // Always overwrite fable-library in case it has been updated, see #3208
        copyDir false fableLibrarySource fableLibraryTarget
        Path.normalizeFullPath fableLibraryTarget

let copyFableLibraryAndPackageSources
    (opts: CrackerOptions)
    (pkgs: FablePackage list)
    =
    let pkgRefs =
        pkgs
        |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)

            let targetDir =
                IO.Path.Combine(
                    opts.FableModulesDir,
                    pkg.Id + "." + pkg.Version
                )

            copyDirIfDoesNotExist true sourceDir targetDir

            let fsprojFile =
                IO.Path.GetFileName(pkg.FsprojPath) |> changeFsprojToFableproj

            { pkg with FsprojPath = IO.Path.Combine(targetDir, fsprojFile) }
        )

    getFableLibraryPath opts, pkgRefs

// Separate handling for Python. Use plain lowercase package names without dots or version info.
let copyFableLibraryAndPackageSourcesPy
    (opts: CrackerOptions)
    (pkgs: FablePackage list)
    =
    let pkgRefs =
        pkgs
        |> List.map (fun pkg ->
            let sourceDir = IO.Path.GetDirectoryName(pkg.FsprojPath)

            let targetDir =
                match opts.FableLib with
                | Some Py.Naming.sitePackages ->
                    let name =
                        Naming.applyCaseRule Core.CaseRules.KebabCase pkg.Id

                    IO.Path.Combine(
                        opts.FableModulesDir,
                        name.Replace(".", "-")
                    )
                | _ ->
                    let name =
                        Naming.applyCaseRule Core.CaseRules.SnakeCase pkg.Id

                    IO.Path.Combine(
                        opts.FableModulesDir,
                        name.Replace(".", "_")
                    )

            copyDirIfDoesNotExist false sourceDir targetDir

            { pkg with
                FsprojPath =
                    IO.Path.Combine(
                        targetDir,
                        IO.Path.GetFileName(pkg.FsprojPath)
                    )
            }
        )

    getFableLibraryPath opts, pkgRefs

// See #1455: F# compiler generates *.AssemblyInfo.fs in obj folder, but we don't need it
let removeFilesInObjFolder (sourceFiles: string[]) =
    let reg = Regex(@"[\\\/]obj[\\\/]")
    sourceFiles |> Array.filter (reg.IsMatch >> not)

let loadPrecompiledInfo (opts: CrackerOptions) otherOptions sourceFiles =
    // Sources in fable_modules correspond to packages and they're qualified with the version
    // (e.g. fable_modules/Fable.Promise.2.1.0/Promise.fs) so we assume they're the same wherever they are
    // TODO: Check if this holds true also for Python which may not include the version number in the path
    let normalizePath (path: string) =
        let i = path.IndexOf(Naming.fableModules, StringComparison.Ordinal)

        if i >= 0 then
            path[i..]
        else
            path

    match opts.PrecompiledLib with
    | Some precompiledLib ->
        // Load PrecompiledInfo
        let info =
            CrackerOptions.GetFableModulesFromDir(precompiledLib)
            |> PrecompiledInfoImpl.Load

        // Check if precompiled compiler version and options match
        if info.CompilerVersion <> Literals.VERSION then
            Fable.FableError(
                $"Library was precompiled using Fable v{info.CompilerVersion} but you're using v{Literals.VERSION}. Please use same version."
            )
            |> raise

        // Sometimes it may be necessary to use different options for the precompiled lib so don't throw an error here
        //if info.CompilerOptions <> opts.FableOptions then
        //    FableError($"Library was precompiled using different compiler options. Please use same options.") |> raise

        // Check if precompiled files are up-to-date
        try
            info.Files
            |> Seq.choose (fun (KeyValue(file, { OutPath = outPath })) ->
                // Empty files are not written to disk so we only check date for existing files
                if IO.File.Exists(outPath) then
                    if
                        IO.File.GetLastWriteTime(file) < IO
                            .File
                            .GetLastWriteTime(outPath)
                    then
                        None
                    else
                        Some file
                else
                    None
            )
            |> Seq.toList
            |> function
                | [] -> ()
                | outdated ->
                    let outdated =
                        outdated
                        |> List.map (fun f -> "    " + File.relPathToCurDir f)
                        |> String.concat Log.newLine
                    // TODO: This should likely be an error but make it a warning for now
                    Log.warning (
                        $"Detected outdated files in precompiled lib:{Log.newLine}{outdated}"
                    )
        with er ->
            Log.warning (
                "Cannot check timestamp of precompiled files: " + er.Message
            )

        // Remove precompiled files from sources and add reference to precompiled .dll to other options
        let otherOptions = Array.append otherOptions [| "-r:" + info.DllPath |]

        let precompiledFiles =
            Map.keys info.Files |> Seq.map normalizePath |> set

        let sourceFiles =
            sourceFiles
            |> Array.filter (fun path ->
                normalizePath path |> precompiledFiles.Contains |> not
            )

        Some info, otherOptions, sourceFiles
    | None -> None, otherOptions, sourceFiles

let getFullProjectOpts (opts: CrackerOptions) =
    if not (IO.File.Exists(opts.ProjFile)) then
        Fable.FableError("Project file does not exist: " + opts.ProjFile)
        |> raise

    // Make sure cache info corresponds to same compiler version and is not outdated
    let cacheInfo =
        opts.CacheInfo
        |> Option.filter (fun cacheInfo ->
            let cacheTimestamp = cacheInfo.GetTimestamp()

            let isOlderThanCache (filePath: string) =
                let fileTimestamp = IO.File.GetLastWriteTime(filePath)
                let isOlder = fileTimestamp < cacheTimestamp

                if not isOlder then
                    Log.verbose (
                        lazy
                            $"Cached project info ({cacheTimestamp}) will be discarded because {File.relPathToCurDir filePath} ({fileTimestamp}) is newer"
                    )

                isOlder

            cacheInfo.Version = Literals.VERSION
            && cacheInfo.Exclude = opts.Exclude
            && cacheInfo.FableOptions.Language = opts.FableOptions.Language
            && ([
                    cacheInfo.ProjectPath
                    yield! cacheInfo.References
                ]
                |> List.forall (fun fsproj ->
                    if IO.File.Exists(fsproj) && isOlderThanCache fsproj then
                        // Check if the project uses Paket
                        let fsprojDir = IO.Path.GetDirectoryName(fsproj)

                        let paketReferences =
                            IO.Path.Combine(fsprojDir, "paket.references")

                        if not (IO.File.Exists(paketReferences)) then
                            true
                        else if isOlderThanCache paketReferences then
                            // Only check paket.lock for main project and assume it's the same for references
                            if fsproj <> cacheInfo.ProjectPath then
                                true
                            else
                                match
                                    File.tryFindUpwards "paket.lock" fsprojDir
                                with
                                | Some paketLock -> isOlderThanCache paketLock
                                | None -> false
                        else
                            false
                    else
                        false
                ))
        )

    match cacheInfo with
    | Some cacheInfo ->
        Log.always
            $"Retrieving project options from cache, in case of issues run `dotnet fable clean` or try `--noCache` option."

        // Check if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
        // (this means the last compilation was done for another build mode so we cannot reuse the files)
        let canReuseCompiledFiles =
            let sameOptions =
                cacheInfo.FableOptions = opts.FableOptions
                && cacheInfo.SourceMaps = opts.SourceMaps
                && cacheInfo.SourceMapsRoot = opts.SourceMapsRoot

            if not sameOptions then
                Log.verbose (
                    lazy
                        "Won't reuse compiled files because last compilation used different options"
                )

                false
            else
                let isMostRecent = cacheInfo.IsMostRecent

                if not isMostRecent then
                    Log.verbose (
                        lazy
                            let otherMode =
                                if cacheInfo.FableOptions.DebugMode then
                                    "Release"
                                else
                                    "Debug"

                            $"Won't reuse compiled files because last compilation was for {otherMode} mode"
                    )

                isMostRecent

        // Update cached info with current options and the timestamp for the corresponding build mode (Debug/Release)
        { cacheInfo with FableOptions = opts.FableOptions }.Write()

        let precompiledInfo, otherOptions, sourcePaths =

            loadPrecompiledInfo
                opts
                cacheInfo.FSharpOptions
                cacheInfo.SourcePaths

        {
            ProjectOptions = makeProjectOptions opts otherOptions sourcePaths
            References = cacheInfo.References
            FableLibDir =
                match precompiledInfo with
                | Some i -> i.FableLibDir
                | None -> cacheInfo.FableLibDir
            FableModulesDir = opts.FableModulesDir
            OutputType = cacheInfo.OutputType
            TargetFramework = cacheInfo.TargetFramework
            PrecompiledInfo = precompiledInfo
            CanReuseCompiledFiles = canReuseCompiledFiles
        }

    | None ->
        let projRefs, mainProj = retryGetCrackedProjects opts

        // The cache was considered outdated / invalid so it is better to make
        // make sure we have are in a clean state
        if IO.Directory.Exists(opts.FableModulesDir) then
            IO.Directory.Delete(opts.FableModulesDir, true)

        let fableLibDir, pkgRefs =
            match opts.FableOptions.Language with
            | Python ->
                copyFableLibraryAndPackageSourcesPy
                    opts
                    mainProj.PackageReferences
            | _ ->
                copyFableLibraryAndPackageSources
                    opts
                    mainProj.PackageReferences

        let pkgRefs =
            pkgRefs
            |> List.map (fun pkg ->
                { pkg with
                    SourcePaths = getSourcesFromFablePkg opts pkg.FsprojPath
                }
            )

        let sourcePaths =
            let pkgSources = pkgRefs |> List.collect (fun x -> x.SourcePaths)
            let refSources = projRefs |> List.collect (fun x -> x.SourceFiles)

            pkgSources @ refSources @ mainProj.SourceFiles
            |> List.toArray
            |> removeFilesInObjFolder

        let refOptions =
            projRefs
            |> List.collect (fun x -> x.OtherCompilerOptions)
            |> List.toArray

        let otherOptions =
            [|
                yield! refOptions // merged options from all referenced projects
                yield! mainProj.OtherCompilerOptions // main project compiler options
                yield! getBasicCompilerArgs () // options from compiler args
                yield
                    "--optimize"
                    + (if opts.FableOptions.OptimizeFSharpAst then
                           "+"
                       else
                           "-")
            |]
            |> Array.distinct

        let dllRefs =
            let coreRefs = HashSet Metadata.coreAssemblies
            // TODO: Not sure if we still need this
            coreRefs.Add("System.Private.CoreLib") |> ignore

            let ignoredRefs =
                HashSet
                    [
                        "FSharp.Core"
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

                if
                    ignoredRefs.Contains(name)
                    || (name.StartsWith("System.", StringComparison.Ordinal)
                        && not (coreRefs.Contains(name)))
                then
                    None
                else
                    Some("-r:" + r)
            )

        let projRefs = projRefs |> List.map (fun p -> p.ProjectFile)

        let otherOptions =
            [|
                yield! otherOptions
                yield! dllRefs
                // For some reason, in my tests it seems to work without the FSharp.Core reference
                // but we add it just in case
                yield
                    "-r:"
                    + typeof<FSharp.Collections.List<obj>>.Assembly.Location
            |]

        let outputType =
            match mainProj.OutputType with
            | Some "Library" -> OutputType.Library
            | _ -> OutputType.Exe

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
                TargetFramework = mainProj.TargetFramework
                Exclude = opts.Exclude
                SourceMaps = opts.SourceMaps
                SourceMapsRoot = opts.SourceMapsRoot
            }

        if not opts.NoCache then
            cacheInfo.Write()

        let precompiledInfo, otherOptions, sourcePaths =
            loadPrecompiledInfo opts otherOptions sourcePaths

        {
            ProjectOptions = makeProjectOptions opts otherOptions sourcePaths
            References = projRefs
            FableLibDir =
                match precompiledInfo with
                | Some i -> i.FableLibDir
                | None -> fableLibDir
            FableModulesDir = opts.FableModulesDir
            OutputType = outputType
            TargetFramework = mainProj.TargetFramework
            PrecompiledInfo = precompiledInfo
            CanReuseCompiledFiles = false
        }
