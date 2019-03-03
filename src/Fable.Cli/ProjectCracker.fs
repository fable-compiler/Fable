/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
/// using Paket .paket.resolved file
module Fable.Cli.ProjectCracker

open System
open System.IO
open System.Xml.Linq
open System.Collections.Generic
open System.Reflection
open FSharp.Compiler.SourceCodeServices
open Fable

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.")
        || pkgName.StartsWith("Microsoft.")
        || pkgName.StartsWith("runtime.")
        || pkgName = "NETStandard.Library"
        || pkgName = "FSharp.Core"
        || pkgName = "Fable.Core"

let logWarningAndReturn (v:'T) str =
    Log.logAlways("[WARNING] " + str); v

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
      OtherOptions = Array.append otherOptions sources
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = System.DateTime.MaxValue
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None
      Stamp = None }

// let getProjectOptionsFromScript (checker: InteractiveChecker) (define: string[]) scriptFile =
//     let otherFlags = [|
//         yield "--target:library"
// #if !NETFX
//         yield "--targetprofile:netcore"
// #endif
//         for constant in define do yield "--define:" + constant
//     |]
//     checker.GetProjectOptionsFromScript(scriptFile, File.ReadAllText scriptFile,
//                                         assumeDotNetFramework=false, otherFlags=otherFlags)
//     |> Async.RunSynchronously
//     |> fun (opts, _errors) ->
//         // TODO: Check errors
//         opts.OtherOptions
//         |> makeProjectOptions scriptFile opts.SourceFiles

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

let tryGetFablePackage (dllPath: string) =
    let tryFileWithPattern dir pattern =
        try
            let files = Directory.GetFiles(dir, pattern)
            match files.Length with
            | 0 -> None
            | 1 -> Some files.[0]
            | _ -> Log.logAlways("More than one file found in " + dir + " with pattern " + pattern)
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
    |> List.map (fun fileName ->
        Path.Combine(projDir, fileName) |> Path.normalizeFullPath)

let private getDllName (dllFullPath: string) =
    let i = dllFullPath.LastIndexOf('/')
    dllFullPath.[(i + 1) .. (dllFullPath.Length - 5)] // -5 removes the .dll extension

let private isUsefulOption (opt : string) =
    [ "--nowarn"
      "--warnon"
      "--warnaserror" ]
    |> List.exists opt.StartsWith

/// Use Dotnet.ProjInfo (through ProjectCoreCracker) to invoke MSBuild
/// and get F# compiler args from an .fsproj file. As we'll merge this
/// later with other projects we'll only take the sources and the references,
/// checking if some .dlls correspond to Fable libraries
let fullCrack (projFile: string): CrackedFsproj =
    // Use case insensitive keys, as package names in .paket.resolved
    // may have a different case, see #1227
    let dllRefs = Dictionary(StringComparer.OrdinalIgnoreCase)
    // Try restoring project
    do
        Process.runCmd
            Console.WriteLine Console.WriteLine
            (IO.Path.GetDirectoryName projFile)
            "dotnet" ["restore"; IO.Path.GetFileName projFile]
        |> ignore
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
        projRefs |> List.map (fun projRef ->
            // Remove dllRefs corresponding to project references
            let projName = Path.GetFileNameWithoutExtension(projRef)
            let removed = dllRefs.Remove(projName)
            if not removed then
                printfn "Couldn't remove project reference %s from dll references" projName
            Path.normalizeFullPath projRef)
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

let getCrackedProjectsFromMainFsproj (projFile: string) =
    let rec crackProjects (acc: CrackedFsproj list) (projFile: string) =
        let crackedFsproj =
            match acc |> List.tryFind (fun x -> x.ProjectFile = projFile) with
            | None -> easyCrack projFile
            | Some crackedFsproj -> crackedFsproj
        // Add always a reference to the front to preserve compilation order
        // Duplicated items will be removed later
        List.fold crackProjects (crackedFsproj::acc) crackedFsproj.ProjectReferences
    let mainProj = fullCrack projFile
    let refProjs =
        List.fold crackProjects [] mainProj.ProjectReferences
        |> List.distinctBy (fun x -> x.ProjectFile)
    refProjs, mainProj
    
type private ScriptOption =
    | Other of string
    | Dll of string
    | FablePackage of FablePackage
    
// TEMP NOJAF
module Utils =

  let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
      let psi = System.Diagnostics.ProcessStartInfo()
      psi.FileName <- exePath
      psi.WorkingDirectory <- workingDir
      psi.RedirectStandardOutput <- true
      psi.RedirectStandardError <- true
      psi.Arguments <- args
      psi.CreateNoWindow <- true
      psi.UseShellExecute <- false

      use p = new System.Diagnostics.Process()
      p.StartInfo <- psi

      p.OutputDataReceived.Add(fun ea -> log (ea.Data))

      p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

      // printfn "running: %s %s" psi.FileName psi.Arguments

      p.Start() |> ignore
      p.BeginOutputReadLine()
      p.BeginErrorReadLine()
      p.WaitForExit()

      let exitCode = p.ExitCode

      exitCode, (workingDir, exePath, args)

  let isWindows () =
    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
        System.Runtime.InteropServices.OSPlatform.Windows)

let private programFilesX86 () =
  let environVar v = Environment.GetEnvironmentVariable v

  let wow64 = environVar "PROCESSOR_ARCHITEW6432"
  let globalArch = environVar "PROCESSOR_ARCHITECTURE"
  match wow64, globalArch with
  | "AMD64", "AMD64"
  | null, "AMD64"
  | "x86", "AMD64" -> environVar "ProgramFiles(x86)"
  | _ -> environVar "ProgramFiles"
  |> fun detected -> if detected = null then @"C:\Program Files (x86)\" else detected

let private vsSkus = ["Community"; "Professional"; "Enterprise"; "BuildTools"]
let private vsVersions = ["2017"]
let cartesian a b =
    [ for a' in a do
        for b' in b do
          yield a', b' ]
    
module private EnvUtils =
      // Below code slightly modified from FSAC and from FAKE MSBuildHelper.fs

      let private tryFindFile dirs file =
          let files =
              dirs
              |> Seq.map (fun (path : string) ->
                  try
                     let path =
                        if path.StartsWith("\"") && path.EndsWith("\"")
                        then path.Substring(1, path.Length - 2)
                        else path
                     let dir = new DirectoryInfo(path)
                     if not dir.Exists then ""
                     else
                         let fi = new FileInfo(Path.Combine(dir.FullName, file))
                         if fi.Exists then fi.FullName
                         else ""
                  with
                  | _ -> "")
              |> Seq.filter ((<>) "")
              |> Seq.toList
          files

      let Stringsplit (splitter: char) (s: string) = s.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

      let tryFindPath backupPaths tool =
          let paths = Environment.GetEnvironmentVariable "PATH" |> Stringsplit Path.PathSeparator
          tryFindFile (paths @ backupPaths) tool



let installedMSBuilds () =

  // TODO remove shadowing
  let programFilesX86 = programFilesX86 ()

  let vsRoots =
    cartesian vsVersions vsSkus 
    |> List.map (fun (version, sku) -> IO.Path.Combine(programFilesX86, "Microsoft Visual Studio", version, sku))

  if not (Utils.isWindows ()) then
    ["msbuild"] // we're way past 5.0 now, time to get updated
  else
    let legacyPaths =
        [ Path.Combine(programFilesX86, @"MSBuild\14.0\Bin")
          Path.Combine(programFilesX86, @"MSBuild\12.0\Bin")
          Path.Combine(programFilesX86, @"MSBuild\12.0\Bin\amd64")
          @"c:\Windows\Microsoft.NET\Framework\v4.0.30319"
          @"c:\Windows\Microsoft.NET\Framework\v4.0.30128"
          @"c:\Windows\Microsoft.NET\Framework\v3.5" ]

    let sideBySidePaths =
      vsRoots
      |> List.map (fun root -> IO.Path.Combine(root, "MSBuild", "15.0", "bin") )

    let ev = Environment.GetEnvironmentVariable "MSBuild"
    if not (String.IsNullOrEmpty ev) then [ev]
    else EnvUtils.tryFindPath (sideBySidePaths @ legacyPaths) "MsBuild.exe"

type MSBuildLocator () =

    let installedMSBuilds = lazy (
        installedMSBuilds () )

    member this.MSBuildFromPATH
        with get() = Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"

    member this.DotnetMSBuildFromPATH
        with get() =  Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"

    member this.InstalledMSBuilds () =
        installedMSBuilds.Force()
        |> List.map (Dotnet.ProjInfo.Inspect.MSBuildExePath.Path)

    member this.LatestInstalledMSBuild () =
        match installedMSBuilds.Force() with
        | [] -> this.MSBuildFromPATH
        | path :: _ -> Dotnet.ProjInfo.Inspect.MSBuildExePath.Path path

type NetFWInfoConfig = {
    MSBuildHost : Dotnet.ProjInfo.Inspect.MSBuildExePath } with

    static member Default (msbuildLocator: MSBuildLocator) =
        let latestMSBuild = msbuildLocator.LatestInstalledMSBuild ()
        { NetFWInfoConfig.MSBuildHost = latestMSBuild }

    static member FromPATH (msbuildLocator: MSBuildLocator) =
        { NetFWInfoConfig.MSBuildHost = msbuildLocator.MSBuildFromPATH }
        
module internal NETFrameworkInfoProvider =

  open System
  open System.IO
  open Dotnet.ProjInfo
  open Dotnet.ProjInfo.Inspect

  let getInstalledNETVersions (msbuildHost: MSBuildExePath) =

    let log = ignore

    let projPath =
        //create the proj file
        NETFrameworkInfoFromMSBuild.createEnvInfoProj ()
        |> Path.GetFullPath

    let projDir = Path.GetDirectoryName(projPath)

    let cmd = NETFrameworkInfoFromMSBuild.installedNETFrameworks

    let runCmd exePath args = Utils.runProcess log projDir exePath (args |> String.concat " ")

    let msbuildExec =
        msbuild msbuildHost runCmd

    let result =
        projPath
        |> getProjectInfo log msbuildExec cmd []

    match result with
    | Ok (Dotnet.ProjInfo.Inspect.GetResult.InstalledNETFw fws) ->
        fws
    | Ok x ->
        failwithf "error getting msbuild info: unexpected %A" x
    | Error r ->
        failwithf "error getting msbuild info: unexpected %A" r

  let private defaultReferencesForNonProjectFiles () =
    // ref https://github.com/fsharp/FSharp.Compiler.Service/blob/1f497ef86fd5d0a18e5a935f3d16984fda91f1de/src/fsharp/CompileOps.fs#L1801
    // This list is the default set of references for "non-project" files
    
    // TODO make somehow this list public on FCS and use that directly instead of hardcode it in FSAC

    let GetDefaultSystemValueTupleReference () =
      //TODO check by tfm
      None

    // from https://github.com/fsharp/FSharp.Compiler.Service/blob/1f497ef86fd5d0a18e5a935f3d16984fda91f1de/src/fsharp/CompileOps.fs#L1803-L1832
    [
          yield "System"
          yield "System.Xml" 
          yield "System.Runtime.Remoting"
          yield "System.Runtime.Serialization.Formatters.Soap"
          yield "System.Data"
          yield "System.Drawing"
          yield "System.Core"
          // These are the Portable-profile and .NET Standard 1.6 dependencies of FSharp.Core.dll.  These are needed
          // when an F# sript references an F# profile 7, 78, 259 or .NET Standard 1.6 component which in turn refers 
          // to FSharp.Core for profile 7, 78, 259 or .NET Standard.
          yield "System.Runtime" // lots of types
          yield "System.Linq" // System.Linq.Expressions.Expression<T> 
          yield "System.Reflection" // System.Reflection.ParameterInfo
          yield "System.Linq.Expressions" // System.Linq.IQueryable<T>
          yield "System.Threading.Tasks" // valuetype [System.Threading.Tasks]System.Threading.CancellationToken
          yield "System.IO"  //  System.IO.TextWriter
          //yield "System.Console"  //  System.Console.Out etc.
          yield "System.Net.Requests"  //  System.Net.WebResponse etc.
          yield "System.Collections" // System.Collections.Generic.List<T>
          yield "System.Runtime.Numerics" // BigInteger
          yield "System.Threading"  // OperationCanceledException
          // always include a default reference to System.ValueTuple.dll in scripts and out-of-project sources
          match GetDefaultSystemValueTupleReference() with 
          | None -> ()
          | Some v -> yield v

          yield "System.Web"
          yield "System.Web.Services"
          yield "System.Windows.Forms"
          yield "System.Numerics" 
    ]

  let getAdditionalArgumentsBy (msbuildHost: MSBuildExePath) targetFramework =
    let refs =
      let log = ignore

      let projPath =
        //create the proj file
        NETFrameworkInfoFromMSBuild.createEnvInfoProj ()
        |> Path.GetFullPath

      let projDir = Path.GetDirectoryName(projPath)

      let allRefs = defaultReferencesForNonProjectFiles ()

      let props =
        targetFramework
        |> fun tfm -> "TargetFrameworkVersion", tfm
        |> List.singleton
        |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

      let cmd () = NETFrameworkInfoFromMSBuild.getReferencePaths allRefs

      let runCmd exePath args = Utils.runProcess log projDir exePath (args |> String.concat " ")

      let msbuildExec =
        msbuild msbuildHost runCmd

      let result =
        projPath
        |> getProjectInfo log msbuildExec cmd props

      match result with
      | Ok (Dotnet.ProjInfo.Inspect.GetResult.ResolvedNETRefs resolvedRefs) ->
          resolvedRefs
      | Ok x ->
          failwithf "error getting msbuild info: unexpected %A" x
      | r ->
          failwithf "error getting msbuild info: unexpected %A" r

    [ yield "--simpleresolution"
      yield "--noframework"
      yield! refs |> List.map (sprintf "-r:%s") ]
    
module FSharpCompilerServiceCheckerHelper =

  open System.IO

  let private isFSharpCore (s : string) = s.EndsWith "FSharp.Core.dll"

  let private fallbackFsharpCore =
    //TODO no, use another way. can be wrong by tfm, etc
    let dir = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    Path.Combine(dir, "FSharp.Core.dll")

  let internal ensureCorrectFSharpCore (options: string[]) =
    let fsharpCores, others = Array.partition isFSharpCore options

    // ensure that there is only one fsharpcore ref provided
    let fsharpCoreRef =
      match fsharpCores with
      | [||] -> sprintf "-r:%s" fallbackFsharpCore
      | [| ref |] -> ref
      | refs -> Array.head refs

    [| yield fsharpCoreRef
       yield! others |]
    
module FSharpCompilerServiceChecker =

  /// checker.GetProjectOptionsFromScript(file, source, otherFlags = additionaRefs, assumeDotNetFramework = true)
  type CheckerGetProjectOptionsFromScriptArgs = string * string * (string array) * bool
  type CheckerGetProjectOptionsFromScript<'a, 'b> = CheckerGetProjectOptionsFromScriptArgs -> Async<'a * 'b>

  let getProjectOptionsFromScript additionalArgumentsBy (checkerGetProjectOptionsFromScript: CheckerGetProjectOptionsFromScript<'a, 'b>) file source targetFramework = async {

    let additionaRefs =
      additionalArgumentsBy targetFramework
      |> Array.ofList

    // TODO SRTP
    let! (rawOptions, _) = checkerGetProjectOptionsFromScript (file, source, additionaRefs, true)

    let mapOtherOptions opts =
      opts
      |> FSharpCompilerServiceCheckerHelper.ensureCorrectFSharpCore
      |> Array.distinct

    return rawOptions, mapOtherOptions

  }

        
type NetFWInfo private (msbuildPath) =

    let installedNETVersionsLazy = lazy (NETFrameworkInfoProvider.getInstalledNETVersions msbuildPath)

    let additionalArgsByTfm = System.Collections.Concurrent.ConcurrentDictionary<string, string list>()

    let additionalArgumentsBy targetFramework =
        let f tfm = NETFrameworkInfoProvider.getAdditionalArgumentsBy msbuildPath tfm
        additionalArgsByTfm.GetOrAdd(targetFramework, f)

    member this.MSBuildPath
        with get () : Dotnet.ProjInfo.Inspect.MSBuildExePath = msbuildPath

    member this.InstalledNetFws() =
        installedNETVersionsLazy.Force()

    member this.LatestVersion () =
        let maxByVersion list =
            //TODO extract and test
            list
            |> List.map (fun (s: string) -> s, (s.TrimStart('v').Replace(".","").PadRight(3, '0')))
            |> List.maxBy snd
            |> fst

        this.InstalledNetFws()
        |> maxByVersion

    member this.GetProjectOptionsFromScript(checkerGetProjectOptionsFromScript, targetFramework, file, source) =
        FSharpCompilerServiceChecker.getProjectOptionsFromScript additionalArgumentsBy checkerGetProjectOptionsFromScript file source targetFramework

    static member Create(config: NetFWInfoConfig) =
        NetFWInfo(config.MSBuildHost)
        
type FsxBinder (netFwInfo: NetFWInfo, checker: FSharp.Compiler.SourceCodeServices.FSharpChecker) =

    member this.GetProjectOptionsFromScriptBy(tfm, file, source) = async {
      let dummy : FSharpCompilerServiceChecker.CheckerGetProjectOptionsFromScript<FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, _> =
        fun (file, source, otherFlags, assumeDotNetFramework) ->
          checker.GetProjectOptionsFromScript(file, source, otherFlags = otherFlags, assumeDotNetFramework = assumeDotNetFramework)

      let! (rawOptions, mapper) =
        netFwInfo.GetProjectOptionsFromScript(dummy, tfm, file, source)

      let rawOptions = { rawOptions with OtherOptions = mapper rawOptions.OtherOptions }

      return rawOptions
    }
// END TEMP
    
let getProjectOptionsFromScript scriptFile =
    let (checker: FSharpChecker) = FSharpChecker.Create(keepAssemblyContents=true)
    let msbuildLocator = MSBuildLocator()
    let netFwInfo = NetFWInfoConfig.Default(msbuildLocator) |> NetFWInfo.Create

    let tfm = netFwInfo.LatestVersion () // or specify a .NET version like `"4.6.1"`
    let fsxBinder = FsxBinder(netFwInfo, checker)

    let input = System.IO.File.ReadAllText(scriptFile)
    
    fsxBinder.GetProjectOptionsFromScriptBy(tfm, scriptFile, input)
    |> Async.RunSynchronously
    
let getCrackedProjectFromScript scriptFile =
        let fsprojOptions = getProjectOptionsFromScript scriptFile
        let parsedOptions =
            fsprojOptions.OtherOptions
            |> List.ofArray
            |> List.map (fun line ->
                if line.StartsWith("-r:") then
                    let dllPath = line.Substring(3)
                    match tryGetFablePackage dllPath with
                    | Some fablePackage -> ScriptOption.FablePackage fablePackage
                    | None -> ScriptOption.Dll dllPath
                else
                    ScriptOption.Other line
            )
    
        let dllReferences =
            let netcoreappPath = Path.GetDirectoryName(typeof<int>.GetTypeInfo().Assembly.Location)
            
            parsedOptions
            |> List.map (function | ScriptOption.Dll dll -> Some dll | _ -> None)
            |> List.choose id
            |> fun dllRx ->
                Array.fold (fun dllRs rf ->
                    if not (List.exists (fun r -> Path.GetFileNameWithoutExtension(r) = rf) dllRs) then
                       let dllPath = Path.Combine(netcoreappPath, (sprintf "%s.dll" rf))
                       dllPath::dllRs
                    else
                        dllRs
                ) dllRx Fable.Standalone.Metadata.references_core
    
            
        let otherOptions =
            parsedOptions
            |> List.map (function | ScriptOption.Other o -> Some o | _ -> None)
            |> List.choose id
    
        let fablePkgs =
            parsedOptions
            |> List.map (function | ScriptOption.FablePackage fp -> Some fp | _ -> None)
            |> List.choose id
            |> sortFablePackages
            
        let crackedProj: CrackedFsproj =
            { ProjectFile = fsprojOptions.ProjectFileName
              SourceFiles = fsprojOptions.SourceFiles |> List.ofArray
              ProjectReferences = []
              DllReferences = dllReferences
              PackageReferences = fablePkgs
              OtherCompilerOptions = otherOptions }
            
        [], crackedProj

let getCrackedProjects (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        getCrackedProjectFromScript projFile
    | ".fsproj" ->
        getCrackedProjectsFromMainFsproj projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetCrackedProjects (projFile: string) =
    let retryUntil = (DateTime.Now + TimeSpan.FromSeconds 2.)
    let rec retry () =
        try
            getCrackedProjects projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.Now then
                System.Threading.Thread.Sleep 500
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | _ -> reraise()
    retry()

/// FAKE and other tools clean dirs but don't remove them, so check whether it doesn't exist or it's empty
let isDirectoryEmpty dir =
    not(Directory.Exists(dir)) || Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty

let createFableDir rootDir =
    let fableDir = IO.Path.Combine(rootDir, Naming.fableHiddenDir)
    if isDirectoryEmpty fableDir then
        Directory.CreateDirectory(fableDir) |> ignore
        File.WriteAllText(IO.Path.Combine(fableDir, ".gitignore"), "*.*")
    fableDir

let copyDirIfDoesNotExist (source: string) (target: string) =
    if GlobalParams.Singleton.ForcePkgs || isDirectoryEmpty target then
        Directory.CreateDirectory(target) |> ignore
        if Directory.Exists source |> not then
            failwith ("Source directory is missing: " + source)
        let source = source.TrimEnd('/', '\\')
        let target = target.TrimEnd('/', '\\')
        for dirPath in Directory.GetDirectories(source, "*", SearchOption.AllDirectories) do
            Directory.CreateDirectory(dirPath.Replace(source, target)) |> ignore
        for newPath in Directory.GetFiles(source, "*.*", SearchOption.AllDirectories) do
            File.Copy(newPath, newPath.Replace(source, target), true)

let copyFableLibraryAndPackageSources rootDir (pkgs: FablePackage list) =
    let fableDir = createFableDir rootDir
    let fableLibrarySource = GlobalParams.Singleton.FableLibraryPath
    let fableLibraryPath =
        if fableLibrarySource.StartsWith(Literals.FORCE)
        then fableLibrarySource.Replace(Literals.FORCE, "")
        else
            if isDirectoryEmpty fableLibrarySource then
                failwithf "fable-library directory is empty, please build FableLibrary: %s" fableLibrarySource
            Log.logVerbose(lazy ("fable-library: " + fableLibrarySource))
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

let getFullProjectOpts (define: string[]) (rootDir: string) (projFile: string) =
    let projFile = Path.GetFullPath(projFile)
    if not(File.Exists(projFile)) then
        failwith ("File does not exist: " + projFile)
    let projRefs, mainProj = retryGetCrackedProjects projFile
    let fableLibraryPath, pkgRefs =
        copyFableLibraryAndPackageSources rootDir mainProj.PackageReferences
    let projOpts =
        let sourceFiles =
            let pkgSources = pkgRefs |> List.collect getSourcesFromFsproj
            let refSources = projRefs |> List.collect (fun x -> x.SourceFiles)
            pkgSources @ refSources @ mainProj.SourceFiles |> List.toArray |> removeFilesInObjFolder
        let sourceFiles =
            match GlobalParams.Singleton.ReplaceFiles with
            | [] -> sourceFiles
            | replacements ->
                try
                    sourceFiles |> Array.map (fun path ->
                        replacements |> List.tryPick (fun (pattern, replacement) ->
                            if path.Contains(pattern)
                            then Path.normalizeFullPath(replacement) |> Some
                            else None)
                        |> Option.defaultValue path)
                with ex ->
                    printfn "Cannot replace files: %s" ex.Message
                    sourceFiles
        for file in sourceFiles do
            if file.EndsWith(".fs") && not(File.Exists(file)) then
                failwithf "File does not exist: %s" file
        let otherOptions =
            let dllRefs =
                // We only keep dllRefs for the main project
                mainProj.DllReferences
                // We can filter out system references not needed for Fable projects
                // though it doesn't seem to improve startup time too much
                |> Seq.filter (fun path ->
                    match IO.Path.GetFileNameWithoutExtension(path) with
                    | "WindowsBase" -> false
                    | Naming.StartsWith "Microsoft." _ -> false
                    | (Naming.StartsWith "System." _) as name
                        when not(Literals.SYSTEM_CORE_REFERENCES.Contains name) -> false
                    | _ -> true)
                |> Seq.map (fun r -> "-r:" + r)
                |> Seq.toArray
            let otherOpts = mainProj.OtherCompilerOptions |> Array.ofList
            [ getBasicCompilerArgs define
              otherOpts
              dllRefs ]
            |> Array.concat
        makeProjectOptions projFile sourceFiles otherOptions
    projOpts, fableLibraryPath
