module Fable.Client.Webpack.ProjectCracker

open System
open System.IO
open System.Xml.Linq
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST

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

let getProjectOptionsFromScript (checker: FSharpChecker) (opts: CompilerOptions) scriptFile =
    let otherFlags = [|
        yield "--target:library"
#if DOTNETCORE
        yield "--targetprofile:netcore"
#endif
        for symbol in opts.symbols do yield "--define:" + symbol
    |]
    checker.GetProjectOptionsFromScript(scriptFile, File.ReadAllText scriptFile,
                                        assumeDotNetFramework=false, otherFlags=otherFlags)
    |> Async.RunSynchronously
    |> fun opts ->
        opts.OtherOptions
        |> Array.filter (fun x ->
            // Keep only relative references
            x.StartsWith("-r:") && (x.Contains("./") || x.Contains(".\\")))
        |> makeProjectOptions opts.ProjectFileName opts.ProjectFileNames

let fsCoreLib = typeof<Microsoft.FSharp.Core.MeasureAttribute>.GetTypeInfo().Assembly.Location
let sysCoreLib = typeof<System.Object>.GetTypeInfo().Assembly.Location
let sysPath = Path.GetDirectoryName(sysCoreLib)
let localPath = Path.GetDirectoryName(typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location)

let getBasicCompilerArgs (opts: CompilerOptions) optimize =
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
        for symbol in opts.symbols do
            yield "--define:" + symbol
        yield "--optimize" + (if optimize then "+" else "-")
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
#if DOTNETCORE
        yield "--targetprofile:netcore"
        yield "-r:" + sysCoreLib // "CoreLib"
#else
        yield "-r:" + resolve "System"
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
    |]

/// Ultra-simplistic resolution of .fsproj files
let crackFsproj (projFile: string) =
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
    let doc = XDocument.Load(projFile)
    let sourceFiles, projectReferences, relativeDllReferences =
        doc.Root.Elements()
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
    let projDir = Path.GetDirectoryName(projFile) |> Path.normalizePath
    let reg = Regex("\\.fs[ix]?$")
    { projectFile = projFile
    ; sourceFiles =
        sourceFiles
        |> List.filter (fun fileName -> reg.IsMatch(fileName))
        |> List.map (fun fileName -> Path.Combine(projDir, Path.normalizePath fileName) |> Path.GetFullPath)
    ; projectReferences =
        projectReferences
        |> List.map (fun x -> Path.Combine(projDir, Path.normalizePath x) |> Path.GetFullPath)
    ; dllReferences =
        relativeDllReferences
        |> List.map (fun x -> Path.Combine(projDir, Path.normalizePath x) |> Path.GetFullPath) }

let getProjectOptionsFromFsproj projFile =
    let rec crackProjects (acc: CrackedFsproj list) projFile =
        match acc |> List.tryFind (fun x -> x.projectFile = projFile) with
        | Some crackedFsproj ->
            // Add a reference to the front to preserve compilation order
            // Duplicated items will be removed later
            crackedFsproj::acc
        | None ->
            let crackedFsproj = crackFsproj projFile
            let acc = crackedFsproj::acc
            (crackedFsproj.projectReferences, acc)
            ||> Seq.foldBack (fun projFile acc ->
                crackProjects acc projFile)
    let crackedFsprojs =
        crackProjects [] projFile
        |> List.distinctBy (fun x -> x.projectFile)
    let sourceFiles =
        crackedFsprojs |> Seq.collect (fun x -> x.sourceFiles) |> Seq.toArray
    let otherOptions =
        crackedFsprojs |> Seq.collect (fun x -> x.dllReferences)
        |> Seq.distinct |> Seq.map ((+) "-r:") |> Seq.toArray
    makeProjectOptions projFile sourceFiles otherOptions

let getProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    match (Path.GetExtension projFile).ToLower() with
    | ".fsx" ->
        getProjectOptionsFromScript checker opts projFile
    | ".fsproj" ->
        getProjectOptionsFromFsproj projFile
    | s -> failwithf "Unsupported project type: %s" s

// It is common for editors with rich editing or 'intellisense' to also be watching the project
// file for changes. In some cases that editor will lock the file which can cause fable to
// get a read error. If that happens the lock is usually brief so we can reasonably wait
// for it to be released.
let retryGetProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    let retryUntil = (DateTime.UtcNow + TimeSpan.FromSeconds 5.)
    let rec retry () =
        try
            getProjectOpts checker opts projFile
        with
        | :? IOException as ioex ->
            if retryUntil > DateTime.UtcNow then
                System.Threading.Thread.Sleep 100
                retry()
            else
                failwithf "IO Error trying read project options: %s " ioex.Message
        | ex -> failwithf "Cannot read project options: %s" ex.Message
    retry()

let getFullProjectOpts (checker: FSharpChecker) (opts: CompilerOptions) (projFile: string) =
    let projOpts = retryGetProjectOpts checker opts projFile
    Array.append (getBasicCompilerArgs opts false) projOpts.OtherOptions
    |> makeProjectOptions projOpts.ProjectFileName projOpts.ProjectFileNames

