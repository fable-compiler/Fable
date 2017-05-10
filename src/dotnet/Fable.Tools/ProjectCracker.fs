module Fable.Tools.ProjectCracker

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

/// At the moment this only supports conditions like "$(MSBuildThisFileDirectory.Contains('node_modules')"
let tryEvalMsBuildCondition (projDir: string) (condition: string) =
    let reg = Regex("(!)?\$\(MSBuildThisFileDirectory\.Contains\('(.*?)'\)", RegexOptions.Compiled)
    let m = reg.Match(condition)
    if m.Success
    then
        let negate = m.Groups.[1].Value = "!"
        let contains = projDir.Contains(m.Groups.[2].Value)
        // printfn "ProjDir contains '%s': %b (negate: %b) (%s)" m.Groups.[2].Value contains negate projDir
        if negate then not contains else contains
    else failwith ("Cannot evaluate MSBuild condition " + condition)

/// Ultra-simplistic resolution of .fsproj files
let crackFsproj (projFile: string) =
    let withName s (xs: XElement seq) =
        xs |> Seq.filter (fun x -> x.Name.LocalName = s)
    let withNameAndFulfillingCondition projDir s (xs: XElement seq) =
        xs |> Seq.filter (fun el ->
            if el.Name.LocalName = s
            then
                match el.Attribute(XName.Get "Condition") with
                | null -> true
                | att -> tryEvalMsBuildCondition projDir att.Value
            else false)
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
    if not(File.Exists(projFile)) then
        failwith ("File does not exist: " + projFile)
    let doc = XDocument.Load(projFile)
    let projDir = Path.GetDirectoryName(projFile) |> Path.normalizePath
    let sourceFiles, projectReferences, relativeDllReferences =
        doc.Root.Elements()
        |> withNameAndFulfillingCondition projDir "ItemGroup"
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

let getPaketProjRefs projFile =
    let rec findPaketDependenciesDir dir =
        if File.Exists(Path.Combine(dir, "paket.dependencies"))
        then dir
        else
            let parent = Directory.GetParent(dir)
            if isNull parent then
                failwith "Couldn't find paket.dependencies directory"
            findPaketDependenciesDir parent.FullName
    let projDir = Path.GetDirectoryName(projFile)
    let projFileName = Path.GetFileName(projFile)
    let paketRefs = IO.Path.Combine(projDir, "obj", projFileName + ".references")
    let paketDependenciesDir = lazy findPaketDependenciesDir projDir
    if File.Exists(paketRefs) then
        File.ReadLines(paketRefs)
        |> Seq.filter(fun line -> line.StartsWith("Fable.") && not(line.StartsWith("Fable.Core")))
        |> Seq.rev  // Paket orders dependencies in inverse compilation order
        |> Seq.map (fun fableDependency ->
            let fableDependency = fableDependency.Substring(0, fableDependency.IndexOf(','))
            IO.Path.Combine(paketDependenciesDir.Value, "packages", fableDependency, "Content", fableDependency + ".fsproj"))
        |> Seq.toList
    else
        []

let getProjectOptionsFromFsproj projFile =
    let rec crackProjects (acc: CrackedFsproj list) extraProjRefs projFile =
        acc |> List.tryFind (fun x ->
            String.Equals(x.projectFile, projFile, StringComparison.OrdinalIgnoreCase))
        |> function
        | Some crackedFsproj ->
            // Add a reference to the front to preserve compilation order
            // Duplicated items will be removed later
            crackedFsproj::acc
        | None ->
            let crackedFsproj = crackFsproj projFile
            let acc = crackedFsproj::acc
            (crackedFsproj.projectReferences @ extraProjRefs, acc)
            ||> Seq.foldBack (fun projFile acc ->
                crackProjects acc [] projFile)
    let crackedFsprojs =
        crackProjects [] (getPaketProjRefs projFile) projFile
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

