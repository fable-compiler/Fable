module Fable.Compiler.ProjectParser

open Fable.Compiler.Platform
open System.Collections.Generic
open System.Text.RegularExpressions

type ReferenceType =
    | ProjectReference of string
    | PackageReference of string * string

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern)
    if m.Success then Some [for x in m.Groups -> x.Value]
    else None

let getXmlWithoutComments xml =
    Regex.Replace(xml, @"<!--[\s\S]*?-->", "")

let getXmlTagContents tag xml =
    let pattern = sprintf @"<%s[^>]*>([^<]*)<\/%s[^>]*>" tag tag
    Regex.Matches(xml, pattern)
    |> Seq.map (fun m -> m.Groups.[1].Value.Trim())

let getXmlTagContentsFirstOrDefault tag defaultValue xml =
    defaultArg (getXmlTagContents tag xml |> Seq.tryHead) defaultValue

let getXmlTagAttributes1 tag attr1 xml =
    let pattern = sprintf """<%s\s+[^>]*%s\s*=\s*("[^"]*|'[^']*)""" tag attr1
    Regex.Matches(xml, pattern)
    |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim())

let getXmlTagAttributes2 tag attr1 attr2 xml =
    let pattern = sprintf """<%s\s+[^>]*%s\s*=\s*("[^"]*|'[^']*)[^>]*%s\s*=\s*("[^"]*|'[^']*)""" tag attr1 attr2
    Regex.Matches(xml, pattern)
    |> Seq.map (fun m ->
        m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim(),
        m.Groups.[2].Value.TrimStart('"').TrimStart(''').Trim())

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.")
    || pkgName.StartsWith("Microsoft.")
    || pkgName.StartsWith("runtime.")
    || pkgName = "NETStandard.Library"
    || pkgName = "FSharp.Core"
    || pkgName = "Fable.Core"

let parsePackageSpec nuspecPath =
    // get package spec xml
    let packageXml = readAllText nuspecPath
    // get package dependencies
    let references =
        packageXml
        |> getXmlWithoutComments
        |> getXmlTagAttributes2 "dependency" "id" "version"
        |> Seq.map PackageReference
        |> Seq.toArray
    references

// let resolvePackage (pkgName, pkgVersion) =
//     if not (isSystemPackage pkgName) then
//         let homePath = getHomePath().Replace('\\', '/')
//         let nugetPath = sprintf ".nuget/packages/%s/%s" pkgName pkgVersion
//         let pkgPath = Path.Combine(homePath, nugetPath.ToLowerInvariant())
//         let libPath = Path.Combine(pkgPath, "lib")
//         let fablePath = Path.Combine(pkgPath, "fable")
//         let binaryPaths = getDirFiles libPath ".dll"
//         let nuspecPaths = getDirFiles pkgPath ".nuspec"
//         let fsprojPaths = getDirFiles fablePath ".fsproj"
//         if Array.isEmpty nuspecPaths then
//             printfn "ERROR: Cannot find package %s" pkgPath
//         let binaryOpt = binaryPaths |> Array.tryLast
//         let dependOpt = nuspecPaths |> Array.tryLast |> Option.map parsePackageSpec
//         let fsprojOpt = fsprojPaths |> Array.tryLast |> Option.map ProjectReference
//         let pkgRefs, dllPaths =
//             match binaryOpt, dependOpt, fsprojOpt with
//             | _, _, Some projRef ->
//                 [| projRef |], [||]
//             | Some dllRef, Some dependencies, _ ->
//                 dependencies, [| dllRef |]
//             | _, _, _ -> [||], [||]
//         pkgRefs, dllPaths
//     else [||], [||]

let parseCompilerOptions projectXml =
    // get project settings,
    let target = projectXml |> getXmlTagContentsFirstOrDefault "OutputType" ""
    let langVersion = projectXml |> getXmlTagContentsFirstOrDefault "LangVersion" ""
    let warnLevel = projectXml |> getXmlTagContentsFirstOrDefault "WarningLevel" ""
    let treatWarningsAsErrors = projectXml |> getXmlTagContentsFirstOrDefault "TreatWarningsAsErrors" ""

    // get conditional defines
    let defines =
        projectXml
        |> getXmlTagContents "DefineConstants"
        |> Seq.collect (fun s -> s.Split(';'))
        |> Seq.append ["FABLE_COMPILER"; "FABLE_COMPILER_JS"]
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(DefineConstants)"; ""]
        |> Seq.toArray

    // get disabled warnings
    let nowarns =
        projectXml
        |> getXmlTagContents "NoWarn"
        |> Seq.collect (fun s -> s.Split(';'))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(NoWarn)"; ""]
        |> Seq.toArray

    // get warnings as errors
    let warnAsErrors =
        projectXml
        |> getXmlTagContents "WarningsAsErrors"
        |> Seq.collect (fun s -> s.Split(';'))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(WarningsAsErrors)"; ""]
        |> Seq.toArray

    // get other flags
    let otherFlags =
        projectXml
        |> getXmlTagContents "OtherFlags"
        |> Seq.collect (fun s -> s.Split(' '))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(OtherFlags)"; ""]
        |> Seq.toArray

    let otherOptions = [|
        if target.Length > 0 then
            yield "--target:" + target
        if langVersion.Length > 0 then
            yield "--langversion:" + langVersion
        if warnLevel.Length > 0 then
            yield "--warn:" + warnLevel
        if treatWarningsAsErrors = "true" then
            yield "--warnaserror+"
        for d in defines do yield "-d:" + d
        for n in nowarns do yield "--nowarn:" + n
        for e in warnAsErrors do yield "--warnaserror:" + e
        for o in otherFlags do yield o
    |]
    otherOptions

let makeFullPath projectFileDir (path: string) =
    let path = path.Replace('\\', '/')
    let isAbsolutePath (path: string) =
        path.StartsWith('/') || path.IndexOf(':') = 1
    if isAbsolutePath path then path
    else Path.Combine(projectFileDir, path)
    |> normalizeFullPath

let parseProjectScript projectFilePath =
    let projectXml = readAllText projectFilePath
    let projectDir = Path.GetDirectoryName projectFilePath
    let dllRefs, srcFiles =
        (([||], [||]), projectXml.Split('\n'))
        ||> Array.fold (fun (dllRefs, srcFiles) line ->
            match line.Trim() with
            | Regex @"^#r\s+""(.*?)""$" [_;path]
                when not(path.EndsWith("Fable.Core.dll")) ->
                Array.append [| Path.Combine(projectDir, path) |] dllRefs, srcFiles
            | Regex @"^#load\s+""(.*?)""$" [_;path] ->
                dllRefs, Array.append [| Path.Combine(projectDir, path) |] srcFiles
            | _ -> dllRefs, srcFiles)
    let projectRefs = [||]
    let sourceFiles = Array.append srcFiles [| Path.GetFileName projectFilePath |]
    let otherOptions = [| "--define:FABLE_COMPILER"; "--define:FABLE_COMPILER_JS" |]
    (projectRefs, dllRefs, sourceFiles, otherOptions)

let parseProjectFile projectFilePath =
    // get project xml without any comments
    let projectXml = readAllText projectFilePath |> getXmlWithoutComments
    let projectDir = Path.GetDirectoryName projectFilePath

    // get package references
    let packageRefs =
        projectXml
        |> getXmlTagAttributes2 "PackageReference" "Include" "Version"
        |> Seq.map PackageReference
        |> Seq.toArray

    // get project references
    let projectRefs =
        projectXml
        |> getXmlTagAttributes1 "ProjectReference" "Include"
        |> Seq.map (makeFullPath projectDir >> ProjectReference)
        |> Seq.toArray

    // replace some variables
    let projectXml = projectXml.Replace("$(MSBuildProjectDirectory)", ".")
    let sourceRoot = projectXml |> getXmlTagContentsFirstOrDefault "FSharpSourcesRoot" ""
    let projectXml = projectXml.Replace("$(FSharpSourcesRoot)", sourceRoot.Replace('\\', '/'))
    let yaccOutput = projectXml |> getXmlTagContentsFirstOrDefault "FsYaccOutputFolder" ""
    let projectXml = projectXml.Replace("$(FsYaccOutputFolder)", yaccOutput.Replace('\\', '/'))

    // get source files
    let sourceFiles =
        projectXml
        |> getXmlTagAttributes1 "Compile" "Include"
        |> Seq.map (makeFullPath projectDir)
        // |> Seq.collect getGlobFiles
        |> Seq.toArray

    let dllRefs = [||]
    let projectRefs = Array.append projectRefs packageRefs
    let otherOptions = parseCompilerOptions projectXml
    (projectRefs, dllRefs, sourceFiles, otherOptions)

let makeHashSetIgnoreCase () =
    let equalityComparerIgnoreCase =
        { new IEqualityComparer<string> with
            member _.Equals(x, y) = x.ToLowerInvariant() = y.ToLowerInvariant()
            member _.GetHashCode(x) = hash (x.ToLowerInvariant()) }
    HashSet<string>(equalityComparerIgnoreCase)

let dedupReferences (refSet: HashSet<string>) references =
    let refName = function
        | ProjectReference path -> path
        | PackageReference (pkgName, pkgVersion) -> pkgName + "," + pkgVersion
    let newRefs = references |> Array.filter (refName >> refSet.Contains >> not)
    refSet.UnionWith(newRefs |> Array.map refName)
    newRefs

let parseProject projectFilePath =

    let rec parseProject (refSet: HashSet<string>) (projectRef: ReferenceType) =
        let projectRefs, dllPaths, sourcePaths, otherOptions =
            match projectRef with
            | ProjectReference path ->
                if path.EndsWith(".fsx")
                then parseProjectScript path
                else parseProjectFile path
            | PackageReference (pkgName, pkgVersion) ->
                // let pkgRefs, dllPaths = resolvePackage (pkgName, pkgVersion)
                // pkgRefs, dllPaths, [||], [||]
                [||], [||], [||], [||]

        // parse and combine all referenced projects into one big project
        let parseResult = projectRefs |> dedupReferences refSet |> Array.map (parseProject refSet)
        let dllPaths = dllPaths |> Array.append (parseResult |> Array.collect (fun (x,_,_) -> x))
        let sourcePaths = sourcePaths |> Array.append (parseResult |> Array.collect (fun (_,x,_) -> x))
        let otherOptions = otherOptions |> Array.append (parseResult |> Array.collect (fun (_,_,x) -> x))

        (dllPaths, sourcePaths, otherOptions)

    let refSet = makeHashSetIgnoreCase ()
    let projectRef = ProjectReference projectFilePath
    let dllPaths, sourcePaths, otherOptions = parseProject refSet projectRef
    (dllPaths |> Array.distinct,
     sourcePaths |> Array.distinct,
     otherOptions |> Array.distinct)
