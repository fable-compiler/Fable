module Fable.Compiler.ProjectParser

open Fable.Compiler.Platform
open System.Collections.Generic
open System.Text.RegularExpressions

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern)
    if m.Success then
        let mutable groups = []
        for i = m.Groups.Count - 1 downto 0 do
            groups <- m.Groups.[i].Value::groups
        Some groups
    else None

let parseCompilerOptions projectText =

    // get project type
    let m = Regex.Match(projectText, @"<OutputType[^>]*>([^<]*)<\/OutputType[^>]*>")
    let target = if m.Success then m.Groups.[1].Value.Trim().ToLowerInvariant() else ""

    // get warning level
    let m = Regex.Match(projectText, @"<WarningLevel[^>]*>([^<]*)<\/WarningLevel[^>]*>")
    let warnLevel = if m.Success then m.Groups.[1].Value.Trim() else ""

    // get treat warnings as errors
    let m = Regex.Match(projectText, @"<TreatWarningsAsErrors[^>]*>([^<]*)<\/TreatWarningsAsErrors[^>]*>")
    let treatWarningsAsErrors = m.Success && m.Groups.[1].Value.Trim().ToLowerInvariant() = "true"

    // get conditional defines
    let defines =
        Regex.Matches(projectText,  @"<DefineConstants[^>]*>([^<]*)<\/DefineConstants[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.append ["FABLE_COMPILER"]
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(DefineConstants)"; ""]
        |> Seq.toArray

    // get disabled warnings
    let nowarns =
        Regex.Matches(projectText, @"<NoWarn[^>]*>([^<]*)<\/NoWarn[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(NoWarn)"; ""]
        |> Seq.toArray

    // get warnings as errors
    let warnAsErrors =
        Regex.Matches(projectText, @"<WarningsAsErrors[^>]*>([^<]*)<\/WarningsAsErrors[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(WarningsAsErrors)"; ""]
        |> Seq.toArray

    // get other flags
    let otherFlags =
        Regex.Matches(projectText, @"<OtherFlags[^>]*>([^<]*)<\/OtherFlags[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(' '))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(OtherFlags)"; ""]
        |> Seq.toArray

    let otherOptions = [|
        if target.Length > 0 then
            yield "--target:" + target
        if warnLevel.Length > 0 then
            yield "--warn:" + warnLevel
        if treatWarningsAsErrors then
            yield "--warnaserror+"
        for d in defines do yield "-d:" + d
        for n in nowarns do yield "--nowarn:" + n
        for e in warnAsErrors do yield "--warnaserror:" + e
        for o in otherFlags do yield o
    |]
    otherOptions

let parseProjectScript projectFileName =
    let projectText = readAllText projectFileName
    let projectDir = Path.GetDirectoryName projectFileName
    let dllRefs, srcFiles =
        (([||], [||]), projectText.Split('\n'))
        ||> Array.fold (fun (dllRefs, srcFiles) line ->
            match line.Trim() with
            | Regex @"^#r\s+""(.*?)""$" [_;path]
                when not(path.EndsWith("Fable.Core.dll")) ->
                Array.append [|Path.Combine(projectDir, path)|] dllRefs, srcFiles
            | Regex @"^#load\s+""(.*?)""$" [_;path] ->
                dllRefs, Array.append [|Path.Combine(projectDir, path)|] srcFiles
            | _ -> dllRefs, srcFiles)
    let projectRefs = [||]
    let sourceFiles = Array.append srcFiles [|Path.GetFileName projectFileName|]
    let otherOptions = [| "--define:FABLE_COMPILER" |]
    (dllRefs, projectRefs, sourceFiles, otherOptions)

let parseProjectFile projectFileName =
    let projectText = readAllText projectFileName

    // remove all comments
    let projectText = Regex.Replace(projectText, @"<!--[\s\S]*?-->", "")

    // get project references
    let projectRefs =
        Regex.Matches(projectText, @"<ProjectReference\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)")
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
        |> Seq.toArray

    // replace some variables
    let projectText = projectText.Replace(@"$(MSBuildProjectDirectory)", ".")
    let m = Regex.Match(projectText, @"<FSharpSourcesRoot[^>]*>([^<]*)<\/FSharpSourcesRoot[^>]*>")
    let sourcesRoot = if m.Success then m.Groups.[1].Value.Replace("\\", "/") else ""
    let projectText = projectText.Replace(@"$(FSharpSourcesRoot)", sourcesRoot)

    // get source files
    let sourceFilesRegex = @"<Compile\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)"
    let sourceFiles =
        Regex.Matches(projectText, sourceFilesRegex)
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
        |> Seq.toArray

    let dllRefs = [||]
    let otherOptions = parseCompilerOptions projectText
    (dllRefs, projectRefs, sourceFiles, otherOptions)

let makeHashSetIgnoreCase () =
    let equalityComparerIgnoreCase =
        { new IEqualityComparer<string> with
            member __.Equals(x, y) = x.ToLowerInvariant() = y.ToLowerInvariant()
            member __.GetHashCode(x) = hash (x.ToLowerInvariant()) }
    HashSet<string>(equalityComparerIgnoreCase)

let dedupProjectRefs (projSet: HashSet<string>) projectRefs =
    let newRefs = projectRefs |> Array.filter (fun x -> projSet.Contains(x) |> not)
    projSet.UnionWith(newRefs)
    newRefs

let dedupFileNames (fileSet: HashSet<string>) fileNames =
    let padName (fileName: string) =
        let pos = fileName.LastIndexOf(".")
        let nm = if pos < 0 then fileName else fileName.Substring(0, pos)
        let ext = if pos < 0 then "" else fileName.Substring(pos)
        nm + "_" + ext
    let rec dedup fileName =
        if fileSet.Contains(fileName) then
            dedup (padName fileName)
        else
            fileSet.Add(fileName) |> ignore
            fileName
    fileNames |> Array.map dedup

let rec parseProject (projSet: HashSet<string>) (projectFileName: string) =
    let (dllRefs, projectRefs, sourceFiles, otherOptions) =
        if projectFileName.EndsWith(".fsx")
        then parseProjectScript projectFileName
        else parseProjectFile projectFileName

    let projectFileDir = Path.GetDirectoryName projectFileName
    let isAbsolutePath (path: string) = path.StartsWith("/") || path.IndexOf(":") = 1
    let makePath path =
        if isAbsolutePath path then path
        else Path.Combine(projectFileDir, path)
        |> normalizeFullPath

    let sourcePaths = sourceFiles |> Array.map makePath
    let sourceTexts = sourcePaths |> Array.map readAllText

     // parse and combine all referenced projects into one big project
    let parsedProjects = projectRefs |> Array.map makePath |> dedupProjectRefs projSet |> Array.map (parseProject projSet)
    let sourcePaths  = sourcePaths  |> Array.append (parsedProjects |> Array.collect (fun (_,x,_,_) -> x))
    let sourceTexts  = sourceTexts  |> Array.append (parsedProjects |> Array.collect (fun (_,_,x,_) -> x))
    let otherOptions = otherOptions |> Array.append (parsedProjects |> Array.collect (fun (_,_,_,x) -> x))

    (dllRefs, sourcePaths, sourceTexts, otherOptions |> Array.distinct)
