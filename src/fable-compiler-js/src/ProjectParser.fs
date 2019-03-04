module Fable.Compiler.ProjectParser

open Fable.Compiler.Platform
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

let parseProjectScript projectPath =
    let projectFileName = Path.GetFileName projectPath
    let projectText = readAllText projectPath
    let projectDir = Path.GetDirectoryName projectPath
    let dllRefs, srcFiles =
        (([||], [||]), projectText.Split('\n'))
        ||> Array.fold (fun (dllRefs, srcFiles) line ->
            let line = line.Trim()
            match line.Trim() with
            | Regex @"^#r\s+""(.*?)""$" [_;path]
                when not(path.EndsWith("Fable.Core.dll")) ->
                Array.append [|Path.Combine(projectDir, path)|] dllRefs, srcFiles
            | Regex @"^#load\s+""(.*?)""$" [_;path] ->
                dllRefs, Array.append [|Path.Combine(projectDir, path)|] srcFiles
            | _ -> dllRefs, srcFiles)
    let projectRefs = [||]
    let sourceFiles = Array.append srcFiles [|Path.GetFileName projectPath|]
    let otherOptions = [| "--define:FABLE_COMPILER" |]
    (projectFileName, dllRefs, projectRefs, sourceFiles, otherOptions)

let parseProjectFile projectPath =
    let projectFileName = Path.GetFileName projectPath
    let projectText = readAllText projectPath

    // remove all comments
    let projectText = Regex.Replace(projectText, @"<!--[\s\S]*?-->", "")

    // get project references
    let projectRefs =
        Regex.Matches(projectText, @"<ProjectReference\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)")
        |> Seq.choose (fun m ->
            let fsproj = m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/")
            if fsproj.EndsWith("/Fable.Core.fsproj") then None else Some fsproj)
        |> Seq.toArray

    // replace some variables
    let projectText = projectText.Replace(@"$(MSBuildProjectDirectory)", __dirname)
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
    (projectFileName, dllRefs, projectRefs, sourceFiles, otherOptions)

let rec parseProject (projectPath: string) =
    let (projectFileName, dllRefs, projectRefs, sourceFiles, otherOptions) =
        if projectPath.EndsWith(".fsx")
        then parseProjectScript projectPath
        else parseProjectFile projectPath

    let projectFileDir = Path.GetDirectoryName projectPath
    let isAbsolutePath (path: string) = path.StartsWith("/") || path.IndexOf(":") = 1
    let makePath path = if isAbsolutePath path then path else Path.Combine(projectFileDir, path)

    let sourcePaths = sourceFiles |> Array.map (fun path -> path |> makePath |> normalizeFullPath)
    let sourceTexts = sourceFiles |> Array.map (fun path -> path |> makePath |> readAllText)

     // parse and combine all referenced projects into one big project
    let parsedProjects = projectRefs |> Array.map makePath |> Array.map parseProject
    let sourcePaths  = sourcePaths  |> Array.append (parsedProjects |> Array.collect (fun (_,_,x,_,_) -> x))
    let sourceTexts  = sourceTexts  |> Array.append (parsedProjects |> Array.collect (fun (_,_,_,x,_) -> x))
    let otherOptions = otherOptions |> Array.append (parsedProjects |> Array.collect (fun (_,_,_,_,x) -> x))

    (projectFileName, dllRefs, sourcePaths, sourceTexts, otherOptions |> Array.distinct)

let dedupFileNames fileNames =
    let comparerIgnoreCase =
        { new System.Collections.Generic.IEqualityComparer<string> with
            member __.Equals(x, y) = x.ToLowerInvariant() = y.ToLowerInvariant()
            member __.GetHashCode(x) = hash (x.ToLowerInvariant()) }
    let nameSet = System.Collections.Generic.HashSet<string>(comparerIgnoreCase)
    let padName (name: string) =
        let pos = name.LastIndexOf(".")
        let nm = if pos < 0 then name else name.Substring(0, pos)
        let ext = if pos < 0 then "" else name.Substring(pos)
        nm + "_" + ext
    let rec dedup name =
        if nameSet.Contains(name) then
            dedup (padName name)
        else
            nameSet.Add(name) |> ignore
            name
    fileNames |> Array.map dedup
