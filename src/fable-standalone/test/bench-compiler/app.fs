module Bench.App

open Bench.Platform
open System.Text.RegularExpressions

let references = Fable.Standalone.Metadata.references_core
let metadataPath = Path.Combine(__SOURCE_DIRECTORY__, "../../../fable-metadata/lib/") // .NET BCL binaries

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern)
    if m.Success then
        let mutable groups = []
        for i = m.Groups.Count - 1 downto 0 do
            groups <- m.Groups.[i].Value::groups
        Some groups
    else None

let makeOtherOptions (target, defines, nowarns, otherFlags) =
    let otherOptions = [|
        yield "--target:" + target
        for d in defines do yield "-d:" + d
        for n in nowarns do yield "-nowarn:" + n
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
    let target = "exe"
    let defines = [|"FABLE_COMPILER"|]
    let nowarns = [||]
    let otherFlags = [||]
    let otherOptions = makeOtherOptions (target, defines, nowarns, otherFlags)
    (projectFileName, dllRefs, projectRefs, sourceFiles, otherOptions)

let parseProjectFile projectPath =
    let projectFileName = Path.GetFileName projectPath
    let projectText = readAllText projectPath

    // remove all comments
    let projectText = Regex.Replace(projectText, @"<!--[\s\S]*?-->", "")

    // get project type
    let m = Regex.Match(projectText, @"<OutputType[^>]*>([^<]*)<\/OutputType[^>]*>")
    let target = if m.Success then m.Groups.[1].Value.ToLowerInvariant() else "library"

    // get conditional defines
    let defines =
        Regex.Matches(projectText,  @"<DefineConstants[^>]*>([^<]*)<\/DefineConstants[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.append ["FABLE_COMPILER"]
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(DefineConstants)"; ""]
        |> Seq.toArray

    // get nowarns
    let nowarns =
        Regex.Matches(projectText, @"<NoWarn[^>]*>([^<]*)<\/NoWarn[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(NoWarn)"; ""]
        |> Seq.toArray

    // get other flags
    let otherFlags =
        Regex.Matches(projectText, @"<OtherFlags[^>]*>([^<]*)<\/OtherFlags[^>]*>")
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(' '))
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(OtherFlags)"; ""]
        |> Seq.toArray

    // get project references
    let projectRefs =
        Regex.Matches(projectText, @"<ProjectReference\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)")
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
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
    let otherOptions = makeOtherOptions (target, defines, nowarns, otherFlags)
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

let printErrors showWarnings (errors: Fable.Standalone.Error[]) =
    let printError (e: Fable.Standalone.Error) =
        let errorType = (if e.IsWarning then "Warning" else "Error")
        printfn "%s (%d,%d--%d,%d): %s: %s" e.FileName e.EndLineAlternate
            e.StartColumn e.EndLineAlternate e.EndColumn errorType e.Message
    let warnings, errors = errors |> Array.partition (fun e -> e.IsWarning)
    let hasErrors = not (Array.isEmpty errors)
    if showWarnings then
        warnings |> Array.iter printError
    if hasErrors then
        errors |> Array.iter printError
        failwith "Too many errors."

type CmdLineOptions = {
    commonjs: bool
    optimize: bool
    watchMode: bool
}

let parseFiles projectPath outDir options =
    // parse project
    let (projectFileName, dllRefs, fileNames, sources, otherOptions) = parseProject projectPath

    // dedup file names
    let fileNames = dedupFileNames fileNames

    // find reference dlls
    let dllRefMap = dllRefs |> Seq.map (fun x -> Path.GetFileName x, x) |> Map
    let references = dllRefs |> Array.map Path.GetFileNameWithoutExtension |> Array.append references
    let findDllPath dllName = Map.tryFind dllName dllRefMap |> Option.defaultValue (metadataPath + dllName)
    let readAllBytes dllName = findDllPath dllName |> readAllBytes

    // create checker
    let fable = Fable.Standalone.Main.init ()
    let otherOptions = Array.append otherOptions [| if options.optimize then yield "--optimize+" |]
    let createChecker () = fable.CreateChecker(references, readAllBytes, otherOptions)
    let ms0, checker = measureTime createChecker ()
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () = fable.ParseFSharpProject(checker, projectFileName, fileNames, sources)
    let ms1, parseRes = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"
    let showWarnings = false // turning off warnings for cleaner output
    parseRes.Errors |> printErrors showWarnings

    // clear cache to lower memory usage
    if not options.watchMode then
        fable.ClearParseCaches(checker)

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to Babel)
    let fableLibraryDir = "fable-library"
    let parseFable (res, fileName) = fable.CompileToBabelAst(fableLibraryDir, res, fileName, options.optimize)
    let trimPath (path: string) = path.Replace("../", "").Replace("./", "").Replace(":", "")
    let projDir = normalizeFullPath projectPath |> Path.GetDirectoryName

    for fileName in fileNames do

        // print F# AST
        if false then
            let fsAstStr = fable.FSharpAstToString(parseRes, fileName, options.optimize)
            printfn "%s Typed AST: %s" fileName fsAstStr

        // transform F# AST to Babel AST
        let ms2, res = measureTime parseFable (parseRes, fileName)
        printfn "File: %s, Fable time: %d ms" fileName ms2
        res.FableErrors |> printErrors showWarnings

        // save Babel AST
        let filePath = getRelativePath projDir fileName |> trimPath
        let jsFileName = Path.ChangeExtension(filePath, ".json")
        let jsFilePath = Path.Combine(outDir, jsFileName)
        let jsFileText = res.BabelAst |> serializeToJson
        // printfn "%s Babel AST: %s" fileName jsFileText
        ensureDirExists(Path.GetDirectoryName jsFilePath)
        writeAllText jsFilePath jsFileText

let parseArguments (argv: string[]) =
    // TODO: more sophisticated argument parsing
    let usage = "Usage: fable <PROJECT_PATH> <OUT_DIR> [--options]"
    let opts, args = argv |> Array.partition (fun s -> s.StartsWith("--"))
    match args with
    | [| projectPath; outDir |] ->
        let options = {
            commonjs = opts |> Array.contains "--commonjs"
            optimize = opts |> Array.contains "--optimize-fcs"
            watchMode = opts |> Array.contains "--watch"
        }
        parseFiles projectPath outDir options
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %A" ex.Message
    0
