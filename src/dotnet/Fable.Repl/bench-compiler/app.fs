module Bench.App

open Bench.Platform
open System.Text.RegularExpressions

let references = Fable.Repl.Metadata.references false
let metadataPath = Fable.Path.Combine(__SOURCE_DIRECTORY__, "../metadata2/") // .NET BCL binaries

let parseProjectFile projectPath =
    let projectFileName = Fable.Path.GetFileName projectPath
    let projectText = readAllText projectPath

    // remove all comments
    let projectText = Regex.Replace(projectText, @"<!--[\s\S]*?-->", "")

    // get conditional defines
    let definesRegex = @"<DefineConstants[^>]*>([^<]*)<\/DefineConstants[^>]*>"
    let defines =
        Regex.Matches(projectText, definesRegex)
        |> Seq.collect (fun m -> m.Groups.[1].Value.Split(';'))
        |> Seq.append ["FABLE_COMPILER"]
        |> Seq.map (fun s -> s.Trim())
        |> Seq.distinct
        |> Seq.except ["$(DefineConstants)"; ""]
        |> Seq.toArray

    // get project references
    let projectRefsRegex = @"<ProjectReference\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)"
    let projectRefs =
        Regex.Matches(projectText, projectRefsRegex)
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
        |> Seq.toArray

    // replace some variables
    let projectText = projectText.Replace(@"$(MSBuildProjectDirectory)", ".")
    let projectText = projectText.Replace(@"$(FSharpSourcesRoot)", "../../src")

    // get source files
    let sourceFilesRegex = @"<Compile\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)"
    let sourceFiles =
        Regex.Matches(projectText, sourceFilesRegex)
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
        |> Seq.toArray

    (projectFileName, projectRefs, sourceFiles, defines)

let rec parseProject projectPath =
    let (projectFileName, projectRefs, sourceFiles, defines) = parseProjectFile projectPath

    let projectFileDir = Fable.Path.GetDirectoryName projectPath
    let isAbsolutePath (path: string) = path.StartsWith("/") || path.IndexOf(":") = 1
    let trimPath (path: string) = path.TrimStart([|'.';'/'|]).Replace(":", "")
    let makePath path = if isAbsolutePath path then path else Fable.Path.Combine(projectFileDir, path)
    let makeName path = Fable.Path.Combine(trimPath projectFileDir, trimPath path)

    let fileNames = sourceFiles |> Array.map (fun path -> path |> makeName)
    let sources = sourceFiles |> Array.map (fun path -> path |> makePath |> readAllText)

    let parsedProjects = projectRefs |> Array.map makePath |> Array.map parseProject
    let fileNames = fileNames |> Array.append (parsedProjects |> Array.collect (fun (_,x,_,_) -> x))
    let sources   = sources   |> Array.append (parsedProjects |> Array.collect (fun (_,_,x,_) -> x))
    let defines   = defines   |> Array.append (parsedProjects |> Array.collect (fun (_,_,_,x) -> x))

    (projectFileName, fileNames, sources, defines |> Array.distinct)

let dedupFileNames fileNames =
    let nameSet = System.Collections.Generic.HashSet<string>()
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

let printErrors showWarnings (errors: Fable.Repl.Error[]) =
    let printError (e: Fable.Repl.Error) =
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
    let (projectFileName, fileNames, sources, defines) = parseProject projectPath

    // dedup file names
    let fileNames = dedupFileNames fileNames

    // create checker
    let fable = Fable.Repl.Main.init ()
    let createChecker () = fable.CreateChecker(references, readAllBytes metadataPath, defines, options.optimize)
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

    for fileName in fileNames do

        // // print F# AST
        // let fsAstStr = fable.FSharpAstToString(parseRes, fileName, options.optimize)
        // printfn "%s Typed AST: %s" fileName fsAstStr

        // transform F# AST to Babel AST
        let ms2, res = measureTime parseFable (parseRes, fileName)
        printfn "File: %s, Fable time: %d ms" fileName ms2
        res.FableErrors |> printErrors showWarnings

        // printfn "%s Babel AST: %s" fileName (toJson res.BabelAst)
        let jsFileName = Fable.Path.ChangeExtension(fileName, ".json")
        let jsFilePath = Fable.Path.Combine(outDir, jsFileName)
        let jsFileText = toJson res.BabelAst
        ensureDirExists(Fable.Path.GetDirectoryName(jsFilePath))
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
