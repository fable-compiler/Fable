module App

open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices
open Platform

let references = Metadata.references_core
let metadataPath = "/temp/repl/metadata2/" // .NET BCL binaries

let parseProjectFile projectPath =
    let projectFileName = Path.GetFileName projectPath
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

    let projectFileDir = Path.GetDirectoryName projectPath
    let isAbsolutePath (path: string) = path.StartsWith("/") || path.IndexOf(":") = 1
    let trimPath (path: string) = path.TrimStart([|'.';'/'|]).Replace(":", "")
    let makePath path = if isAbsolutePath path then path else Path.Combine(projectFileDir, path)
    let makeName path = Path.Combine(trimPath projectFileDir, trimPath path)

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

let printErrors showWarnings (errors: FSharpErrorInfo[]) =
    let isWarning (e: FSharpErrorInfo) =
        e.Severity = FSharpErrorSeverity.Warning
    let printError (e: FSharpErrorInfo) =
        let errorType = (if isWarning e then "Warning" else "Error")
        printfn "%s (%d,%d--%d,%d): %s: %s" e.FileName e.EndLineAlternate
            e.StartColumn e.EndLineAlternate e.EndColumn errorType e.Message
    let warnings, errors = errors |> Array.partition isWarning
    let hasErrors = not (Array.isEmpty errors)
    if showWarnings then
        warnings |> Array.iter printError
    if hasErrors then
        errors |> Array.iter printError
        failwith "Too many errors."

let parseFiles projectPath outDir optimized =
    // parse project
    let (projectFileName, fileNames, sources, defines) = parseProject projectPath

    // dedup file names
    let fileNames = dedupFileNames fileNames

    // create checker
    let createChecker () = InteractiveChecker.Create(references, readAllBytes metadataPath, defines, optimize=false)
    let ms0, checker = measureTime createChecker ()
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    let ms1, projectResults = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"
    let showWarnings = false // supress warnings for clarity
    projectResults.Errors |> printErrors showWarnings

    // // modify last file
    // sources.[sources.Length - 1] <- sources.[sources.Length - 1] + "\n"
    // let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    // let ms1, projectResults = measureTime parseFSharpProject ()
    // printfn "Project: %s, FCS time: %d ms (modified last file)" projectFileName ms1

    // // modify middle file
    // sources.[sources.Length / 2] <- sources.[sources.Length / 2] + "\n"
    // let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    // let ms1, projectResults = measureTime parseFSharpProject ()
    // printfn "Project: %s, FCS time: %d ms (modified middle file)" projectFileName ms1

    // // modify first file
    // sources.[0] <- sources.[0] + "\n"
    // let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    // let ms1, projectResults = measureTime parseFSharpProject ()
    // printfn "Project: %s, FCS time: %d ms (modified first file)" projectFileName ms1

    // // clear cache
    // checker.ClearCache()

    // // after clear cache
    // sources.[0] <- sources.[0] + "\n"
    // let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    // let ms1, projectResults = measureTime parseFSharpProject ()
    // printfn "Project: %s, FCS time: %d ms (after clear cache)" projectFileName ms1

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // this is memory intensive, only do it once
    let implFiles = if optimized
                    then projectResults.GetOptimizedAssemblyContents().ImplementationFiles
                    else projectResults.AssemblyContents.ImplementationFiles

    // for each file
    for implFile in implFiles do
        printfn "%s" implFile.FileName

        // printfn "--------------------------------------------"
        // let fsAst = implFile.Declarations |> AstPrint.printFSharpDecls "" |> String.concat "\n"
        // printfn "%s" fsAst

let parseArguments (argv: string[]) =
    let usage = "Usage: bench <PROJECT_PATH> [--options]"
    let opts, args = argv |> Array.partition (fun s -> s.StartsWith("--"))
    match args with
    | [| projectPath |] ->
        let outDir = "./out-test"
        let optimized = opts |> Array.contains "--optimize-fcs"
        parseFiles projectPath outDir optimized
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %A" ex.Message
    0
