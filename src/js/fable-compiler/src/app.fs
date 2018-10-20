module Fable.Compiler.App

open Fable.Compiler.Platform
open System.Text.RegularExpressions

let references = Fable.Repl.Metadata.references false
let metadataPath = __dirname + "/metadata2/" // .NET BCL binaries (metadata)

let parseProject projectPath =
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

    // replace some variables
    let projectText = projectText.Replace(@"$(MSBuildProjectDirectory)", __dirname)

    // get files list
    let fileNamesRegex = @"<Compile\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)"
    let fileNames =
        Regex.Matches(projectText, fileNamesRegex)
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim().Replace("\\", "/"))
        |> Seq.toArray

    (projectFileName, fileNames, defines)

let parseFiles projectPath outDir optimized =
    let (projectFileName, fileNames, defines) = parseProject projectPath

    // get file sources
    let projectFileDir = Path.GetDirectoryName projectPath
    let makePath fileName = Path.Combine(projectFileDir, fileName)
    let sources = fileNames |> Array.map (fun fileName -> fileName |> makePath |> readAllText)

    // create checker
    let fable = initFable ()
    let createChecker () = fable.CreateChecker(references, readAllBytes metadataPath, Some defines)
    let ms0, checker = measureTime createChecker ()
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // Parse F# files to AST
    let parseFSharp () = fable.ParseFSharpProjectSimple(checker, projectFileName, fileNames, sources)
    let ms1, parseRes = measureTime parseFSharp ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"

    // Check for errors
    let printError (e: Fable.Repl.Error) =
        printfn "%s: %s (%d,%d--%d,%d): %s"
            (if e.IsWarning then "Warning" else "Error")
            e.FileName e.EndLineAlternate e.StartColumn
            e.EndLineAlternate e.EndColumn e.Message
    let hasErrors (errors: Fable.Repl.Error[]) =
        errors |> Array.exists (fun e -> not e.IsWarning)
    parseRes.Errors |> Array.iter printError
    if parseRes.Errors |> hasErrors then failwith "Too many errors."

    // Exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to Babel)
    let fableCoreDir = "fable-core"
    copyFolder (__dirname + "/bundle/fable-core", Path.Combine(outDir, fableCoreDir))
    let parseFable (fileName, ast) = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
    let fsAst = parseRes.ProjectResults
    for fileName in fileNames do

        // transform F# AST to Babel AST
        let ms2, (babelAst, errors) = measureTime parseFable (fileName, fsAst)
        printfn "File: %s, Fable time: %d ms" fileName ms2
        errors |> Array.iter printError
        if errors |> hasErrors then failwith "Too many errors."

        // transform and save Babel AST
        transformAndSaveBabelAst(babelAst, fileName, outDir)

let parseArguments (argv: string[]) =
    // TODO: more sophisticated argument parsing
    let usage = "Usage: fable projectPath outDir [--options]"
    let opts, args = argv |> Array.partition (fun s -> s.StartsWith("--"))
    match opts, args with
    | [| "--help" |], _ -> printfn "%s" usage
    | [| "--version" |], _ -> printfn "v%s" (getVersion())
    | _, [| projectPath; outDir |] ->
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
