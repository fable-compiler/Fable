module Fable.Compiler.App

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

let references = Metadata.references_core
let metadataPath = __SOURCE_DIRECTORY__ + "/../../../../../Fable/src/fable-metadata/lib/" // .NET BCL binaries

let printErrors showWarnings (errors: FSharpDiagnostic[]) =
    let isWarning (e: FSharpDiagnostic) =
        e.Severity = FSharpDiagnosticSeverity.Warning
    let printError (e: FSharpDiagnostic) =
        let errorType = (if isWarning e then "Warning" else "Error")
        printfn "%s (%d,%d): %s: %s" e.FileName e.StartLine e.StartColumn errorType e.Message
    let warnings, errors = errors |> Array.partition isWarning
    let hasErrors = not (Array.isEmpty errors)
    if showWarnings then
        warnings |> Array.iter printError
    if hasErrors then
        errors |> Array.iter printError
        failwith "Too many errors."

let parseFiles projectFileName outDir optimize =
    // parse project
    let (dllRefs, fileNames, otherOptions) = parseProject projectFileName
    let sources = fileNames |> Array.map readAllText

    // create checker
    let readAllBytes dllName = readAllBytes (metadataPath + dllName)
    let optimizeFlag = "--optimize" + (if optimize then "+" else "-")
    let otherOptions = otherOptions |> Array.append [| optimizeFlag |]
    let createChecker () = InteractiveChecker.Create(references, readAllBytes, otherOptions)
    let ms0, checker = measureTime createChecker ()
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () = checker.ParseAndCheckProject(projectFileName, fileNames, sources)
    let ms1, projectResults = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"
    let showWarnings = false // supress warnings for clarity
    projectResults.Diagnostics  |> printErrors showWarnings

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
    let implFiles = if optimize
                    then projectResults.GetOptimizedAssemblyContents().ImplementationFiles
                    else projectResults.AssemblyContents.ImplementationFiles

    let fileCount = Seq.length implFiles
    printfn "Typechecked %d files" fileCount
    // // for each file
    // for implFile in implFiles do
    //     printfn "%s" implFile.FileName

    //     // printfn "--------------------------------------------"
    //     // let fsAst = implFile.Declarations |> AstPrint.printFSharpDecls "" |> String.concat "\n"
    //     // printfn "%s" fsAst

let parseArguments (argv: string[]) =
    let usage = "Usage: bench <PROJECT_PATH> [--options]"
    let opts, args = argv |> Array.partition (fun s -> s.StartsWith("--"))
    match args with
    | [| projectFileName |] ->
        let outDir = "./out-test"
        let optimize = opts |> Array.contains "--optimize"
        parseFiles projectFileName outDir optimize
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %A" ex.Message
    0
