module Fable.Compiler.App

open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

let references = Fable.Standalone.Metadata.references_core
let metadataPath = Path.Combine(__SOURCE_DIRECTORY__, "../../../fable-metadata/lib/") // .NET BCL binaries

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
    let projSet = makeHashSetIgnoreCase ()
    let (projectFileName, dllRefs, fileNames, sources, otherOptions) = parseProject projSet projectPath

    // dedup file names
    let fileSet = makeHashSetIgnoreCase ()
    let fileNames = dedupFileNames fileSet fileNames

    // find reference dlls
    let dllRefMap = dllRefs |> Seq.map (fun x -> Path.GetFileName x, x) |> Map
    let references = dllRefs |> Array.map Path.GetFileNameWithoutExtension |> Array.append references
    let findDllPath dllName = Map.tryFind dllName dllRefMap |> Option.defaultValue (metadataPath + dllName)
    let readAllBytes dllName = findDllPath dllName |> readAllBytes

    // create checker
    let fable = Fable.Standalone.Main.init ()
    let optimizeFlag = "--optimize" + (if options.optimize then "+" else "-")
    let otherOptions = otherOptions |> Array.append [| optimizeFlag |]
    let createChecker () = fable.CreateChecker(references, readAllBytes, otherOptions)
    let ms0, checker = measureTime createChecker ()
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () = fable.ParseFSharpProject(checker, projectFileName, fileNames, sources)
    let ms1, parseRes = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"
    let showWarnings = true // turning off warnings for cleaner output
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
    let projDir = projectPath |> normalizeFullPath |> Path.GetDirectoryName

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
