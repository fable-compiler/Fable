module Fable.Compiler.App

open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

let references = Fable.Standalone.Metadata.references_core
let metadataPath = "../../../fable-metadata/lib/" // .NET BCL binaries

let getVersion() = ".next"
let getFableLibDir() = "../../../../build/fable-library" // TODO: get from options

type SourceWriter() =
    let sb = System.Text.StringBuilder()
    interface Fable.Standalone.IWriter with
        member _.Write(str) = async { return sb.Append(str) |> ignore }
        member _.EscapeJsStringLiteral(str) = javaScriptStringEncode(str)
        member _.Dispose() = ()
    member __.Result = sb.ToString()

let printErrors showWarnings (errors: Fable.Standalone.Error[]) =
    let printError (e: Fable.Standalone.Error) =
        let errorType = (if e.IsWarning then "Warning" else "Error")
        printfn "%s (%d,%d): %s: %s" e.FileName e.StartLineAlternate e.StartColumn errorType e.Message
    let warnings, errors = errors |> Array.partition (fun e -> e.IsWarning)
    let hasErrors = not (Array.isEmpty errors)
    if showWarnings then
        warnings |> Array.iter printError
    if hasErrors then
        errors |> Array.iter printError
        failwith "Too many errors."

let runAsync computation =
    async {
        try
            do! computation
        with e ->
            printfn "[ERROR] %s" e.Message
            printfn "%s" e.StackTrace
    } |> Async.StartImmediate

let parseFiles projectFileName outDir options =
    // parse project
    let (dllRefs, fileNames, otherOptions) = parseProject projectFileName
    let sources = fileNames |> Array.map readAllText
    let nugetPath = Path.Combine(getHomePath().Replace('\\', '/'), ".nuget")
    let fileNames = fileNames |> Array.map (fun x -> x.Replace(nugetPath, ""))

    // find referenced dlls
    let dllRefMap = dllRefs |> Array.rev |> Array.map (fun x -> Path.GetFileName x, x) |> Map
    let references = Map.toArray dllRefMap |> Array.map fst |> Array.append references
    let findDllPath dllName = Map.tryFind dllName dllRefMap |> Option.defaultValue (metadataPath + dllName)
    let readAllBytes dllName = findDllPath dllName |> readAllBytes

    // create checker
    let fable = Fable.Standalone.Main.init ()
    let optimizeFlag = "--optimize" + (if options.optimize then "+" else "-")
    let otherOptions = otherOptions |> Array.append [| optimizeFlag |]
    let createChecker () = fable.CreateChecker(references, readAllBytes, otherOptions)
    let checker, ms0 = measureTime createChecker ()
    printfn "fable-compiler-js v%s" (getVersion())
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () = fable.ParseFSharpProject(checker, projectFileName, fileNames, sources)
    let parseRes, ms1 = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"
    let showWarnings = not options.benchmark
    parseRes.Errors |> printErrors showWarnings

    // early stop for benchmarking
    if options.benchmark then ()
    else

    // clear cache to lower memory usage
    if not options.watchMode then
        fable.ClearParseCaches(checker)

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to JS)
    // let fableLibraryDir = getFableLibDir() |> normalizeFullPath
    let fableLibraryDir =
        let projDir = projectFileName |> normalizeFullPath |> Path.GetDirectoryName
        Path.Combine(projDir, getFableLibDir()) |> normalizeFullPath
    let parseFable (res, fileName) =
        let fableLibraryDir = getRelativePath (Path.GetDirectoryName fileName) fableLibraryDir
        fable.CompileToBabelAst(fableLibraryDir, res, fileName, typescript=options.typescript)

    // let trimPath (path: string) = path.Replace("../", "").Replace("./", "").Replace(":", "")
    // let projDir = projectFileName |> normalizeFullPath |> Path.GetDirectoryName

    async {
        for fileName in fileNames do
            // transform F# AST to Babel AST
            let res, ms2 = measureTime parseFable (parseRes, fileName)
            printfn "File: %s, Fable time: %d ms" fileName ms2
            res.FableErrors |> printErrors showWarnings

            // Print Babel AST (measure time?)
            let fileExt = if options.typescript then ".ts" else ".js"
            // let filePath = getRelativePath projDir fileName |> trimPath |> Path.GetFileNameWithoutExtension
            // let jsFilePath = Path.Combine(outDir, fileName + fileExt)
            let jsFilePath = fileName + fileExt
            let writer = new SourceWriter()
            do! fable.PrintBabelAst(res, writer)
            writeAllText jsFilePath writer.Result
    } |> runAsync

let run opts projectFileName outDir =
    let commandToRun =
        opts |> Array.tryFindIndex ((=) "--run")
        |> Option.map (fun i ->
            // TODO: This only works if the project is an .fsx file
            let scriptFile = Path.Combine(outDir, Path.GetFileNameWithoutExtension(projectFileName) + ".js")
            let runArgs = opts.[i+1..] |> String.concat " "
            sprintf "node %s %s" scriptFile runArgs)
    let options = {
        benchmark = opts |> Array.contains "--benchmark"
        optimize = opts |> Array.contains "--optimize-fcs"
        sourceMaps = opts |> Array.contains "--sourceMaps"
        typescript = opts |> Array.contains "--typescript"
        watchMode = opts |> Array.contains "--watch"
    }
    parseFiles projectFileName outDir options
    // commandToRun |> Option.iter runCmdAndExitIfFails

let parseArguments (argv: string[]) =
    // TODO: more sophisticated argument parsing
    let usage = "Usage: fable <PROJECT_PATH> <OUT_DIR> [--options]"
    let args, opts =
        match argv |> Array.tryFindIndex (fun s -> s.StartsWith("--")) with
        | None -> argv, [||]
        | Some i -> argv.[..i-1], argv.[i..]
    match opts, args with
    | [| "--help" |], _ -> printfn "%s" usage
    | [| "--version" |], _ -> printfn "v%s" (getVersion())
    | _, [| projectFileName |] ->
        let outDir = Path.Combine(Path.GetDirectoryName(projectFileName), "bin")
        run opts projectFileName outDir
    | _, [| projectFileName; outDir |] ->
        run opts projectFileName outDir
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %s\n%s" ex.Message ex.StackTrace
    0
