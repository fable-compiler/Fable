module Fable.Compiler.App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

#if LOCAL_TEST
let [<Global>] __dirname = "__dirname"
let getMetadataDir(): string = __dirname + "/../../../fable-metadata/"
let getFableLibDir(): string = __dirname + "/../../../fable-library/"
let getVersion(): string = ".next"
let initFable (): Fable.Standalone.IFableManager = import "init" "../../../build/fable-standalone/bundle/Main.js"
#else
let getMetadataDir(): string = import "getAssembliesDir" "fable-metadata"
let getFableLibDir(): string = importMember "./util.js"
let getVersion(): string = importMember "./util.js"
let initFable (): Fable.Standalone.IFableManager = import "init" "fable-standalone"
#endif

let references = Fable.Standalone.Metadata.references_core
let metadataPath = getMetadataDir().TrimEnd('\\', '/') + "/" // .NET BCL binaries (metadata)

module Imports =
    let trimPath (path: string) = path.Replace("../", "").Replace("./", "").Replace(":", "")
    let isRelativePath (path: string) = path.StartsWith("./") || path.StartsWith("../")
    let isAbsolutePath (path: string) = path.StartsWith('/') || path.IndexOf(':') = 1

    let getTargetRelPath importPath targetDir projDir outDir =
        let relPath = getRelativePath projDir importPath |> trimPath
        let relPath = getRelativePath targetDir (Path.Combine(outDir, relPath))
        let relPath = if relPath.StartsWith("..") then relPath else "./" + relPath
        let relPath = if relPath.EndsWith(".fs.js") then relPath.Replace(".fs.js", ".js") else relPath
        relPath

    let getImportPath sourcePath targetPath projDir outDir importPath =
        match outDir with
        | None -> importPath
        | Some outDir ->
            let sourceDir = Path.GetDirectoryName(sourcePath)
            let targetDir = Path.GetDirectoryName(targetPath)
            let importPath =
                if isRelativePath importPath
                then Path.Combine(sourceDir, importPath) |> normalizeFullPath
                else importPath
            if isAbsolutePath importPath then
                if importPath.EndsWith(".fs.js")
                then getTargetRelPath importPath targetDir projDir outDir
                else getRelativePath targetDir importPath
            else importPath

type SourceWriter(sourcePath, targetPath, projDir, outDir) =
    let sb = System.Text.StringBuilder()
    interface Fable.Standalone.IWriter with
        member _.Write(str) = async { return sb.Append(str) |> ignore }
        member _.EscapeJsStringLiteral(str) = javaScriptStringEncode(str)
        member _.MakeImportPath(path) = Imports.getImportPath sourcePath targetPath projDir outDir path
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
    let fable = initFable ()
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
    if options.benchmark then () else

    // clear cache to lower memory usage
    if not options.watchMode then
        fable.ClearParseCaches(checker)

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to JS)
    let projDir = projectFileName |> normalizeFullPath |> Path.GetDirectoryName
    let libDir = getFableLibDir() |> normalizeFullPath
    let parseFable (res, fileName) =
        fable.CompileToBabelAst(libDir, res, fileName, typescript=options.typescript)

    async {
        for fileName in fileNames do
            // transform F# AST to Babel AST
            let res, ms2 = measureTime parseFable (parseRes, fileName)
            printfn "File: %s, Fable time: %d ms" fileName ms2
            res.FableErrors |> printErrors showWarnings

            // print Babel AST as JavaScript/TypeScript
            let fileExt = if options.typescript then ".ts" else ".js"
            let outPath =
                match outDir with
                | None ->
                    fileName + fileExt
                | Some outDir ->
                    let relPath = getRelativePath projDir fileName |> Imports.trimPath
                    let relPath = Path.ChangeExtension(relPath, fileExt)
                    Path.Combine(outDir, relPath)
            let writer = new SourceWriter(fileName, outPath, projDir, outDir)
            do! fable.PrintBabelAst(res, writer)

            // write the result to file
            ensureDirExists(Path.GetDirectoryName(outPath))
            writeAllText outPath writer.Result
    } |> runAsync

let run opts projectFileName outDir =
    let commandToRun =
        opts |> Array.tryFindIndex ((=) "--run")
        |> Option.map (fun i ->
            // TODO: This only works if the project is an .fsx file
            let outDir = Option.defaultValue "." outDir
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
    commandToRun |> Option.iter runCmdAndExitIfFails

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
        run opts projectFileName None
    | _, [| projectFileName; outDir |] ->
        run opts projectFileName (Some outDir)
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %s\n%s" ex.Message ex.StackTrace
    0
