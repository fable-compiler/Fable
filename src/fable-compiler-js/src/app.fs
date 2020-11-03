module Fable.Compiler.App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

#if LOCAL_TEST
let [<Global>] __dirname = "__dirname"
let getMetadataDir(): string = __dirname + "/../../fable-metadata/lib/"
let getFableLibDir(): string = __dirname + "/../../../build/fable-library/"
let getVersion(): string = ".next"
let initFable (): Fable.Standalone.IFableManager = import "init" "../../fable-standalone/src/Main.fs.js"
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
        let relPath = if isRelativePath relPath then relPath else "./" + relPath
        relPath

    let getImportPath sourcePath targetPath projDir outDir (importPath: string) =
        match outDir with
        | None -> importPath.Replace("${outDir}", ".")
        | Some outDir ->
            let importPath =
                if importPath.StartsWith("${outDir}")
                then Path.Combine(outDir, importPath.Replace("${outDir}", ""))
                else importPath
            let sourceDir = Path.GetDirectoryName(sourcePath)
            let targetDir = Path.GetDirectoryName(targetPath)
            let importPath =
                if isRelativePath importPath
                then Path.Combine(sourceDir, importPath) |> normalizeFullPath
                else importPath
            if isAbsolutePath importPath then
                if importPath.EndsWith(".fs")
                then getTargetRelPath importPath targetDir projDir outDir
                else getRelativePath targetDir importPath
            else importPath

type SourceWriter(sourcePath, targetPath, projDir, outDir, fileExt: string) =
    // In imports *.ts extensions have to be converted to *.js extensions instead
    let fileExt = if fileExt.EndsWith(".ts") then Path.ChangeExtension(fileExt, ".js") else fileExt
    let sb = System.Text.StringBuilder()
    interface Fable.Standalone.IWriter with
        member _.Write(str) = async { return sb.Append(str) |> ignore }
        member _.EscapeJsStringLiteral(str) = javaScriptStringEncode(str)
        member _.MakeImportPath(path) =
            let path = Imports.getImportPath sourcePath targetPath projDir outDir path
            if path.EndsWith(".fs") then Path.ChangeExtension(path, fileExt) else path
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

let parseFiles projectFileName options =
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
    // if not options.watch then
    fable.ClearParseCaches(checker)

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to JS)
    let projDir = projectFileName |> normalizeFullPath |> Path.GetDirectoryName
    let libDir = getFableLibDir() |> normalizeFullPath
    let parseFable (res, fileName) =
        fable.CompileToBabelAst(libDir, res, fileName,
            typedArrays = options.typedArrays,
            typescript = options.typescript)

    let fileExt =
        match options.outDir with
        | Some _ -> if options.typescript then ".ts" else ".js"
        | None -> if options.typescript then ".fs.ts" else ".fs.js"

    async {
        for fileName in fileNames do

            // print F# AST
            if options.printAst then
                let fsAstStr = fable.FSharpAstToString(parseRes, fileName)
                printfn "%s Typed AST: %s" fileName fsAstStr

            // transform F# AST to Babel AST
            let res, ms2 = measureTime parseFable (parseRes, fileName)
            printfn "File: %s, Fable time: %d ms" fileName ms2
            res.FableErrors |> printErrors showWarnings

            // print Babel AST as JavaScript/TypeScript
            let outPath =
                match options.outDir with
                | None ->
                    Path.ChangeExtension(fileName, fileExt)
                | Some outDir ->
                    let relPath = getRelativePath projDir fileName |> Imports.trimPath
                    let relPath = Path.ChangeExtension(relPath, fileExt)
                    Path.Combine(outDir, relPath)
            let writer = new SourceWriter(fileName, outPath, projDir, options.outDir, fileExt)
            do! fable.PrintBabelAst(res, writer)

            // write the result to file
            ensureDirExists(Path.GetDirectoryName(outPath))
            writeAllText outPath writer.Result
    } |> runAsync

let argValue key (args: string[]) =
    args
    |> Array.pairwise
    |> Array.tryPick (fun (k, v) ->
        if k = key && not (v.StartsWith("-"))
        then Some v else None)

let tryFlag flag (args: string[]) =
    match argValue flag args with
    | Some flag ->
        match System.Boolean.TryParse(flag) with
        | true, flag -> Some flag
        | false, _ -> None
    // Flags can be activated without an explicit value
    | None when Array.contains flag args -> Some true
    | None -> None

let hasFlag flag (args: string[]) =
    tryFlag flag args |> Option.defaultValue false

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
        outDir = opts |> argValue "--outDir" |> Option.orElse outDir
        // fableDir = opts |> argValue "--fableDir"
        benchmark = opts |> hasFlag "--benchmark"
        optimize = opts |> hasFlag "--optimize"
        // sourceMaps = opts |> hasFlag "--sourceMaps"
        typedArrays = opts |> tryFlag "--typedArrays"
                           |> Option.defaultValue (opts |> hasFlag "--typescript" |> not)
        typescript = opts |> hasFlag "--typescript"
        printAst = opts |> hasFlag "--printAst"
        // watch = opts |> hasFlag "--watch"
    }
    parseFiles projectFileName options
    commandToRun |> Option.iter runCmdAndExitIfFails

let parseArguments (argv: string[]) =
    let usage = """Usage: fable <F#_PROJECT_PATH> [OUT_DIR] [--options]

Options:
  --help            Show help
  --version         Print version
  --outDir          Redirect compilation output to a directory
  --optimize        Compile with optimized F# AST (experimental)
  --typescript      Compile to TypeScript (experimental)
  --typedArrays     Compile numeric arrays as JS typed arrays (default is true for JS, false for TS)
  --run             Execute the script after compilation
"""

    let args, opts =
        match argv |> Array.tryFindIndex (fun s -> s.StartsWith("--")) with
        | None -> argv, [||]
        | Some i -> Array.splitAt i argv
    match opts, args with
    | _, _ when argv |> hasFlag "--help" -> printfn "%s" usage
    | _, _ when argv |> hasFlag "--version" -> printfn "v%s" (getVersion())
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
