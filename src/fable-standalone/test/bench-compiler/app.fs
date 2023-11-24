module Fable.Compiler.App

open Fable.Compiler.Platform
open Fable.Compiler.ProjectParser

let getMetadataDir () : string =
    __SOURCE_DIRECTORY__ + "/../../../fable-metadata/lib/"

let getFableLibDir () : string =
    __SOURCE_DIRECTORY__ + "/../../../../temp/fable-library"

let getVersion () : string = ".next"

let initFable () : Fable.Standalone.IFableManager =
    Fable.Standalone.Main.init ()

let references = Fable.Metadata.coreAssemblies
let metadataPath = getMetadataDir().TrimEnd('\\', '/') + "/" // .NET BCL binaries (metadata)

module Imports =
    let trimPath (path: string) =
        path.Replace("../", "").Replace("./", "").Replace(":", "")

    let isRelativePath (path: string) =
        path.StartsWith("./") || path.StartsWith("../")

    let isAbsolutePath (path: string) =
        path.StartsWith('/') || path.IndexOf(':') = 1

    let preventConflicts conflicts originalName =
        let rec check originalName n =
            let name =
                if n > 0 then
                    originalName + "_" + (string n)
                else
                    originalName

            if not (conflicts name) then
                name
            else
                check originalName (n + 1)

        check originalName 0

    let getTargetAbsolutePath
        getOrAddDeduplicateTargetDir
        importPath
        projDir
        outDir
        =
        let importPath = normalizePath importPath
        let outDir = normalizePath outDir
        // It may happen the importPath is already in outDir,
        // for example package sources in fable_modules folder
        if importPath.StartsWith(outDir + "/") then
            importPath
        // if importPath.StartsWith(outDir + "/", StringComparison.OrdinalIgnoreCase) then importPath
        else
            let importDir = Path.GetDirectoryName(importPath)

            let targetDir =
                getOrAddDeduplicateTargetDir
                    importDir
                    (fun (currentTargetDirs: Set<string>) ->
                        let relDir =
                            getRelativePath projDir importDir |> trimPath

                        Path.Combine(outDir, relDir)
                        |> preventConflicts currentTargetDirs.Contains
                    )

            let importFile = Path.GetFileName(importPath)
            Path.Combine(targetDir, importFile)

    let getTargetRelativePath
        getOrAddDeduplicateTargetDir
        (importPath: string)
        targetDir
        projDir
        (outDir: string)
        =
        let absPath =
            getTargetAbsolutePath
                getOrAddDeduplicateTargetDir
                importPath
                projDir
                outDir

        let relPath = getRelativePath targetDir absPath

        if isRelativePath relPath then
            relPath
        else
            "./" + relPath

    let getImportPath
        getOrAddDeduplicateTargetDir
        sourcePath
        targetPath
        projDir
        outDir
        (importPath: string)
        =
        match outDir with
        | None -> importPath.Replace("${outDir}", ".")
        | Some outDir ->
            let importPath =
                if
                    importPath.StartsWith("${outDir}")
                // NOTE: Path.Combine in Fable Prelude trims / at the start
                // of the 2nd argument, unlike .NET IO.Path.Combine
                then
                    Path.Combine(outDir, importPath.Replace("${outDir}", ""))
                    |> normalizeFullPath
                else
                    importPath

            let sourceDir = Path.GetDirectoryName(sourcePath)
            let targetDir = Path.GetDirectoryName(targetPath)

            let importPath =
                if isRelativePath importPath then
                    Path.Combine(sourceDir, importPath) |> normalizeFullPath
                else
                    importPath

            if isAbsolutePath importPath then
                if importPath.EndsWith(".fs") then
                    getTargetRelativePath
                        getOrAddDeduplicateTargetDir
                        importPath
                        targetDir
                        projDir
                        outDir
                else
                    getRelativePath targetDir importPath
            else
                importPath

type SourceWriter
    (
        sourcePath,
        targetPath,
        projDir,
        options: CmdLineOptions,
        fileExt: string,
        dedupTargetDir
    )
    =
    // In imports *.ts extensions have to be converted to *.js extensions instead
    let fileExt =
        if fileExt.EndsWith(".ts") then
            Path.ChangeExtension(fileExt, ".js")
        else
            fileExt

    let sb = System.Text.StringBuilder()
    let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator())

    interface Fable.Standalone.IWriter with
        member _.Write(str) =
            async { return sb.Append(str) |> ignore }

        member _.MakeImportPath(path) =
            let path =
                Imports.getImportPath
                    dedupTargetDir
                    sourcePath
                    targetPath
                    projDir
                    options.outDir
                    path

            if path.EndsWith(".fs") then
                Path.ChangeExtension(path, fileExt)
            else
                path

        member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
            if options.sourceMaps then
                let generated: SourceMapSharp.Util.MappingIndex =
                    {
                        line = genLine
                        column = genCol
                    }

                let original: SourceMapSharp.Util.MappingIndex =
                    {
                        line = srcLine
                        column = srcCol
                    }

                mapGenerator
                    .Force()
                    .AddMapping(
                        generated,
                        original,
                        source = sourcePath,
                        ?name = name
                    )

        member _.Dispose() = ()

    member _.SourceMap = mapGenerator.Force().toJSON ()
    member _.Result = sb.ToString()

let printErrors showWarnings (errors: Fable.Standalone.Error[]) =
    let printError (e: Fable.Standalone.Error) =
        let errorType =
            (if e.IsWarning then
                 "Warning"
             else
                 "Error")

        printfn
            "%s"
            $"{e.FileName} ({e.StartLine},{e.StartColumn}): {errorType}: {e.Message}"

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
    }
    |> Async.StartImmediate

let parseFiles projectFileName options =
    // parse project
    let (dllRefs, fileNames, otherOptions) = parseProject projectFileName
    let sources = fileNames |> Array.map readAllText
    let nugetPath = Path.Combine(getHomePath().Replace('\\', '/'), ".nuget")
    let fileNames = fileNames |> Array.map (fun x -> x.Replace(nugetPath, ""))

    // find referenced dlls
    let dllRefMap =
        dllRefs
        |> Array.rev
        |> Array.map (fun x -> Path.GetFileName x, x)
        |> Map

    let references =
        Map.toArray dllRefMap |> Array.map fst |> Array.append references

    let findDllPath dllName =
        Map.tryFind dllName dllRefMap
        |> Option.defaultValue (metadataPath + dllName)

    let readAllBytes dllName = findDllPath dllName |> readAllBytes

    // create checker
    let fable = initFable ()

    let optimizeFlag =
        "--optimize"
        + (if options.optimize then
               "+"
           else
               "-")

    let otherOptions = otherOptions |> Array.append [| optimizeFlag |]

    let createChecker () =
        fable.CreateChecker(references, readAllBytes, otherOptions)

    let checker, ms0 = measureTime createChecker ()
    printfn "fable-compiler-js v%s" (getVersion ())
    printfn "--------------------------------------------"
    printfn "InteractiveChecker created in %d ms" ms0

    // parse F# files to AST
    let parseFSharpProject () =
        fable.ParseAndCheckProject(checker, projectFileName, fileNames, sources)

    let parseRes, ms1 = measureTime parseFSharpProject ()
    printfn "Project: %s, FCS time: %d ms" projectFileName ms1
    printfn "--------------------------------------------"

    // print warnings and errors
    let showWarnings = not options.benchmark
    parseRes.Errors |> printErrors showWarnings

    // early stop for benchmarking
    if options.benchmark then
        ()
    else

        // clear cache to lower memory usage
        // if not options.watch then
        fable.ClearCache(checker)

        // exclude signature files
        let fileNames =
            fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

        // Fable (F# to JS)
        let projDir =
            projectFileName |> normalizeFullPath |> Path.GetDirectoryName

        let libDir =
            options.libDir
            |> Option.defaultValue (getFableLibDir ())
            |> normalizeFullPath

        let parseFable (res, fileName) =
            fable.CompileToTargetAst(
                libDir,
                res,
                fileName,
                options.typedArrays,
                options.language
            )

        let fileExt =
            match options.language.ToLowerInvariant() with
            | "js"
            | "javascript" -> ".js"
            | "ts"
            | "typescript" -> ".ts"
            | "py"
            | "python" -> ".py"
            | "php" -> ".php"
            | "dart" -> ".dart"
            | "rust" -> ".rs"
            | _ -> failwithf "Unsupported language: %s" options.language

        let fileExt =
            if Option.isNone options.outDir then
                ".fs" + fileExt
            else
                fileExt

        let getOrAddDeduplicateTargetDir =
            let dedupDic = System.Collections.Generic.Dictionary()

            fun importDir addTargetDir ->
                // Lower importDir as some OS use case insensitive paths
                let importDir = (normalizeFullPath importDir).ToLower()

                match dedupDic.TryGetValue(importDir) with
                | true, v -> v
                | false, _ ->
                    let v = set dedupDic.Values |> addTargetDir
                    dedupDic.Add(importDir, v)
                    v

        async {
            for fileName in fileNames do

                // transform F# AST to target language AST
                let res, ms2 = measureTime parseFable (parseRes, fileName)
                printfn "File: %s, Fable time: %d ms" fileName ms2
                res.FableErrors |> printErrors showWarnings

                // get output path
                let outPath =
                    match options.outDir with
                    | None -> Path.ChangeExtension(fileName, fileExt)
                    | Some outDir ->
                        let absPath =
                            Imports.getTargetAbsolutePath
                                getOrAddDeduplicateTargetDir
                                fileName
                                projDir
                                outDir

                        Path.ChangeExtension(absPath, fileExt)

                // print F# AST to file
                if options.printAst then
                    let fsAstStr = fable.FSharpAstToString(parseRes, fileName)

                    let astPath =
                        outPath.Substring(0, outPath.LastIndexOf(fileExt))
                        + ".fs.ast"

                    writeAllText astPath fsAstStr

                // print target language AST to writer
                let writer =
                    new SourceWriter(
                        fileName,
                        outPath,
                        projDir,
                        options,
                        fileExt,
                        getOrAddDeduplicateTargetDir
                    )

                do! fable.PrintTargetAst(res, writer)

                // create output folder
                ensureDirExists (Path.GetDirectoryName(outPath))

                // write source map to file
                if options.sourceMaps then
                    let mapPath = outPath + ".map"

                    let sourceMapUrl =
                        "//# sourceMappingURL=" + Path.GetFileName(mapPath)

                    do! (writer :> Fable.Standalone.IWriter).Write(sourceMapUrl)
                    writeAllText mapPath (serializeToJson writer.SourceMap)

                // write the result to file
                writeAllText outPath writer.Result
        }
        |> runAsync

let argValue keys (args: string[]) =
    args
    |> Array.pairwise
    |> Array.tryFindBack (fun (k, v) ->
        not (v.StartsWith("-")) && (List.contains k keys)
    )
    |> Option.map snd

let tryFlag flag (args: string[]) =
    match argValue [ flag ] args with
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
        opts
        |> Array.tryFindIndex ((=) "--run")
        |> Option.map (fun i ->
            // TODO: This only works if the project is an .fsx file
            let outDir = Option.defaultValue "." outDir

            let scriptFile =
                Path.Combine(
                    outDir,
                    Path.GetFileNameWithoutExtension(projectFileName) + ".js"
                )

            let runArgs = opts[i + 1 ..] |> String.concat " "
            sprintf "node %s %s" scriptFile runArgs
        )

    let options =
        {
            outDir =
                opts
                |> argValue
                    [
                        "--outDir"
                        "-o"
                    ]
                |> Option.orElse outDir
            libDir = opts |> argValue [ "--fableLib" ]
            benchmark = opts |> hasFlag "--benchmark"
            optimize = opts |> hasFlag "--optimize"
            sourceMaps =
                (opts |> hasFlag "--sourceMaps") || (opts |> hasFlag "-s")
            typedArrays = opts |> tryFlag "--typedArrays"
            language =
                opts
                |> argValue
                    [
                        "--language"
                        "--lang"
                    ]
                |> Option.map (fun _ -> "TypeScript")
                |> Option.defaultValue "JavaScript"
            printAst = opts |> hasFlag "--printAst"
        // watch = opts |> hasFlag "--watch"
        }

    parseFiles projectFileName options
// commandToRun |> Option.iter runCmdAndExitIfFails

let parseArguments (argv: string[]) =
    let usage =
        """Usage: fable <F#_PROJECT_PATH> [OUT_DIR] [--options]

Options:
  --help            Show help
  --version         Print version
  -o|--outDir       Redirect compilation output to a directory
  -s|--sourceMaps   Enable source maps
  --fableLib        Specify Fable library path
  --typedArrays     Compile numeric arrays as JS typed arrays (default is true for JS, false for TS)
  --run             Execute the script after compilation

  --optimize        Compile with optimized F# AST (experimental)
"""

    let args, opts =
        match argv |> Array.tryFindIndex (fun s -> s.StartsWith("-")) with
        | None -> argv, [||]
        | Some i -> Array.splitAt i argv

    match opts, args with
    | _, _ when argv |> hasFlag "--help" -> printfn "%s" usage
    | _, _ when argv |> hasFlag "--version" -> printfn "v%s" (getVersion ())
    | _, [| projectFileName |] -> run opts projectFileName None
    | _, [| projectFileName; outDir |] -> run opts projectFileName (Some outDir)
    | _ -> printfn "%s" usage

[<EntryPoint>]
let main argv =
    try
        parseArguments argv
    with ex ->
        printfn "Error: %s\n%s" ex.Message ex.StackTrace

    0
