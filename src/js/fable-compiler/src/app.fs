module Fable.Compiler.App

open Fable.Compiler.Platform
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    try
        let references = Fable.Repl.Metadata.references false
        let metadataPath = __dirname + "/metadata2/" // .NET BCL binaries (metadata)
        let fableCoreDir = __dirname + "/fable-core"

        let defines = [| "FABLE_COMPILER" |]

        // TODO: read arguments from fable.config.js
        let projectPath, outDir =
            match argv with
            | [| projectPath; outDir |] -> projectPath, outDir
            | _ -> failwith "Usage: fable-compiler projectPath outDir"

        let optimized = false

        let projectFileName = Path.GetFileName projectPath
        let projectFileDir = Path.GetDirectoryName projectPath
        let projectText = readAllText projectPath

        // remove comments
        let projectText = Regex.Replace(projectText, @"<!--.*?-->", "", RegexOptions.Multiline)

        // get files list
        let fileNamesRegex = @"<Compile\s+Include\s*=\s*(""[^""]*|'[^']*)"
        let fileNames =
            Regex.Matches(projectText, fileNamesRegex, RegexOptions.Multiline)
            |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim())
            |> Seq.toArray

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
        let parseFable (fileName, ast) = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
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
        let fsAst = parseRes.ProjectResults
        for fileName in fileNames do

            // transform F# AST to Babel AST
            let ms2, (babelAst, errors) = measureTime parseFable (fileName, fsAst)
            printfn "File: %s, Fable time: %d ms" fileName ms2
            errors |> Array.iter printError
            if errors |> hasErrors then failwith "Too many errors."

            // transform Babel AST to js
            let jsFileName = Path.ChangeExtension(fileName, ".js")
            let jsFilePath = Path.Combine(outDir, jsFileName)
            ensureDirExists(Path.GetDirectoryName(jsFilePath))
            writeJs jsFilePath babelAst

     with ex ->
        printfn "Error: %A" ex.Message
    0
