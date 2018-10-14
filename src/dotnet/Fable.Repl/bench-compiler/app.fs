module Bench.App

open Bench.Platform
//open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    try
        let references = Fable.Repl.Metadata.references false
        let metadataPath = Fable.Path.Combine(__SOURCE_DIRECTORY__, "../metadata2/") // .NET BCL binaries

        // let testProj = "../../../../../FSharp.Compiler.Service_fable/fcs/fcs-fable/fcs-fable.fsproj"
        let testProj = "../Fable.Core.fsproj"
        let defines = [|
            "FABLE_COMPILER"
            // "FX_NO_CORHOST_SIGNER"
            // "FX_NO_LINKEDRESOURCES"
            // "FX_NO_PDB_READER"
            // "FX_NO_PDB_WRITER"
            // "FX_NO_WEAKTABLE"
            // "FX_REDUCED_EXCEPTIONS"
            // "NO_COMPILER_BACKEND"
            // "NO_EXTENSIONTYPING"
            // "NO_INLINE_IL_PARSER"
        |]

        let projectPath = Fable.Path.Combine(__SOURCE_DIRECTORY__, testProj)
        let outDir = Fable.Path.Combine(__SOURCE_DIRECTORY__, "./out")

        // TODO: read arguments from fable.config.js
        let projectPath, outDir =
            match argv with
            | [| projectPath; outDir |] -> projectPath, outDir
            | [| projectPath |] -> projectPath, outDir
            | _ -> projectPath, outDir

        let optimized = false

        let projectFileName = Fable.Path.GetFileName projectPath
        let projectFileDir = Fable.Path.GetDirectoryName projectPath
        let projectText = readAllText projectPath

        let projectText = Regex.Replace(projectText, @"<!--.*?-->", "", RegexOptions.Multiline)
        // let projectText = projectText.Replace(@"$(FSharpSourcesRoot)", "../../src")

        let fileNamesRegex = @"<Compile\s+Include\s*=\s*(""[^""]*|'[^']*)"
        let fileNames =
            Regex.Matches(projectText, fileNamesRegex, RegexOptions.Multiline)
            |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim())
            |> Seq.toArray

        let makePath fileName = Fable.Path.Combine(projectFileDir, fileName)
        let sources = fileNames |> Array.map (fun fileName -> fileName |> makePath |> readAllText)

        //let defines = [| "DEBUG" |]
        //let checker = InteractiveChecker.Create(references, readAllBytes metadataPath, defines)
        //let results = checker.ParseAndCheckProject(projectFileName, fileNames, sources)

        // create checker
        let fable = Fable.Repl.Main.init ()
        let createChecker () = fable.CreateChecker(references, readAllBytes metadataPath, Some defines)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0

        // Parse F# files to AST
        let parseFSharp () = fable.ParseFSharpProjectFiles(checker, projectFileName, fileNames, sources)
        let parseFable (fileName, ast) = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
        let ms1, parseRes = measureTime parseFSharp ()
        printfn "Project: %s, FCS time: %d ms" projectFileName ms1

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

        // Fable (F# to Babel)
        for fileName in fileNames do
            match parseRes.GetResults(fileName) with
            | Some fsAst ->
                // transform F# AST to Babel AST
                let ms2, (babelAst, errors) = measureTime parseFable (fileName, fsAst)
                printfn "File: %s, Fable time: %d ms" fileName ms2
                errors |> Array.iter printError
                if errors |> hasErrors then failwith "Too many errors."

                //let fsAstStr = fable.FSharpAstToString(fsAst, optimized)
                //printfn "%s Typed AST: %s" fileName fsAstStr
                //printfn "%s Babel AST: %s" fileName (toJson babelAst)

                // let jsFileName = Fable.Path.ChangeExtension(fileName, ".json")
                // let jsFilePath = Fable.Path.Combine(outDir, jsFileName)
                // let jsFileText = toJson babelAst
                // ensureDirExists(Fable.Path.GetDirectoryName(jsFilePath))
                // writeAllText jsFilePath jsFileText

                // // transform Babel AST to js
                // let jsFileName = Fable.Path.ChangeExtension(fileName, ".js")
                // let jsFilePath = Fable.Path.Combine(outDir, jsFileName)
                // ensureDirExists(Fable.Path.GetDirectoryName(jsFilePath))
                // writeJs jsFilePath babelAst

            | None -> ()
     with ex ->
        printfn "Error: %A" ex.Message
    0
