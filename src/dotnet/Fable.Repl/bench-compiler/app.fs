module Bench.App

open Bench.Platform
open System.Text.RegularExpressions

let references = Fable.Repl.Metadata.references false
let metadataPath = Fable.Path.Combine(__SOURCE_DIRECTORY__, "../metadata2/") // .NET BCL binaries

let parseProject projectPath outDir =
    let optimized = false

    let projectFileName = Fable.Path.GetFileName projectPath
    let projectFileDir = Fable.Path.GetDirectoryName projectPath
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
    let projectText = projectText.Replace(@"$(MSBuildProjectDirectory)", ".")
    let projectText = projectText.Replace(@"$(FSharpSourcesRoot)", "../../src")

    // get files list
    let fileNamesRegex = @"<Compile\s+[^>]*Include\s*=\s*(""[^""]*|'[^']*)"
    let fileNames =
        Regex.Matches(projectText, fileNamesRegex)
        |> Seq.map (fun m -> m.Groups.[1].Value.TrimStart('"').TrimStart(''').Trim())
        |> Seq.toArray

    let makePath fileName = Fable.Path.Combine(projectFileDir, fileName)
    let sources = fileNames |> Array.map (fun fileName -> fileName |> makePath |> readAllText)

    // create checker
    let fable = Fable.Repl.Main.init ()
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
    if parseRes.Errors |> hasErrors then
        parseRes.Errors |> Array.iter printError
        failwith "Too many errors."

    // exclude signature files
    let fileNames = fileNames |> Array.filter (fun x -> not (x.EndsWith(".fsi")))

    // Fable (F# to Babel)
    let fableCoreDir = Fable.Path.Combine(__SOURCE_DIRECTORY__, "/fable-core")
    let parseFable (fileName, ast) = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
    let fsAst = parseRes.ProjectResults

    for fileName in fileNames do

        // transform F# AST to Babel AST
        let ms2, (babelAst, errors) = measureTime parseFable (fileName, fsAst)
        printfn "File: %s, Fable time: %d ms" fileName ms2
        if errors |> hasErrors then
            errors |> Array.iter printError
            failwith "Too many errors."

        // let fsAstStr = fable.FSharpAstToString(fsAst, fileName, optimized)
        // printfn "%s Typed AST: %s" fileName fsAstStr
        // printfn "%s Babel AST: %s" fileName (toJson babelAst)

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

[<EntryPoint>]
let main argv =
    try
        // TODO: read arguments from fable.config.js
        let testProj = "../../../../../FSharp.Compiler.Service_fable/fcs/fcs-fable/fcs-fable.fsproj"
        let outDir = "./out"

        let projectPath = Fable.Path.Combine(__SOURCE_DIRECTORY__, testProj)
        let outDir = Fable.Path.Combine(__SOURCE_DIRECTORY__, outDir)

        // TODO: read arguments from fable.config.js
        let projectPath, outDir =
            match argv with
            | [| projectPath; outDir |] -> projectPath, outDir
            | [| projectPath |] -> projectPath, outDir
            | _ -> projectPath, outDir

        parseProject projectPath outDir

     with ex ->
        printfn "Error: %A" ex.Message
    0
