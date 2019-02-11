module Bench.App

open Bench.Platform

let references = Fable.Standalone.Metadata.references_core

// TODO: fix
let metadataPath = "/temp/repl/metadata/"

[<EntryPoint>]
let main argv =
    let metadataPath, testScriptPath, compiledScriptPath =
        match argv with
        | [|metadataPath; testScriptPath; compiledScriptPath|] -> metadataPath, testScriptPath, compiledScriptPath
        | _ -> metadataPath, testScriptPath, testScriptPath.Replace(".fsx", ".js")
    try
        let optimized = false // TODO: from compiler option
        // let fsAstFile = Fable.Path.ChangeExtension(testScriptPath, ".fsharp.ast.txt")
        // let babelAstFile = Fable.Path.ChangeExtension(testScriptPath, ".babel.ast.json")
        let source = readAllText testScriptPath
        let fable = initFable ()
        let createChecker () = fable.CreateChecker(references, readAllBytes metadataPath, [||], false)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let parseFSharpScript () = fable.ParseFSharpScript(checker, testScriptPath, source)
        let parseFable (res, fileName) = fable.CompileToBabelAst(fableLibraryDir, res, fileName, optimized)
        let fcsMeasures = ResizeArray()
        let fableMeasures = ResizeArray()
        let bench i =
            let fileName = testScriptPath
            let ms1, parseRes = measureTime parseFSharpScript ()
            let errors = fable.GetParseErrors parseRes
            errors |> Array.iter (printfn "Error: %A")
            if errors.Length > 0 then failwith "Too many errors."
            let ms2, res = measureTime parseFable (parseRes, fileName)
            if i = 1 then
                writeJs compiledScriptPath res.BabelAst
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
            fcsMeasures.Add(ms1)
            fableMeasures.Add(ms2)
        [1..10] |> List.iter bench
        printfn "After 1st iteration: FCS average %.2f, median %d & Fable average %.2f, median %d"
            (fcsMeasures |> Seq.skip 1 |> Seq.averageBy float)
            (fcsMeasures |> Seq.skip 1 |> Seq.sort |> Seq.item 4)
            (fableMeasures |> Seq.skip 1 |> Seq.averageBy float)
            (fableMeasures |> Seq.skip 1 |> Seq.sort |> Seq.item 4)

    with ex ->
        printfn "Error: %A" ex.Message
    0