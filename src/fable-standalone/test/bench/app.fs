module Bench.App

open Bench.Platform

let references = Fable.Metadata.coreAssemblies
let metadataPath = "../../../fable-metadata/lib/" // .NET BCL binaries

// // Note: importing babel-core has 30% performance impact on the bench
// #if DOTNET_FILE_SYSTEM
// let writeJs (filePath:string) (babelAst:obj) =
//     () // Does nothing in .NET for now
// #else
// type private IBabelResult =
//     abstract code: string
// let private transformFromAst (babelAst: obj): IBabelResult =
//     Fable.Core.JsInterop.importMember "babel-core"
// let writeJs (filePath:string) (babelAst:obj) =
//     let res = transformFromAst babelAst
//     writeAllText filePath res.code
// #endif

[<EntryPoint>]
let main argv =
    let testScriptPath = "test_script_50k.fsx"

    let metadataPath, testScriptPath, compiledScriptPath =
        match argv with
        | [| metadataPath; testScriptPath; compiledScriptPath |] ->
            metadataPath, testScriptPath, compiledScriptPath
        | _ ->
            metadataPath, testScriptPath, testScriptPath.Replace(".fsx", ".js")

    try
        let optimize = false
        // let fsAstFile = Fable.Path.ChangeExtension(testScriptPath, ".fsharp.ast.txt")
        // let babelAstFile = Fable.Path.ChangeExtension(testScriptPath, ".babel.ast.json")
        let projectFileName = "project"
        let fileName = testScriptPath
        let source = readAllText testScriptPath
        let fable = Fable.Standalone.Main.init ()
        let readAllBytes dllName = readAllBytes (metadataPath + dllName)

        let optimizeFlag =
            "--optimize"
            + (if optimize then
                   "+"
               else
                   "-")

        let otherOptions = [| optimizeFlag |]

        let createChecker () =
            fable.CreateChecker(references, readAllBytes, otherOptions)

        let checker, ms0 = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        // let parseFSharpScript () = fable.ParseFSharpScript(checker, fileName, source)
        let parseFSharpScript () =
            fable.ParseAndCheckFileInProject(
                checker,
                fileName,
                projectFileName,
                [| fileName |],
                [| source |]
            )

        let fableLibraryDir = "fable-library"

        let parseFable (res, fileName) =
            fable.CompileToBabelAst(fableLibraryDir, res, fileName)

        let bench i =
            let parseRes, ms1 = measureTime parseFSharpScript ()
            let errors = fable.GetErrors parseRes
            errors |> Array.iter (printfn "Error: %A")

            if errors.Length > 0 then
                failwith "Too many errors."

            let babelAst, ms2 = measureTime parseFable (parseRes, fileName)
            // if i = 1 then
            //     // let fsAstStr = fable.FSharpAstToString(parseRes, fileName)
            //     // printfn "%s Typed AST: %s" fileName fsAstStr
            //     // writeAllText fsAstFile fsAstStr
            //     // printfn "Babel AST: %s" (toJson babelAst)
            //     writeAllText babelAstFile (toJson babelAst)
            //     // writeJs compiledScriptPath babelAst
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2

        [ 1..10 ] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message

    0
