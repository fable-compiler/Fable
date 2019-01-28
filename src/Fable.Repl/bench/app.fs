module Bench.App

open Bench.Platform

let use_net45_meta = false
let references = Fable.Repl.Metadata.references use_net45_meta
let metadataPath =
    if use_net45_meta
    then "/temp/repl/metadata/"  // dotnet 4.5 binaries
    else "/temp/repl/metadata2/" // dotnet core 2.0 binaries

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
        | [|metadataPath; testScriptPath; compiledScriptPath|] -> metadataPath, testScriptPath, compiledScriptPath
        | _ -> metadataPath, testScriptPath, testScriptPath.Replace(".fsx", ".js")
    try
        let optimize = false
        // let fsAstFile = Fable.Path.ChangeExtension(testScriptPath, ".fsharp.ast.txt")
        // let babelAstFile = Fable.Path.ChangeExtension(testScriptPath, ".babel.ast.json")
        let projectFileName = "project"
        let fileName = testScriptPath
        let source = readAllText testScriptPath
        let fable = Fable.Repl.Main.init ()
        let createChecker () = fable.CreateChecker(references, readAllBytes metadataPath, [||], optimize)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        // let parseFSharpScript () = fable.ParseFSharpScript(checker, fileName, source)
        let parseFSharpScript () = fable.ParseFSharpFileInProject(checker, fileName, projectFileName, [|fileName|], [|source|])
        let parseFable (res, fileName) = fable.CompileToBabelAst(fableLibraryDir, res, fileName, optimize)
        let bench i =
            let ms1, parseRes = measureTime parseFSharpScript ()
            let errors = fable.GetParseErrors parseRes
            errors |> Array.iter (printfn "Error: %A")
            if errors.Length > 0 then failwith "Too many errors."
            let ms2, babelAst = measureTime parseFable (parseRes, fileName)
            // if i = 1 then
            //     // let fsAstStr = fable.FSharpAstToString(parseRes, fileName, optimize)
            //     // printfn "%s Typed AST: %s" fileName fsAstStr
            //     // writeAllText fsAstFile fsAstStr
            //     // printfn "Babel AST: %s" (toJson babelAst)
            //     writeAllText babelAstFile (toJson babelAst)
            //     // writeJs compiledScriptPath babelAst
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
