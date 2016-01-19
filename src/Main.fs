module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fabel

let parseFSharpScript (mainFilePath: string) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let projOptions =
        let code = File.ReadAllText mainFilePath
        checker.GetProjectOptionsFromScript(mainFilePath, code, otherFlags=[|"--define:DEBUG"|])
        |> Async.RunSynchronously
    checker.ParseAndCheckProject(projOptions)
    |> Async.RunSynchronously
    // TODO: Check errors

[<EntryPoint>]
let main argv =
    try
        let projFile = argv.[0]
        let opts = {
            sourceRootPath = Path.GetDirectoryName projFile
            targetRootPath = Path.GetDirectoryName projFile
            environment = "browser"
            jsLibFolder = "./lib"
        }   
        let com = {
            new ICompiler with
                member __.Options = opts            
        }
        parseFSharpScript projFile
        |> FSharp2Fabel.transformFiles com
        |> Fabel2Babel.transformFiles com
        |> List.iteri (fun i babelAst ->
            let json = JsonConvert.SerializeObject (
                        babelAst, Json.ErasedUnionConverter())              
            File.WriteAllText(sprintf "%s/File%i.json" opts.targetRootPath i, json)
            printfn "Complete")
    with e ->
        printfn "ERROR: %s" e.Message
    0 // return an integer exit code
