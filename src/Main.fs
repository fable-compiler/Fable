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
        let com: ICompiler = failwith "TODO"
        parseFSharpScript argv.[0]
        |> FSharp2Fabel.transformFiles com
        |> Fabel2Babel.transformFiles com
        |> List.iteri (fun i babelAst -> 
            let json = JsonConvert.SerializeObject (babelAst, Fabel.Util.Json.converters)
            File.WriteAllText(sprintf "./File%i.json" i, json))
    with e ->
        printfn "ERROR: %s" e.Message
    0 // return an integer exit code
