module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json

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
        let proj = parseFSharpScript argv.[0]
        for file in proj.AssemblyContents.ImplementationFiles do
            let babelAst =
                Transform.FSharp2Fabel.transformFile file
                |> Transform.Fabel2Babel.transformFile
            let json = JsonConvert.SerializeObject (babelAst, Fabel.Util.Json.converters)
            File.WriteAllText("./Program.json", json)
    with e ->
        printfn "ERROR: %s" e.Message
    0 // return an integer exit code
