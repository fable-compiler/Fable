module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fabel

let parseFSharpScript (input: string) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let file =
        if File.Exists input
        then input
        else
            let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
            File.WriteAllText(file, input)
            file
    let projOptions =
        let code = File.ReadAllText file
        checker.GetProjectOptionsFromScript(file, code, otherFlags=[|"--define:DEBUG"|])
        |> Async.RunSynchronously
    checker.ParseAndCheckProject(projOptions)
    |> Async.RunSynchronously
    // TODO: Check errors

[<EntryPoint>]
let main argv =
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
    |> List.iter (fun babelAst ->
        JsonConvert.SerializeObject (
            babelAst, Json.ErasedUnionConverter())
        |> printfn "%s")
    0 // return an integer exit code
