module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fabel

let parseFSharpScript (projFile: string) (projCode: string option) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let projOptions =
        let projCode = match projCode with Some x -> x | None -> File.ReadAllText projFile
        checker.GetProjectOptionsFromScript(projFile, projCode, otherFlags=[|"--define:DEBUG"|])
        |> Async.RunSynchronously
    checker.ParseAndCheckProject({ projOptions with UseScriptResolutionRules = false })
    |> Async.RunSynchronously
    // TODO: Check errors

[<EntryPoint>]
let main argv =
    let projFile, projCode =
        match argv.[0] with
        | "--file" -> argv.[1], None
        | code ->
            let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
            File.WriteAllText(file, code)
            file, Some code
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
    parseFSharpScript projFile projCode
    |> FSharp2Fabel.transformFiles com
    |> Fabel2Babel.transformFiles com
    |> List.iter (fun babelAst ->
        JsonConvert.SerializeObject (
            babelAst, Json.ErasedUnionConverter())
        |> printfn "%s")
    0 // return an integer exit code
