module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fabel

let parseFSharpProject (com: ICompiler) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let projOptions =
        let projCode, projFile =
            if com.Options.code <> null
            then com.Options.code, com.Options.projFile
            else File.ReadAllText com.Options.projFile, com.Options.projFile
        match (Path.GetExtension projFile).ToLower() with
        | ".fsx" ->
            let otherFlags =
                com.Options.symbols |> Array.map (sprintf "--define:%s")
            checker.GetProjectOptionsFromScript(projFile, projCode, otherFlags=otherFlags)
            |> Async.RunSynchronously
        | _ ->
            let properties = [("DefineConstants", String.concat ";" com.Options.symbols)]
            ProjectCracker.GetProjectOptionsFromProjectFile(projFile, properties)
    let checkProjectResults =
        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously
    match checkProjectResults.Errors with
    | [||] -> checkProjectResults
    | errors ->
        errors
        |> Seq.map (fun e -> "\t" + e.Message)
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

[<EntryPoint>]
let main argv =
    try
        let opts =
            if argv.[0] = "--projFile" then
                let projDir = Path.GetDirectoryName argv.[1] |> Path.GetFullPath
                let opts =
                    let json = File.ReadAllText(Path.Combine(projDir, "fabelconfig.json"))
                    JsonConvert.DeserializeObject<CompilerOptions>(json)
                Directory.SetCurrentDirectory projDir
                { opts with projFile = Path.GetFileName argv.[1] }
            else
                JsonConvert.DeserializeObject<CompilerOptions>(argv.[0])
            |> function
                | opts when opts.code <> null ->
                    let projFile = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
                    File.WriteAllText(projFile, opts.code)
                    { opts with projFile = projFile }
                | opts -> opts
        let com = { new ICompiler with
                        member __.Options = opts }
        let babelAstList =
            parseFSharpProject com
            |> FSharp2Fabel.transformFiles com 
            |> Fabel2Babel.transformFiles com
        JsonConvert.SerializeObject (
            babelAstList, Json.ErasedUnionConverter())
        |> printfn "%s"
        0 // return an integer exit code
    with ex ->
        printfn "%s" ex.Message
        1 
