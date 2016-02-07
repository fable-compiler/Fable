module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fabel

let readOptions projFile =
    let projDir = Path.GetDirectoryName(projFile)
    if File.Exists(Path.Combine(projDir, "fabelconfig.json")) then
        let json = File.ReadAllText(Path.Combine(projDir, "fabelconfig.json"))
        let opts = JsonConvert.DeserializeObject<CompilerOptions>(json)
        { opts with projFile = projFile }
    else
        CompilerOptions.Default projFile

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
    if not checkProjectResults.HasCriticalErrors
    then checkProjectResults
    else checkProjectResults.Errors
        |> Seq.map (fun e -> "\t" + e.Message)
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

[<EntryPoint>]
let main argv =
    let opts =
        if argv.[0] = "--projFile"
        then readOptions argv.[1]
        else JsonConvert.DeserializeObject<_>(argv.[0])
        |> CompilerOptions.Sanitize 
        |> function
            | opts when opts.code <> null ->
                let projFile = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
                File.WriteAllText(projFile, opts.code)
                { opts with projFile = projFile }
            | opts -> opts
    let com = { new ICompiler with
                    member __.Options = opts }
    parseFSharpProject com
    |> FSharp2Fabel.transformFiles com 
    |> Fabel2Babel.transformFiles com
    |> Seq.iter (fun ast ->
        JsonConvert.SerializeObject (ast, Json.ErasedUnionConverter())
        |> Console.Out.WriteLine
        Console.Out.Flush ())
    0 // return an integer exit code
