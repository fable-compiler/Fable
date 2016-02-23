module Fable.Main

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Fable

let readOptions argv =
    let def opts key defArg f =
        defaultArg (Map.tryFind key opts |> Option.map f) defArg
    let rec readOpts opts = function
        | [] -> opts
        | (opt: string)::rest ->
            let k = opt.Substring(2)
            match Map.tryFind k opts with
            | None -> Map.add k rest.Head opts
            | Some v -> Map.add k (v + "," + rest.Head) opts
            |> readOpts <| rest.Tail
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    {
        code = def opts "code" null id
        outDir = def opts "outDir" "." id
        lib = def opts "lib" "." id
        projFile = def opts "projFile" null id
        watch = def opts "watch" false bool.Parse
        symbols = def opts "symbols" [||] (fun x -> x.Split(','))
    }

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker) (projCode: string option) =
    let checkProjectResults =
        let projCode =
            match projCode with
            | None -> File.ReadAllText com.Options.projFile
            | Some projCode ->
                File.WriteAllText(com.Options.projFile, projCode)
                projCode
        match (Path.GetExtension com.Options.projFile).ToLower() with
        | ".fsx" ->
            let otherFlags =
                com.Options.symbols |> Array.map (sprintf "--define:%s")
            checker.GetProjectOptionsFromScript(
                com.Options.projFile, projCode, DateTime.UtcNow, otherFlags=otherFlags)
            |> Async.RunSynchronously
        | _ ->
            let properties =
                [("DefineConstants", String.concat ";" com.Options.symbols)]
            ProjectCracker.GetProjectOptionsFromProjectFile(
                com.Options.projFile, properties, DateTime.UtcNow)
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors =
        checkProjectResults.Errors
        |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)
    if errors.Length = 0
    then checkProjectResults
    else errors
        |> Seq.map (fun e -> "> " + e.Message)
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> failwith

let compile com checker projCode fileMask =
    let printFile =
        let jsonSettings = 
            JsonSerializerSettings(
                Converters=[|Json.ErasedUnionConverter()|],
                StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
        fun file ->
            JsonConvert.SerializeObject (file, jsonSettings)
            |> Console.Out.WriteLine
    let printError =
        CompilerError
        >> JsonConvert.SerializeObject
        >> Console.Out.WriteLine
    try
        parseFSharpProject com checker projCode
        |> FSharp2Fable.transformFiles com fileMask
        |> Fable2Babel.transformFiles com
        |> function
            | files when Seq.isEmpty files ->
                printError "No code available to compile"
            | files ->
                Seq.iter printFile files
    with ex ->
        printError ex.Message

[<EntryPoint>]
let main argv =
    let opts =
        readOptions argv
        |> function
            | opts when opts.code <> null ->
                { opts with projFile = Path.ChangeExtension(Path.GetTempFileName(), "fsx") }
            | opts -> opts
    let com = { new ICompiler with member __.Options = opts }
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    // First full compilation
    compile com checker (Option.ofObj opts.code) None
    while opts.watch do
        let input = Console.In.ReadLine()
        let projCode, fileMask =
            if com.Options.code = null
            then None, Some input
            else input.Replace("\\n","\n") |> Some, None
        compile com checker projCode fileMask
    0
