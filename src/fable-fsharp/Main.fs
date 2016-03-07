module Fable.Main

open System
open System.IO
open System.Reflection
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
        plugins = def opts "plugins" [||] (fun x -> x.Split(','))
    }
    
// TODO: Error management
let loadPlugins (opts: CompilerOptions): IPlugin list =
    opts.plugins
    |> Seq.collect (fun path ->
        try
            (Path.GetFullPath path |> Assembly.LoadFile).GetTypes()
            |> Seq.filter typeof<IPlugin>.IsAssignableFrom
            |> Seq.map Activator.CreateInstance
        with
        | ex -> failwithf "Cannot load plugin %s: %s" path ex.Message)
    |> Seq.cast<_>
    |> Seq.toList

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
                com.Options.projFile, projCode, otherFlags=otherFlags)
            |> Async.RunSynchronously
        | _ ->
            let properties =
                [("DefineConstants", String.concat ";" com.Options.symbols)]
            ProjectCracker.GetProjectOptionsFromProjectFile(
                com.Options.projFile, properties)
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors =
        checkProjectResults.Errors
        |> Array.filter (fun x -> x.Severity = FSharpErrorSeverity.Error)
    if errors.Length = 0
    then checkProjectResults
    else errors
        |> Seq.map (fun e ->
            sprintf "> %s: L%i (%s)"
                e.Message e.StartLineAlternate (Path.GetFileName e.FileName))
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
            Console.Out.Flush()
    let printError =
        CompilerError
        >> JsonConvert.SerializeObject
        >> Console.Out.WriteLine
    try
        parseFSharpProject com checker projCode
        |> FSharp2Fable.Compiler.transformFiles com fileMask
        |> Fable2Babel.Compiler.transformFiles com
        |> Seq.iter printFile
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
    let plugins = loadPlugins opts
    let com = { new ICompiler with
                member __.Options = opts
                member __.Plugins = plugins }
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
    // Wait a bit so node has time to process remaining messages
    System.Threading.Thread.Sleep(500)
    // Send empty string to finish node process
    Console.Out.WriteLine()
    0
