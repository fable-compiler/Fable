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
            | Some v -> Map.add k (v + "|" + rest.Head) opts
            |> readOpts <| rest.Tail
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    {
        code = def opts "code" null id
        outDir = def opts "outDir" "." id
        lib = def opts "lib" "." id
        projFile = def opts "projFile" null Path.GetFullPath
        watch = def opts "watch" false bool.Parse
        symbols = def opts "symbols" [||] (fun x -> x.Split('|'))
        plugins = def opts "plugins" [||] (fun x -> x.Split('|'))
    }

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
    
let getProjectOptions (com: ICompiler) (checker: FSharpChecker)
                      (prevResults: FSharpProjectOptions option) (fileMask: string option) =
    match prevResults, fileMask with
    | Some res, Some file when com.Options.projFile <> file -> res
    | _ ->
        let projFile = com.Options.projFile
        if not(File.Exists projFile) then
            failwithf "Cannot find project file %s" projFile
        match (Path.GetExtension projFile).ToLower() with
        | ".fsx" ->
            let projCode =
                match com.Options.code with
                | null -> File.ReadAllText projFile
                | projCode ->
                    // TODO: use File System
                    File.WriteAllText(projFile, projCode)
                    projCode
            let otherFlags = com.Options.symbols |> Array.map (sprintf "--define:%s")
            checker.GetProjectOptionsFromScript(projFile, projCode, otherFlags=otherFlags)
            |> Async.RunSynchronously
        | _ ->
            let properties = [("DefineConstants", String.concat ";" com.Options.symbols)]
            ProjectCracker.GetProjectOptionsFromProjectFile(projFile, properties)

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                       (projOptions: FSharpProjectOptions) =
    let checkProjectResults =
        projOptions
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
        
let makeCompiler plugins opts =
    { new ICompiler with
        member __.Options = opts
        member __.Plugins = plugins }

let compile (com: ICompiler) checker projOpts fileMask =
    let printFile =
        let jsonSettings = 
            JsonSerializerSettings( 
                Converters=[|Json.ErasedUnionConverter()|],
                StringEscapeHandling=StringEscapeHandling.EscapeNonAscii)
        fun (file: AST.Babel.Program) ->
            JsonConvert.SerializeObject (file, jsonSettings)
            |> Console.Out.WriteLine
    let printError =
        CompilerError
        >> JsonConvert.SerializeObject
        >> Console.Out.WriteLine
    try
        let projOpts =
            getProjectOptions com checker projOpts fileMask
        projOpts
        |> parseFSharpProject com checker
        |> FSharp2Fable.Compiler.transformFiles com fileMask
        |> Seq.map (Fable2Babel.Compiler.transformFile com)
        |> Seq.choose id
        |> Seq.iter printFile
        Some projOpts
    with ex ->
        printError ex.Message
        None
        
let rec awaitInput (com: ICompiler) checker projOpts =
    let input = Console.In.ReadLine()
    let com, fileMask =
        if com.Options.code <> null then
            makeCompiler com.Plugins {
                com.Options with code = input.Replace("\\n","\n")
            }, Some com.Options.projFile
        else
            com, Some input
    compile com checker projOpts fileMask
    |> awaitInput com checker

[<EntryPoint>]
let main argv =
    let opts =
        readOptions argv
        |> function
            | opts when opts.code <> null ->
                { opts with projFile = Path.ChangeExtension(Path.GetTempFileName(), "fsx") }
            | opts -> opts
    let com = makeCompiler (loadPlugins opts) opts
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    // First full compilation
    let projOpts = compile com checker None None
    if opts.watch then
        awaitInput com checker projOpts
    // Send empty string to finish node process
    Console.Out.WriteLine()
    0
