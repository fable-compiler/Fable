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
            | None -> Map.add k (U2.Case1 rest.Head) opts
            | Some (U2.Case1 v) -> Map.add k (U2.Case2 [rest.Head;v]) opts
            | Some (U2.Case2 v) -> Map.add k (U2.Case2 (rest.Head::v)) opts
            |> readOpts <| rest.Tail
    let un f = function U2.Case1 v -> f v | U2.Case2 _ -> failwith "Unexpected multiple argument"
    let li f = function U2.Case1 v -> [f v] | U2.Case2 v -> List.map f v
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    {
        code = def opts "code" null (un id)
        projFile = def opts "projFile" null (un Path.GetFullPath)
        watch = def opts "watch" false (un bool.Parse)
        clamp = def opts "clamp" false (un bool.Parse)
        symbols = def opts "symbols" [] (li id)
        plugins = def opts "plugins" [] (li id)
        references = Map(def opts "references" [] (li (fun (x: string) ->
            let xs = x.Split('=') in xs.[0], xs.[1])))
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
        let replaceConstants (otherOpts: string[]) =
            otherOpts
            |> Array.filter (fun s -> s.StartsWith "--define:" = false)
            |> Array.append (List.map (sprintf "--define:%s") com.Options.symbols |> List.toArray)
        let projFile = com.Options.projFile
        if not(File.Exists projFile) then
            failwithf "Cannot find project file %s" projFile
        try
            match (Path.GetExtension projFile).ToLower() with
            | ".fsx" ->
                let projCode =
                    match com.Options.code with
                    | null -> File.ReadAllText projFile
                    | projCode ->
                        // TODO: use File System
                        File.WriteAllText(projFile, projCode)
                        projCode
                checker.GetProjectOptionsFromScript(projFile, projCode)
                |> Async.RunSynchronously
            | _ ->
                ProjectCracker.GetProjectOptionsFromProjectFile(projFile)
            |> fun opts -> { opts with OtherOptions = replaceConstants opts.OtherOptions}
        with
        | ex -> failwithf "Cannot read project options: %s" ex.Message

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
        |> Fable2Babel.Compiler.transformFile com
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
    // Full compilation
    let projOpts = compile com checker None None
    // Send empty string to signal end of compilation
    Console.Out.WriteLine()
    // Keep on watching if necessary
    if opts.watch then
        awaitInput com checker projOpts
    0
