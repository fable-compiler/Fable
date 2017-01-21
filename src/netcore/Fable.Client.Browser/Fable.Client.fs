module Fable.Client

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST

type FSProjInfo = FSharp2Fable.Compiler.FSProjectInfo

type CompilerMessage =
    | Error of message: string * stack: string option
    | Log of message: string
    static member toDic = function
        | Error (msg, Some stack) ->
            dict [ ("type", "ERROR"); ("message", msg); ("stack", stack) ]
        | Error (msg, None) ->
            dict [ ("type", "ERROR"); ("message", msg) ]
        | Log msg ->
            dict [ ("type", "LOG"); ("message", msg) ]

let readOptions argv =
    let splitKeyValue (x: string) =
        let xs = x.Split('=')
        if xs.Length > 1
        then xs.[0], xs.[1]
        else xs.[0], (string true)
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
    let boolParse (s:string) = (s.ToLower() = "true")
    let un f = function U2.Case1 v -> f v | U2.Case2 _ -> failwith "Unexpected multiple argument"
    let li f = function U2.Case1 v -> [f v] | U2.Case2 v -> List.map f v
    let opts = readOpts Map.empty<_,_> (List.ofArray argv)
    let opts = {
        projFile = def opts "projFile" [] (li Path.GetFullPath) |> List.rev
        outDir = def opts "outDir" "." (un Path.GetFullPath)
        coreLib = "fable-core"
        moduleSystem = def opts "module" "es2015" (un id)
        symbols = def opts "symbols" [] (li id) |> List.append ["FABLE_COMPILER"] |> List.distinct
        plugins = def opts "plugins" [] (li id)
        rollup = def opts "rollup" false (un boolParse)
        watch = def opts "watch" false (un boolParse)
        dll = def opts "dll" false (un boolParse)
        includeJs = def opts "includeJs" false (un boolParse)
        noTypedArrays = def opts "noTypedArrays" false (un boolParse)
        clamp = def opts "clamp" false (un boolParse)
        declaration = def opts "declaration" false (un boolParse)
        refs = Map(def opts "refs" [] (li splitKeyValue))
        extra = Map(def opts "extra" [] (li splitKeyValue))
    }
    match Map.tryFind "Fable.Core" opts.refs with
    | Some coreLib -> { opts with coreLib = coreLib }
    | None when not opts.rollup && Naming.umdModules.Contains opts.moduleSystem ->
        { opts with coreLib = "fable-core/umd" }
    | None -> opts

/// Returns an (errors, warnings) tuple
let parseErrors errors =
    let parseError (er: FSharpErrorInfo) =
        let isError, severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> false, "warning"
            | FSharpErrorSeverity.Error -> true, "error"
        isError, sprintf "%s(L%i,%i) : %s FSHARP: %s"
            er.FileName er.StartLineAlternate er.StartColumn
            severity er.Message
    errors
    |> Array.map parseError
    |> Array.partition fst
    |> fun (ers, wns) -> Array.map snd ers, Array.map snd wns

let parseFSharpProject (com: ICompiler) (checker: InteractiveChecker) (fileName,source) =
    let _,_,checkProjectResults =
        (fileName,source) |> checker.ParseAndCheckScript
    let errors, warnings =
        parseErrors checkProjectResults.Errors
    if errors.Length = 0
    then warnings |> Array.map Warning, checkProjectResults
    else errors
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> FableError |> raise

let makeCompiler opts plugins =
    let id = ref 0
    let logs = ResizeArray()
    let projDir = "." //Fable.Path.getCommonBaseDir opts.projFile
    { new ICompiler with
        member __.Options = opts
        member __.ProjDir = projDir
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetLogs() =
            let copy = logs.ToArray()
            logs.Clear()
            upcast copy
        member __.GetUniqueVar() =
            id := !id + 1
            "$var" + string !id }

let printFile =
    fun (file: AST.Babel.Program) ->
        printfn "%A" file

let printMessages (msgs: seq<CompilerMessage>) =
    msgs
    |> Seq.map CompilerMessage.toDic
    |> Seq.iter (printfn "%A")

let printException (ex: Exception) =
    let msg, stackTrace =
        match ex with
        // Don't print stack trace for known Fable errors
        | :? FableError as err -> err.FormattedMessage, None
        | ex -> ex.Message, Some(ex.StackTrace)
    printMessages [Error(msg, stackTrace)]

let makeProjInfo (com: ICompiler) fileName =
    let projOptions: FSharpProjectOptions =
      { ProjectFileName = "test.fsx"
        ProjectFileNames = [| |]
        OtherOptions = [| |]
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now }
    let filePairs =
        [fileName]
        |> Seq.map (fun fileName ->
            fileName, Fable.Path.Combine(com.Options.outDir, Fable.Path.ChangeExtension(fileName, ".js")))
        |> Map
    let projInfo = FSProjInfo(projOptions, filePairs)
    projInfo

let compileAst (com: ICompiler) checker (fileName, source) =
    let warnings, parsedProj = parseFSharpProject com checker (fileName, source)
    let projInfo = makeProjInfo com fileName
    let extraInfo, files =
        FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
        |> Fable2Babel.Compiler.transformFiles com
    files

let compile (com: ICompiler) checker (fileName, source) =
    try
        // Parse project (F# Compiler Services) and print diagnostic info
        // --------------------------------------------------------------
        let warnings, parsedProj =
            parseFSharpProject com checker (fileName, source)

        warnings |> Seq.map (string >> Log) |> printMessages

        [Log "F# compilation complete. Starting Fable..."] |> printMessages

        // make projInfo
        let projInfo = makeProjInfo com fileName

        // Compile project files, print them and get extra info
        // ----------------------------------------------------
        let extraInfo, files =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> Fable2Babel.Compiler.transformFiles com

        files
        |> Seq.iter printFile

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        printfn "[SIGSUCCESS]"
        true
    with ex ->
        printException ex
        printfn "[SIGFAIL]"
        false
