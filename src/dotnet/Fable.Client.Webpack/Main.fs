module Fable.Client.Webpack.Main

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Newtonsoft.Json
open Fable
open Fable.AST
open Fable.Core
open Printers

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


let loadPlugins (checker: FSharpChecker) (opts: CompilerOptions) =
    let compileScript (path: string) =
        if Path.GetExtension(path) = ".fsx"
        then
            // The current version can compile scripts but when I load them, apparently assemblies are not
            // redirected correctly: if I call Fable.Core from the plugin it'll say "Method not found"
            "Compiling .fsx plugins on the fly is not supported in Fable netcore version"
            |> FableError |> raise
            // let dllPath = Path.ChangeExtension(path, ".dll")
            // if not(File.Exists dllPath) || File.GetLastWriteTime path > File.GetLastWriteTime dllPath then
            //     let scriptOptions = getProjectOptionsFromScript checker opts path
            //     let otherOptions = Array.append (getBasicCompilerArgs opts true) scriptOptions.OtherOptions
            //     compileDll checker otherOptions false [|path|] dllPath
            // dllPath
        else path
    opts.plugins
    |> Seq.collect (fun path ->
        try
            let filePath = Path.GetFullPath path |> compileScript
#if DOTNETCORE
            let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
            let assembly = globalLoadContext.LoadFromAssemblyPath(filePath)
#else
            let assembly = (filePath |> Assembly.LoadFrom)
#endif
            assembly.GetTypes()
            |> Seq.filter typeof<IPlugin>.IsAssignableFrom
            |> Seq.map (fun x ->
                Path.GetFileNameWithoutExtension path,
                Activator.CreateInstance x |> unbox<IPlugin>)
        with
        | :? FableError as er -> raise er
        | ex -> FableError("Cannot load plugin "+path+": "+ex.Message) |> raise)
    |> Seq.toList

let parseFSharpProject (com: ICompiler) (checker: FSharpChecker)
                        (projOptions: FSharpProjectOptions) =
    let checkProjectResults =
        projOptions
        |> checker.ParseAndCheckProject
        |> Async.RunSynchronously
    let errors, warnings =
        parseErrors checkProjectResults.Errors
    if errors.Length = 0
    then warnings |> Array.map Warning, checkProjectResults
    else errors
        |> Seq.append ["F# project contains errors:"]
        |> String.concat "\n"
        |> FableError |> raise

let makeCompiler opts plugins =
    let mutable id = 0
    let logs = ResizeArray()
    let projDir = Path.GetDirectoryName opts.projFile
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
            "$var" + (System.Threading.Interlocked.Increment(&id).ToString())  }

(*
let compile (com: ICompiler) checker (projInfo: FSProjInfo) =
    try
        // Reload project options if necessary
        // -----------------------------------
        let projInfo =
            match projInfo.FileMask with
            | Some file when com.Options.projFile = file ->
                let projOpts, filePairs = getFullProjectOpts checker com.Options
                FSProjInfo(projOpts, filePairs, ?fileMask=projInfo.FileMask, extra=projInfo.Extra)
            | _ -> projInfo

        if Array.isEmpty projInfo.ProjectOpts.ProjectFileNames then
            FableError("Couldn't find any F# source file") |> raise

        // Print F# compiler options (verbose mode) on first compilation
        // (when projInfo.fileMask is None)
        if Option.isNone projInfo.FileMask then
            let print label msgs =
                msgs |> String.concat "\n" |> sprintf "\n%s:\n%s\n" label
                |> Log |> List.singleton |> printMessages
            print "F# COMPILER OPTIONS" projInfo.ProjectOpts.OtherOptions
            print "F# SOURCE FILES" projInfo.ProjectOpts.ProjectFileNames
            projInfo.FilePairs |> Seq.map (fun kv -> kv.Value) |> print "JS TARGET FILES"

        // Parse project (F# Compiler Services) and print diagnostic info
        // --------------------------------------------------------------
        //let timer = PerfTimer("Warmup") |> Some
        let warnings, parsedProj =
            parseFSharpProject com checker projInfo.ProjectOpts

        //let warnings = match timer with Some timer -> (timer.Finish())::warnings | None -> warnings
        warnings |> Seq.map (string >> Log) |> printMessages

        let saveAstFile sourceFile ext decls =
            let filePath = projInfo.FilePairs.Item(sourceFile)
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)) |> ignore
            File.WriteAllLines(Path.ChangeExtension(filePath, ext), decls |> Seq.toArray)

        // save FSharp AST declarations
        if com.Options.extra |> Map.containsKey "saveFSharpAst" then
            parsedProj.AssemblyContents.ImplementationFiles
            |> Seq.iter (fun file ->
                printFSharpDecls "" file.Declarations |> saveAstFile file.FileName ".fs.ast" )

        // save Fable AST declarations
        let saveFableAst (extra, files) =
            if com.Options.extra |> Map.containsKey "saveFableAst" then
                files |> Seq.iter (fun (file: Fable.File) ->
                    printFableDecls file.Declarations |> saveAstFile file.SourceFile ".fable.ast" )
            (extra, files)

        [Log "F# compilation complete. Starting Fable..."] |> printMessages

        // Compile project files, print them and get extra info
        // ----------------------------------------------------
        let extraInfo, files =
            FSharp2Fable.Compiler.transformFiles com parsedProj projInfo
            |> saveFableAst
            |> Fable2Babel.Compiler.transformFiles com

        files
        |> Seq.iter (printFile com)

        // Print logs
        // ----------
        com.GetLogs() |> Seq.map (string >> Log) |> printMessages

        Console.Out.WriteLine "[SIGSUCCESS]"
        true, FSProjInfo(projInfo.ProjectOpts, projInfo.FilePairs,
                            ?fileMask=projInfo.FileMask, extra=extraInfo)
    with ex ->
        printException ex
        Console.Out.WriteLine "[SIGFAIL]"
        false, projInfo
*)

[<EntryPoint>]
let main argv =
    0
