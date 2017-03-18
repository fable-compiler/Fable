module Fable.Client

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Core


type State(projectOptions: FSharpProjectOptions, checkedProject: FSharpCheckProjectResults) =
    // let entities = ConcurrentDictionary<string, Fable.Entity>()
    // let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    let compiledFiles =
        let dic = System.Collections.Generic.Dictionary()
        for file in projectOptions.ProjectFileNames do
            dic.Add(Path.normalizeFullPath file, false)
        dic
    let rootModules =
        checkedProject.AssemblyContents.ImplementationFiles
        |> Seq.map (fun file -> file.FileName, FSharp2Fable.Compiler.getRootModuleFullName file)
        |> Map
    member __.CheckedProject = checkedProject
    member __.ProjectOptions = projectOptions
    member __.ProjectFile = projectOptions.ProjectFileName
    member __.CompiledFiles = compiledFiles
    interface ICompilerState with
        member __.GetRootModule(fileName) =
            match Map.tryFind fileName rootModules with
            | Some rootModule -> rootModule
            | None -> FableError("Cannot find root module for " + fileName) |> raise
        member __.GetOrAddEntity(fullName, generate) =
            generate() //entities.GetOrAdd(fullName, fun _ -> generate())
        member __.GetOrAddInlineExpr(fullName, generate) =
            generate() //inlineExprs.GetOrAdd(fullName, fun _ -> generate())

type Compiler(options, plugins) =
    let mutable id = 0
    let logs = ResizeArray()
    member __.Logs = logs
    interface ICompiler with
        member __.Options = options
        member __.Plugins = plugins
        member __.AddLog msg = logs.Add msg
        member __.GetUniqueVar() =
            id <- id + 1
            "$var" + (string id)

let makeCompiler options plugins = Compiler(options, plugins)

let readOptions _argv : CompilerOptions =
    { fableCore = "fable-core"
    ; declaration = false
    ; typedArrays = true
    ; clampByteArrays = false }

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

let makeProjOptions (com: ICompiler) projFile =
    let projOptions: FSharpProjectOptions =
      { ProjectFileName = projFile
        ProjectFileNames = [| |]
        OtherOptions = [| |]
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        ExtraProjectInfo = None }
    projOptions

let compileAst (com: ICompiler) checker (fileName, source) =
    let projectOptions = makeProjOptions com fileName
    let warnings, checkedProject = parseFSharpProject com checker (fileName, source)
    for warning in warnings do
        com.AddLog(warning)
    let state = State(projectOptions, checkedProject)
    let file: Babel.Program =
        FSharp2Fable.Compiler.transformFile com state state.CheckedProject fileName
        |> Fable2Babel.Compiler.transformFile com state
    file
