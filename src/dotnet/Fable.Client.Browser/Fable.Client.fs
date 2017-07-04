module Fable.Client

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Core
open Fable.State

let parseFSharpProject (checker: InteractiveChecker) (com: ICompiler) fileName source =
    let _,_,checkProjectResults = checker.ParseAndCheckScript (fileName, source)
    for er in checkProjectResults.Errors do
        let severity =
            match er.Severity with
            | FSharpErrorSeverity.Warning -> Severity.Warning
            | FSharpErrorSeverity.Error -> Severity.Error
        let range =
            { start={ line=er.StartLineAlternate; column=er.StartColumn}
            ; ``end``={ line=er.EndLineAlternate; column=er.EndColumn} }
        com.AddLog(er.Message, severity, range, er.FileName, "FSHARP")
    checkProjectResults

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

let compileAst (com: Compiler) checkedProject fileName =
    let errors = com.Logs |> Map.tryFind "error"
    if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let projectOptions = makeProjOptions com fileName
    let project = Project(projectOptions, checkedProject)
    let file: Babel.Program =
        FSharp2Fable.Compiler.transformFile com project project.CheckedProject fileName
        |> Fable2Babel.Compiler.transformFile com project
    let loc = defaultArg file.loc SourceLocation.Empty
    Babel.Program(file.fileName, loc, file.body, file.directives, com.Logs)

let createChecker readAllBytes references =
    InteractiveChecker.Create(List.ofArray references, readAllBytes)

let makeCompiler () = Compiler()

let compileSource checker source =
    let com = makeCompiler ()
    let fileName = "stdin.fsx"
    let checkedProject = parseFSharpProject checker com fileName source
    let file = compileAst com checkedProject fileName
    file

let convertToJson babelAst =
    babelAst
    |> Fable.Core.JsInterop.toJson
