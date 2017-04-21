module Fable.Client

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Core
open Fable.Tools.State

let parseFSharpProject (com: ICompiler) (checker: InteractiveChecker) (fileName,source) =
    let _,_,checkProjectResults = (fileName,source) |> checker.ParseAndCheckScript
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

let compileAst (com: ICompiler) checker (fileName, source) =
    let projectOptions = makeProjOptions com fileName
    let checkedProject = parseFSharpProject com checker (fileName, source)
    let state = State(projectOptions, checkedProject)
    let file: Babel.Program =
        FSharp2Fable.Compiler.transformFile com state state.CheckedProject fileName
        |> Fable2Babel.Compiler.transformFile com state
    file

let createChecker readAllBytes references =
    InteractiveChecker(List.ofArray references, readAllBytes)

let makeCompiler () = Compiler()

let compileSource checker source =
    let com = makeCompiler ()
    let fileName = "stdin.fsx"
    let file = compileAst com checker (fileName, source)
    file

let compileToJson checker source =
    compileSource checker source
    |> Fable.Core.JsInterop.toJson
