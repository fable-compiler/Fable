module Fable.JS.Main

open System
open Interfaces
open Fable
open Fable.AST
open Fable.Core
open Fable.State
open FsAutoComplete
open Microsoft.FSharp.Compiler.SourceCodeServices

type private CheckerImpl(c: InteractiveChecker) =
    member __.Checker = c
    interface IChecker

type private CompilerImpl(c: Compiler) =
    member __.Compiler = c
    interface IFableCompiler

type ParseResults =
    { ParseFile: FSharpParseFileResults
      CheckFile: FSharpCheckFileResults
      CheckProject: FSharpCheckProjectResults }
    interface IParseResults with
        member this.Errors = this.CheckProject.Errors |> Array.map (fun er ->
            { StartLineAlternate = er.StartLineAlternate
              StartColumn = er.StartColumn
              EndLineAlternate = er.EndLineAlternate
              EndColumn = er.EndColumn
              Message = er.Message
              IsWarning =
                match er.Severity with
                | FSharpErrorSeverity.Error -> false
                | FSharpErrorSeverity.Warning -> true
            })

let inline private tryGetLexerSymbolIslands (sym: Lexer.LexerSymbol) =
  match sym.Text with
  | "" -> None
  | _ -> Some (sym.RightColumn, sym.Text.Split '.' |> Array.toList)

// Parsing - find the identifier around the current location
// (we look for full identifier in the backward direction, but only
// for a short identifier forward - this means that when you hover
// 'B' in 'A.B.C', you will get intellisense for 'A.B' module)
let findIdents col lineStr lookupType =
  if lineStr = "" then None
  else
      Lexer.getSymbol 0 col lineStr lookupType [||]
      |> Option.bind tryGetLexerSymbolIslands

let findLongIdents (col, lineStr) =
    findIdents col lineStr Lexer.SymbolLookupKind.Fuzzy

let findLongIdentsAndResidue (col: int, lineStr:string) =
  let lineStr = lineStr.Substring(0, col)
  match Lexer.getSymbol 0 col lineStr Lexer.SymbolLookupKind.ByLongIdent [||] with
  | Some sym ->
      match sym.Text with
      | "" -> [], ""
      | text ->
          let res = text.Split '.' |> List.ofArray |> List.rev
          if lineStr.[col - 1] = '.' then res |> List.rev, ""
          else
              match res with
              | head :: tail -> tail |> List.rev, head
              | [] -> [], ""
  | _ -> [], ""

let convertGlyph glyph =
    match glyph with
    | FSharpGlyph.Class | FSharpGlyph.Struct | FSharpGlyph.Union
    | FSharpGlyph.Type | FSharpGlyph.Typedef ->
        Glyph.Class
    | FSharpGlyph.Enum | FSharpGlyph.EnumMember ->
        Glyph.Enum
    | FSharpGlyph.Constant ->
        Glyph.Value
    | FSharpGlyph.Variable ->
        Glyph.Variable
    | FSharpGlyph.Interface ->
        Glyph.Interface
    | FSharpGlyph.Module | FSharpGlyph.NameSpace ->
        Glyph.Module
    | FSharpGlyph.Method | FSharpGlyph.OverridenMethod | FSharpGlyph.ExtensionMethod ->
        Glyph.Method
    | FSharpGlyph.Property ->
        Glyph.Property
    | FSharpGlyph.Field ->
        Glyph.Field
    | FSharpGlyph.Delegate ->
        Glyph.LambdaType
    | FSharpGlyph.Error | FSharpGlyph.Exception ->
        Glyph.Error
    | FSharpGlyph.Event ->
        Glyph.Event

let parseFSharpProject (checker: InteractiveChecker) fileName source =
    let parseResults, typeCheckResults, projectResults = checker.ParseAndCheckScript (fileName, source)
    { ParseFile = parseResults
      CheckFile = typeCheckResults
      CheckProject = projectResults }

let tooltipToString (el: FSharpToolTipElement<string>): string[] =
    let dataToString (data: FSharpToolTipElementData<string>) =
        [| match data.ParamName with
           | Some x -> yield x + ": " + data.MainDescription
           | None -> yield data.MainDescription
           match data.XmlDoc with
           | FSharpXmlDoc.Text doc -> yield doc
           | _ -> ()
           yield! data.TypeMapping
           match data.Remarks with
           | Some x -> yield x
           | None -> ()
        |]
    match el with
    | FSharpToolTipElement.None -> [||]
    | FSharpToolTipElement.Group(els) ->
        Seq.map dataToString els |> Array.concat
    | FSharpToolTipElement.CompositionError err -> [|err|]

/// Get tool tip at the specified location
let getToolTipAtLocation (parseResults: ParseResults) line col lineText = async {
    match findLongIdents(col - 1, lineText) with
    | None -> return [|"Cannot find ident for tooltip"|]
    | Some(col,identIsland) ->
        let! (FSharpToolTipText els) = parseResults.CheckFile.GetToolTipText(line, col, lineText, identIsland, FSharpTokenTag.Identifier)
        return Seq.map tooltipToString els |> Array.concat
}

let getCompletionsAtLocation (parseResults: ParseResults) (line: int) (col: int) lineText = async {
    let ln, residue = findLongIdentsAndResidue(col - 1, lineText)
    let longName = Microsoft.FSharp.Compiler.QuickParse.GetPartialLongNameEx(lineText, col - 1)
    let longName = { longName with QualifyingIdents = ln; PartialIdent = residue }

    let! decls = parseResults.CheckFile.GetDeclarationListInfo(Some parseResults.ParseFile, line, lineText, longName, fun () -> [])
    return decls.Items |> Array.map (fun decl ->
        { Name = decl.Name; Glyph = convertGlyph decl.Glyph })
}

let makeProjOptions (_com: ICompiler) projFile =
    let projOptions: FSharpProjectOptions =
      { ProjectFileName = projFile
        SourceFiles = [| |]
        OtherOptions = [| |]
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        ExtraProjectInfo = None
        Stamp = None }
    projOptions

let compileAst (com: Compiler) fileName optimized (parseResults: ParseResults) =
    // let errors = com.ReadAllLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let checkedProject = parseResults.CheckProject
    let projectOptions = makeProjOptions com fileName
    let implFiles =
        (if optimized
         then checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
         else checkedProject.AssemblyContents.ImplementationFiles)
        |> Seq.map (fun file -> Path.normalizePath file.FileName, file) |> Map
    // Dealing with fableCoreDir is a bit messy atm, for the REPL, only the value in the Compiler options matters
    let project = Project(projectOptions, implFiles, parseResults.CheckProject.Errors, Map.empty, NonFilePath "", isWatchCompile=false)

    let file: Babel.Program =
        FSharp2Fable.Compiler.transformFile com project project.ImplementationFiles fileName
        |> Fable2Babel.Compiler.transformFile com project
    let loc = defaultArg file.loc SourceLocation.Empty
    Babel.Program(file.fileName, loc, file.body, file.directives, com.ReadAllLogs())

let defaultManager =
  { new IFableManager with
        member __.CreateChecker(references, readAllBytes) =
            InteractiveChecker.Create(references, readAllBytes)
            |> CheckerImpl :> IChecker
        member __.CreateCompiler(fableCoreDir, replacements) =
            let options =
                { fableCore = fableCoreDir
                  emitReplacements = defaultArg replacements (upcast [||]) |> Map
                  typedArrays = true
                  clampByteArrays = false
                  declaration = false }
            Compiler(options) |> CompilerImpl :> IFableCompiler
        member __.ParseFSharpProject(checker, fileName, source) =
            let c = checker :?> CheckerImpl
            parseFSharpProject c.Checker fileName source :> IParseResults
        member __.GetToolTipText(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getToolTipAtLocation res line col lineText
        member __.GetCompletionsAtLocation(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getCompletionsAtLocation res line col lineText
        member __.CompileToBabelAst(com: IFableCompiler, parseResults:IParseResults, fileName:string, optimized: bool) =
            let com = com :?> CompilerImpl
            let res = parseResults :?> ParseResults
            res |> compileAst com.Compiler fileName optimized
        member x.CompileToBabelJsonAst(com: IFableCompiler, parseResults:IParseResults, fileName:string, ?optimized: bool) =
            let optimized = defaultArg optimized false
            x.CompileToBabelAst(com, parseResults, fileName, optimized)
            |> JsInterop.toJson
  }

[<ExportDefault>]
let exports = defaultManager
