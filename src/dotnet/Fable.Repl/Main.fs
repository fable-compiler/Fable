module Fable.Repl.Main

open System
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open FsAutoComplete
open Microsoft.FSharp.Compiler.SourceCodeServices

type CheckerImpl(checker: InteractiveChecker) =
    member __.Checker = checker
    interface IChecker

type ParseResults
   (parseFile: FSharpParseFileResults,
    checkFile: FSharpCheckFileResults,
    checkProject: FSharpCheckProjectResults) =

    member __.ParseFile = parseFile
    member __.CheckFile = checkFile
    member __.CheckProject = checkProject

    interface IParseResults with
        member __.Errors = checkProject.Errors |> Array.map (fun er ->
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
        Glyph.Function
    | FSharpGlyph.Error | FSharpGlyph.Exception ->
        Glyph.Error
    | FSharpGlyph.Event ->
        Glyph.Event

let parseFSharpProject (checker: InteractiveChecker) fileName source =
    let parseResults, typeCheckResults, projectResults = checker.ParseAndCheckScript (fileName, source)
    ParseResults (parseResults, typeCheckResults, projectResults)

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

let makeProjOptions projFile =
    let projOptions: FSharpProjectOptions =
      { ProjectFileName = projFile
        ProjectId = None
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

let makeProject fileName optimized (parseResults: ParseResults) =
    // let errors = com.GetFormattedLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let checkedProject = parseResults.CheckProject
    let projectOptions = makeProjOptions fileName
    let implFiles =
        (if optimized
         then checkedProject.GetOptimizedAssemblyContents().ImplementationFiles
         else checkedProject.AssemblyContents.ImplementationFiles)
        |> Seq.map (fun file -> Fable.Path.normalizePath file.FileName, file) |> Map
    // Dealing with fableCoreDir is a bit messy atm, for the REPL, only the value in the Compiler options matters
    let project = Project(projectOptions, implFiles, parseResults.CheckProject.Errors, Map.empty, "", isWatchCompile=false)
    project

let makeCompiler fableCore filePath (project: Project) =
    let options: Fable.CompilerOptions =
        { typedArrays = true
          clampByteArrays = false
          verbose = false }
    let com = Compiler(filePath, project, options, fableCore)
    com

let compileAst (com: Compiler) (project: Project) =
    FSharp2Fable.Compiler.transformFile com project.ImplementationFiles
    |> FableTransforms.optimizeFile com
    |> Fable2Babel.Compiler.transformFile com

let init () =
  { new IFableManager with
        member __.CreateChecker(references, readAllBytes) =
            InteractiveChecker.Create(references, readAllBytes)
            |> CheckerImpl :> IChecker
        member __.ParseFSharpProject(checker, fileName, source) =
            let c = checker :?> CheckerImpl
            parseFSharpProject c.Checker fileName source :> IParseResults
        member __.GetParseErrors(parseResults:IParseResults) =
            parseResults.Errors
        member __.GetToolTipText(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getToolTipAtLocation res line col lineText
        member __.GetCompletionsAtLocation(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getCompletionsAtLocation res line col lineText
        member __.CompileToBabelAst(fableCore:string, parseResults:IParseResults, fileName:string, optimized: bool) =
            let res = parseResults :?> ParseResults
            let project = makeProject fileName optimized res
            let com = makeCompiler fableCore fileName project
            let ast = compileAst com project
            let errors =
                com.GetLogs()
                |> List.map (fun log ->
                    let r = defaultArg log.Range Fable.SourceLocation.Empty
                    { StartLineAlternate = r.start.line
                      StartColumn = r.start.column
                      EndLineAlternate = r.``end``.line
                      EndColumn = r.``end``.column
                      Message =
                        if log.Tag = "FABLE"
                        then "FABLE: " + log.Message
                        else log.Message
                      IsWarning =
                        match log.Severity with
                        | Fable.Severity.Error -> false
                        | Fable.Severity.Warning
                        | Fable.Severity.Info -> true
                    })
                |> List.toArray
            ast :> obj, Array.append parseResults.Errors errors
        member __.FSharpAstToString(parseResults:IParseResults, optimized: bool) =
            let res = parseResults :?> ParseResults
            if not optimized then res.CheckProject.AssemblyContents.ImplementationFiles
            else res.CheckProject.GetOptimizedAssemblyContents().ImplementationFiles
            |> Seq.collect (fun file -> AstPrint.printFSharpDecls "" file.Declarations)
            |> String.concat "\n"
  }
