module Fable.Standalone.Main

open System
open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open FsAutoComplete
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols

type CheckerImpl(checker: InteractiveChecker) =
    member __.Checker = checker
    interface IChecker

let mapError (error: FSharpDiagnostic) =
    {
        FileName = error.FileName
        StartLine = error.StartLine
        StartColumn = error.StartColumn
        EndLine = error.EndLine
        EndColumn = error.EndColumn
        Message = error.Message
        IsWarning =
            match error.Severity with
            | FSharpDiagnosticSeverity.Info
            | FSharpDiagnosticSeverity.Hidden
            | FSharpDiagnosticSeverity.Warning -> true
            | FSharpDiagnosticSeverity.Error -> false
    }

type ParseResults (project: Lazy<Project>,
                   parseFileResultsOpt: FSharpParseFileResults option,
                   checkFileResultsOpt: FSharpCheckFileResults option,
                   checkProjectResults: FSharpCheckProjectResults,
                   otherFSharpOptions: string[]) =

    member __.GetProject () = project.Force()
    member __.ParseFileResultsOpt = parseFileResultsOpt
    member __.CheckFileResultsOpt = checkFileResultsOpt
    member __.CheckProjectResults = checkProjectResults

    interface IParseResults with
        member __.OtherFSharpOptions = otherFSharpOptions
        member __.Errors = checkProjectResults.Diagnostics |> Array.map mapError

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

let makeProjOptions projectFileName fileNames otherFSharpOptions =
    let projOptions: FSharpProjectOptions =
      { ProjectFileName = projectFileName
        ProjectId = None
        SourceFiles = fileNames
        OtherOptions = otherFSharpOptions
        ReferencedProjects = [| |]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None }
    projOptions

let makeProject (projectOptions: FSharpProjectOptions) (projectResults: FSharpCheckProjectResults) =
    // let errors = com.GetFormattedLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let optimize = projectOptions.OtherOptions |> Array.exists ((=) "--optimize+")
    Project(projectOptions.ProjectFileName, projectResults, optimizeFSharpAst=optimize)

let parseFSharpProject (checker: InteractiveChecker) projectFileName fileNames sources otherFSharpOptions =
    let projectResults = checker.ParseAndCheckProject (projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseResults (project, None, None, projectResults, otherFSharpOptions)

let parseFSharpFileInProject (checker: InteractiveChecker) fileName projectFileName fileNames sources otherFSharpOptions =
    let parseResults, checkResults, projectResults = checker.ParseAndCheckFileInProject (fileName, projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseResults (project, Some parseResults, Some checkResults, projectResults, otherFSharpOptions)

let tooltipToString (el: ToolTipElement): string[] =
    let dataToString (data: ToolTipElementData) =
        let toString (tts: FSharp.Compiler.Text.TaggedText[]) =
            tts |> Array.map (fun x -> x.Text) |> String.concat " "
        [| match data.ParamName with
           | Some x -> yield x + ": "
           | None -> ()
           yield data.MainDescription |> toString
           match data.XmlDoc with
           | FSharp.Compiler.Symbols.FSharpXmlDoc.FromXmlText xmlDoc ->
                yield! xmlDoc.UnprocessedLines
                yield! xmlDoc.GetElaboratedXmlLines()
           | _ -> ()
           yield! data.TypeMapping |> List.map toString
           match data.Remarks with
           | Some x -> yield x |> toString
           | None -> ()
        |]
    match el with
    | ToolTipElement.None -> [||]
    | ToolTipElement.Group(els) ->
        Seq.map dataToString els |> Array.concat
    | ToolTipElement.CompositionError err -> [|err|]

/// Get tool tip at the specified location
let getDeclarationLocation (parseResults: ParseResults) line col lineText =
    match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents(col - 1, lineText) with
        | None -> None
        | Some(col,identIsland) ->
            let (declarations: FindDeclResult) =
                checkFile.GetDeclarationLocation(line, col, lineText, identIsland)
            match declarations with
            | FindDeclResult.DeclNotFound _
            | FindDeclResult.ExternalDecl _ ->
                None
            | FindDeclResult.DeclFound range ->
                Some { StartLine = range.StartLine
                       StartColumn = range.StartColumn
                       EndLine = range.EndLine
                       EndColumn = range.EndColumn }
    | None -> None

/// Get tool tip at the specified location
let getToolTipAtLocation (parseResults: ParseResults) line col lineText =
    match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents(col - 1, lineText) with
        | None ->
            [|"Cannot find ident for tooltip"|]
        | Some(col,identIsland) ->
            let (ToolTipText els) =
                checkFile.GetToolTip(line, col, lineText, identIsland,
                    FSharp.Compiler.Tokenization.FSharpTokenTag.IDENT)
            Seq.map tooltipToString els |> Array.concat
    | None ->
        [||]

let getCompletionsAtLocation (parseResults: ParseResults) (line: int) (col: int) lineText =
   match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        let ln, residue = findLongIdentsAndResidue(col - 1, lineText)
        let longName = QuickParse.GetPartialLongNameEx(lineText, col - 1)
        let longName = { longName with QualifyingIdents = ln; PartialIdent = residue }
        let decls = checkFile.GetDeclarationListInfo(parseResults.ParseFileResultsOpt, line, lineText, longName, fun () -> [])
        decls.Items |> Array.map (fun decl ->
            { Name = decl.Name; Glyph = convertGlyph decl.Glyph })
    | None ->
        [||]

let compileToFableAst (parseResults: IParseResults) fileName fableLibrary typedArrays language =
    let res = parseResults :?> ParseResults
    let project = res.GetProject()
    let define = parseResults.OtherFSharpOptions |> Array.choose (fun x ->
        if x.StartsWith("--define:") || x.StartsWith("-d:")
        then x.[(x.IndexOf(':') + 1)..] |> Some
        else None) |> Array.toList
    let options = Fable.CompilerOptionsHelper.Make(language=language, define=define, ?typedArrays=typedArrays)
    let outputType = OutputType.Library // TODO: This is the default. Fix to actual output type
    let com = CompilerImpl(fileName, project, options, outputType, fableLibrary)
    let fableAst =
        FSharp2Fable.Compiler.transformFile com
        |> FableTransforms.transformFile com
    let errors =
        com.Logs |> Array.map (fun log ->
            let r = defaultArg log.Range Fable.AST.SourceLocation.Empty
            {
                FileName = fileName
                StartLine = r.start.line
                StartColumn = r.start.column
                EndLine = r.``end``.line
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
    (com, fableAst, errors)

type BabelResult(program: Babel.Program, errors) =
    member _.Program = program
    interface IBabelResult with
        member _.FableErrors = errors

let init () =
  { new IFableManager with
        member __.Version = Fable.Literals.VERSION

        member __.CreateChecker(references, readAllBytes, otherOptions) =
            InteractiveChecker.Create(references, readAllBytes, otherOptions)
            |> CheckerImpl :> IChecker

        member __.ClearParseCaches(checker) =
            let c = checker :?> CheckerImpl
            c.Checker.ClearCache()

        member __.ParseFSharpProject(checker, projectFileName, fileNames, sources, ?otherFSharpOptions) =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]
            parseFSharpProject c.Checker projectFileName fileNames sources otherFSharpOptions :> IParseResults

        member __.ParseFSharpFileInProject(checker, fileName, projectFileName, fileNames, sources, ?otherFSharpOptions) =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]
            parseFSharpFileInProject c.Checker fileName projectFileName fileNames sources otherFSharpOptions :> IParseResults

        member __.GetParseErrors(parseResults:IParseResults) =
            parseResults.Errors

        member __.GetDeclarationLocation(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getDeclarationLocation res line col lineText

        member __.GetToolTipText(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getToolTipAtLocation res line col lineText

        member __.GetCompletionsAtLocation(parseResults:IParseResults, line:int, col:int, lineText:string) =
            let res = parseResults :?> ParseResults
            getCompletionsAtLocation res line col lineText

        member __.CompileToBabelAst(fableLibrary:string, parseResults:IParseResults, fileName:string,
                                    ?typedArrays, ?typescript) =
            let language = match typescript with | Some true -> TypeScript | _ -> JavaScript
            let com, fableAst, errors =
                compileToFableAst parseResults fileName fableLibrary typedArrays language
            let babelAst =
                fableAst |> Fable2Babel.Compiler.transformFile com
            upcast BabelResult(babelAst, errors)

        member _.PrintBabelAst(babelResult, writer) =
            match babelResult with
            | :? BabelResult as babel ->
                let writer =
                    { new BabelPrinter.Writer with
                        member _.Dispose() = writer.Dispose()
                        member _.EscapeJsStringLiteral(str) = writer.EscapeJsStringLiteral(str)
                        member _.MakeImportPath(path) = writer.MakeImportPath(path)
                        member _.AddSourceMapping(mapping) = writer.AddSourceMapping(mapping)
                        member _.Write(str) = writer.Write(str) }

                BabelPrinter.run writer babel.Program
            | _ ->
                failwith "Unexpected Babel result"

        member __.FSharpAstToString(parseResults:IParseResults, fileName:string) =
            let res = parseResults :?> ParseResults
            let project = res.GetProject()
            let implFile = project.ImplementationFiles.Item(fileName)
            AstPrint.printFSharpDecls "" implFile.Ast.Declarations |> String.concat "\n"
  }
