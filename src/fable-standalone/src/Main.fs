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
    member _.Checker = checker
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

type ParseAndCheckResults
                  (project: Lazy<Project>,
                   parseFileResultsOpt: FSharpParseFileResults option,
                   checkFileResultsOpt: FSharpCheckFileResults option,
                   checkProjectResults: FSharpCheckProjectResults,
                   otherFSharpOptions: string[]) =

    member _.GetProject () = project.Force()
    member _.ParseFileResultsOpt = parseFileResultsOpt
    member _.CheckFileResultsOpt = checkFileResultsOpt
    member _.CheckProjectResults = checkProjectResults

    interface IParseAndCheckResults with
        member _.OtherFSharpOptions = otherFSharpOptions
        member _.Errors = checkProjectResults.Diagnostics |> Array.map mapError

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

let makeCompiler fableLibrary typedArrays language fsharpOptions project fileName: CompilerImpl =
    let define = fsharpOptions |> Array.choose (fun (x: string) ->
        if x.StartsWith("--define:") || x.StartsWith("-d:")
        then x.[(x.IndexOf(':') + 1)..] |> Some
        else None) |> Array.toList
    let options = Fable.CompilerOptionsHelper.Make(language=language, define=define, ?typedArrays=typedArrays)
    CompilerImpl(fileName, project, options, fableLibrary)

let makeProject (projectOptions: FSharpProjectOptions) (checkResults: FSharpCheckProjectResults) =
    // let errors = com.GetFormattedLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let optimize = projectOptions.OtherOptions |> Array.exists ((=) "--optimize+")
    let implFiles =
        if optimize then checkResults.GetOptimizedAssemblyContents().ImplementationFiles
        else checkResults.AssemblyContents.ImplementationFiles
    Project.From(
        projectOptions.ProjectFileName,
        implFiles,
        checkResults.ProjectContext.GetReferencedAssemblies())

let parseAndCheckProject (checker: InteractiveChecker) projectFileName fileNames sources otherFSharpOptions =
    let checkResults = checker.ParseAndCheckProject (projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions checkResults)
    ParseAndCheckResults (project, None, None, checkResults, otherFSharpOptions)

let parseAndCheckFileInProject (checker: InteractiveChecker) fileName projectFileName fileNames sources otherFSharpOptions =
    let results, checkResults, projectResults = checker.ParseAndCheckFileInProject (fileName, projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseAndCheckResults (project, Some results, Some checkResults, projectResults, otherFSharpOptions)

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
let getDeclarationLocation (results: ParseAndCheckResults) line col lineText =
    match results.CheckFileResultsOpt with
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
let getToolTipAtLocation (results: ParseAndCheckResults) line col lineText =
    match results.CheckFileResultsOpt with
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

let getCompletionsAtLocation (results: ParseAndCheckResults) (line: int) (col: int) lineText =
   match results.CheckFileResultsOpt with
    | Some checkFile ->
        let ln, residue = findLongIdentsAndResidue(col - 1, lineText)
        let longName = QuickParse.GetPartialLongNameEx(lineText, col - 1)
        let longName = { longName with QualifyingIdents = ln; PartialIdent = residue }
        let decls = checkFile.GetDeclarationListInfo(results.ParseFileResultsOpt, line, lineText, longName, fun () -> [])
        decls.Items |> Array.map (fun decl ->
            { Name = decl.Name; Glyph = convertGlyph decl.Glyph })
    | None ->
        [||]

let compileToFableAst (results: IParseAndCheckResults) fileName fableLibrary typedArrays language =
    let res = results :?> ParseAndCheckResults
    let project = res.GetProject()
    let com = makeCompiler fableLibrary typedArrays language results.OtherFSharpOptions project fileName
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
    interface IFableResult with
        member _.FableErrors = errors

type RustResult(crate: Rust.AST.Types.Crate, errors) =
    member _.Crate = crate
    interface IFableResult with
        member _.FableErrors = errors

let printBabelAst (babel: BabelResult) (writer: IWriter) =
    let writer =
        { new BabelPrinter.Writer with
            member _.Dispose() = writer.Dispose()
            member _.EscapeJsStringLiteral(str) = writer.EscapeJsStringLiteral(str)
            member _.MakeImportPath(path) = writer.MakeImportPath(path)
            member _.AddSourceMapping(mapping) = writer.AddSourceMapping(mapping)
            member _.Write(str) = writer.Write(str) }

    BabelPrinter.run writer babel.Program

let printRustAst (rust: RustResult) (writer: IWriter) =
    let writer =
        { new Rust.RustPrinter.Writer with
            member _.Dispose() = writer.Dispose()
            member _.MakeImportPath(path) = writer.MakeImportPath(path)
            member _.AddSourceMapping(mapping) = writer.AddSourceMapping(mapping)
            member _.Write(str) = writer.Write(str) }

    Rust.RustPrinter.run writer rust.Crate

let getLanguage (language: string) =
    match language.ToLowerInvariant() with
    | "js" | "javascript" -> JavaScript
    | "ts" | "typescript" -> TypeScript
    | "py" | "python" -> Python
    | "php" -> Php
    | "dart" -> Dart
    | "rust" -> Rust
    | _ -> failwithf "Unsupported language: %s" language

let init () =
  { new IFableManager with
        member _.Version = Fable.Literals.VERSION

        member _.CreateChecker(references, readAllBytes, otherOptions) =
            InteractiveChecker.Create(references, readAllBytes, otherOptions)
            |> CheckerImpl :> IChecker

        member _.ClearCache(checker) =
            let c = checker :?> CheckerImpl
            c.Checker.ClearCache()

        member _.ParseAndCheckProject(checker, projectFileName, fileNames, sources, ?otherFSharpOptions) =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]
            parseAndCheckProject c.Checker projectFileName fileNames sources otherFSharpOptions :> IParseAndCheckResults

        member _.ParseAndCheckFileInProject(checker, fileName, projectFileName, fileNames, sources, ?otherFSharpOptions) =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]
            parseAndCheckFileInProject c.Checker fileName projectFileName fileNames sources otherFSharpOptions :> IParseAndCheckResults

        member _.GetErrors(results:IParseAndCheckResults) =
            results.Errors

        member _.GetDeclarationLocation(results:IParseAndCheckResults, line:int, col:int, lineText:string) =
            let res = results :?> ParseAndCheckResults
            getDeclarationLocation res line col lineText

        member _.GetToolTipText(results:IParseAndCheckResults, line:int, col:int, lineText:string) =
            let res = results :?> ParseAndCheckResults
            getToolTipAtLocation res line col lineText

        member _.GetCompletionsAtLocation(results:IParseAndCheckResults, line:int, col:int, lineText:string) =
            let res = results :?> ParseAndCheckResults
            getCompletionsAtLocation res line col lineText

        member _.CompileToTargetAst(fableLibrary:string, results:IParseAndCheckResults, fileName:string, typedArrays, language) =
            let language = getLanguage language
            let typedArrays =
                if language = JavaScript then typedArrays else None // not used for other languages

            let com, fableAst, errors =
                compileToFableAst results fileName fableLibrary typedArrays language

            match language with
            | JavaScript | TypeScript ->
                let babelAst = fableAst |> Fable2Babel.Compiler.transformFile com
                upcast BabelResult(babelAst, errors)
            | Rust ->
                let rustAst = fableAst |> Rust.Fable2Rust.Compiler.transformFile com
                upcast RustResult(rustAst, errors)
            // TODO: add other languages
            | _ -> failwithf "Unsupported language: %A" language

        member _.PrintTargetAst(fableResult, writer) =
            match fableResult with
            | :? BabelResult as babel -> printBabelAst babel writer
            | :? RustResult as rust -> printRustAst rust writer
            // TODO: add other languages
            | _ -> failwith "Unexpected Fable result"

        member _.FSharpAstToString(results:IParseAndCheckResults, fileName:string) =
            let res = results :?> ParseAndCheckResults
            let project = res.GetProject()
            let implFile = project.ImplementationFiles.Item(fileName)
            AstPrint.printFSharpDecls "" implFile.Declarations |> String.concat "\n"
  }
