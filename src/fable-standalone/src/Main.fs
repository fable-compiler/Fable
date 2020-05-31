module Fable.Standalone.Main

open System
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open FsAutoComplete
open FSharp.Compiler.SourceCodeServices

type CheckerImpl(checker: InteractiveChecker) =
    member __.Checker = checker
    interface IChecker

let mapError (error: FSharpErrorInfo) =
    {
        FileName = error.FileName
        StartLineAlternate = error.StartLineAlternate
        StartColumn = error.StartColumn
        EndLineAlternate = error.EndLineAlternate
        EndColumn = error.EndColumn
        Message = error.Message
        IsWarning =
            match error.Severity with
            | FSharpErrorSeverity.Error -> false
            | FSharpErrorSeverity.Warning -> true
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
        member __.Errors = checkProjectResults.Errors |> Array.map mapError

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
        ExtraProjectInfo = None
        Stamp = None }
    projOptions

let makeProject (projectOptions: FSharpProjectOptions) (projectResults: FSharpCheckProjectResults) =
    // let errors = com.GetFormattedLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let optimized = projectOptions.OtherOptions |> Array.exists ((=) "--optimize+")
    let implFiles =
        (if optimized
        then projectResults.GetOptimizedAssemblyContents()
        else projectResults.AssemblyContents).ImplementationFiles
    let implFilesMap =
        implFiles |> Seq.map (fun file -> Fable.Path.normalizePathAndEnsureFsExtension file.FileName, file) |> dict
    Project(projectOptions, implFilesMap, projectResults.Errors)

let parseFSharpScript (checker: InteractiveChecker) projectFileName fileName source otherFSharpOptions =
    let parseResults, checkResults, projectResults =
        checker.ParseAndCheckScript (projectFileName, fileName, source)
    let projectOptions = makeProjOptions projectFileName [| fileName |] otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseResults (project, Some parseResults, Some checkResults, projectResults, otherFSharpOptions)

let parseFSharpProject (checker: InteractiveChecker) projectFileName fileNames sources otherFSharpOptions =
    let projectResults = checker.ParseAndCheckProject (projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseResults (project, None, None, projectResults, otherFSharpOptions)

let parseFSharpFileInProject (checker: InteractiveChecker) fileName projectFileName fileNames sources otherFSharpOptions =
    let parseResults, checkResultsOpt, projectResults = checker.ParseAndCheckFileInProject (fileName, projectFileName, fileNames, sources)
    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions
    let project = lazy (makeProject projectOptions projectResults)
    ParseResults (project, Some parseResults, checkResultsOpt, projectResults, otherFSharpOptions)

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
let getDeclarationLocation (parseResults: ParseResults) line col lineText = async {
    match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents(col - 1, lineText) with
        | None -> return None
        | Some(col,identIsland) ->
            let! (declarations: FSharpFindDeclResult) = checkFile.GetDeclarationLocation(line, col, lineText, identIsland)
            match declarations with
            | FSharpFindDeclResult.DeclNotFound _
            | FSharpFindDeclResult.ExternalDecl _ ->
                return None
            | FSharpFindDeclResult.DeclFound range ->
                return Some { StartLine = range.StartLine
                              StartColumn = range.StartColumn
                              EndLine = range.EndLine
                              EndColumn = range.EndColumn }
    | None -> return None
}

/// Get tool tip at the specified location
let getToolTipAtLocation (parseResults: ParseResults) line col lineText = async {
    match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents(col - 1, lineText) with
        | None -> return [|"Cannot find ident for tooltip"|]
        | Some(col,identIsland) ->
            let! (FSharpToolTipText els) = checkFile.GetToolTipText(line, col, lineText, identIsland, FSharpTokenTag.Identifier)
            return Seq.map tooltipToString els |> Array.concat
    | None ->
        return [||]
}

let getCompletionsAtLocation (parseResults: ParseResults) (line: int) (col: int) lineText = async {
   match parseResults.CheckFileResultsOpt with
    | Some checkFile ->
        let ln, residue = findLongIdentsAndResidue(col - 1, lineText)
        let longName = FSharp.Compiler.QuickParse.GetPartialLongNameEx(lineText, col - 1)
        let longName = { longName with QualifyingIdents = ln; PartialIdent = residue }
        let! decls = checkFile.GetDeclarationListInfo(parseResults.ParseFileResultsOpt, line, lineText, longName, fun () -> [])
        return decls.Items |> Array.map (fun decl ->
            { Name = decl.Name; Glyph = convertGlyph decl.Glyph })
    | None ->
        return [||]
}

let defaultCompilerConfig: CompilerConfig =
    { typedArrays = true
      clampByteArrays = false
      classTypes = false
      typescript = false
      precompiledLib = None }

let makeCompilerOptions (config: CompilerConfig option) (otherFSharpOptions: string[]): Fable.CompilerOptions =
    let config = defaultArg config defaultCompilerConfig
    let isDebug = otherFSharpOptions |> Array.exists (fun x -> x = "--define:DEBUG" || x = "-d:DEBUG")
    { typedArrays = config.typedArrays
      clampByteArrays = config.clampByteArrays
      classTypes = config.classTypes
      typescript = config.typescript
      debugMode = isDebug
      verbosity = Fable.Verbosity.Normal
      outputPublicInlinedFunctions = false
      precompiledLib = config.precompiledLib }

let compileAst (com: Compiler) (project: Project) =
    FSharp2Fable.Compiler.transformFile com project.ImplementationFiles
    |> FableTransforms.optimizeFile com
    |> Fable2Babel.Compiler.transformFile com

let init () =
  { new IFableManager with
        member __.CreateChecker(references, readAllBytes, otherOptions) =
            InteractiveChecker.Create(references, readAllBytes, otherOptions)
            |> CheckerImpl :> IChecker

        member __.ClearParseCaches(checker) =
            let c = checker :?> CheckerImpl
            c.Checker.ClearCache()

        member __.ParseFSharpScript(checker, fileName, source, ?otherFSharpOptions) =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]
            let projectFileName = "project" // TODO: make it an argument
            parseFSharpScript c.Checker projectFileName fileName source otherFSharpOptions :> IParseResults

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

        member __.CompileToBabelAst(fableLibrary:string, parseResults:IParseResults, fileName:string, ?config) =
            let res = parseResults :?> ParseResults
            let project = res.GetProject()
            let options = makeCompilerOptions config parseResults.OtherFSharpOptions
            let com = Compiler(fileName, project, options, fableLibrary)
            let ast = compileAst com project
            let errors =
                com.GetLogs()
                |> List.map (fun log ->
                    let r = defaultArg log.Range Fable.SourceLocation.Empty
                    { FileName = fileName
                      StartLineAlternate = r.start.line
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
            { new IBabelResult with
                member __.BabelAst = ast :> obj
                member __.FableErrors = errors }

        member __.FSharpAstToString(parseResults:IParseResults, fileName:string) =
            let res = parseResults :?> ParseResults
            let project = res.GetProject()
            let implFile = project.ImplementationFiles.Item(fileName)
            AstPrint.printFSharpDecls "" implFile.Declarations |> String.concat "\n"
  }
