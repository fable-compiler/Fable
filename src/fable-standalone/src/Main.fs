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
open FSharp.Compiler.Tokenization

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
        Severity =
            match error.Severity with
            | FSharpDiagnosticSeverity.Hidden -> DiagnosticSeverity.Hidden
            | FSharpDiagnosticSeverity.Info -> DiagnosticSeverity.Info
            | FSharpDiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
            | FSharpDiagnosticSeverity.Error -> DiagnosticSeverity.Error
        Code = error.ErrorNumberText
    }

type ParseAndCheckResults
    (
        project: Lazy<Project>,
        projectLogs: ResizeArray<LogEntry>,
        parseFileResultsOpt: FSharpParseFileResults option,
        checkFileResultsOpt: FSharpCheckFileResults option,
        checkProjectResults: FSharpCheckProjectResults,
        otherFSharpOptions: string[]
    )
    =

    member _.GetProject() = project.Force()

    member _.TakeProjectLogs() =
        let logs = projectLogs.ToArray()
        projectLogs.Clear()
        logs

    member _.ParseFileResultsOpt = parseFileResultsOpt
    member _.CheckFileResultsOpt = checkFileResultsOpt
    member _.CheckProjectResults = checkProjectResults

    interface IParseAndCheckResults with
        member _.OtherFSharpOptions = otherFSharpOptions
        member _.Errors = checkProjectResults.Diagnostics |> Array.map mapError

let inline private tryGetLexerSymbolIslands (sym: Lexer.LexerSymbol) =
    match sym.Text with
    | "" -> None
    | _ -> Some(sym.RightColumn, sym.Text.Split '.' |> Array.toList)

// Parsing - find the identifier around the current location
// (we look for full identifier in the backward direction, but only
// for a short identifier forward - this means that when you hover
// 'B' in 'A.B.C', you will get intellisense for 'A.B' module)
let findIdents col lineStr lookupType =
    if lineStr = "" then
        None
    else
        Lexer.getSymbol 0 col lineStr lookupType [||]
        |> Option.bind tryGetLexerSymbolIslands

let findLongIdents (col, lineStr) =
    findIdents col lineStr Lexer.SymbolLookupKind.Fuzzy

let findLongIdentsAndResidue (col: int, lineStr: string) =
    let lineStr = lineStr.Substring(0, col)

    match Lexer.getSymbol 0 col lineStr Lexer.SymbolLookupKind.ByLongIdent [||] with
    | Some sym ->
        match sym.Text with
        | "" -> [], ""
        | text ->
            let res = text.Split '.' |> List.ofArray |> List.rev

            if lineStr[col - 1] = '.' then
                res |> List.rev, ""
            else
                match res with
                | head :: tail -> tail |> List.rev, head
                | [] -> [], ""
    | _ -> [], ""

let convertGlyph glyph =
    match glyph with
    | FSharpGlyph.Class
    | FSharpGlyph.Struct
    | FSharpGlyph.Union
    | FSharpGlyph.Type
    | FSharpGlyph.Typedef -> Glyph.Class
    | FSharpGlyph.Enum
    | FSharpGlyph.EnumMember -> Glyph.Enum
    | FSharpGlyph.Constant -> Glyph.Value
    | FSharpGlyph.Variable -> Glyph.Variable
    | FSharpGlyph.Interface -> Glyph.Interface
    | FSharpGlyph.Module
    | FSharpGlyph.NameSpace -> Glyph.Module
    | FSharpGlyph.Method
    | FSharpGlyph.OverridenMethod
    | FSharpGlyph.ExtensionMethod -> Glyph.Method
    | FSharpGlyph.Property -> Glyph.Property
    | FSharpGlyph.Field -> Glyph.Field
    | FSharpGlyph.Delegate -> Glyph.Function
    | FSharpGlyph.Error
    | FSharpGlyph.Exception -> Glyph.Error
    | FSharpGlyph.Event -> Glyph.Event
    | FSharpGlyph.TypeParameter -> Glyph.TypeParameter

let makeProjOptions projectFileName fileNames otherFSharpOptions =
    let projOptions: FSharpProjectOptions =
        {
            ProjectFileName = projectFileName
            ProjectId = None
            SourceFiles = fileNames
            OtherOptions = otherFSharpOptions
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }

    projOptions

let makeCompiler fableLibrary typedArrays language fsharpOptions project fileName : CompilerImpl =
    let define =
        fsharpOptions
        |> Array.choose (fun (x: string) ->
            if x.StartsWith("--define:") || x.StartsWith("-d:") then
                x[(x.IndexOf(':') + 1) ..] |> Some
            else
                None
        )
        |> Array.toList

    let debugMode = define |> List.contains "DEBUG"

    let options =
        Fable.CompilerOptionsHelper.Make(
            language = language,
            define = define,
            debugMode = debugMode,
            ?typedArrays = typedArrays
        )

    // No `warningSuppression`: the REPL has no `SourceReader` to scan, so `// fable-disable` comments are a no-op here.
    CompilerImpl(fileName, project, options, fableLibrary)

/// `fable precompile` sorts inline expressions by member name, splits them into chunks and records
/// the first name of each, so the chunk holding a name is the last header not sorting after it.
let private findInlineExprsChunk (headers: string[]) (memberUniqueName: string) =
    let mutable low = 0
    let mutable high = headers.Length - 1
    let mutable found = -1

    while low <= high do
        let mid = (low + high) / 2

        if String.CompareOrdinal(headers[mid], memberUniqueName) <= 0 then
            found <- mid
            low <- mid + 1
        else
            high <- mid - 1

    found

let private toCompilerPrecompiledInfo (info: IPrecompiledInfo) =
    let decodedChunks =
        Collections.Generic.Dictionary<int, Map<string, Fable.InlineExpr>>()

    { new PrecompiledInfo with
        member _.DllPath = info.DllPath

        member _.TryGetRootModule(normalizedFullPath) =
            info.TryGetRootModule(normalizedFullPath)

        member _.TryGetInlineExpr(memberUniqueName) =
            match findInlineExprsChunk info.InlineExprHeaders memberUniqueName with
            | -1 -> None
            | index ->
                let chunk =
                    match decodedChunks.TryGetValue(index) with
                    | true, chunk -> chunk
                    | false, _ ->
                        let chunk =
                            match Fable.BrowserInlineExprs.fromString (info.ReadInlineExprsChunk index) with
                            | Ok exprs -> Map.ofArray exprs
                            | Error error ->
                                failwith
                                    $"Cannot read the precompiled library's inline expressions (chunk %d{index}): %s{error}"

                        decodedChunks[index] <- chunk
                        chunk

                Map.tryFind memberUniqueName chunk
    }

let makeProject
    (projectOptions: FSharpProjectOptions)
    (checkResults: FSharpCheckProjectResults)
    (precompiledInfo: IPrecompiledInfo option)
    (logs: ResizeArray<LogEntry>)
    =
    // let errors = com.GetFormattedLogs() |> Map.tryFind "error"
    // if errors.IsSome then failwith (errors.Value |> String.concat "\n")
    let optimize = projectOptions.OtherOptions |> Array.exists ((=) "--optimize+")

    let implFiles =
        if optimize then
            checkResults.GetOptimizedAssemblyContents().ImplementationFiles
        else
            checkResults.AssemblyContents.ImplementationFiles

    let addLog (severity: Fable.Severity) (msg: string) = logs.Add(LogEntry.Make(severity, msg))

    Project.From(
        projectOptions.ProjectFileName,
        projectOptions,
        implFiles,
        checkResults.ProjectContext.GetReferencedAssemblies(),
        addLog,
        ?precompiledInfo = (precompiledInfo |> Option.map toCompilerPrecompiledInfo)
    )

let parseAndCheckProject
    (checker: InteractiveChecker)
    projectFileName
    fileNames
    sources
    otherFSharpOptions
    precompiledInfo
    =
    let checkResults = checker.ParseAndCheckProject(projectFileName, fileNames, sources)

    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions

    let projectLogs = ResizeArray<LogEntry>()

    let project =
        lazy (makeProject projectOptions checkResults precompiledInfo projectLogs)

    ParseAndCheckResults(project, projectLogs, None, None, checkResults, otherFSharpOptions)

let parseAndCheckFileInProject
    (checker: InteractiveChecker)
    fileName
    projectFileName
    fileNames
    sources
    otherFSharpOptions
    precompiledInfo
    =
    let results, checkResults, projectResults =
        checker.ParseAndCheckFileInProject(fileName, projectFileName, fileNames, sources)

    let projectOptions = makeProjOptions projectFileName fileNames otherFSharpOptions

    let projectLogs = ResizeArray<LogEntry>()

    let project =
        lazy (makeProject projectOptions projectResults precompiledInfo projectLogs)

    ParseAndCheckResults(project, projectLogs, Some results, Some checkResults, projectResults, otherFSharpOptions)

type private FcsTextTag = FSharp.Compiler.Text.TextTag

let private toTextTag (tag: FcsTextTag) =
    match tag with
    | FcsTextTag.ActivePatternCase -> TextTag.ActivePatternCase
    | FcsTextTag.ActivePatternResult -> TextTag.ActivePatternResult
    | FcsTextTag.Alias -> TextTag.Alias
    | FcsTextTag.Class -> TextTag.Class
    | FcsTextTag.Union -> TextTag.Union
    | FcsTextTag.UnionCase -> TextTag.UnionCase
    | FcsTextTag.Delegate -> TextTag.Delegate
    | FcsTextTag.Enum -> TextTag.Enum
    | FcsTextTag.Event -> TextTag.Event
    | FcsTextTag.Field -> TextTag.Field
    | FcsTextTag.Interface -> TextTag.Interface
    | FcsTextTag.Keyword -> TextTag.Keyword
    | FcsTextTag.LineBreak -> TextTag.LineBreak
    | FcsTextTag.Local -> TextTag.Local
    | FcsTextTag.Record -> TextTag.Record
    | FcsTextTag.RecordField -> TextTag.RecordField
    | FcsTextTag.Method -> TextTag.Method
    | FcsTextTag.Member -> TextTag.Member
    | FcsTextTag.ModuleBinding -> TextTag.ModuleBinding
    | FcsTextTag.Function -> TextTag.Function
    | FcsTextTag.Module -> TextTag.Module
    | FcsTextTag.Namespace -> TextTag.Namespace
    | FcsTextTag.NumericLiteral -> TextTag.NumericLiteral
    | FcsTextTag.Operator -> TextTag.Operator
    | FcsTextTag.Parameter -> TextTag.Parameter
    | FcsTextTag.Property -> TextTag.Property
    | FcsTextTag.Space -> TextTag.Space
    | FcsTextTag.StringLiteral -> TextTag.StringLiteral
    | FcsTextTag.Struct -> TextTag.Struct
    | FcsTextTag.TypeParameter -> TextTag.TypeParameter
    | FcsTextTag.Text -> TextTag.Text
    | FcsTextTag.Punctuation -> TextTag.Punctuation
    | FcsTextTag.UnknownType -> TextTag.UnknownType
    | FcsTextTag.UnknownEntity -> TextTag.UnknownEntity

let private toTaggedText (tts: FSharp.Compiler.Text.TaggedText[]) : TaggedText[] =
    tts
    |> Array.map (fun x ->
        {
            Tag = toTextTag x.Tag
            Text = x.Text
        }
    )

let private xmlDocLines (xmlDoc: FSharpXmlDoc) =
    match xmlDoc with
    | FSharpXmlDoc.FromXmlText xmlDoc -> xmlDoc.GetElaboratedXmlLines()
    | _ -> [||]

let toToolTips (el: ToolTipElement) : Fable.Standalone.ToolTip[] =
    let toToolTip (data: ToolTipElementData) : Fable.Standalone.ToolTip =
        {
            ParamName = data.ParamName
            Signature = toTaggedText data.MainDescription
            XmlDoc = xmlDocLines data.XmlDoc
            TypeMapping = data.TypeMapping |> List.map toTaggedText |> List.toArray
            Remarks = data.Remarks |> Option.map toTaggedText |> Option.defaultValue [||]
        }

    match el with
    | ToolTipElement.None -> [||]
    | ToolTipElement.Group(els) -> els |> List.map toToolTip |> List.toArray
    | ToolTipElement.CompositionError err ->
        [|
            {
                ParamName = None
                Signature =
                    [|
                        {
                            Tag = TextTag.Text
                            Text = err
                        }
                    |]
                XmlDoc = [||]
                TypeMapping = [||]
                Remarks = [||]
            }
        |]

let private declarationNotFoundReason (reason: FindDeclFailureReason) =
    match reason with
    | FindDeclFailureReason.Unknown message -> message
    | FindDeclFailureReason.NoSourceCode -> "no source code"
    | FindDeclFailureReason.ProvidedType typeName -> "provided type " + typeName
    | FindDeclFailureReason.ProvidedMember memberName -> "provided member " + memberName

let private toRange (range: FSharp.Compiler.Text.range) : Range =
    {
        FileName = range.FileName
        StartLine = range.StartLine
        StartColumn = range.StartColumn
        EndLine = range.EndLine
        EndColumn = range.EndColumn
    }

type private SymbolImpl(symbol: FSharpSymbol) =
    member _.Symbol = symbol

    interface ISymbol with
        member _.DisplayName = symbol.DisplayName

let getSymbolAtLocation (results: ParseAndCheckResults) line col lineText =
    match results.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents (col, lineText) with
        | None -> None
        | Some(col, identIsland) ->
            checkFile.GetSymbolUseAtLocation(line, col, lineText, identIsland)
            |> Option.map (fun symbolUse -> SymbolImpl(symbolUse.Symbol) :> ISymbol)
    | None -> None

let getUsesOfSymbolInFile (results: ParseAndCheckResults) (symbol: ISymbol) =
    match results.CheckFileResultsOpt, symbol with
    | Some checkFile, (:? SymbolImpl as symbol) ->
        checkFile.GetUsesOfSymbolInFile(symbol.Symbol)
        |> Array.map (fun symbolUse ->
            {
                Range = toRange symbolUse.Range
                IsDefinition = symbolUse.IsFromDefinition
            }
        )
    | _ -> [||]

/// Get the declaration location of the symbol at the specified location
let getDeclarationLocation (results: ParseAndCheckResults) line col lineText =
    match results.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents (col, lineText) with
        | None -> DeclarationLocation.NotFound "no identifier at location"
        | Some(col, identIsland) ->
            let (declarations: FindDeclResult) =
                checkFile.GetDeclarationLocation(line, col, lineText, identIsland)

            match declarations with
            | FindDeclResult.DeclNotFound reason -> DeclarationLocation.NotFound(declarationNotFoundReason reason)
            | FindDeclResult.ExternalDecl(assembly, externalSym) ->
                DeclarationLocation.External(assembly, string externalSym)
            | FindDeclResult.DeclFound range -> DeclarationLocation.Found(toRange range)
    | None -> DeclarationLocation.NotFound "file not checked"

let private toTokenColor (color: FSharpTokenColorKind) =
    match color with
    | FSharpTokenColorKind.Keyword -> TokenColor.Keyword
    | FSharpTokenColorKind.Comment -> TokenColor.Comment
    | FSharpTokenColorKind.Identifier -> TokenColor.Identifier
    | FSharpTokenColorKind.String -> TokenColor.String
    | FSharpTokenColorKind.UpperIdentifier -> TokenColor.UpperIdentifier
    | FSharpTokenColorKind.InactiveCode -> TokenColor.InactiveCode
    | FSharpTokenColorKind.PreprocessorKeyword -> TokenColor.PreprocessorKeyword
    | FSharpTokenColorKind.Number -> TokenColor.Number
    | FSharpTokenColorKind.Operator -> TokenColor.Operator
    | FSharpTokenColorKind.Punctuation -> TokenColor.Punctuation
    | _ -> TokenColor.Default

let tokenize (source: string) (defines: string[]) : Token[][] =
    let tokenizer = FSharpSourceTokenizer(List.ofArray defines, None, None, None)

    let tokenizeLine (state: FSharpTokenizerLexState) (line: string) =
        let lineTokenizer = tokenizer.CreateLineTokenizer(line.TrimEnd('\r'))

        let rec loop state acc =
            match lineTokenizer.ScanToken state with
            | Some token, state ->
                let token: Token =
                    {
                        StartColumn = token.LeftColumn
                        EndColumn = token.RightColumn + 1
                        Color = toTokenColor token.ColorClass
                        Name = token.TokenName
                    }

                loop state (token :: acc)
            | None, state -> List.rev acc |> List.toArray, state

        loop state []

    source.Split('\n')
    |> Array.mapFold tokenizeLine FSharpTokenizerLexState.Initial
    |> fst

let private toSemanticClass (classification: SemanticClassificationType) =
    match classification with
    | SemanticClassificationType.ReferenceType -> SemanticClass.ReferenceType
    | SemanticClassificationType.ValueType -> SemanticClass.ValueType
    | SemanticClassificationType.UnionCase -> SemanticClass.UnionCase
    | SemanticClassificationType.UnionCaseField -> SemanticClass.UnionCaseField
    | SemanticClassificationType.Function -> SemanticClass.Function
    | SemanticClassificationType.Property -> SemanticClass.Property
    | SemanticClassificationType.MutableVar -> SemanticClass.MutableVar
    | SemanticClassificationType.Module -> SemanticClass.Module
    | SemanticClassificationType.Namespace -> SemanticClass.Namespace
    | SemanticClassificationType.Printf -> SemanticClass.Printf
    | SemanticClassificationType.ComputationExpression -> SemanticClass.ComputationExpression
    | SemanticClassificationType.IntrinsicFunction -> SemanticClass.IntrinsicFunction
    | SemanticClassificationType.Enumeration -> SemanticClass.Enumeration
    | SemanticClassificationType.Interface -> SemanticClass.Interface
    | SemanticClassificationType.TypeArgument -> SemanticClass.TypeArgument
    | SemanticClassificationType.Operator -> SemanticClass.Operator
    | SemanticClassificationType.DisposableType -> SemanticClass.DisposableType
    | SemanticClassificationType.DisposableTopLevelValue -> SemanticClass.DisposableTopLevelValue
    | SemanticClassificationType.DisposableLocalValue -> SemanticClass.DisposableLocalValue
    | SemanticClassificationType.Method -> SemanticClass.Method
    | SemanticClassificationType.ExtensionMethod -> SemanticClass.ExtensionMethod
    | SemanticClassificationType.ConstructorForReferenceType -> SemanticClass.ConstructorForReferenceType
    | SemanticClassificationType.ConstructorForValueType -> SemanticClass.ConstructorForValueType
    | SemanticClassificationType.Literal -> SemanticClass.Literal
    | SemanticClassificationType.RecordField -> SemanticClass.RecordField
    | SemanticClassificationType.MutableRecordField -> SemanticClass.MutableRecordField
    | SemanticClassificationType.RecordFieldAsFunction -> SemanticClass.RecordFieldAsFunction
    | SemanticClassificationType.Exception -> SemanticClass.Exception
    | SemanticClassificationType.Field -> SemanticClass.Field
    | SemanticClassificationType.Event -> SemanticClass.Event
    | SemanticClassificationType.Delegate -> SemanticClass.Delegate
    | SemanticClassificationType.NamedArgument -> SemanticClass.NamedArgument
    | SemanticClassificationType.Value -> SemanticClass.Value
    | SemanticClassificationType.LocalValue -> SemanticClass.LocalValue
    | SemanticClassificationType.Type -> SemanticClass.Type
    | SemanticClassificationType.TypeDef -> SemanticClass.TypeDef
    | _ -> SemanticClass.Plaintext

let getSemanticClassification (results: ParseAndCheckResults) : SemanticClassification[] =
    match results.CheckFileResultsOpt with
    | Some checkFile ->
        checkFile.GetSemanticClassification(None)
        |> Array.map (fun item ->
            {
                Range = toRange item.Range
                Type = toSemanticClass item.Type
            }
        )
    | None -> [||]

/// Get tool tip at the specified location
let getToolTipAtLocation (results: ParseAndCheckResults) line col lineText =
    match results.CheckFileResultsOpt with
    | Some checkFile ->
        match findLongIdents (col, lineText) with
        | None -> [||]
        | Some(col, identIsland) ->
            let (ToolTipText els) =
                checkFile.GetToolTip(
                    line,
                    col,
                    lineText,
                    identIsland,
                    FSharp.Compiler.Tokenization.FSharpTokenTag.IDENT
                )

            List.map toToolTips els |> Array.concat
    | None -> [||]

let private toCompletionKind (kind: CompletionItemKind) =
    match kind with
    | CompletionItemKind.SuggestedName -> CompletionKind.SuggestedName
    | CompletionItemKind.Field -> CompletionKind.Field
    | CompletionItemKind.Property -> CompletionKind.Property
    | CompletionItemKind.Method false -> CompletionKind.Method
    | CompletionItemKind.Method true -> CompletionKind.ExtensionMethod
    | CompletionItemKind.Event -> CompletionKind.Event
    | CompletionItemKind.Argument -> CompletionKind.Argument
    | CompletionItemKind.CustomOperation -> CompletionKind.CustomOperation
    | CompletionItemKind.Other -> CompletionKind.Other

let private toCompletionItem (decl: DeclarationListItem) : Fable.Standalone.CompletionItem =
    {
        Completion =
            {
                NameInList = decl.NameInList
                NameInCode = decl.NameInCode
                Glyph = convertGlyph decl.Glyph
                Kind = toCompletionKind decl.Kind
                FullName = decl.FullName
                IsOwnMember = decl.IsOwnMember
                MinorPriority = decl.MinorPriority
            }
        GetDescription =
            fun () ->
                let (ToolTipText els) = decl.Description
                List.map toToolTips els |> Array.concat
    }

let getCompletionsAtLocation (results: ParseAndCheckResults) (line: int) (col: int) lineText =
    match results.CheckFileResultsOpt with
    | Some checkFile ->
        let ln, residue = findLongIdentsAndResidue (col, lineText)
        let longName = QuickParse.GetPartialLongNameEx(lineText, col)

        let longName =
            { longName with
                QualifyingIdents = ln
                PartialIdent = residue
            }

        let decls =
            checkFile.GetDeclarationListInfo(results.ParseFileResultsOpt, line, lineText, longName, (fun () -> []))

        decls.Items |> Array.map toCompletionItem
    | None -> [||]

let mapFableError (com: Compiler) (log: LogEntry) =
    let r = defaultArg log.Range Fable.AST.SourceLocation.Empty

    {
        FileName = com.CurrentFile
        StartLine = r.start.line
        StartColumn = r.start.column
        EndLine = r.``end``.line
        EndColumn = r.``end``.column
        Message = log.Message
        Severity =
            match log.Severity with
            | Fable.Severity.Info -> DiagnosticSeverity.Info
            | Fable.Severity.Warning -> DiagnosticSeverity.Warning
            | Fable.Severity.Error -> DiagnosticSeverity.Error
        Code = log.Tag
    }

type BabelResult(ast: Babel.Program, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

type DartResult(ast: Dart.File, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

type PhpResult(ast: Php.PhpFile, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

type PythonResult(ast: Python.AST.Module, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

type RustResult(ast: Rust.AST.Types.Crate, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

type BeamResult(ast: Fable.AST.Beam.ErlModule, errors) =
    member _.Ast = ast

    interface IFableResult with
        member _.FableErrors = errors

let transformToFableAst (com: Compiler) : Fable.File =
    let fileName = com.CurrentFile

    try
        FSharp2Fable.Compiler.transformFile com |> FableTransforms.transformFile com
    with
    | Fable.FableError msg ->
        com.AddLog(msg, Severity.Error, fileName = fileName)
        Fable.File([])
    | ex ->
        let msg = ex.Message + Environment.NewLine + ex.StackTrace
        com.AddLog(msg, Severity.Error, fileName = fileName, tag = "EXCEPTION")
        Fable.File([])

let transformToTargetAst (com: CompilerImpl) (fableAst: Fable.File) : IFableResult =
    // get errors
    let errors = com.Logs |> Array.map (mapFableError com)

    // transform Fable AST to target language AST
    match (com :> Compiler).Options.Language with
    | JavaScript
    | TypeScript ->
        let ast = Fable2Babel.Compiler.transformFile com fableAst
        upcast BabelResult(ast, errors)
    | Php ->
        let ast = Fable2Php.Compiler.transformFile com fableAst
        upcast PhpResult(ast, errors)
    | Python ->
        let ast = Fable.Transforms.Python.Compiler.transformFile com fableAst
        upcast PythonResult(ast, errors)
    | Dart ->
        let ast = Fable2Dart.Compiler.transformFile com fableAst
        upcast DartResult(ast, errors)
    | Rust ->
        let ast = Rust.Fable2Rust.Compiler.transformFile com fableAst
        upcast RustResult(ast, errors)
    | Beam ->
        let ast = Fable.Transforms.Beam.Compiler.transformFile com fableAst
        // Naming diagnostics are raised while constructing the target AST, after the earlier snapshot.
        let errors = com.Logs |> Array.map (mapFableError com)
        upcast BeamResult(ast, errors)

let compileToTargetAst (results: IParseAndCheckResults) fileName fableLibrary typedArrays language : IFableResult =
    let res = results :?> ParseAndCheckResults
    let project = res.GetProject()

    let com =
        makeCompiler fableLibrary typedArrays language results.OtherFSharpOptions project fileName

    for log in res.TakeProjectLogs() do
        (com :> Compiler).AddLog(log.Message, log.Severity, tag = log.Tag)

    let fableAst = transformToFableAst com
    fableAst |> transformToTargetAst com

let makeWriter (writer: IWriter) =
    { new Printer.Writer with
        member _.Dispose() = writer.Dispose()
        member _.MakeImportPath(path) = writer.MakeImportPath(path)
        member _.AddLog(msg, severity, ?range) = ()

        member _.AddSourceMapping(srcLine, srcCol, genLine, genCol, _file, displayName) =
            writer.AddSourceMapping(srcLine, srcCol, genLine, genCol, displayName)

        member _.Write(str) = writer.Write(str)
    }

let getLanguage (language: string) =
    match language.ToLowerInvariant() with
    | "js"
    | "javascript" -> JavaScript
    | "ts"
    | "typescript" -> TypeScript
    | "py"
    | "python" -> Python
    | "php" -> Php
    | "dart" -> Dart
    | "rs"
    | "rust" -> Rust
    | "beam"
    | "erlang" -> Beam
    | _ -> failwithf "Unsupported language: %s" language

let init () =
    { new IFableManager with
        member _.Version = Fable.Literals.VERSION

        member _.CreateChecker(references, readAllBytes, otherOptions) =
            let otherOptions = Array.append [| "--define:FABLE_STANDALONE" |] otherOptions

            InteractiveChecker.Create(references, readAllBytes, otherOptions) |> CheckerImpl :> IChecker

        member _.ClearCache(checker) =
            let c = checker :?> CheckerImpl
            c.Checker.ClearCache()

        member _.ParseAndCheckProject
            (checker, projectFileName, fileNames, sources, ?otherFSharpOptions, ?precompiledInfo)
            =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]

            parseAndCheckProject c.Checker projectFileName fileNames sources otherFSharpOptions precompiledInfo
            :> IParseAndCheckResults

        member _.ParseAndCheckFileInProject
            (checker, fileName, projectFileName, fileNames, sources, ?otherFSharpOptions, ?precompiledInfo)
            =
            let c = checker :?> CheckerImpl
            let otherFSharpOptions = defaultArg otherFSharpOptions [||]

            parseAndCheckFileInProject
                c.Checker
                fileName
                projectFileName
                fileNames
                sources
                otherFSharpOptions
                precompiledInfo
            :> IParseAndCheckResults

        member _.GetSymbolAtLocation(results: IParseAndCheckResults, line: int, col: int, lineText: string) =
            let res = results :?> ParseAndCheckResults
            getSymbolAtLocation res line col lineText

        member _.GetUsesOfSymbolInFile(results: IParseAndCheckResults, symbol: ISymbol) =
            let res = results :?> ParseAndCheckResults
            getUsesOfSymbolInFile res symbol

        member _.GetDeclarationLocation(results: IParseAndCheckResults, line: int, col: int, lineText: string) =
            let res = results :?> ParseAndCheckResults
            getDeclarationLocation res line col lineText

        member _.GetToolTips(results: IParseAndCheckResults, line: int, col: int, lineText: string) =
            let res = results :?> ParseAndCheckResults
            getToolTipAtLocation res line col lineText

        member _.Tokenize(source: string, defines: string[]) = tokenize source defines

        member _.GetSemanticClassification(results: IParseAndCheckResults) =
            let res = results :?> ParseAndCheckResults
            getSemanticClassification res

        member _.GetCompletionsAtLocation(results: IParseAndCheckResults, line: int, col: int, lineText: string) =
            let res = results :?> ParseAndCheckResults
            getCompletionsAtLocation res line col lineText

        member _.CompileToTargetAst
            (fableLibrary: string, results: IParseAndCheckResults, fileName: string, typedArrays, language)
            =
            let language = getLanguage language
            compileToTargetAst results fileName fableLibrary typedArrays language

        member _.PrintTargetAst(fableResult, writer) =
            let writer = makeWriter writer

            match fableResult with
            | :? BabelResult as babel -> BabelPrinter.run writer babel.Ast
            | :? DartResult as dart -> DartPrinter.run writer dart.Ast
            | :? PhpResult as php -> PhpPrinter.run writer php.Ast
            | :? PythonResult as python -> PythonPrinter.run writer python.Ast
            | :? RustResult as rust -> Rust.RustPrinter.run writer rust.Ast
            | :? BeamResult as beam -> ErlangPrinter.run writer beam.Ast
            | _ -> failwith "Unexpected Fable result"

        member _.FSharpAstToString(results: IParseAndCheckResults, fileName: string) =
            let res = results :?> ParseAndCheckResults
            let project = res.GetProject()
            let implFile = project.ImplementationFiles.Item(fileName)

            AstPrint.printFSharpDecls "" implFile.Declarations |> String.concat "\n"
    }
