namespace Fable.Standalone

open Fable.Core

[<StringEnum; RequireQualifiedAccess>]
type Glyph =
    | Class
    | Enum
    | Value
    | Variable
    | Interface
    | Module
    | Method
    | Property
    | Field
    | Function
    | Error
    | Event
    | TypeParameter

[<StringEnum; RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Hidden
    | Info
    | Warning
    | Error

type Error =
    {
        FileName: string
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
        Message: string
        Severity: DiagnosticSeverity
        /// FCS error number such as "FS0039", or the Fable log tag such as "FABLE"
        Code: string
    }

type Range =
    {
        FileName: string
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

[<RequireQualifiedAccess>]
type DeclarationLocation =
    /// The file is the one FCS recorded for the symbol, so it can be a source outside the project
    | Found of Range
    /// A .NET symbol from a referenced assembly, e.g. `System.Math.Abs`
    | External of assembly: string * symbol: string
    | NotFound of reason: string

[<StringEnum; RequireQualifiedAccess>]
type CompletionKind =
    | SuggestedName
    | Field
    | Property
    | Method
    | ExtensionMethod
    | Event
    | Argument
    | CustomOperation
    | Other

type Completion =
    {
        NameInList: string
        /// Text to insert, e.g. with backticks for names that need them
        NameInCode: string
        Glyph: Glyph
        Kind: CompletionKind
        FullName: string
        IsOwnMember: bool
        MinorPriority: int
    }

/// Mirrors FCS `FSharp.Compiler.Text.TextTag`
[<StringEnum; RequireQualifiedAccess>]
type TextTag =
    | ActivePatternCase
    | ActivePatternResult
    | Alias
    | Class
    | Union
    | UnionCase
    | Delegate
    | Enum
    | Event
    | Field
    | Interface
    | Keyword
    | LineBreak
    | Local
    | Record
    | RecordField
    | Method
    | Member
    | ModuleBinding
    | Function
    | Module
    | Namespace
    | NumericLiteral
    | Operator
    | Parameter
    | Property
    | Space
    | StringLiteral
    | Struct
    | TypeParameter
    | Text
    | Punctuation
    | UnknownType
    | UnknownEntity

type TaggedText =
    {
        Tag: TextTag
        Text: string
    }

/// One signature with its documentation. Overloads come as separate tooltips.
type ToolTip =
    {
        ParamName: string option
        Signature: TaggedText[]
        /// XML doc lines, with the implicit `<summary>` already inserted
        XmlDoc: string[]
        TypeMapping: TaggedText[][]
        Remarks: TaggedText[]
    }

type CompletionItem =
    {
        Completion: Completion
        /// Formats the tooltips on every call, so only call it for the highlighted row
        GetDescription: unit -> ToolTip[]
    }

[<StringEnum; RequireQualifiedAccess>]
type TokenColor =
    | Default
    | Keyword
    | Comment
    | Identifier
    | String
    | UpperIdentifier
    | InactiveCode
    | PreprocessorKeyword
    | Number
    | Operator
    | Punctuation

/// A lexical token of one line. Columns are 0-based and the end is exclusive.
type Token =
    {
        StartColumn: int
        EndColumn: int
        Color: TokenColor
        /// FCS token name such as "IDENT" or "STRING_TEXT"
        Name: string
    }

/// Mirrors FCS `SemanticClassificationType`
[<StringEnum; RequireQualifiedAccess>]
type SemanticClass =
    | ReferenceType
    | ValueType
    | UnionCase
    | UnionCaseField
    | Function
    | Property
    | MutableVar
    | Module
    | Namespace
    | Printf
    | ComputationExpression
    | IntrinsicFunction
    | Enumeration
    | Interface
    | TypeArgument
    | Operator
    | DisposableType
    | DisposableTopLevelValue
    | DisposableLocalValue
    | Method
    | ExtensionMethod
    | ConstructorForReferenceType
    | ConstructorForValueType
    | Literal
    | RecordField
    | MutableRecordField
    | RecordFieldAsFunction
    | Exception
    | Field
    | Event
    | Delegate
    | NamedArgument
    | Value
    | LocalValue
    | Type
    | TypeDef
    | Plaintext

type SemanticClassification =
    {
        Range: Range
        Type: SemanticClass
    }

type SourceMapping = int * int * int * int * string option

type IChecker = interface end

/// Opaque FCS symbol, valid for the checker it came from
type ISymbol =
    abstract DisplayName: string

type SymbolUse =
    {
        Range: Range
        IsDefinition: bool
    }

/// Root modules and inline member bodies from a `fable precompile` output.
type IPrecompiledInfo =
    /// Must be the name the precompiled assembly is referenced under, plus ".dll"
    abstract DllPath: string
    abstract TryGetRootModule: normalizedFullPath: string -> string option

    /// The first member name of each inline-expression chunk, straight out of
    /// precompiled_info.json. Leave it empty to compile without inline support, in which case
    /// calling an inline member of the library reports that its body could not be found.
    abstract InlineExprHeaders: string[]

    /// Contents of the matching inline_exprs_<index>.browser.json. Called while compiling, so the
    /// host has to be holding it already rather than fetching it here.
    abstract ReadInlineExprsChunk: index: int -> string

type IParseAndCheckResults =
    abstract OtherFSharpOptions: string[]
    abstract Errors: Error[]

type IFableResult =
    abstract FableErrors: Error[]

type IWriter =
    inherit System.IDisposable
    abstract AddSourceMapping: SourceMapping -> unit
    abstract MakeImportPath: string -> string
    abstract Write: string -> Async<unit>

type IFableManager =
    abstract Version: string

    abstract CreateChecker: references: string[] * readAllBytes: (string -> byte[]) * otherOptions: string[] -> IChecker

    abstract ClearCache: checker: IChecker -> unit

    abstract ParseAndCheckProject:
        checker: IChecker *
        projectFileName: string *
        fileNames: string[] *
        sources: string[] *
        ?otherFSharpOptions: string[] *
        ?precompiledInfo: IPrecompiledInfo ->
            IParseAndCheckResults

    abstract ParseAndCheckFileInProject:
        checker: IChecker *
        fileName: string *
        projectFileName: string *
        fileNames: string[] *
        sources: string[] *
        ?otherFSharpOptions: string[] *
        ?precompiledInfo: IPrecompiledInfo ->
            IParseAndCheckResults

    abstract GetErrors: parseResults: IParseAndCheckResults -> Error[]

    abstract GetSymbolAtLocation:
        parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> ISymbol option

    abstract GetUsesOfSymbolInFile: parseResults: IParseAndCheckResults * symbol: ISymbol -> SymbolUse[]

    abstract GetDeclarationLocation:
        parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> DeclarationLocation

    abstract GetToolTips: parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> ToolTip[]

    /// Lexical tokens of every line of the source, with `#if` blocks resolved against the defines
    abstract Tokenize: source: string * defines: string[] -> Token[][]

    abstract GetSemanticClassification: parseResults: IParseAndCheckResults -> SemanticClassification[]

    abstract GetCompletionsAtLocation:
        parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> CompletionItem[]

    abstract CompileToTargetAst:
        fableLibrary: string *
        parseResults: IParseAndCheckResults *
        fileName: string *
        typedArrays: bool option *
        language: string ->
            IFableResult

    abstract PrintTargetAst: fableResult: IFableResult * IWriter -> Async<unit>

    abstract FSharpAstToString: parseResults: IParseAndCheckResults * fileName: string -> string
