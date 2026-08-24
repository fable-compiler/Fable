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

type Error =
    {
        FileName: string
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
        Message: string
        IsWarning: bool
    }

type Range =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

type Completion =
    {
        Name: string
        Glyph: Glyph
    }

type SourceMapping = int * int * int * int * string option

type IChecker = interface end

/// Root modules from a `fable precompile` output. Inline members of the library cannot be called:
/// FCS hands Fable a call rather than an expansion, and their bodies are not readable here.
type IPrecompiledInfo =
    /// Must be the name the precompiled assembly is referenced under, plus ".dll"
    abstract DllPath: string
    abstract TryGetRootModule: normalizedFullPath: string -> string option

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

    abstract GetDeclarationLocation:
        parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> Range option

    abstract GetToolTipText: parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> string[]

    abstract GetCompletionsAtLocation:
        parseResults: IParseAndCheckResults * line: int * col: int * lineText: string -> Completion[]

    abstract CompileToTargetAst:
        fableLibrary: string *
        parseResults: IParseAndCheckResults *
        fileName: string *
        typedArrays: bool option *
        language: string ->
            IFableResult

    abstract PrintTargetAst: fableResult: IFableResult * IWriter -> Async<unit>

    abstract FSharpAstToString: parseResults: IParseAndCheckResults * fileName: string -> string
