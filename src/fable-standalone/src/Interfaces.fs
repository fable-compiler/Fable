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

type Error =
    { FileName: string
      StartLineAlternate: int
      StartColumn: int
      EndLineAlternate: int
      EndColumn: int
      Message: string
      IsWarning: bool }

type Range =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int }

type Completion =
    { Name: string
      Glyph: Glyph }

type IChecker =
    interface end

type IParseResults =
    abstract OtherFSharpOptions: string[]
    abstract Errors: Error[]

type IBabelResult =
    abstract BabelAst: obj
    abstract FableErrors: Error[]

type CompilerConfig =
    { typedArrays: bool
      clampByteArrays: bool
      typescript: bool
      precompiledLib: (string -> (string * string) option) option
    }

type IFableManager =
    abstract CreateChecker: references: string[] * readAllBytes: (string -> byte[]) * otherOptions: string[] -> IChecker
    abstract ClearParseCaches: checker: IChecker -> unit
    abstract ParseFSharpScript: checker: IChecker * fileName: string * source: string * ?otherFSharpOptions: string[] -> IParseResults
    abstract ParseFSharpProject: checker: IChecker * projectFileName: string * fileNames: string[] * sources: string[] * ?otherFSharpOptions: string[] -> IParseResults
    abstract ParseFSharpFileInProject: checker: IChecker * fileName: string * projectFileName: string * fileNames: string[] * sources: string[] * ?otherFSharpOptions: string[] -> IParseResults
    abstract GetParseErrors: parseResults: IParseResults -> Error[]
    abstract GetDeclarationLocation: parseResults: IParseResults * line: int * col: int * lineText: string -> Async<Range option>
    abstract GetToolTipText: parseResults: IParseResults * line: int * col: int * lineText: string -> Async<string[]>
    abstract GetCompletionsAtLocation: parseResults: IParseResults * line: int * col: int * lineText: string -> Async<Completion[]>
    abstract CompileToBabelAst: fableLibrary: string * parseResults: IParseResults * fileName: string * ?config: CompilerConfig -> IBabelResult
    abstract FSharpAstToString: parseResults: IParseResults * fileName: string -> string
