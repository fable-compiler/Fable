module Fable.JS.Interfaces

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
    | LambdaType
    | Error
    | Event

type Error =
    { StartLineAlternate: int
      StartColumn: int
      EndLineAlternate: int
      EndColumn: int
      Message: string
      IsWarning: bool }

type Completion =
    { Name: string
      Glyph: Glyph }

type IChecker =
    interface end

// type IFableCompiler =
//     interface end

type IParseResults =
    interface end
    // abstract Errors: Error[] // see #1452

type IFableManager =
    abstract CreateChecker: references: string[] * readAllBytes: (string -> byte[]) -> IChecker
    //abstract CreateCompiler: fableCorDir:string * ?replacements: seq<string * string> -> IFableCompiler
    abstract ParseFSharpProject: checker: IChecker * fileName: string * source: string -> IParseResults
    abstract GetParseErrors: parseResults: IParseResults -> Error[]
    abstract GetToolTipText: parseResults: IParseResults * line: int * col: int * lineText: string -> Async<string[]>
    abstract GetCompletionsAtLocation: parseResults: IParseResults * line: int * col: int * lineText: string -> Async<Completion[]>
    abstract CompileToBabelAst: fableCore: string * parseResults: IParseResults * fileName: string * optimized: bool -> Fable.AST.Babel.Program
    // abstract CompileToBabelJsonAst: fableCore: string * parseResults: IParseResults * fileName: string * ?optimized: bool -> string
    abstract PrintFSharpAst: parseResults: IParseResults * optimized: bool * printFn: (string -> unit) -> unit
