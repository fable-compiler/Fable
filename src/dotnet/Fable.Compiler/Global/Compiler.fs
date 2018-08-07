namespace Fable

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
      verbose: bool
      aggressiveInline: bool
    }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

open Microsoft.FSharp.Compiler.SourceCodeServices

type InlineExpr = FSharpMemberOrFunctionOrValue list * FSharpExpr

type ICompiler =
    abstract FableCore: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract GetUniqueVar: ?name: string -> string
    abstract GetRootModule: string -> string
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr option) -> InlineExpr option
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
