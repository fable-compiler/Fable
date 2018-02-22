namespace Fable

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
    }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

type ICompiler =
    abstract FableCore: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract GetUniqueVar: unit->string
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit

open Microsoft.FSharp.Compiler.SourceCodeServices

type InlineExpr = FSharpMemberOrFunctionOrValue list * FSharpExpr

type ICompilerState =
    abstract GetRootModule: string -> string
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
