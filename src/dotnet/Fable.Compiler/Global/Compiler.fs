namespace Fable

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
      verbose: bool
      fableOptimize: bool
    }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

open Microsoft.FSharp.Compiler.SourceCodeServices

type InlinedMember = FSharpMemberOrFunctionOrValue list * FSharpExpr

type ICompiler =
    abstract FableCore: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract GetUniqueVar: ?name: string -> string
    abstract GetRootModule: string -> string
    abstract GetOrAddInlinedMember: string * (unit->InlinedMember option) -> InlinedMember option
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
