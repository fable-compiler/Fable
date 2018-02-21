namespace Fable

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
      addReflectionInfo: bool
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

open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices

type InlineExpr = IDictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr

type ICompilerState =
    abstract GetRootModule: string -> string
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
