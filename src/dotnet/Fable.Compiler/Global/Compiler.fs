namespace Fable

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
      verbose: bool
      /// Meant for replacements libraries (like the Repl Lib)
      /// to make public inlined functions part of the JS
      outputPublicInlinedFunctions: bool
      /// Mainly intended for the REPL to compile REPL lib calls
      replacementsLib: (string -> (string*string) option) option
  }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

open Microsoft.FSharp.Compiler.SourceCodeServices

type InlineExpr =
    { Args: FSharpMemberOrFunctionOrValue list
      Body: FSharpExpr
      FileName: string }

type ICompiler =
    abstract ReplacementsDir: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract GetUniqueVar: ?name: string -> string
    abstract GetRootModule: string -> string
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
