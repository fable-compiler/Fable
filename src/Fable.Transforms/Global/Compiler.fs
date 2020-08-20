namespace Fable

[<RequireQualifiedAccessAttribute>]
type Verbosity =
    | Normal
    | Verbose
    | Silent

type CompilerOptions =
    { typedArrays: bool
      clampByteArrays: bool
      typescript: bool
      debugMode: bool
      verbosity: Verbosity
      /// Meant for precompiled libraries (like the Repl Lib)
      /// to make public inlined functions part of the JS
      outputPublicInlinedFunctions: bool
      /// Mainly intended for the REPL to compile REPL lib calls
      precompiledLib: (string -> (string*string) option) option
  }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

open FSharp.Compiler.SourceCodeServices

type InlineExpr =
    { Args: FSharpMemberOrFunctionOrValue list
      Body: FSharpExpr
      FileName: string }

type ICompiler =
    abstract LibraryDir: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract GetRootModule: string -> string
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
