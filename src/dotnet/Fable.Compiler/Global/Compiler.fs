namespace Fable

type CompilerOptions =
    { fableCore: string
      typedArrays: bool
      clampByteArrays: bool }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

// type InlineExpr = IDictionary<string, int> * Expr

type ICompiler =
    abstract ProjectFile: string
    abstract Options: CompilerOptions
    abstract GetUniqueVar: unit->string
    abstract GetRootModule: string -> string
    // abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
    abstract AddLog: msg:string * severity: Severity * ?range:SourceLocation
                        * ?fileName:string * ?tag: string -> unit
