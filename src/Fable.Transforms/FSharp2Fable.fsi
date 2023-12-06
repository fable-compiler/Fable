module rec Fable.Transforms.FSharp2Fable.Compiler

open FSharp.Compiler.Symbols
open Fable
open Fable.AST

val getRootFSharpEntities:
    declarations: FSharpImplementationFileDeclaration list -> FSharpEntity seq

val getRootModule:
    declarations: FSharpImplementationFileDeclaration list -> string

val getInlineExprs:
    fileName: string ->
    declarations: FSharpImplementationFileDeclaration list ->
        (string * InlineExprLazy) list

val transformFile: com: Compiler -> Fable.File
