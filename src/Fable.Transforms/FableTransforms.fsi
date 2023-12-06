module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable

val isIdentCaptured: identName: string -> expr: Expr -> bool
val isTailRecursive: identName: string -> expr: Expr -> bool * bool
val replaceValues: replacements: Map<string, Expr> -> expr: Expr -> Expr
val uncurryType: typ: Type -> Type

val getTransformations: _com: Compiler -> (#Compiler -> Expr -> Expr) list

val transformDeclaration:
    transformations: (Compiler -> Expr -> Expr) list ->
    com: Compiler ->
    file: File ->
    decl: Declaration ->
        Declaration

val transformFile: com: Compiler -> file: File -> File
