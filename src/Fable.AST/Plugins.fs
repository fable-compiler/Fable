namespace Fable

open System
open Fable.AST
open Fable.AST.Fable

type PluginHelper =
    abstract LogWarning: string * ?range: SourceLocation -> unit
    abstract LogError: string * ?range: SourceLocation -> unit

[<AbstractClass>]
type PluginAttribute() =
    inherit System.Attribute()
    abstract FableMinimumVersion: string

[<AbstractClass>]
type MemberDeclarationPluginAttribute() =
    inherit PluginAttribute()
    abstract Transform: PluginHelper * MemberDecl -> MemberDecl

[<AbstractClass>]
type CallPluginAttribute() =
    inherit PluginAttribute()
    abstract Transform: PluginHelper * expr: Expr -> Expr
