namespace Fable

open System
open Fable.AST
open Fable.AST.Fable

type PluginHelper =
    abstract LogWarning: string * ?range: SourceLocation -> unit
    abstract LogError: string * ?range: SourceLocation -> unit

[<AttributeUsage(AttributeTargets.Assembly)>]
type RegisterPluginAttribute(t: System.Type) =
    inherit System.Attribute()

[<AbstractClass>]
type PluginAttribute() =
    inherit System.Attribute()
    abstract FableVersion: string

[<AbstractClass>]
type MemberDeclarationPluginAttribute() =
    inherit PluginAttribute()
    abstract Transform: PluginHelper * MemberDecl -> MemberDecl

[<AbstractClass>]
type CallPluginAttribute() =
    inherit PluginAttribute()
    abstract Transform: PluginHelper * callee: Expr * info: CallInfo * typ: Type * range: SourceLocation option -> Expr
