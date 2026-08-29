namespace Fable.Tests

open Fable.AST
open Fable.AST.Fable

[<assembly: Fable.ScanForPlugins>]
do ()

/// Replaces the whole member body with a constant, proving `Transform` fires.
type ReturnConstPlugin() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "5.0"

    override _.Transform(_, _, decl) =
        { decl with Body = Value(NumberConstant(NumberValue.Int32 42, NumberInfo.Empty), None) }

    override _.TransformCall(_, _, expr) = expr

/// Replaces the call site with the called member's name, proving `TransformCall` fires.
type ReturnMemberNamePlugin() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "5.0"

    override _.Transform(_, _, decl) = decl

    override _.TransformCall(_, member_, expr) =
        Value(StringConstant member_.DisplayName, expr.Range)
