namespace Fable

open Fable.AST
open Fable.AST.Fable

[<RequireQualifiedAccess>]
type Verbosity =
    | Normal
    | Verbose
    | Silent

type Language =
    | JavaScript
    | TypeScript
    | Python
    | Php
    | Dart
    | Rust

type CompilerOptions =
    {
        TypedArrays: bool
        ClampByteArrays: bool
        Language: Language
        Define: string list
        DebugMode: bool
        OptimizeFSharpAst: bool
        TrimRootModule: bool
        Verbosity: Verbosity
        FileExtension: string
    }

type PluginHelper =
    abstract LibraryDir: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract LogWarning: string * ?range: SourceLocation -> unit
    abstract LogError: string * ?range: SourceLocation -> unit
    abstract GetRootModule: fileName: string -> string
    abstract GetEntity: EntityRef -> Entity
    abstract GetOutputPath: unit -> string

[<System.AttributeUsage(System.AttributeTargets.Assembly)>]
type ScanForPluginsAttribute() =
    inherit System.Attribute()

[<AbstractClass>]
type PluginAttribute() =
    inherit System.Attribute()
    abstract FableMinimumVersion: string

[<AbstractClass>]
type MemberDeclarationPluginAttribute() =
    inherit PluginAttribute()
    abstract Transform: PluginHelper * File * MemberDecl -> MemberDecl
    abstract TransformCall: PluginHelper * member_: MemberFunctionOrValue * expr: Expr -> Expr
