namespace Fable

type CompilerOptionsHelper =
    static member DefaultFileExtension = ".fs.js"
    static member Make(?eraseUnions,
                       ?typedArrays,
                       ?typescript,
                       ?define,
                       ?optimizeFSharpAst,
                       ?verbosity,
                       ?fileExtension,
                       ?clampByteArrays) =
        let define = defaultArg define []
        let isDebug = List.contains "DEBUG" define
        { new CompilerOptions with
              member _.Define = define
              member _.DebugMode = isDebug
              member _.EraseUnions = defaultArg eraseUnions false
              member _.Typescript = defaultArg typescript false
              member _.TypedArrays = defaultArg typedArrays true
              member _.OptimizeFSharpAst = defaultArg optimizeFSharpAst false
              member _.Verbosity = defaultArg verbosity Verbosity.Normal
              member _.FileExtension = defaultArg fileExtension CompilerOptionsHelper.DefaultFileExtension
              member _.ClampByteArrays = defaultArg clampByteArrays false }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices
open Fable.AST

type InlineExpr =
    { Args: FSharpMemberOrFunctionOrValue list
      Body: FSharpExpr
      FileName: string }

type CompilerPlugins =
    { MemberDeclarationPlugins: Map<Fable.EntityRef, System.Type> }

type Compiler =
    abstract LibraryDir: string
    abstract CurrentFile: string
    abstract Options: CompilerOptions
    abstract Plugins: CompilerPlugins
    abstract ImplementationFiles: IDictionary<string, FSharpImplementationFileContents>
    abstract GetRootModule: fileName: string -> string
    abstract GetEntity: Fable.EntityRef -> Fable.Entity
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
    abstract AddWatchDependency: file: string -> unit
    abstract AddLog: msg:string * severity: Severity * ?range: SourceLocation
                        * ?fileName:string * ?tag: string -> unit

[<AutoOpen>]
module CompilerExt =
    type Compiler with
        member com.ToPluginHelper() =
            { new PluginHelper with
                member _.LibraryDir = com.LibraryDir
                member _.CurrentFile = com.CurrentFile
                member _.Options = com.Options
                member _.GetRootModule(fileName) = com.GetRootModule(fileName)
                member _.GetEntity(ref) = com.GetEntity(ref)
                member _.LogWarning(msg, r) = com.AddLog(msg, Severity.Warning, ?range=r, fileName=com.CurrentFile)
                member _.LogError(msg, r) = com.AddLog(msg, Severity.Error, ?range=r, fileName=com.CurrentFile)
             }

        member com.ApplyPlugin<'Plugin, 'Input>(plugins: Map<_,_>, atts: Fable.Attribute seq, input: 'Input, transform) =
            if Map.isEmpty plugins then input
            else
                (input, atts) ||> Seq.fold (fun input att ->
                    match Map.tryFind att.Entity plugins with
                    | None -> input
                    | Some plugin ->
                        let plugin = System.Activator.CreateInstance(plugin, List.toArray att.ConstructorArgs) :?> 'Plugin
                        let helper = com.ToPluginHelper()
                        transform plugin helper input)

        member com.ApplyMemberDeclarationPlugin(decl: Fable.MemberDecl) =
            com.ApplyPlugin<MemberDeclarationPluginAttribute,_>
                (com.Plugins.MemberDeclarationPlugins, decl.Info.Attributes, decl, fun p h i -> p.Transform(h, i))

        member com.ApplyMemberCallPlugin(memb: Fable.MemberFunctionOrValue, expr: Fable.Expr) =
            com.ApplyPlugin<MemberDeclarationPluginAttribute,_>
                (com.Plugins.MemberDeclarationPlugins, memb.Attributes, expr, fun p h e -> p.TransformCall(h, memb, e))
