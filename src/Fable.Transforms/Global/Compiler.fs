namespace Fable

module Literals =
    let [<Literal>] VERSION = "3.0.1"

type CompilerOptionsHelper =
    static member DefaultExtension = ".fs.js"
    static member Make(?typedArrays,
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
              member _.Typescript = defaultArg typescript false
              member _.TypedArrays = defaultArg typedArrays true
              member _.OptimizeFSharpAst = defaultArg optimizeFSharpAst false
              member _.Verbosity = defaultArg verbosity Verbosity.Normal
              member _.FileExtension = defaultArg fileExtension CompilerOptionsHelper.DefaultExtension
              member _.ClampByteArrays = defaultArg clampByteArrays false }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

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
    abstract GetImplementationFile: fileName: string -> FSharpImplementationFileContents
    abstract GetRootModule: fileName: string -> string
    abstract GetEntity: Fable.EntityRef -> Fable.Entity
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr
    abstract AddWatchDependency: file: string -> unit
    abstract AddLog: msg:string * severity: Severity * ?range: SourceLocation
                        * ?fileName:string * ?tag: string -> unit

[<AutoOpen>]
module CompilerExt =
    let expectedVersionMatchesActual (expected: string) (actual: string) =
        try
            let r = System.Text.RegularExpressions.Regex("^(\d+)\.(\d+)")
            let parse v =
                let m = r.Match(v)
                int m.Groups.[1].Value, int m.Groups.[2].Value
            let actualMajor, actualMinor = parse actual
            let expectedMajor, expectedMinor = parse expected
            actualMajor = expectedMajor && actualMinor >= expectedMinor
        with _ -> false

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

        member com.ApplyPlugin<'Plugin, 'Input when 'Plugin :> PluginAttribute>(plugins: Map<_,_>, atts: Fable.Attribute seq, input: 'Input, transform) =
            if Map.isEmpty plugins then input
            else
                (input, atts) ||> Seq.fold (fun input att ->
                    match Map.tryFind att.Entity plugins with
                    | None -> input
                    | Some plugin ->
                        let pluginInstance = System.Activator.CreateInstance(plugin, List.toArray att.ConstructorArgs) :?> 'Plugin
                        if not(expectedVersionMatchesActual pluginInstance.FableMinimumVersion Literals.VERSION) then
                            failwithf "Plugin %s expects v%s but currently running Fable v%s"
                                        plugin.FullName pluginInstance.FableMinimumVersion Literals.VERSION
                        let helper = com.ToPluginHelper()
                        transform pluginInstance helper input)

        member com.ApplyMemberDeclarationPlugin(file: Fable.File, decl: Fable.MemberDecl) =
            com.ApplyPlugin<MemberDeclarationPluginAttribute,_>
                (com.Plugins.MemberDeclarationPlugins, decl.Info.Attributes, decl, fun p h i -> p.Transform(h, file, i))

        member com.ApplyMemberCallPlugin(memb: Fable.MemberFunctionOrValue, expr: Fable.Expr) =
            com.ApplyPlugin<MemberDeclarationPluginAttribute,_>
                (com.Plugins.MemberDeclarationPlugins, memb.Attributes, expr, fun p h e -> p.TransformCall(h, memb, e))
