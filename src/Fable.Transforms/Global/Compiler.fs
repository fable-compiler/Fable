namespace Fable

module Literals =
    let [<Literal>] VERSION = "4.0.0"

type CompilerOptionsHelper =
    static member DefaultExtension = ".fs.js"
    static member Make(?language,
                       ?typedArrays,
                       ?define,
                       ?debugMode,
                       ?optimizeFSharpAst,
                       ?verbosity,
                       ?fileExtension,
                       ?clampByteArrays) =
        {
            CompilerOptions.Define = defaultArg define []
            DebugMode = defaultArg debugMode true
            Language = defaultArg language JavaScript
            TypedArrays = defaultArg typedArrays true
            OptimizeFSharpAst = defaultArg optimizeFSharpAst false
            Verbosity = defaultArg verbosity Verbosity.Normal
            FileExtension = defaultArg fileExtension CompilerOptionsHelper.DefaultExtension
            ClampByteArrays = defaultArg clampByteArrays false
            TriggeredByDependency = false
        }

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error
    | Info

[<RequireQualifiedAccess>]
type OutputType =
    | Library
    | Exe
    | Module
    | Winexe

open FSharp.Compiler.Symbols
open Fable.AST

type InlineExpr =
    { Args: Fable.Ident list
      Body: Fable.Expr
      FileName: string
      ScopeIdents: Set<string> }

type CompilerPlugins =
    { MemberDeclarationPlugins: Map<Fable.EntityRef, System.Type> }

type Compiler =
    abstract LibraryDir: string
    abstract CurrentFile: string
    abstract OutputDir: string option
    abstract OutputType: OutputType
    abstract ProjectFile: string
    abstract Options: CompilerOptions
    abstract Plugins: CompilerPlugins
    abstract GetImplementationFile: fileName: string -> FSharpImplementationFileDeclaration list
    abstract GetRootModule: fileName: string -> string
    abstract TryGetEntity: Fable.EntityRef -> Fable.Entity option
    abstract GetInlineExpr: string -> InlineExpr
    abstract AddWatchDependency: file: string -> unit
    abstract AddLog: msg:string * severity: Severity * ?range: SourceLocation
                        * ?fileName:string * ?tag: string -> unit

type InlineExprLazy(f: Compiler -> InlineExpr) =
    let mutable value: InlineExpr voption = ValueNone
    member this.Calculate(com: Compiler) =
        lock this <| fun () ->
            match value with
            | ValueSome v -> v
            | ValueNone ->
                let v = f com
                value <- ValueSome v
                v
[<AutoOpen>]
module CompilerExt =
    let private expectedVersionMatchesActual (expected: string) (actual: string) =
        try
            let r = System.Text.RegularExpressions.Regex(@"^(\d+)\.(\d+)(?:\.(\d+))?")
            let parse v =
                let m = r.Match(v)
                int m.Groups.[1].Value,
                int m.Groups.[2].Value,
                if m.Groups.[3].Success then int m.Groups.[3].Value else 0

            let actualMajor, actualMinor, actualPatch = parse actual
            let expectedMajor, expectedMinor, expectedPatch = parse expected

            // Fail also if actual major is bigger than expected major version
            actualMajor = expectedMajor && (
                actualMinor > expectedMinor
                || (actualMinor = expectedMinor && actualPatch >= expectedPatch)
            )
        with _ -> false

    let private coreAssemblyNames = set Metadata.coreAssemblies

    type Compiler with
        static member CoreAssemblyNames = coreAssemblyNames

        member com.GetEntity(entityRef: Fable.EntityRef) =
            match com.TryGetEntity(entityRef) with
            | Some e -> e
            | None ->
                let category =
                    match entityRef.Path with
                    | Fable.CoreAssemblyName _ -> "core"
                    | Fable.AssemblyPath _ -> "external"
                    | Fable.PrecompiledLib _ -> "precompiled"
                    | Fable.SourcePath _ -> "user"
                failwith $"Cannot find {category} entity %s{entityRef.FullName}"

        member com.ToPluginHelper() =
            { new PluginHelper with
                member _.LibraryDir = com.LibraryDir
                member _.CurrentFile = com.CurrentFile
                member _.Options = com.Options
                member _.GetRootModule(fileName) = com.GetRootModule(fileName)
                member _.GetEntity(ref) = com.GetEntity(ref)
                member _.LogWarning(msg, r) = com.AddLog(msg, Severity.Warning, ?range=r, fileName=com.CurrentFile)
                member _.LogError(msg, r) = com.AddLog(msg, Severity.Error, ?range=r, fileName=com.CurrentFile)
                member _.GetOutputPath() =
                    let file = Path.replaceExtension com.Options.FileExtension com.CurrentFile
                    match com.OutputDir with
                    | None -> file
                    | Some outDir ->
                        // TODO: This is a simplified version of the actual mechanism and will not work with deduplicated paths
                        let projDir = Path.GetDirectoryName(com.ProjectFile)
                        let relPath = Path.getRelativeFileOrDirPath true projDir false file
                        let relPath = if relPath.StartsWith("./") then relPath.[2..] else relPath
                        Path.Combine(outDir, relPath)
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
