namespace Fable

open System

module Literals =
    [<Literal>]
    let VERSION = "4.9.0"

    [<Literal>]
    let JS_LIBRARY_VERSION = "1.1.1"

type CompilerOptionsHelper =
    static member Make
        (
            ?language,
            ?typedArrays,
            ?define,
            ?debugMode,
            ?optimizeFSharpAst,
            ?verbosity,
            ?fileExtension,
            ?clampByteArrays,
            ?noReflection
        )
        =
        {
            CompilerOptions.Define = defaultArg define []
            DebugMode = defaultArg debugMode true
            Language = defaultArg language JavaScript
            FileExtension = defaultArg fileExtension ".fs.js"
            TypedArrays = defaultArg typedArrays true
            OptimizeFSharpAst = defaultArg optimizeFSharpAst false
            Verbosity = defaultArg verbosity Verbosity.Normal
            ClampByteArrays = defaultArg clampByteArrays false
            NoReflection = defaultArg noReflection false
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

open FSharp.Compiler.Symbols
open Fable.AST

type InlineExpr =
    {
        Args: Fable.Ident list
        Body: Fable.Expr
        FileName: string
        GenericArgs: string list
        ScopeIdents: Set<string>
    }

type CompilerPlugins =
    { MemberDeclarationPlugins: Map<Fable.EntityRef, System.Type> }

type SourceReader = string -> int * Lazy<string>

type Compiler =
    abstract LibraryDir: string
    abstract CurrentFile: string
    abstract OutputDir: string option
    abstract OutputType: OutputType
    abstract ProjectFile: string
    abstract SourceFiles: string[]
    abstract Options: CompilerOptions
    abstract Plugins: CompilerPlugins
    abstract IncrementCounter: unit -> int
    abstract IsPrecompilingInlineFunction: bool
    abstract WillPrecompileInlineFunction: file: string -> Compiler

    abstract GetImplementationFile:
        fileName: string -> FSharpImplementationFileDeclaration list

    abstract GetRootModule: fileName: string -> string
    abstract TryGetEntity: Fable.EntityRef -> Fable.Entity option
    abstract GetInlineExpr: string -> InlineExpr
    abstract AddWatchDependency: file: string -> unit

    abstract AddLog:
        msg: string *
        severity: Severity *
        ?range: SourceLocation *
        ?fileName: string *
        ?tag: string ->
            unit

type InlineExprLazy(f: Compiler -> InlineExpr) =
    let mutable value: InlineExpr voption = ValueNone

    member this.Calculate(com: Compiler) =
        lock this
        <| fun () ->
            match value with
            | ValueSome v -> v
            | ValueNone ->
                let v = f com
                value <- ValueSome v
                v

[<AutoOpen>]
module CompilerExt =
    let private expectedVersionMatchesActual
        (expected: string)
        (actual: string)
        =
        try
            let r =
                System.Text.RegularExpressions.Regex(
                    @"^(\d+)\.(\d+)(?:\.(\d+))?"
                )

            let parse v =
                let m = r.Match(v)

                int m.Groups[1].Value,
                int m.Groups[2].Value,
                if m.Groups[3].Success then
                    int m.Groups[3].Value
                else
                    0

            let actualMajor, actualMinor, actualPatch = parse actual
            let expectedMajor, expectedMinor, expectedPatch = parse expected

            // Fail also if actual major is bigger than expected major version
            actualMajor = expectedMajor
            && (actualMinor > expectedMinor
                || (actualMinor = expectedMinor && actualPatch >= expectedPatch))
        with _ ->
            false

    let private coreAssemblyNames = set Metadata.coreAssemblies
    let mutable private _lang = JavaScript

    type Compiler with

        static member CoreAssemblyNames = coreAssemblyNames

        static member Language = _lang
        /// Use this only once at the start of the program
        static member SetLanguageUnsafe lang = _lang <- lang

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

        member com.TryGetMember
            (memberRef: Fable.MemberRef)
            : Fable.MemberFunctionOrValue option
            =
            match memberRef with
            | Fable.GeneratedMemberRef gen -> Some(gen :> _)
            | Fable.MemberRef(declaringEntity, memberInfo) ->
                com.TryGetEntity(declaringEntity)
                |> Option.bind (fun ent -> ent.TryFindMember(memberInfo))

        member com.GetMember
            (memberRef: Fable.MemberRef)
            : Fable.MemberFunctionOrValue
            =
            match com.TryGetMember(memberRef) with
            | Some e -> e
            | None -> failwith $"Cannot find member ref: %A{memberRef}"

        member com.ToPluginHelper() =
            { new PluginHelper with
                member _.LibraryDir = com.LibraryDir
                member _.CurrentFile = com.CurrentFile
                member _.OutputDir = com.OutputDir
                member _.ProjectFile = com.ProjectFile
                member _.SourceFiles = com.SourceFiles
                member _.Options = com.Options
                member _.GetRootModule(fileName) = com.GetRootModule(fileName)
                member _.GetEntity(ref) = com.GetEntity(ref)
                member _.GetMember(ref) = com.GetMember(ref)

                member _.LogWarning(msg, r) =
                    com.AddLog(
                        msg,
                        Severity.Warning,
                        ?range = r,
                        fileName = com.CurrentFile
                    )

                member _.LogError(msg, r) =
                    com.AddLog(
                        msg,
                        Severity.Error,
                        ?range = r,
                        fileName = com.CurrentFile
                    )

                member _.GetOutputPath(file) =
                    let file =
                        Path.ChangeExtension(file, com.Options.FileExtension)

                    match com.OutputDir with
                    | None -> file
                    | Some outDir ->
                        // TODO: This is a simplified version of the actual mechanism and will not work with deduplicated paths
                        let projDir = Path.GetDirectoryName(com.ProjectFile)

                        let relPath =
                            Path.getRelativeFileOrDirPath
                                true
                                projDir
                                false
                                file

                        let relPath =
                            if
                                relPath.StartsWith(
                                    "./",
                                    StringComparison.Ordinal
                                )
                            then
                                relPath[2..]
                            else
                                relPath

                        Path.Combine(outDir, relPath)

                member this.GetOutputPath() =
                    this.GetOutputPath(com.CurrentFile)
            }

        member com.ApplyPlugin<'Plugin, 'Input when 'Plugin :> PluginAttribute>
            (
                plugins: Map<_, _>,
                atts: Fable.Attribute seq,
                input: 'Input,
                transform
            )
            =
            if Map.isEmpty plugins then
                input
            else
                // Reverse attributes so plugins closer to member/type are applied first
                (input, Seq.rev atts)
                ||> Seq.fold (fun input att ->
                    match Map.tryFind att.Entity plugins with
                    | None -> input
                    | Some plugin ->
                        let pluginInstance =
                            System.Activator.CreateInstance(
                                plugin,
                                List.toArray att.ConstructorArgs
                            )
                            :?> 'Plugin

                        if
                            not (
                                expectedVersionMatchesActual
                                    pluginInstance.FableMinimumVersion
                                    Literals.VERSION
                            )
                        then
                            failwithf
                                "Plugin %s expects v%s but currently running Fable v%s"
                                plugin.FullName
                                pluginInstance.FableMinimumVersion
                                Literals.VERSION

                        let helper = com.ToPluginHelper()
                        transform pluginInstance helper input
                )

        member com.ApplyMemberDeclarationPlugin
            (
                file: Fable.File,
                decl: Fable.MemberDecl
            )
            =
            match com.TryGetMember(decl.MemberRef) with
            | None -> decl
            | Some memb ->
                com.ApplyPlugin<MemberDeclarationPluginAttribute, _>(
                    com.Plugins.MemberDeclarationPlugins,
                    memb.Attributes,
                    decl,
                    fun p h i -> p.Transform(h, file, i)
                )

        member com.ApplyMemberCallPlugin
            (
                memb: Fable.MemberFunctionOrValue,
                expr: Fable.Expr
            )
            =
            com.ApplyPlugin<MemberDeclarationPluginAttribute, _>(
                com.Plugins.MemberDeclarationPlugins,
                memb.Attributes,
                expr,
                fun p h e -> p.TransformCall(h, memb, e)
            )
