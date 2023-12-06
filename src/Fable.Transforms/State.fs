module Fable.Transforms.State

open Fable
open Fable.AST
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Compiler.Symbols
open System

type PluginRef =
    {
        DllPath: string
        TypeFullName: string
    }

type Assemblies(getPlugin, fsharpAssemblies: FSharpAssembly list) =
    let assemblies = Dictionary()
    let coreAssemblies = Dictionary()
    let entities = ConcurrentDictionary()

    let plugins =
        let plugins = Dictionary<Fable.EntityRef, System.Type>()
        let mutable hasSkippedAssembly = false

        for asm in fsharpAssemblies do
            match asm.FileName with
            | Some path ->
                let path = Path.normalizePath path
                let asmName = path.Substring(path.LastIndexOf('/') + 1)
                let asmName = asmName.Substring(0, asmName.Length - 4) // Remove .dll extension

                if Compiler.CoreAssemblyNames.Contains(asmName) then
                    coreAssemblies.Add(asmName, asm)
                else
                    let scanForPlugins =
                        try
                            asm.Contents.Attributes
                            |> Seq.exists (fun attr ->
                                attr.AttributeType.TryFullName = Some
                                    "Fable.ScanForPluginsAttribute"
                            )
                        with _ ->
                            // To help identify problem, log information about the exception
                            // but keep the process going to mimic previous Fable behavior
                            // and because these exception seems harmless
                            let errorMessage =
                                $"Could not scan {path} for Fable plugins, skipping this assembly"

#if !FABLE_COMPILER
                            Console.ForegroundColor <- ConsoleColor.Gray
#endif
                            Console.WriteLine(errorMessage)
#if !FABLE_COMPILER
                            Console.ResetColor()
#endif

                            hasSkippedAssembly <- true
                            false

                    if scanForPlugins then
                        for e in asm.Contents.Entities do
                            if
                                e.IsAttributeType
                                && FSharp2Fable.Util.inherits
                                    e
                                    "Fable.PluginAttribute"
                            then
                                try
                                    let plugin =
                                        getPlugin
                                            {
                                                DllPath = path
                                                TypeFullName = e.FullName
                                            }

                                    plugins.Add(
                                        FSharp2Fable.FsEnt.Ref e,
                                        plugin
                                    )
                                with ex ->
                                    let errorMessage =
                                        [
                                            $"Error while loading plugin: {e.FullName}"
                                            ""
                                            "This error often happens if you are trying to use a plugin that is not compatible with the current version of Fable."

                                            "If you see this error please open an issue at https://github.com/fable-compiler/Fable/"
                                            "so we can check if we can improve the plugin detection mechanism."
                                        ]
                                        |> String.concat "\n"

#if !FABLE_COMPILER
                                    Console.ForegroundColor <-
                                        ConsoleColor.DarkRed
#endif
                                    Console.WriteLine(errorMessage)
#if !FABLE_COMPILER
                                    Console.ResetColor()
#endif

                                    raise ex

                    assemblies.Add(path, asm)
            | None -> ()

        // Add a blank line to separate the error message from the rest of the output
        if hasSkippedAssembly then
            Console.WriteLine()

        ({ MemberDeclarationPlugins = Map.empty }, plugins)
        ||> Seq.fold (fun acc kv ->
            if
                kv.Value.IsSubclassOf(typeof<MemberDeclarationPluginAttribute>)
            then
                {
                    MemberDeclarationPlugins =
                        Map.add kv.Key kv.Value acc.MemberDeclarationPlugins
                }
            else
                acc
        )

    let tryFindEntityByPath (entityFullName: string) (asm: FSharpAssembly) =
        let key = asm.SimpleName + "|" + entityFullName

        entities
        |> Dictionary.tryFind key
        |> Option.orElseWith (fun () ->
            let entPath = List.ofArray (entityFullName.Split('.'))

            asm.Contents.FindEntityByPath(entPath)
            |> Option.map (fun e ->
                let fableEnt = FSharp2Fable.FsEnt e :> Fable.Entity
                entities[key] <- fableEnt
                fableEnt
            )
        )

    member _.TryGetEntityByAssemblyPath(asmPath, entityFullName) =
        assemblies
        |> Dictionary.tryFind asmPath
        |> Option.bind (tryFindEntityByPath entityFullName)

    member _.TryGetEntityByCoreAssemblyName(asmName, entityFullName) =
        coreAssemblies
        |> Dictionary.tryFind asmName
        |> Option.bind (tryFindEntityByPath entityFullName)

    member _.Plugins = plugins

type ImplFile =
    {
        Declarations: FSharpImplementationFileDeclaration list
        RootModule: string
        Entities: IReadOnlyDictionary<string, Fable.Entity>
        InlineExprs: (string * InlineExprLazy) list
    }

    static member From(file: FSharpImplementationFileContents) =
        let rec loop (entities: IDictionary<_, _>) (ents: FSharpEntity seq) =
            for e in ents do
                let fullName = FSharp2Fable.FsEnt.FullName e

                if
                    not e.IsFSharpAbbreviation
                    || not (entities.ContainsKey(fullName))
                then
                    entities[fullName] <- FSharp2Fable.FsEnt e :> Fable.Entity

                loop entities e.NestedEntities

        // add all entities to the entity cache
        let entities = Dictionary()
        let declarations = file.Declarations

        FSharp2Fable.Compiler.getRootFSharpEntities declarations
        |> loop entities

        {
            Declarations = declarations
            Entities = entities
            RootModule = FSharp2Fable.Compiler.getRootModule declarations
            InlineExprs =
                FSharp2Fable.Compiler.getInlineExprs file.FileName declarations
        }

type PrecompiledInfo =
    abstract DllPath: string
    abstract TryGetRootModule: normalizedFullPath: string -> string option
    abstract TryGetInlineExpr: memberUniqueName: string -> InlineExpr option

type Project
    private
    (
        projFile: string,
        sourceFiles: string[],
        implFiles: Map<string, ImplFile>,
        assemblies: Assemblies,
        ?precompiledInfo: PrecompiledInfo
    )
    =

    let inlineExprsDic =
        implFiles |> Map.values |> Seq.collect (fun f -> f.InlineExprs) |> dict

    let precompiledInfo =
        precompiledInfo
        |> Option.defaultWith (fun () ->
            { new PrecompiledInfo with
                member _.DllPath = ""
                member _.TryGetRootModule(_) = None
                member _.TryGetInlineExpr(_) = None
            }
        )

    static member From
        (
            projFile: string,
            sourceFiles: string[],
            fsharpFiles: FSharpImplementationFileContents list,
            fsharpAssemblies: FSharpAssembly list,
            ?getPlugin: PluginRef -> System.Type,
            ?precompiledInfo: PrecompiledInfo
        )
        =

        let getPlugin =
            defaultArg getPlugin (fun _ -> failwith "Plugins are not supported")

        let assemblies = Assemblies(getPlugin, fsharpAssemblies)

        let implFilesMap =
            fsharpFiles
            |> List.toArray
            |> Array.Parallel.map (fun file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                key, ImplFile.From(file)
            )
            |> Map

        Project(
            projFile,
            sourceFiles,
            implFilesMap,
            assemblies,
            ?precompiledInfo = precompiledInfo
        )

    member this.Update(files: FSharpImplementationFileContents list) =
        let implFiles =
            (this.ImplementationFiles, files)
            ||> List.fold (fun implFiles file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                let file = ImplFile.From(file)
                Map.add key file implFiles
            )

        Project(
            this.ProjectFile,
            this.SourceFiles,
            implFiles,
            this.Assemblies,
            this.PrecompiledInfo
        )

    member _.TryGetInlineExpr(com: Compiler, memberUniqueName: string) =
        inlineExprsDic
        |> Dictionary.tryFind memberUniqueName
        |> Option.map (fun e -> e.Calculate(com))

    member _.GetFileInlineExprs(com: Compiler) : (string * InlineExpr)[] =
        match Map.tryFind com.CurrentFile implFiles with
        | None -> [||]
        | Some implFile ->
            implFile.InlineExprs
            |> List.mapToArray (fun (uniqueName, expr) ->
                uniqueName, expr.Calculate(com)
            )

    member _.ProjectFile = projFile
    member _.SourceFiles = sourceFiles
    member _.ImplementationFiles = implFiles
    member _.Assemblies = assemblies
    member _.PrecompiledInfo = precompiledInfo

type Log =
    {
        Message: string
        Tag: string
        Severity: Severity
        Range: SourceLocation option
        FileName: string option
    }

    static member Make(severity, msg, ?fileName, ?range, ?tag) =
        {
            Message = msg
            Tag = defaultArg tag "FABLE"
            Severity = severity
            Range = range
            FileName = fileName
        }

    static member MakeError(msg, ?fileName, ?range, ?tag) =
        Log.Make(
            Severity.Error,
            msg,
            ?fileName = fileName,
            ?range = range,
            ?tag = tag
        )

/// Type with utilities for compiling F# files to JS.
/// Not thread-safe, an instance must be created per file
type CompilerImpl
    (
        currentFile,
        project: Project,
        options,
        fableLibDir: string,
        ?outType: OutputType,
        ?outDir: string,
        ?watchDependencies: HashSet<string>,
        ?logs: ResizeArray<Log>,
        ?isPrecompilingInlineFunction: bool
    )
    =

    let mutable counter = -1
    let outType = defaultArg outType OutputType.Exe
    let logs = Option.defaultWith ResizeArray logs
    let fableLibraryDir = fableLibDir.TrimEnd('/')

    member _.Logs = logs.ToArray()

    member _.WatchDependencies =
        match watchDependencies with
        | Some w -> Array.ofSeq w
        | None -> [||]

    interface Compiler with
        member _.Options = options
        member _.Plugins = project.Assemblies.Plugins
        member _.LibraryDir = fableLibraryDir
        member _.CurrentFile = currentFile
        member _.OutputDir = outDir
        member _.OutputType = outType
        member _.ProjectFile = project.ProjectFile
        member _.SourceFiles = project.SourceFiles

        member _.IncrementCounter() =
            counter <- counter + 1
            counter

        member _.IsPrecompilingInlineFunction =
            defaultArg isPrecompilingInlineFunction false

        member _.WillPrecompileInlineFunction(file) =
            let fableLibraryDir =
                if Path.isRelativePath fableLibraryDir then
                    Path.Combine(
                        Path.GetDirectoryName(currentFile),
                        fableLibraryDir
                    )
                else
                    fableLibraryDir
                |> Path.getRelativeFileOrDirPath false file true

            CompilerImpl(
                file,
                project,
                options,
                fableLibraryDir,
                outType,
                ?outDir = outDir,
                ?watchDependencies = watchDependencies,
                logs = logs,
                isPrecompilingInlineFunction = true
            )

        member _.GetImplementationFile(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName

            match Map.tryFind fileName project.ImplementationFiles with
            | Some file -> file.Declarations
            | None -> failwith ("Cannot find implementation file " + fileName)

        member this.GetRootModule(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName

            match Dictionary.tryFind fileName project.ImplementationFiles with
            | Some file -> file.RootModule
            | None ->
                match project.PrecompiledInfo.TryGetRootModule(fileName) with
                | Some r -> r
                | None ->
                    let msg =
                        $"Cannot find root module for {fileName}. If this belongs to a package, make sure it includes the source files."

                    (this :> Compiler)
                        .AddLog(msg, Severity.Warning, fileName = currentFile)

                    "" // failwith msg

        member _.TryGetEntity(entityRef: Fable.EntityRef) =
            match entityRef.Path with
            | Fable.CoreAssemblyName name ->
                project.Assemblies.TryGetEntityByCoreAssemblyName(
                    name,
                    entityRef.FullName
                )
            | Fable.AssemblyPath path
            | Fable.PrecompiledLib(_, path) ->
                project.Assemblies.TryGetEntityByAssemblyPath(
                    path,
                    entityRef.FullName
                )
            | Fable.SourcePath fileName ->
                // let fileName = Path.normalizePathAndEnsureFsExtension fileName
                project.ImplementationFiles
                |> Dictionary.tryFind fileName
                |> Option.bind (fun file ->
                    ReadOnlyDictionary.tryFind entityRef.FullName file.Entities
                )
                |> Option.orElseWith (fun () ->
                    // Check also the precompiled dll because this may come from a precompiled inline expr
                    project.Assemblies.TryGetEntityByAssemblyPath(
                        project.PrecompiledInfo.DllPath,
                        entityRef.FullName
                    )
                )

        member this.GetInlineExpr(memberUniqueName) =
            match project.TryGetInlineExpr(this, memberUniqueName) with
            | Some e -> e
            | None ->
                match
                    project.PrecompiledInfo.TryGetInlineExpr(memberUniqueName)
                with
                | Some e -> e
                | None ->
                    failwith ("Cannot find inline member: " + memberUniqueName)

        member _.AddWatchDependency(file) =
            match watchDependencies with
            | Some watchDependencies when file <> currentFile ->
                watchDependencies.Add(file) |> ignore
            | _ -> ()

        member _.AddLog
            (
                msg,
                severity,
                ?range,
                ?fileName: string,
                ?tag: string
            )
            =
            Log.Make(
                severity,
                msg,
                ?range = range,
                ?fileName = fileName,
                ?tag = tag
            )
            |> logs.Add
