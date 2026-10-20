module Fable.Transforms.State

open Fable
open Fable.AST
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open System

type PluginRef =
    {
        DllPath: string
        TypeFullName: string
    }

type Assemblies
    (
        getPlugin: (PluginRef -> System.Type) option,
        fsharpAssemblies: FSharpAssembly list,
        addLog: Severity -> string -> unit
    )
    =
    let assemblies = Dictionary()
    let coreAssemblies = Dictionary()
    let entities = ConcurrentDictionary<string, Fable.Entity>()

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
                        if Metadata.isSystemPackage asmName then
                            false
                        else
                            try
                                asm.Contents.Attributes
                                |> Seq.exists (fun attr ->
                                    attr.AttributeType.TryFullName = Some "Fable.ScanForPluginsAttribute"
                                )
                            with error ->
                                // To help identify problem, log information about the exception
                                // but keep the process going to mimic previous Fable behavior
                                // and because these exception seems harmless
                                let errorMessage =
                                    $"Could not scan %s{path} for Fable plugins, skipping this assembly. Original error: %s{error.Message}"

                                addLog Severity.Info errorMessage

                                hasSkippedAssembly <- true
                                false

                    if scanForPlugins then
                        let pluginEntities =
                            asm.Contents.Entities
                            |> Seq.filter (fun e ->
                                e.IsAttributeType && FSharp2Fable.Util.inherits e "Fable.PluginAttribute"
                            )
                            |> Seq.toList

                        match getPlugin with
                        | None ->
                            if not pluginEntities.IsEmpty then
                                let names = pluginEntities |> List.map (fun e -> e.FullName) |> String.concat ", "

                                addLog
                                    Severity.Warning
                                    $"Skipping Fable plugins from %s{asmName} because plugins cannot be loaded in this environment: %s{names}"
                        | Some getPlugin ->
                            for e in pluginEntities do
                                try
                                    let plugin =
                                        getPlugin
                                            {
                                                DllPath = path
                                                TypeFullName = e.FullName
                                            }

                                    plugins.Add(FSharp2Fable.FsEnt.Ref e, plugin)
                                with ex ->
                                    let errorMessage =
                                        [
                                            $"Error while loading plugin: %s{e.FullName}. Original error: %s{ex.Message}"
                                            ""
                                            "This error often happens if you are trying to use a plugin that is not compatible with the current version of Fable."
                                            "If you see this error please open an issue at https://github.com/fable-compiler/Fable/"
                                            "so we can check if we can improve the plugin detection mechanism."
                                        ]
                                        |> String.concat Environment.NewLine

                                    addLog Severity.Error errorMessage

                                    raise ex

                    assemblies.Add(path, asm)
            | None -> ()

        // Add a blank line to separate the error message from the rest of the output
        if hasSkippedAssembly then
            addLog Severity.Info "" // or Environment.NewLine

        ({ MemberDeclarationPlugins = Map.empty }, plugins)
        ||> Seq.fold (fun acc kv ->
            if kv.Value.IsSubclassOf(typeof<MemberDeclarationPluginAttribute>) then
                { MemberDeclarationPlugins = Map.add kv.Key kv.Value acc.MemberDeclarationPlugins }
            else
                acc
        )

    let tryFindEntityByPath (entityFullName: string) (asm: FSharpAssembly) =
        let key = asm.SimpleName + "|" + entityFullName

        match entities.TryGetValue(key) with
        | true, v -> Some v
        | false, _ ->
            let entPath = List.ofArray (entityFullName.Split('.'))

            asm.Contents.FindEntityByPath(entPath)
            |> Option.map (fun e ->
                let fableEnt = FSharp2Fable.FsEnt e :> Fable.Entity
                entities[key] <- fableEnt
                fableEnt
            )

    member _.TryGetEntityByAssemblyPath(asmPath, entityFullName) =
        assemblies
        |> Dictionary.tryFind asmPath
        |> Option.bind (tryFindEntityByPath entityFullName)

    member _.TryGetEntityByCoreAssemblyName(asmName, entityFullName) =
        let key = asmName + "|" + entityFullName

        match entities.TryGetValue(key) with
        | true, v -> Some v
        | false, _ ->
            coreAssemblies
            |> Dictionary.tryFind asmName
            |> Option.bind (tryFindEntityByPath entityFullName)
            // A facade like netstandard.dll forwards its types instead of declaring them, so the
            // assembly an entity was recorded under may not be the one that has it
            |> Option.orElseWith (fun () -> coreAssemblies.Values |> Seq.tryPick (tryFindEntityByPath entityFullName))
            |> Option.map (fun ent ->
                entities[key] <- ent
                ent
            )

    member _.Plugins = plugins

type ParameterSignature =
    {
        Name: string option
        IsOptional: bool
        IsParamArray: bool
    }

type MemberSignature =
    {
        Type: Fable.Type
        Parameters: ParameterSignature list list
        IsInline: FSharpInlineAnnotation
        IsMutable: bool
        Attributes: (string * obj list) list
    }

type EntitySignature =
    {
        Attributes: (string * obj list) list
        Kind: bool * bool * bool * bool * bool
        Fields: (string * Fable.Type * obj) list
        UnionCases: (string * (string * Fable.Type) list) list
    }

/// The parts of a file other files can depend on without naming them in their own typed tree
type FileSignature =
    {
        IsUnknown: bool
        Literals: Map<string, obj>
        Members: Map<string, MemberSignature>
        Entities: Map<string, EntitySignature>
    }

    static member Unknown =
        {
            IsUnknown = true
            Literals = Map.empty
            Members = Map.empty
            Entities = Map.empty
        }

    static member Changed(before: FileSignature, after: FileSignature) =
        before.IsUnknown || after.IsUnknown || before <> after

    static member LiteralsChanged(before: FileSignature, after: FileSignature) =
        before.IsUnknown || after.IsUnknown || before.Literals <> after.Literals

type ImplFile =
    {
        Declarations: FSharpImplementationFileDeclaration list
        RootModule: string
        RootComment: FSharpXmlDoc option
        Entities: IReadOnlyDictionary<string, Fable.Entity>
        InlineExprs: (string * InlineExprLazy) list
        Signature: FileSignature
    }

    static member From(file: FSharpImplementationFileContents) =
        let rec loop (entities: IDictionary<_, _>) (ents: FSharpEntity seq) =
            for e in ents do
                let fullName = FSharp2Fable.FsEnt.FullName e

                if not e.IsFSharpAbbreviation || not (entities.ContainsKey(fullName)) then
                    entities[fullName] <- FSharp2Fable.FsEnt e :> Fable.Entity

                loop entities e.NestedEntities

        let signature (declarations: FSharpImplementationFileDeclaration list) =
            try
                let makeType = FSharp2Fable.TypeHelpers.makeType Map.empty

                let attributes (atts: FSharpAttribute seq) =
                    atts
                    |> Seq.map (fun att ->
                        FSharp2Fable.FsEnt.FullName att.AttributeType,
                        att.ConstructorArguments |> Seq.map snd |> List.ofSeq
                    )
                    |> List.ofSeq

                let rec members (decls: FSharpImplementationFileDeclaration list) =
                    decls
                    |> List.collect (
                        function
                        | FSharpImplementationFileDeclaration.Entity(_, decls) -> members decls
                        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb, _, _) when
                            not memb.IsCompilerGenerated && not memb.Accessibility.IsPrivate
                            ->
                            [ memb ]
                        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue _
                        | FSharpImplementationFileDeclaration.InitAction _ -> []
                    )

                let rec entities (ents: FSharpEntity seq) =
                    ents |> Seq.collect (fun e -> Seq.append [ e ] (entities e.NestedEntities))

                let members = members declarations

                {
                    IsUnknown = false
                    Literals =
                        members
                        |> List.choose (fun memb ->
                            memb.LiteralValue
                            |> Option.map (fun value -> FSharp2Fable.Helpers.getMemberUniqueName memb, value)
                        )
                        |> Map
                    Members =
                        members
                        |> List.map (fun memb ->
                            FSharp2Fable.Helpers.getMemberUniqueName memb,
                            {
                                Type = makeType memb.FullType
                                Parameters =
                                    memb.CurriedParameterGroups
                                    |> Seq.map (
                                        Seq.map (fun p ->
                                            {
                                                Name = p.Name
                                                IsOptional = p.IsOptionalArg
                                                IsParamArray = p.IsParamArrayArg
                                            }
                                        )
                                        >> List.ofSeq
                                    )
                                    |> List.ofSeq
                                IsInline = memb.InlineAnnotation
                                IsMutable = memb.IsMutable
                                Attributes = attributes memb.Attributes
                            }
                        )
                        |> Map
                    Entities =
                        FSharp2Fable.Compiler.getRootFSharpEntities declarations
                        |> entities
                        |> Seq.map (fun e ->
                            FSharp2Fable.FsEnt.FullName e,
                            {
                                Attributes = attributes e.Attributes
                                Kind = e.IsFSharpRecord, e.IsFSharpUnion, e.IsEnum, e.IsInterface, e.IsValueType
                                Fields =
                                    e.FSharpFields
                                    |> Seq.map (fun f -> f.Name, makeType f.FieldType, Option.toObj f.LiteralValue)
                                    |> List.ofSeq
                                UnionCases =
                                    e.UnionCases
                                    |> Seq.map (fun c ->
                                        c.Name,
                                        c.Fields |> Seq.map (fun f -> f.Name, makeType f.FieldType) |> List.ofSeq
                                    )
                                    |> List.ofSeq
                            }
                        )
                        |> Map
                }
            with _ ->
                FileSignature.Unknown

        // add all entities to the entity cache
        let entities = Dictionary()

        let declarations =
            try
                file.Declarations // this may throw
            with _ex ->
                []

        FSharp2Fable.Compiler.getRootFSharpEntities declarations |> loop entities
        let rootModule, rootComment = FSharp2Fable.Compiler.getRootModule declarations

        {
            Declarations = declarations
            Entities = entities
            RootModule = rootModule
            RootComment = rootComment
            InlineExprs = FSharp2Fable.Compiler.getInlineExprs file.FileName declarations
            Signature = signature declarations
        }

type PrecompiledInfo =
    abstract DllPath: string
    abstract TryGetRootModule: normalizedFullPath: string -> string option
    abstract TryGetInlineExpr: memberUniqueName: string -> InlineExpr option

let private removeAll keys map =
    (map, keys) ||> Seq.fold (fun map key -> Map.remove key map)

let private addAll entries map =
    (map, entries) ||> Seq.fold (fun map (key, value) -> Map.add key value map)

type Project
    private
    (
        projectFile: string,
        projectOptions: FSharpProjectOptions,
        implFiles: Map<string, ImplFile>,
        assemblies: Assemblies,
        ?precompiledInfo: PrecompiledInfo,
        ?inlineExprs: Map<string, InlineExprLazy>
    )
    =

    let inlineExprs =
        inlineExprs
        |> Option.defaultWith (fun () -> implFiles |> Map.values |> Seq.collect (fun f -> f.InlineExprs) |> Map)

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
            projectFile: string,
            projectOptions: FSharpProjectOptions,
            fsharpFiles: FSharpImplementationFileContents list,
            fsharpAssemblies: FSharpAssembly list,
            addLog: Severity -> string -> unit,
            ?getPlugin: PluginRef -> System.Type,
            ?precompiledInfo: PrecompiledInfo
        )
        =

        let checknulls = projectOptions.OtherOptions |> Array.exists ((=) "--checknulls+")
        Compiler.SetCheckNullsUnsafe checknulls // set it one time only as early as possible

        let assemblies = Assemblies(getPlugin, fsharpAssemblies, addLog)

        let implFilesMap =
            fsharpFiles
            |> List.toArray
            |> Array.Parallel.map (fun file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                key, ImplFile.From(file)
            )
            |> Map

        Project(projectFile, projectOptions, implFilesMap, assemblies, ?precompiledInfo = precompiledInfo)

    member this.Update(files: FSharpImplementationFileContents list) =
        let implFiles, inlineExprs =
            ((this.ImplementationFiles, inlineExprs), files)
            ||> List.fold (fun (implFiles, inlineExprs) file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                let file = ImplFile.From(file)

                let previousInlineExprs =
                    match Map.tryFind key implFiles with
                    | Some previous -> previous.InlineExprs
                    | None -> []

                let inlineExprs =
                    inlineExprs
                    |> removeAll (List.map fst previousInlineExprs)
                    |> addAll file.InlineExprs

                Map.add key file implFiles, inlineExprs
            )

        Project(
            this.ProjectFile,
            this.ProjectOptions,
            implFiles,
            this.Assemblies,
            this.PrecompiledInfo,
            inlineExprs = inlineExprs
        )

    member _.TryGetInlineExpr(com: Compiler, memberUniqueName: string) =
        Map.tryFind memberUniqueName inlineExprs
        |> Option.map (fun e -> e.Calculate(com))

    member _.GetFileInlineExprs(com: Compiler) : (string * InlineExpr) array =
        match Map.tryFind com.CurrentFile implFiles with
        | None -> [||]
        | Some implFile ->
            implFile.InlineExprs
            |> List.mapToArray (fun (uniqueName, expr) -> uniqueName, expr.Calculate(com))

    member _.ProjectFile = projectFile
    member _.ProjectOptions = projectOptions
    member _.ImplementationFiles = implFiles
    member _.Assemblies = assemblies
    member _.PrecompiledInfo = precompiledInfo

type LogEntry =
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
        LogEntry.Make(Severity.Error, msg, ?fileName = fileName, ?range = range, ?tag = tag)

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
        ?signatureDependencies: HashSet<string>,
        ?logs: ResizeArray<LogEntry>,
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

    member _.SignatureDependencies =
        match signatureDependencies with
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
        member _.ProjectOptions = project.ProjectOptions
        member _.SourceFiles = project.ProjectOptions.SourceFiles

        member _.IncrementCounter() =
            counter <- counter + 1
            counter

        member _.IsPrecompilingInlineFunction = defaultArg isPrecompilingInlineFunction false

        member _.WillPrecompileInlineFunction(file) =
            let fableLibraryDir =
                if Path.isRelativePath fableLibraryDir then
                    Path.Combine(Path.GetDirectoryName(currentFile), fableLibraryDir)
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
                ?signatureDependencies = signatureDependencies,
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
            | Some file -> file.RootModule, file.RootComment
            | None ->
                match project.PrecompiledInfo.TryGetRootModule(fileName) with
                | Some r -> r, None
                | None ->
                    let msg =
                        $"Cannot find root module for %s{fileName}. If this belongs to a package, make sure it includes the source files."

                    (this :> Compiler).AddLog(msg, Severity.Warning, fileName = currentFile)

                    "", None // failwith msg

        member _.TryGetEntity(entRef: Fable.EntityRef) =
            match entRef.Path with
            | Fable.CoreAssemblyName name -> project.Assemblies.TryGetEntityByCoreAssemblyName(name, entRef.FullName)
            | Fable.AssemblyPath path
            | Fable.PrecompiledLib(_, path) -> project.Assemblies.TryGetEntityByAssemblyPath(path, entRef.FullName)
            | Fable.SourcePath fileName ->
                // let fileName = Path.normalizePathAndEnsureFsExtension fileName
                project.ImplementationFiles
                |> Dictionary.tryFind fileName
                |> Option.bind (fun file -> ReadOnlyDictionary.tryFind entRef.FullName file.Entities)
                |> Option.orElseWith (fun () ->
                    // Check also the precompiled dll because this may come from a precompiled inline expr
                    project.Assemblies.TryGetEntityByAssemblyPath(project.PrecompiledInfo.DllPath, entRef.FullName)
                )

        member this.GetInlineExpr(memberUniqueName) =
            match project.TryGetInlineExpr(this, memberUniqueName) with
            | Some e -> e
            | None ->
                match project.PrecompiledInfo.TryGetInlineExpr(memberUniqueName) with
                | Some e -> e
                | None ->
                    failwith (
                        $"Cannot find the body of inline member '%s{memberUniqueName}' while compiling %s{currentFile}. "
                        + "If it belongs to a package, make sure the package includes its F# sources; "
                        + $"if it belongs to a precompiled library, make sure it was precompiled with Fable %s{Literals.VERSION}."
                    )

        member _.AddWatchDependency(file) =
            match watchDependencies with
            | Some watchDependencies when file <> currentFile -> watchDependencies.Add(file) |> ignore
            | _ -> ()

        member _.AddSignatureDependency(file) =
            match signatureDependencies with
            | Some signatureDependencies when file <> currentFile -> signatureDependencies.Add(file) |> ignore
            | _ -> ()

        member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
            LogEntry.Make(severity, msg, ?range = range, ?fileName = fileName, ?tag = tag)
            |> logs.Add
