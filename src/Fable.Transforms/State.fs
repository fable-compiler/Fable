module Fable.Transforms.State

open Fable
open Fable.AST
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols

#if FABLE_COMPILER
type Dictionary<'TKey, 'TValue> with
    member x.GetOrAdd (key, valueFactory) =
        match x.TryGetValue key with
        | true, v -> v
        | false, _ -> let v = valueFactory(key) in x.Add(key, v); v
    member x.AddOrUpdate (key, valueFactory, updateFactory) =
        if x.ContainsKey(key)
        then let v = updateFactory key x.[key] in x.[key] <- v; v
        else let v = valueFactory(key) in x.Add(key, v); v
    static member From<'K, 'V when 'K : equality>(kvs: KeyValuePair<'K, 'V> seq) =
        let d = Dictionary()
        for kv in kvs do
            d.Add(kv.Key, kv.Value)
        d

type ConcurrentDictionary<'TKey, 'TValue> = Dictionary<'TKey, 'TValue>
#else
open System.Collections.Concurrent
type ConcurrentDictionary<'TKey, 'TValue> with
    static member From(kvs: KeyValuePair<'TKey, 'TValue> seq) =
        ConcurrentDictionary(kvs)
#endif

type Package =
    { Id: string
      Version: string
      FsprojPath: string
      DllPath: string
      SourcePaths: string list
      Dependencies: Set<string> }

type PluginRef =
    { DllPath: string
      TypeFullName: string }

type Assemblies(getPlugin, checkResults: FSharpCheckProjectResults) =
    let assemblies = Dictionary()
    let coreAssemblies = Dictionary()

    let plugins =
        let plugins = Dictionary<Fable.EntityRef, System.Type>()
        let coreAssemblyNames = HashSet Metadata.coreAssemblies

        for asm in checkResults.ProjectContext.GetReferencedAssemblies() do
            match asm.FileName with
            | Some path ->
                let path = Path.normalizePath path
                let asmName = path.Substring(path.LastIndexOf('/') + 1).Replace(".dll", "")
                let isCoreAssembly = coreAssemblyNames.Contains(asmName)
                try
                    let scanForPlugins =
                        not isCoreAssembly && asm.Contents.Attributes |> Seq.exists (fun attr ->
                            attr.AttributeType.TryFullName = Some "Fable.ScanForPluginsAttribute")
                    if scanForPlugins then
                       for e in asm.Contents.Entities do
                               if e.IsAttributeType && FSharp2Fable.Util.inherits e "Fable.PluginAttribute" then
                                   let plugin = getPlugin { DllPath = path; TypeFullName = e.FullName }
                                   plugins.Add(FSharp2Fable.FsEnt.Ref e, plugin)
                with _ -> ()

                assemblies.Add(path, asm)
                if isCoreAssembly then
                    coreAssemblies.Add(asmName, asm)
            | None -> ()

        ({ MemberDeclarationPlugins = Map.empty }, plugins)
        ||> Seq.fold (fun acc kv ->
            if kv.Value.IsSubclassOf(typeof<MemberDeclarationPluginAttribute>) then
                { acc with MemberDeclarationPlugins = Map.add kv.Key kv.Value acc.MemberDeclarationPlugins }
            else acc)

    let findEntityByPath asmPathOrName (entityRef: Fable.EntityRef) (asm: FSharpAssembly option) =
        match asm with
        | Some asm ->
            let entPath = List.ofArray (entityRef.FullName.Split('.'))
            match asm.Contents.FindEntityByPath(entPath) with
            | Some e -> FSharp2Fable.FsEnt e :> Fable.Entity
            | None -> failwithf "Cannot find dll entity %s" entityRef.FullName
        | None -> failwithf "Cannot find assembly %s" asmPathOrName

    member _.GetEntityByAssemblyPath(asmPath, entityRef) =
        match assemblies.TryGetValue(asmPath) with
        | true, asm -> Some asm
        | false, _ -> None
        |> findEntityByPath asmPath entityRef

    member _.GetEntityByCoreAssemblyName(asmName, entityRef: Fable.EntityRef) =
        match coreAssemblies.TryGetValue(asmName) with
        | true, asm -> Some asm
        | false, _ -> None
        |> findEntityByPath asmName entityRef

    member _.Plugins = plugins

type ImplFile =
    {
        Ast: FSharpImplementationFileContents
        RootModule: string
        Entities: IReadOnlyDictionary<string, Fable.Entity>
    }

type Project(projFile: string,
             checkResults: FSharpCheckProjectResults,
             ?getPlugin: PluginRef -> System.Type,
             ?optimizeFSharpAst,
             ?assemblies,
             ?rootModule) =

    let rootModule = defaultArg rootModule true
    let optimizeFSharpAst = defaultArg optimizeFSharpAst false
    let getPlugin = defaultArg getPlugin (fun _ -> failwith "Plugins are not supported")
    let assemblies =
        match assemblies with
        | Some assemblies -> assemblies
        | None -> Assemblies(getPlugin, checkResults)
//    do printfn "MEMORY %i" (System.GC.GetTotalMemory(true))

    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()

    let implFiles =
        (if optimizeFSharpAst then checkResults.GetOptimizedAssemblyContents().ImplementationFiles
         else checkResults.AssemblyContents.ImplementationFiles)
        |> Seq.map (fun file ->
            let entities = Dictionary()
            let rec loop (ents: FSharpEntity seq) =
                for e in ents do
                    if e.IsFSharpAbbreviation then ()
                    else
                        let fableEnt = FSharp2Fable.FsEnt e :> Fable.Entity
                        entities.Add(fableEnt.FullName, fableEnt)
                        loop e.NestedEntities
            FSharp2Fable.Compiler.getRootFSharpEntities file |> loop
            let key = Path.normalizePathAndEnsureFsExtension file.FileName
            let rootModule = if rootModule then FSharp2Fable.Compiler.getRootModule file else ""
            key, { Ast = file; RootModule = rootModule; Entities = entities })
        |> dict

    member this.Update(checkResults: FSharpCheckProjectResults) =
        Project(this.ProjectFile, checkResults,
                optimizeFSharpAst=optimizeFSharpAst,
                rootModule=rootModule,
                assemblies=assemblies)

    member _.ProjectFile = projFile
    member _.ImplementationFiles = implFiles
    member _.Assemblies = assemblies
    member _.InlineExprs = inlineExprs
    member _.Errors = checkResults.Diagnostics

type Log =
    { Message: string
      Tag: string
      Severity: Severity
      Range: SourceLocation option
      FileName: string option }

    static member Make(severity, msg, ?fileName, ?range, ?tag) =
        { Message = msg
          Tag = defaultArg tag "FABLE"
          Severity = severity
          Range = range
          FileName = fileName }

    static member MakeError(msg, ?fileName, ?range, ?tag) =
        Log.Make(Severity.Error, msg, ?fileName=fileName, ?range=range, ?tag=tag)

/// Type with utilities for compiling F# files to JS
/// Not thread-safe, an instance must be created per file
type CompilerImpl(currentFile, project: Project, options, fableLibraryDir: string, ?outDir: string) =
    let logs = ResizeArray<Log>()
    let watchDependencies = HashSet<string>()
    let fableLibraryDir = fableLibraryDir.TrimEnd('/')

    member _.Options = options
    member _.CurrentFile = currentFile
    member _.Logs = logs.ToArray()
    member _.WatchDependencies = Array.ofSeq watchDependencies

    interface Compiler with
        member _.Options = options
        member _.Plugins = project.Assemblies.Plugins
        member _.LibraryDir = fableLibraryDir
        member _.CurrentFile = currentFile
        member _.OutputDir = outDir
        member _.ProjectFile = project.ProjectFile

        member _.GetImplementationFile(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match project.ImplementationFiles.TryGetValue(fileName) with
            | true, file -> file.Ast
            | false, _ -> failwith ("Cannot find implementation file " + fileName)

        member this.GetRootModule(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match project.ImplementationFiles.TryGetValue(fileName) with
            | true, file -> file.RootModule
            | false, _ ->
                let msg = $"Cannot find root module for {fileName}. If this belongs to a package, make sure it includes the source files."
                (this :> Compiler).AddLog(msg, Severity.Warning, fileName=currentFile)
                "" // failwith msg

        member _.GetEntity(entityRef: Fable.EntityRef) =
            match entityRef.Path with
            | Fable.CoreAssemblyName name -> project.Assemblies.GetEntityByCoreAssemblyName(name, entityRef)
            | Fable.PrecompiledLib(_, path)
            | Fable.AssemblyPath path -> project.Assemblies.GetEntityByAssemblyPath(path, entityRef)
            | Fable.SourcePath fileName ->
                // let fileName = Path.normalizePathAndEnsureFsExtension fileName
                match project.ImplementationFiles.TryGetValue(fileName) with
                | true, file ->
                    match file.Entities.TryGetValue(entityRef.FullName) with
                    | true, e -> e
                    | false, _ -> failwithf "Cannot find entity %s in %s" entityRef.FullName fileName
                | false, _ -> failwith ("Cannot find implementation file " + fileName)

        member _.GetOrAddInlineExpr(fullName, generate) =
            project.InlineExprs.GetOrAdd(fullName, fun _ -> generate())

        member _.AddWatchDependency(file) =
            if file <> currentFile then
                watchDependencies.Add(file) |> ignore

        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            Log.Make(severity, msg, ?range=range, ?fileName=fileName, ?tag=tag)
            |> logs.Add
