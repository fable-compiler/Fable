module Fable.Transforms.State

open Fable
open Fable.AST
open System.Collections.Generic
open FSharp.Compiler.Symbols

type IDictionary<'Key, 'Value> with
    member this.TryValue(key: 'Key) =
        match this.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

type IReadOnlyDictionary<'Key, 'Value> with
    member this.TryValue(key: 'Key) =
        match this.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

type PluginRef =
    { DllPath: string
      TypeFullName: string }

type Assemblies(getPlugin, fsharpAssemblies: FSharpAssembly list) =
    let assemblies = Dictionary()
    let coreAssemblies = Dictionary()

    let plugins =
        let plugins = Dictionary<Fable.EntityRef, System.Type>()
        let coreAssemblyNames = HashSet Metadata.coreAssemblies

        for asm in fsharpAssemblies do
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

    let tryFindEntityByPath (entityRef: Fable.EntityRef) (asm: FSharpAssembly) =
        let entPath = List.ofArray (entityRef.FullName.Split('.'))
        asm.Contents.FindEntityByPath(entPath)
        |> Option.map(fun e -> FSharp2Fable.FsEnt e :> Fable.Entity)

    member _.TryGetEntityByAssemblyPath(asmPath, entityRef: Fable.EntityRef) =
        assemblies.TryValue(asmPath)
        |> Option.bind (tryFindEntityByPath entityRef)

    member _.TryGetEntityByCoreAssemblyName(asmName, entityRef: Fable.EntityRef) =
        coreAssemblies.TryValue(asmName)
        |> Option.bind (tryFindEntityByPath entityRef)

    member _.Plugins = plugins

type ImplFile =
    {
        Ast: FSharpImplementationFileContents
        RootModule: string
        Entities: IReadOnlyDictionary<string, Fable.Entity>
    }
    static member From(file: FSharpImplementationFileContents) =
        let entities = Dictionary()
        let rec loop (ents: FSharpEntity seq) =
            for e in ents do
                if e.IsFSharpAbbreviation then ()
                else
                    let fableEnt = FSharp2Fable.FsEnt e :> Fable.Entity
                    entities.Add(fableEnt.FullName, fableEnt)
                    loop e.NestedEntities

        FSharp2Fable.Compiler.getRootFSharpEntities file |> loop
        {
            Ast = file
            Entities = entities
            RootModule = FSharp2Fable.Compiler.getRootModule file
        }

type PrecompiledInfo =
    abstract TryGetRootModule: normalizedFullPath: string -> string option
    abstract TryGetInlineExpr: memberUniqueName: string -> InlineExpr option

type Project(projFile: string,
             implFiles: Map<string, ImplFile>,
             assemblies: Assemblies,
             ?inlineExprs: Map<string, InlineExpr>,
             ?precompiledInfo: PrecompiledInfo,
             ?trimRootModule: bool) =

    let inlineExprs = defaultArg inlineExprs Map.empty
    let trimRootModule = defaultArg trimRootModule true
    let precompiledInfo = precompiledInfo |> Option.defaultWith (fun () ->
        { new PrecompiledInfo with
            member _.TryGetRootModule(_) = None
            member _.TryGetInlineExpr(_) = None })

    static member From(projFile,
                       fsharpFiles: FSharpImplementationFileContents list,
                       fsharpAssemblies: FSharpAssembly list,
                       mkCompiler: Project -> string -> Compiler,
                       ?getPlugin: PluginRef -> System.Type,
                       ?precompiledInfo: PrecompiledInfo,
                       ?trimRootModule) =

        let getPlugin = defaultArg getPlugin (fun _ -> failwith "Plugins are not supported")
        let assemblies = Assemblies(getPlugin, fsharpAssemblies)

        let implFilesMap =
            fsharpFiles
            |> List.map (fun file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                key, ImplFile.From(file))
            |> Map

        Project(projFile, implFilesMap, assemblies, ?precompiledInfo=precompiledInfo, ?trimRootModule=trimRootModule)
            .UpdateInlineExprs(fsharpFiles, mkCompiler)

    member this.UpdateInlineExprs(fsharpFiles: FSharpImplementationFileContents list, mkCompiler: Project -> string -> Compiler) =
        let inlineExprs =
            (Map.empty, fsharpFiles) ||> List.fold (fun inlineExprs implFile ->
                let com = mkCompiler this implFile.FileName
                (inlineExprs, FSharp2Fable.Compiler.getInlineExprs com implFile)
                ||> List.fold (fun inlineExprs (k, v) -> Map.add k v inlineExprs)
            )
        Project(this.ProjectFile, this.ImplementationFiles, this.Assemblies, inlineExprs, this.PrecompiledInfo, this.TrimRootModule)

    member this.Update(fsharpFiles: FSharpImplementationFileContents list, mkCompiler) =
        let implFiles =
            (this.ImplementationFiles, fsharpFiles) ||> List.fold (fun implFiles file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                let file = ImplFile.From(file)
                Map.add key file implFiles)

        Project(this.ProjectFile, implFiles, this.Assemblies, this.InlineExprs, this.PrecompiledInfo, this.TrimRootModule)
            .UpdateInlineExprs(fsharpFiles, mkCompiler)

    member _.ProjectFile = projFile
    member _.ImplementationFiles = implFiles
    member _.Assemblies = assemblies
    member _.InlineExprs = inlineExprs
    member _.TrimRootModule = trimRootModule
    member _.PrecompiledInfo = precompiledInfo

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
            match Map.tryFind fileName project.ImplementationFiles with
            | Some file -> file.Ast
            | None -> failwith ("Cannot find implementation file " + fileName)

        member this.GetRootModule(fileName) =
            if not project.TrimRootModule then ""
            else
                let fileName = Path.normalizePathAndEnsureFsExtension fileName
                match project.ImplementationFiles.TryValue(fileName) with
                | Some file -> file.RootModule
                | None ->
                    match project.PrecompiledInfo.TryGetRootModule(fileName) with
                    | Some r -> r
                    | None ->
                        let msg = $"Cannot find root module for {fileName}. If this belongs to a package, make sure it includes the source files."
                        (this :> Compiler).AddLog(msg, Severity.Warning, fileName=currentFile)
                        "" // failwith msg

        member this.TryGetEntity(entityRef: Fable.EntityRef) =
            match entityRef.Path with
            | Fable.CoreAssemblyName name -> project.Assemblies.TryGetEntityByCoreAssemblyName(name, entityRef)
            | Fable.PrecompiledLib(_, path)
            | Fable.AssemblyPath path -> project.Assemblies.TryGetEntityByAssemblyPath(path, entityRef)
            | Fable.SourcePath fileName ->
                // let fileName = Path.normalizePathAndEnsureFsExtension fileName
                project.ImplementationFiles.TryValue(fileName)
                |> Option.bind (fun file -> file.Entities.TryValue(entityRef.FullName))

        member _.GetInlineExpr(memberUniqueName) =
            match project.InlineExprs.TryValue(memberUniqueName) with
            | Some e -> e
            | None ->
                match project.PrecompiledInfo.TryGetInlineExpr(memberUniqueName) with
                | Some e -> e
                | None -> failwith ("Cannot find inline member: " + memberUniqueName)

        member _.AddWatchDependency(file) =
            if file <> currentFile then
                watchDependencies.Add(file) |> ignore

        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            Log.Make(severity, msg, ?range=range, ?fileName=fileName, ?tag=tag)
            |> logs.Add
