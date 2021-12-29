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
        for asm in fsharpAssemblies do
            match asm.FileName with
            | Some path ->
                let path = Path.normalizePath path
                let asmName = path.Substring(path.LastIndexOf('/') + 1)
                let asmName = asmName.Substring(0, asmName.Length - 4) // Remove .dll extension
                if Compiler.CoreAssemblyNames.Contains(asmName) then
                    coreAssemblies.Add(asmName, asm)
                else
                    try
                        let scanForPlugins =
                            asm.Contents.Attributes |> Seq.exists (fun attr ->
                                attr.AttributeType.TryFullName = Some "Fable.ScanForPluginsAttribute")
                        if scanForPlugins then
                           for e in asm.Contents.Entities do
                                   if e.IsAttributeType && FSharp2Fable.Util.inherits e "Fable.PluginAttribute" then
                                       let plugin = getPlugin { DllPath = path; TypeFullName = e.FullName }
                                       plugins.Add(FSharp2Fable.FsEnt.Ref e, plugin)
                    with _ -> ()
                    assemblies.Add(path, asm)
            | None -> ()

        ({ MemberDeclarationPlugins = Map.empty }, plugins)
        ||> Seq.fold (fun acc kv ->
            if kv.Value.IsSubclassOf(typeof<MemberDeclarationPluginAttribute>) then
                { acc with MemberDeclarationPlugins = Map.add kv.Key kv.Value acc.MemberDeclarationPlugins }
            else acc)

    let tryFindEntityByPath (entityFullName: string) (asm: FSharpAssembly) =
        let entPath = List.ofArray (entityFullName.Split('.'))
        asm.Contents.FindEntityByPath(entPath)
        |> Option.map(fun e -> FSharp2Fable.FsEnt e :> Fable.Entity)

    member _.TryGetEntityByAssemblyPath(asmPath, entityFullName) =
        assemblies.TryValue(asmPath)
        |> Option.bind (tryFindEntityByPath entityFullName)

    member _.TryGetEntityByCoreAssemblyName(asmName, entityFullName) =
        coreAssemblies.TryValue(asmName)
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
        let declarations = file.Declarations
        let entities = Dictionary()
        let rec loop (ents: FSharpEntity seq) =
            for e in ents do
                if e.IsFSharpAbbreviation then ()
                else
                    let fableEnt = FSharp2Fable.FsEnt e :> Fable.Entity
                    entities.Add(fableEnt.FullName, fableEnt)
                    loop e.NestedEntities

        FSharp2Fable.Compiler.getRootFSharpEntities declarations |> loop
        {
            Declarations = file.Declarations
            Entities = entities
            RootModule = FSharp2Fable.Compiler.getRootModule declarations
            InlineExprs = FSharp2Fable.Compiler.getInlineExprs file.FileName declarations
        }

type PrecompiledInfo =
    abstract DllPath: string
    abstract TryGetRootModule: normalizedFullPath: string -> string option
    abstract TryGetInlineExpr: memberUniqueName: string -> InlineExpr option

type Project(projFile: string,
             implFiles: Map<string, ImplFile>,
             assemblies: Assemblies,
             ?precompiledInfo: PrecompiledInfo) =

    let inlineExprsDic =
        implFiles
        |> Map.values
        |> Seq.map (fun f -> f.InlineExprs)
        |> Seq.concat
        |> dict

    let precompiledInfo = precompiledInfo |> Option.defaultWith (fun () ->
        { new PrecompiledInfo with
            member _.DllPath = ""
            member _.TryGetRootModule(_) = None
            member _.TryGetInlineExpr(_) = None })

    static member From(projFile: string,
                       fsharpFiles: FSharpImplementationFileContents list,
                       fsharpAssemblies: FSharpAssembly list,
                       ?getPlugin: PluginRef -> System.Type,
                       ?precompiledInfo: PrecompiledInfo) =

        let getPlugin = defaultArg getPlugin (fun _ -> failwith "Plugins are not supported")
        let assemblies = Assemblies(getPlugin, fsharpAssemblies)

        let implFilesMap =
            fsharpFiles
            |> List.toArray
            |> Array.Parallel.map (fun file ->
                let key = Path.normalizePathAndEnsureFsExtension file.FileName
                key, ImplFile.From(file))
            |> Map

        Project(projFile, implFilesMap, assemblies, ?precompiledInfo=precompiledInfo)

    member this.Update(file: FSharpImplementationFileContents) =
        let implFiles =
            let key = Path.normalizePathAndEnsureFsExtension file.FileName
            let file = ImplFile.From(file)
            Map.add key file this.ImplementationFiles
        Project(this.ProjectFile, implFiles, this.Assemblies, this.PrecompiledInfo)

    member _.TryGetInlineExpr(com: Compiler, memberUniqueName: string) =
        inlineExprsDic.TryValue(memberUniqueName)
        |> Option.map (fun e -> e.Calculate(com))

    member _.GetFileInlineExprs(com: Compiler): (string * InlineExpr)[] =
        match Map.tryFind com.CurrentFile implFiles with
        | None -> [||]
        | Some implFile ->
            implFile.InlineExprs
            |> List.mapToArray (fun (uniqueName, expr) ->
                uniqueName, expr.Calculate(com))

    member _.ProjectFile = projFile
    member _.ImplementationFiles = implFiles
    member _.Assemblies = assemblies
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

/// Type with utilities for compiling F# files to JS.
/// Not thread-safe, an instance must be created per file
type CompilerImpl(currentFile, project: Project, options, fableLibraryDir: string, ?outDir: string, ?outType: string, ?watchDependencies) =
    let logs = ResizeArray<Log>()
    let watchDependencies = defaultArg watchDependencies false
    let watchDependenciesSet = HashSet<string>()
    let fableLibraryDir = fableLibraryDir.TrimEnd('/')
    let outputType =
        match outType with
        | Some "Exe" -> OutputType.Exe
        | Some "Module" -> OutputType.Module
        | Some "Winexe" -> OutputType.Winexe
        | _ -> OutputType.Library

    member _.Options = options
    member _.CurrentFile = currentFile
    member _.Logs = logs.ToArray()
    member _.WatchDependencies = Array.ofSeq watchDependenciesSet

    interface Compiler with
        member _.Options = options
        member _.Plugins = project.Assemblies.Plugins
        member _.LibraryDir = fableLibraryDir
        member _.CurrentFile = currentFile
        member _.OutputDir = outDir
        member _.OutputType = outputType
        member _.ProjectFile = project.ProjectFile

        member _.GetImplementationFile(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match Map.tryFind fileName project.ImplementationFiles with
            | Some file -> file.Declarations
            | None -> failwith ("Cannot find implementation file " + fileName)

        member this.GetRootModule(fileName) =
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

        member _.TryGetEntity(entityRef: Fable.EntityRef) =
            match entityRef.Path with
            | Fable.CoreAssemblyName name -> project.Assemblies.TryGetEntityByCoreAssemblyName(name, entityRef.FullName)
            | Fable.AssemblyPath path
            | Fable.PrecompiledLib(_, path) -> project.Assemblies.TryGetEntityByAssemblyPath(path, entityRef.FullName)
            | Fable.SourcePath fileName ->
                // let fileName = Path.normalizePathAndEnsureFsExtension fileName
                project.ImplementationFiles.TryValue(fileName)
                |> Option.bind (fun file -> file.Entities.TryValue(entityRef.FullName))
                |> Option.orElseWith (fun () ->
                    project.Assemblies.TryGetEntityByAssemblyPath(project.PrecompiledInfo.DllPath, entityRef.FullName))

        member this.GetInlineExpr(memberUniqueName) =
            match project.TryGetInlineExpr(this, memberUniqueName) with
            | Some e -> e
            | None ->
                match project.PrecompiledInfo.TryGetInlineExpr(memberUniqueName) with
                | Some e -> e
                | None -> failwith ("Cannot find inline member: " + memberUniqueName)

        member _.AddWatchDependency(file) =
            if watchDependencies && file <> currentFile then
                watchDependenciesSet.Add(file) |> ignore

        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            Log.Make(severity, msg, ?range=range, ?fileName=fileName, ?tag=tag)
            |> logs.Add
