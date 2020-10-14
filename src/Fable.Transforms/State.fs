module Fable.Transforms.State

open Fable
open Fable.AST
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices

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

let private entityRefToString (r: Fable.EntityRef) =
    match r.SourcePath with
    | Some x -> x + "::" + r.QualifiedName
    | None -> r.QualifiedName

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

type Project(projectOptions: FSharpProjectOptions,
             checkResults: FSharpCheckProjectResults,
             ?packages: Package list,
             ?getPlugin: PluginRef -> System.Type,
             ?optimize: bool) =

    let packages = defaultArg packages []
    let getPlugin = defaultArg getPlugin (fun _ -> failwith "Plugins are not supported")
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    let plugins = Dictionary<Fable.EntityRef, System.Type>()

    let registerPlugins (atts: IList<FSharpAttribute>) =
        for att in atts do
            try
                match att.AttributeType.TryFullName with
                | Some "Fable.RegisterPluginAttribute" ->
                    let t = snd att.ConstructorArguments.[0] :?> FSharpType
                    match FSharp2Fable.Helpers.tryDefinition t with
                    | Some(pluginEntity, Some fullName) ->
                        let dllPath =
                            match pluginEntity.Assembly.FileName with
                            | Some dllPath -> dllPath
                            | None ->
                                let sourcePath = FSharp2Fable.FsEnt.SourcePath pluginEntity
                                let pkg =
                                    packages
                                    |> List.tryFind (fun pkg ->
                                        pkg.SourcePaths |> List.contains sourcePath)
                                    |> Option.defaultWith (fun () -> failwithf "Cannot find dll path for plugin %s" fullName)
                                pkg.DllPath
                        let plugin = getPlugin { DllPath = dllPath; TypeFullName = fullName }
                        plugins.Add(FSharp2Fable.FsEnt.Ref pluginEntity, plugin)
                    | _ -> ()
                | _ -> ()
            with _ -> ()

    let rec withNestedEntities useSourcePath (e: FSharpEntity) =
        if e.IsFSharpAbbreviation then Seq.empty
        else
            seq  {
                let sourcePath = if useSourcePath then Some(FSharp2Fable.FsEnt.SourcePath e) else None
                yield ({ QualifiedName = FSharp2Fable.FsEnt.QualifiedName e
                         SourcePath = sourcePath }: Fable.EntityRef), e
                if e.IsFSharpModule then
                    for sub in e.NestedEntities do
                        yield! withNestedEntities useSourcePath sub
            }

    let implFiles =
        (if defaultArg optimize false then checkResults.GetOptimizedAssemblyContents().ImplementationFiles
         else checkResults.AssemblyContents.ImplementationFiles)
        |> Seq.map (fun file -> Path.normalizePathAndEnsureFsExtension file.FileName, file)
        |> dict

//    if List.isEmpty implFiles then
//        Log.always "The list of files returned by F# compiler is empty"

    let rootModules =
        implFiles |> Seq.map (fun kv ->
            kv.Key, FSharp2Fable.Compiler.getRootModule kv.Value) |> dict

    let entities =
        let dllEntities =
            checkResults.ProjectContext.GetReferencedAssemblies()
            |> Seq.collect (fun a ->
                registerPlugins a.Contents.Attributes
                a.Contents.Entities
                |> Seq.collect (withNestedEntities false))

        registerPlugins checkResults.AssemblySignature.Attributes

        let projEntities =
            checkResults.AssemblyContents.ImplementationFiles
            |> Seq.collect (fun file ->
                FSharp2Fable.Compiler.getRootFSharpEntities file
                |> Seq.collect (withNestedEntities true))

        Seq.append dllEntities projEntities
        |> Seq.map (fun (r, e) ->
            KeyValuePair(entityRefToString r, FSharp2Fable.FsEnt e :> Fable.Entity))
        |> ConcurrentDictionary.From

    let plugins =
        ({ CallPlugins = Map.empty
           MemberDeclarationPlugins = Map.empty }, plugins)
        ||> Seq.fold (fun acc kv ->
            if kv.Value.IsSubclassOf(typeof<MemberDeclarationPluginAttribute>) then
                { acc with MemberDeclarationPlugins = Map.add kv.Key kv.Value acc.MemberDeclarationPlugins }
            elif kv.Value.IsSubclassOf(typeof<CallPluginAttribute>) then
                { acc with CallPlugins = Map.add kv.Key kv.Value acc.CallPlugins }
            else acc)

    member _.Plugins = plugins
    member _.ImplementationFiles = implFiles
    member _.RootModules = rootModules
    member _.Entities = entities
    member _.InlineExprs = inlineExprs
    member _.Errors = checkResults.Errors
    member _.ProjectOptions = projectOptions

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
type CompilerImpl(currentFile, project: Project, options, fableLibraryDir: string) =
    let logs = ResizeArray<Log>()
    let watchDependencies = HashSet<string>()
    let fableLibraryDir = fableLibraryDir.TrimEnd('/')

    member _.Options = options
    member _.CurrentFile = currentFile
    member _.Logs = logs.ToArray()
    member _.WatchDependencies = Array.ofSeq watchDependencies

    interface Compiler with
        member _.Options = options
        member _.Plugins = project.Plugins
        member _.LibraryDir = fableLibraryDir
        member _.CurrentFile = currentFile
        member _.ImplementationFiles = project.ImplementationFiles

        member this.GetRootModule(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match project.RootModules.TryGetValue(fileName) with
            | true, rootModule -> rootModule
            | false, _ ->
                let msg = sprintf "Cannot find root module for %s. If this belongs to a package, make sure it includes the source files." fileName
                (this :> Compiler).AddLog(msg, Severity.Warning, fileName=currentFile)
                "" // failwith msg

        member _.GetEntity(entityRef) =
            let rec findParent fullName (trimmed: string) =
                let i = trimmed.LastIndexOf("+")
                if i > 0 then
                    let trimmed = trimmed.[..i-1]
                    if project.Entities.ContainsKey(trimmed) then project.Entities.[trimmed]
                    else findParent fullName trimmed
                else
                    failwithf "Cannot find parent of %s" fullName

            let rec findEntity fullName (entities: FSharpEntity seq) =
                entities
                |> Seq.tryPick(fun e ->
                    match e.TryFullName with
                    | Some fullName2 when fullName = fullName2 -> Some e
                    | Some fullName2 when fullName.StartsWith(fullName2 + ".") ->
                        findEntity fullName e.NestedEntities
                    | _ -> None)

            let fullName = entityRefToString entityRef
            project.Entities.GetOrAdd(fullName, fun _ ->
                match findParent fullName fullName with
                | :? FSharp2Fable.FsEnt as e -> findEntity fullName e.FSharpEntity.NestedEntities
                | _ -> None
                |> function
                    | Some e -> FSharp2Fable.FsEnt e :> Fable.Entity
                    | None -> failwithf "Cannot find entity %s" fullName)

        member _.GetOrAddInlineExpr(fullName, generate) =
            project.InlineExprs.GetOrAdd(fullName, fun _ -> generate())

        member _.AddWatchDependency(file) =
            if file <> currentFile then
                watchDependencies.Add(file) |> ignore

        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            Log.Make(severity, msg, ?range=range, ?fileName=fileName, ?tag=tag)
            |> logs.Add
