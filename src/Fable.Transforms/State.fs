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

type Project(projectOptions: FSharpProjectOptions,
             checkResults: FSharpCheckProjectResults,
             ?optimize: bool) =

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

    let rec withNestedEntities (e: FSharpEntity) =
        seq  {
            yield e
            if e.IsFSharpModule then
                for sub in e.NestedEntities do
                    yield! withNestedEntities sub
        }

    let entities =
        checkResults.ProjectContext.GetReferencedAssemblies()
        |> Seq.collect (fun a -> a.Contents.Entities)
        |> Seq.append (Seq.collect FSharp2Fable.Compiler.getRootFSharpEntities
                        checkResults.AssemblyContents.ImplementationFiles)
        |> Seq.collect withNestedEntities
        |> Seq.choose (fun e ->
            match e.IsFSharpAbbreviation, e.TryFullName with
            | false, Some name -> KeyValuePair(name, FSharp2Fable.FsEnt e :> Fable.Entity) |> Some
            | _ -> None)
        |> Seq.distinctBy (fun kv -> kv.Key)
        |> ConcurrentDictionary<string, Fable.Entity>.From

    let entitySourcePaths = ConcurrentDictionary<string, string>()
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()

    member _.ImplementationFiles = implFiles
    member _.RootModules = rootModules
    member _.Entities = entities
    member _.EntitySourcePaths = entitySourcePaths
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
        member _.LibraryDir = fableLibraryDir
        member _.CurrentFile = currentFile
        member _.ImplementationFiles = project.ImplementationFiles

        member x.GetRootModule(fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match project.RootModules.TryGetValue(fileName) with
            | true, rootModule -> rootModule
            | false, _ ->
                let msg = sprintf "Cannot find root module for %s. If this belongs to a package, make sure it includes the source files." fileName
                (x :> Compiler).AddLog(msg, Severity.Warning)
                "" // failwith msg

        member _.GetEntity(fullName) =
            let rec findEntity fullName (entities: FSharpEntity seq) =
                entities
                |> Seq.tryPick(fun e ->
                    match e.TryFullName with
                    | Some fullName2 when fullName = fullName2 -> Some e
                    | Some fullName2 when fullName.StartsWith(fullName2 + ".") ->
                        findEntity fullName e.NestedEntities
                    | _ -> None)

            let rec findParent fullName (trimmed: string) =
                let i = trimmed.LastIndexOf(".")
                if i > 0 then
                    let trimmed = trimmed.[..i-1]
                    if project.Entities.ContainsKey(trimmed) then project.Entities.[trimmed]
                    else findParent fullName trimmed
                else
                    failwithf "Cannot find parent of %s" fullName

            project.Entities.GetOrAdd(fullName, fun _ ->
                match findParent fullName fullName with
                | :? FSharp2Fable.FsEnt as e -> findEntity fullName e.FSharpEntity.NestedEntities
                | _ -> None
                |> function
                    | Some e -> FSharp2Fable.FsEnt e :> Fable.Entity
                    | None -> failwithf "Cannot find entity %s" fullName)

        member this.GetEntitySourcePath(fullName) =
            project.EntitySourcePaths.GetOrAdd(fullName, fun _ ->
                let ent = (this :> Compiler).GetEntity(fullName)
                match ent with
                | :? FSharp2Fable.FsEnt as e ->
                    e.FSharpEntity.DeclarationLocation.FileName
                    |> Path.normalizePathAndEnsureFsExtension
                | _ -> failwithf "Cannot get source path for %s" ent.FullName)

        member _.GetOrAddInlineExpr(fullName, generate) =
            project.InlineExprs.GetOrAdd(fullName, fun _ -> generate())

        member _.AddWatchDependency(file) =
            if file <> currentFile then
                watchDependencies.Add(file) |> ignore

        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            Log.Make(severity, msg, ?range=range, ?fileName=fileName, ?tag=tag)
            |> logs.Add
