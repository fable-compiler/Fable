module Fable.State

open Fable
open Fable.AST
open System
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

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

type ConcurrentDictionary<'TKey, 'TValue> = Dictionary<'TKey, 'TValue>
#endif

type PathRef =
    | FilePath of string
    | NonFilePath of string

type Project(projectOptions: FSharpProjectOptions, implFiles: Map<string, FSharpImplementationFileContents>,
             errors: FSharpErrorInfo array,  fableCore: PathRef, isWatchCompile: bool) =
    let timestamp = DateTime.Now
    let projectFile = Path.normalizePath projectOptions.ProjectFileName
    let entities = ConcurrentDictionary<string, Fable.Entity>()
    let inlineExprs = ConcurrentDictionary<string, InlineExpr>()
    let normalizedFiles =
        projectOptions.SourceFiles
        |> Seq.map Path.normalizeFullPath
        |> Set
    let rootModules =
        implFiles |> Map.map (fun _ file -> FSharp2Fable.Compiler.getRootModuleFullName file)
    member __.TimeStamp = timestamp
    member __.FableCore = fableCore
    member __.IsWatchCompile = isWatchCompile
    member __.ImplementationFiles = implFiles
    member __.Errors = errors
    member __.ProjectOptions = projectOptions
    member __.ProjectFile = projectFile
    member __.NormalizedFilesSet = normalizedFiles
    interface ICompilerState with
        member this.ProjectFile = projectOptions.ProjectFileName
        member this.GetRootModule(fileName) =
            match Map.tryFind fileName rootModules with
            | Some rootModule -> rootModule
            | None -> failwithf "Cannot find root module for %s" fileName
        member this.GetOrAddEntity(fullName, generate) =
            entities.GetOrAdd(fullName, fun _ -> generate())
        member this.GetOrAddInlineExpr(fullName, generate) =
            inlineExprs.GetOrAdd(fullName, fun _ -> generate())

type State = Map<string, Project>

let getDefaultFableCore() = "fable-core"

let getDefaultOptions(replacements) =
    let replacements =
        match replacements with
        | Some repls -> Map repls
        | None -> Map.empty
    { fableCore = getDefaultFableCore()
      emitReplacements = replacements
      typedArrays = true
      clampByteArrays = false
      declaration = false }

/// Type with utilities for compiling F# files to JS
/// No thread-safe, an instance must be created per file
type Compiler(?options, ?replacements, ?plugins) =
    let mutable id = 0
    let options = defaultArg options (getDefaultOptions replacements)
    let plugins: PluginInfo list = defaultArg plugins []
    let logs = Dictionary<string, string list>()
    member __.ReadAllLogs() =
        logs |> Seq.map (fun kv -> kv.Key, List.rev kv.Value) |> Map
    member __.Options = options
    member __.Plugins = plugins
    interface ICompiler with
        member __.Options = options
        member __.Plugins = plugins
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            let tag = defaultArg tag "FABLE"
            let severity =
                match severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"
            let formattedMsg =
                match fileName with
                | Some file ->
                    match range with
                    | Some r -> sprintf "%s(%i,%i): (%i,%i) %s %s: %s" file r.start.line r.start.column r.``end``.line r.``end``.column severity tag msg
                    | None -> sprintf "%s(1,1): %s %s: %s" file severity tag msg
                | None -> msg
            if logs.ContainsKey(severity)
            then logs.[severity] <- formattedMsg::logs.[severity]
            else logs.Add(severity, [formattedMsg])
        member __.GetUniqueVar() =
            id <- id + 1; "$var" + (string id)
