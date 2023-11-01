module Fable.Compiler.ProjectCracker

open System
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.AST
open Fable.Compiler.Util

type FablePackage =
    { Id: string
      Version: string
      FsprojPath: string
      DllPath: string
      SourcePaths: string list
      Dependencies: Set<string> }

type CacheInfo =
    {
        Version: string
        FableOptions: CompilerOptions
        ProjectPath: string
        SourcePaths: string array
        FSharpOptions: string array
        References: string list
        OutDir: string option
        FableLibDir: string
        FableModulesDir: string
        OutputType: OutputType
        TargetFramework: string
        Exclude: string list
        SourceMaps: bool
        SourceMapsRoot: string option
    }
    static member GetPath(fableModulesDir: string, isDebug: bool) =
        IO.Path.Combine(fableModulesDir, $"""project_cracked{if isDebug then "_debug" else ""}.json""")

    member this.GetTimestamp() =
        CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)
        |> IO.File.GetLastWriteTime

    static member TryRead(fableModulesDir: string, isDebug): CacheInfo option =
        try
            CacheInfo.GetPath(fableModulesDir, isDebug) |> Json.read<CacheInfo> |> Some
        with _ -> None

    member this.Write() =
        let path = CacheInfo.GetPath(this.FableModulesDir, this.FableOptions.DebugMode)

        // Ensure the destination folder exists
        if not (IO.File.Exists path) then
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName path) |> ignore

        Json.write path this

    /// Checks if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
    member this.IsMostRecent =
        match CacheInfo.TryRead(this.FableModulesDir, not this.FableOptions.DebugMode) with
        | None -> true
        | Some other -> this.GetTimestamp() > other.GetTimestamp()

type CrackerOptions(cliArgs: CliArgs) =
    let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile
    let fableModulesDir = CrackerOptions.GetFableModulesFromProject(projDir, cliArgs.OutDir, cliArgs.NoCache)
    let builtDlls = HashSet()
    let cacheInfo =
        if cliArgs.NoCache then None
        else CacheInfo.TryRead(fableModulesDir, cliArgs.CompilerOptions.DebugMode)

    member _.NoCache = cliArgs.NoCache
    member _.CacheInfo = cacheInfo
    member _.FableModulesDir = fableModulesDir
    member _.FableOptions: CompilerOptions = cliArgs.CompilerOptions
    member _.FableLib: string option = cliArgs.FableLibraryPath
    member _.OutDir: string option = cliArgs.OutDir
    member _.Configuration: string = cliArgs.Configuration
    member _.Exclude: string list = cliArgs.Exclude
    member _.Replace: Map<string, string> = cliArgs.Replace
    member _.PrecompiledLib: string option = cliArgs.PrecompiledLib
    member _.NoRestore: bool = cliArgs.NoRestore
    member _.ProjFile: string = cliArgs.ProjectFile
    member _.SourceMaps: bool = cliArgs.SourceMaps
    member _.SourceMapsRoot: string option = cliArgs.SourceMapsRoot

    member _.BuildDll(normalizedDllPath: string) =
        if not(builtDlls.Contains(normalizedDllPath)) then
            let projDir =
                normalizedDllPath.Split('/')
                |> Array.rev
                |> Array.skipWhile (fun part -> part <> "bin")
                |> Array.skip 1
                |> Array.rev
                |> String.concat "/"
            Process.runSync projDir "dotnet" ["build"; "-c"; cliArgs.Configuration] |> ignore
            builtDlls.Add(normalizedDllPath) |> ignore

    static member GetFableModulesFromDir(baseDir: string): string =
        IO.Path.Combine(baseDir, Naming.fableModules)
        |> Path.normalizePath

    static member GetFableModulesFromProject(projDir: string, outDir: string option, noCache: bool): string =
        let fableModulesDir =
            outDir
            |> Option.defaultWith (fun () -> projDir)
            |> CrackerOptions.GetFableModulesFromDir

        if noCache then
            if IO.Directory.Exists(fableModulesDir) then
                IO.Directory.Delete(fableModulesDir, recursive=true)

        if File.isDirectoryEmpty fableModulesDir then
            IO.Directory.CreateDirectory(fableModulesDir) |> ignore
            IO.File.WriteAllText(IO.Path.Combine(fableModulesDir, ".gitignore"), "**/*")

        fableModulesDir

type CrackerResponse =
    { FableLibDir: string
      FableModulesDir: string
      References: string list
      ProjectOptions: FSharpProjectOptions
      OutputType: OutputType
      TargetFramework: string
      PrecompiledInfo: PrecompiledInfoImpl option
      CanReuseCompiledFiles: bool }

type CrackedFsproj =
    { ProjectFile: string
      SourceFiles: string list
      ProjectReferences: string list
      DllReferences: IDictionary<string, string>
      PackageReferences: FablePackage list
      OtherCompilerOptions: string list
      OutputType: string option
      TargetFramework: string }





