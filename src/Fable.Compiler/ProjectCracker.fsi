/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Compiler.ProjectCracker

open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.AST
open Fable.Compiler.Util

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

type CrackerOptions =
    new: cliArgs: CliArgs -> CrackerOptions
    member NoCache: bool
    member CacheInfo: CacheInfo option
    member FableModulesDir: string
    member FableOptions: CompilerOptions
    member FableLib: string option
    member OutDir: string option
    member Configuration: string
    member Exclude: string list
    member Replace: Map<string, string>
    member PrecompiledLib: string option
    member NoRestore: bool
    member ProjFile: string
    member SourceMaps: bool
    member SourceMapsRoot: string option
    member BuildDll: normalizedDllPath: string -> unit
    static member GetFableModulesFromDir: baseDir: string -> string

    static member GetFableModulesFromProject:
        projDir: string * outDir: string option * noCache: bool -> string

type CrackerResponse =
    {
        FableLibDir: string
        FableModulesDir: string
        References: string list
        ProjectOptions: FSharpProjectOptions
        OutputType: OutputType
        TargetFramework: string
        PrecompiledInfo: PrecompiledInfoImpl option
        CanReuseCompiledFiles: bool
    }

val getFullProjectOpts: opts: CrackerOptions -> CrackerResponse
