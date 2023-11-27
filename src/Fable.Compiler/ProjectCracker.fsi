/// This module gets the F# compiler arguments from .fsproj as well as some
/// Fable-specific tasks like tracking the sources of Fable Nuget packages
module Fable.Compiler.ProjectCracker

open System
open System.Xml.Linq
open System.Text.RegularExpressions
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Fable
open Fable.AST
open Fable.Compiler.Util
open Globbing.Operators
open Buildalyzer

// type FablePackage =
//     { Id: string
//       Version: string
//       FsprojPath: string
//       DllPath: string
//       SourcePaths: string list
//       Dependencies: Set<string> }
//
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
//
//     static member GetPath: fableModulesDir: string * isDebug: bool -> string
//     member GetTimestamp: unit -> DateTime
//     static member TryRead: fableModulesDir: string * isDebug: bool -> CacheInfo option
//     member Write: unit -> unit
//     /// Checks if there's also cache info for the alternate build mode (Debug/Release) and whether is more recent
//     member IsMostRecent: bool
//
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
//
// val isSystemPackage: pkgName: string -> bool
//
// type CrackedFsproj =
//     { ProjectFile: string
//       SourceFiles: string list
//       ProjectReferences: string list
//       DllReferences: IDictionary<string, string>
//       PackageReferences: FablePackage list
//       OtherCompilerOptions: string list
//       OutputType: string option
//       TargetFramework: string }
//
// val makeProjectOptions:
//     opts: CrackerOptions -> otherOptions: string seq -> sources: string array -> FSharpProjectOptions
//
// val tryGetFablePackage: opts: CrackerOptions -> dllPath: string -> FablePackage option
// val sortFablePackages: pkgs: FablePackage list -> FablePackage list
// val getBasicCompilerArgs: unit -> string array
// val MSBUILD_CONDITION: Regex
// /// Simplistic XML-parsing of .fsproj to get source files, as we cannot
// /// run `dotnet restore` on .fsproj files embedded in Nuget packages.
// val getSourcesFromFablePkg: opts: CrackerOptions -> projFile: string -> string list
// val excludeProjRef: opts: CrackerOptions -> dllRefs: IDictionary<string, string> -> projRef: string -> string option
//
// val getCrackedMainFsproj:
//     opts: CrackerOptions ->
//     projOpts: string array *
//     projRefs: string array *
//     msbuildProps: IReadOnlyDictionary<string, string> *
//     targetFramework: string ->
//         CrackedFsproj
//
// val getProjectOptionsFromScript: opts: CrackerOptions -> CrackedFsproj
//
// val getProjectOptionsFromProjectFile:
//     (bool -> CrackerOptions -> string -> string array * string array * IReadOnlyDictionary<string, string> * string)
//
// /// Use Buildalyzer to invoke MSBuild and get F# compiler args from an .fsproj file.
// /// As we'll merge this later with other projects we'll only take the sources and
// /// the references, checking if some .dlls correspond to Fable libraries
// val crackMainProject: opts: CrackerOptions -> CrackedFsproj
//
// /// For project references of main project, ignore dll and package references
// val crackReferenceProject:
//     opts: CrackerOptions -> dllRefs: IDictionary<string, string> -> projFile: string -> CrackedFsproj
//
// val getCrackedProjectsFromMainFsproj: opts: CrackerOptions -> CrackedFsproj list * CrackedFsproj
// val getCrackedProjects: opts: CrackerOptions -> CrackedFsproj list * CrackedFsproj
// val retryGetCrackedProjects: opts: CrackerOptions -> CrackedFsproj list * CrackedFsproj
// val changeFsprojToFableproj: path: string -> string
// val copyDir: replaceFsprojExt: bool -> source: string -> target: string -> unit
// val copyDirIfDoesNotExist: replaceFsprojExt: bool -> source: string -> target: string -> unit
// val getFableLibraryPath: opts: CrackerOptions -> string
// val copyFableLibraryAndPackageSources: opts: CrackerOptions -> pkgs: FablePackage list -> string * FablePackage list
// val copyFableLibraryAndPackageSourcesPy: opts: CrackerOptions -> pkgs: FablePackage list -> string * FablePackage list
// val removeFilesInObjFolder: sourceFiles: string array -> string array
//
// val loadPrecompiledInfo:
//     opts: CrackerOptions ->
//     otherOptions: string array ->
//     sourceFiles: string array ->
//         PrecompiledInfoImpl option * string array * string array
//
val getFullProjectOpts: opts: CrackerOptions -> CrackerResponse
