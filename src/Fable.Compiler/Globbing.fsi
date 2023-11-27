module Fable.Compiler.Globbing

open System.Collections.Generic
open System.IO

/// Globbing support and operators
///
/// Forked from `Fake.IO.FileSystem`
///
/// This module contains a file pattern globbing implementation.
[<RequireQualifiedAccess>]
module Glob =
    open System
    open System.Text.RegularExpressions

    val inline normalizePath: path: string -> string

    type private SearchOption =
        | Directory of string
        | Drive of string
        | Recursive
        | FilePattern of string

    val internal getRoot: baseDirectory: string -> pattern: string -> string
    val internal search: baseDir: string -> originalInput: string -> string list
    val internal compileGlobToRegex: pattern: string -> Regex
    val isMatch: pattern: string -> path: string -> bool

type IGlobbingPattern =
    inherit IEnumerable<string>
    abstract BaseDirectory: string
    abstract Includes: string list
    abstract Excludes: string list

type LazyGlobbingPattern =
    {
        BaseDirectory: string
        Includes: string list
        Excludes: string list
    }

    interface IGlobbingPattern
    interface IEnumerable<string>

type ResolvedGlobbingPattern =
    {
        BaseDirectory: string
        Includes: string list
        Excludes: string list
        Results: string list
    }

    interface IGlobbingPattern
    interface IEnumerable<string>

[<AutoOpen>]
module GlobbingPatternExtensions =
    type IGlobbingPattern with

        member internal Pattern: LazyGlobbingPattern
        member Resolve: unit -> IGlobbingPattern
        /// Adds the given pattern to the file includes
        member And: pattern: string -> IGlobbingPattern
        /// Ignores files with the given pattern
        member ButNot: pattern: string -> IGlobbingPattern
        /// Sets a directory as BaseDirectory.
        member SetBaseDirectory: dir: string -> IGlobbingPattern
        /// Checks if a particular file is matched
        member IsMatch: path: string -> bool

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GlobbingPattern =
    /// Include files
    val create: x: string -> IGlobbingPattern
    /// Start an empty globbing pattern from the specified directory
    val createFrom: dir: string -> IGlobbingPattern

    /// Sets a directory as baseDirectory for fileIncludes.
    val setBaseDir:
        dir: string -> fileIncludes: IGlobbingPattern -> IGlobbingPattern

    /// Get base include directories.
    ///
    /// Used to get a smaller set of directories from a globbing pattern.
    val getBaseDirectoryIncludes: fileIncludes: IGlobbingPattern -> string list

/// Contains operators to find and process files.
///
/// ### Simple glob using as list
///
///     let csProjectFiles = !! "src/*.csproj"
///
///     for projectFile in csProjectFiles do
///         printf "F# ProjectFile: %s" projectFile
///
/// ### Combine globs
///
///     let projectFiles =
///         !! "src/*/*.*proj"
///         ++ "src/*/*.target"
///         -- "src/*/*.vbproj"
///
///     for projectFile in projectFiles do
///         printf "ProjectFile: %s" projectFile
///
module Operators =
    /// Add Include operator
    val inline (++): x: IGlobbingPattern -> pattern: string -> IGlobbingPattern
    /// Exclude operator
    val inline (--): x: IGlobbingPattern -> pattern: string -> IGlobbingPattern
    /// Includes a single pattern and scans the files - !! x = AllFilesMatching x
    val inline (!!): x: string -> IGlobbingPattern
