module Fable.Cli.FileWatcher

open System
open System.IO

type IFileSystemWatcher =
    inherit IDisposable

    [<CLIEvent>]
    abstract OnFileChange: IEvent<string>

    [<CLIEvent>]
    abstract OnError: IEvent<ErrorEventArgs>

    /// Directory path
    abstract BasePath: string with get, set
    abstract EnableRaisingEvents: bool with get, set
    /// File name filters
    abstract GlobFilters: string list

/// An alternative file watcher based on polling.
/// ignoredDirectoryNameRegexes allows ignoring directories to improve performance.
type PollingFileWatcher =
    new:
        watchedDirectoryPath: string * ignoredDirectoryNameRegexes: string seq ->
            PollingFileWatcher

    /// Defaults to false. Must be set to true to start raising events.
    member EnableRaisingEvents: bool with get, set
    interface IDisposable

/// A wrapper around the immutable polling watcher,
/// implementing IFileSystemWatcher with its mutable BasePath.
type ResetablePollingFileWatcher =
    new:
        fileNameGlobFilters: string list *
        ignoredDirectoryNameRegexes: string seq ->
            ResetablePollingFileWatcher

    interface IFileSystemWatcher

/// A FileSystemWatcher wrapper that implements the IFileSystemWatcher interface.
type DotnetFileWatcher =
    new: globFilters: string list -> DotnetFileWatcher
    interface IFileSystemWatcher
