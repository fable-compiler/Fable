// Ported from C# to F#
// from https://github.com/dotnet/aspnetcore/blob/5fd4f879779b131bf841c95d2a783d7f7742d2fa/src/Tools/dotnet-watch/src/Internal/FileWatcher/PollingFileWatcher.cs
// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

module Fable.Cli.FileWatcher

open System
open System.IO
open System.Runtime.CompilerServices
open System.Threading
open System.Collections.Generic
open System.Diagnostics
open System.Text.RegularExpressions
open Fable.Compiler.Globbing

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

[<IsReadOnly; Struct>]
type private FileMeta =
    {
        FileInfo: FileSystemInfo
        FoundAgain: bool
    }

/// An alternative file watcher based on polling.
/// ignoredDirectoryNameRegexes allows ignoring directories to improve performance.
type PollingFileWatcher(watchedDirectoryPath, ignoredDirectoryNameRegexes) =
    // The minimum interval to rerun the scan
    let minRunInternal = TimeSpan.FromSeconds(0.5)

    let watchedDirectory = new DirectoryInfo(watchedDirectoryPath)

    let mutable knownEntities = new Dictionary<string, FileMeta>()
    let mutable tempDictionary = new Dictionary<string, FileMeta>()
    let changes = new HashSet<string>()
    let mutable knownEntitiesCount = 0

    let onFileChange = new Event<string>()
    let mutable raiseEvents = false
    let mutable disposed = false

    let compiledIgnoredDirNames =
        ignoredDirectoryNameRegexes
        |> Seq.map (fun (regex: string) ->
            Regex(
                "^" + regex + "$",
                RegexOptions.Compiled ||| RegexOptions.CultureInvariant
            )
        )
        |> Array.ofSeq

    let notifyChanges () =
        for path in changes do
            if not disposed && raiseEvents then
                onFileChange.Trigger(path)

    let isIgnored (dirInfo: DirectoryInfo) =
        compiledIgnoredDirNames
        |> Array.exists (fun regex -> regex.IsMatch(dirInfo.Name))

    let rec foreachEntityInDirectory (dirInfo: DirectoryInfo) fileAction =
        if dirInfo.Exists && not (isIgnored dirInfo) then
            let entities =
                // If the directory is deleted after the exists check
                // this will throw and could crash the process
                try
                    Some(dirInfo.EnumerateFileSystemInfos())
                with :? DirectoryNotFoundException ->
                    None

            if Option.isSome entities then
                for entity in entities.Value do
                    fileAction entity

                    match entity with
                    | :? DirectoryInfo as subdirInfo ->
                        foreachEntityInDirectory subdirInfo fileAction
                    | _ -> ()

    let rec recordChange (fileInfo: FileSystemInfo) =
        if
            not (isNull fileInfo)
            && not (changes.Contains(fileInfo.Name))
            && not (
                fileInfo.FullName.Equals(
                    watchedDirectory.FullName,
                    StringComparison.Ordinal
                )
            )
        then
            changes.Add(fileInfo.FullName) |> ignore

            if fileInfo.FullName <> watchedDirectory.FullName then
                match fileInfo with
                | :? FileInfo as file -> recordChange (file.Directory)
                | :? DirectoryInfo as dir -> recordChange (dir.Parent)
                | _ -> ()

    let checkForChangedFiles () =
        changes.Clear()

        foreachEntityInDirectory
            watchedDirectory
            (fun f ->
                let fullFilePath = f.FullName

                match knownEntities.TryGetValue fullFilePath with
                | false, _ -> recordChange f // New file
                | true, fileMeta ->

                    try
                        if
                            fileMeta.FileInfo.LastWriteTime <> f.LastWriteTime
                        then
                            recordChange f // File changed

                        knownEntities.[fullFilePath] <-
                            { fileMeta with FoundAgain = true }
                    with :? FileNotFoundException ->
                        knownEntities.[fullFilePath] <-
                            { fileMeta with FoundAgain = false }
                // TryAdd instead of Add because sometimes we get duplicates (?!)
                // (Saw multiple times on Linux. Not sure where it came from...)
                tempDictionary.TryAdd(
                    f.FullName,
                    {
                        FileInfo = f
                        FoundAgain = false
                    }
                )
                |> ignore
            )

        for file in knownEntities do
            if not (file.Value.FoundAgain) then
                recordChange (file.Value.FileInfo) // File deleted

        notifyChanges ()

        // Swap the two dictionaries
        let swap = knownEntities
        knownEntities <- tempDictionary
        tempDictionary <- swap

        tempDictionary.Clear()

    let createKnownFilesSnapshot () =
        knownEntities.Clear()

        foreachEntityInDirectory
            watchedDirectory
            (fun f ->
                knownEntities.Add(
                    f.FullName,
                    {
                        FileInfo = f
                        FoundAgain = false
                    }
                )
            )

        Volatile.Write(&knownEntitiesCount, knownEntities.Count)

    let pollingLoop () =
        let stopWatch = Stopwatch.StartNew()
        stopWatch.Start() // Present in the C# code but it looks like an oversight

        while not disposed do
            // Don't run too often
            // The min wait time here can be double
            // the value of the variable (FYI)
            if stopWatch.Elapsed < minRunInternal then
                Thread.Sleep(minRunInternal)

            stopWatch.Reset()

            if raiseEvents then
                checkForChangedFiles ()

        stopWatch.Stop()

    do
        let pollingThread = new Thread(new ThreadStart(pollingLoop))
        pollingThread.IsBackground <- true
        pollingThread.Name <- nameof PollingFileWatcher

        createKnownFilesSnapshot ()

        pollingThread.Start()

    [<CLIEvent>]
    member this.OnFileChange = onFileChange.Publish

    member this.BasePath = watchedDirectory.FullName
    member this.KnownEntitiesCount = Volatile.Read(&knownEntitiesCount)

    /// Defaults to false. Must be set to true to start raising events.
    member this.EnableRaisingEvents
        with get () = raiseEvents
        and set (value) =
            if disposed then
                raise (ObjectDisposedException(nameof PollingFileWatcher))
            else
                raiseEvents <- value

    interface IDisposable with
        member this.Dispose() =
            if not disposed then
                this.EnableRaisingEvents <- false

            disposed <- true

type private WatcherInstance =
    {
        Watcher: PollingFileWatcher
        FileChangeSubscription: IDisposable
    }

/// A wrapper around the immutable polling watcher,
/// implementing IFileSystemWatcher with its mutable BasePath.
type ResetablePollingFileWatcher
    (fileNameGlobFilters, ignoredDirectoryNameRegexes)
    =
    let mutable disposed = false
    let resetLocker = new obj ()

    let onFileChange = new Event<string>()
    /// Currently only used to publish the unused interface event
    let onError = new Event<ErrorEventArgs>()

    /// Dispose previous, and return a new instance
    let createInstance basePath (previous: WatcherInstance option) =
        // Creating a new instance before stopping the previous one
        // might return duplicate changes, but at least we should not be losing any.
        let newInstance =
            new PollingFileWatcher(basePath, ignoredDirectoryNameRegexes)

        let previousEnableRaisingEvents =
            match previous with
            | Some instance ->
                let previousEnableRaisingEvents =
                    instance.Watcher.EnableRaisingEvents

                (instance.Watcher :> IDisposable).Dispose()
                instance.FileChangeSubscription.Dispose()
                previousEnableRaisingEvents
            | None -> false // Defaults to EnableRaisingEvents = false to be consistent

        let watcherChangeHandler e =
            let name = Path.GetFileName(e: string) // Should also work for directories

            let matchesFilter =
                List.isEmpty fileNameGlobFilters
                || fileNameGlobFilters
                   |> List.exists (fun filter -> Glob.isMatch filter name)

            if matchesFilter then
                onFileChange.Trigger(e)

        newInstance.EnableRaisingEvents <- previousEnableRaisingEvents

        {
            Watcher = newInstance
            FileChangeSubscription =
                newInstance.OnFileChange.Subscribe(watcherChangeHandler)
        }

    /// Should always be used under lock
    let mutable current = None

    interface IFileSystemWatcher with
        [<CLIEvent>]
        member this.OnFileChange = onFileChange.Publish

        /// Currently unused for this implementation
        [<CLIEvent>]
        member this.OnError = onError.Publish

        member this.BasePath
            with get () =
                lock
                    resetLocker
                    (fun () ->
                        current
                        |> Option.map (fun x -> x.Watcher.BasePath)
                        |> Option.defaultValue ""
                    )
            and set (value) =
                lock
                    resetLocker
                    (fun () ->
                        // Compare normalized paths before recreating the watcher:
                        if
                            current.IsNone
                            || String.IsNullOrWhiteSpace(
                                current.Value.Watcher.BasePath
                            )
                            || Path.GetFullPath(current.Value.Watcher.BasePath)
                               <> Path.GetFullPath(value)
                        then
                            current <- Some(createInstance value current)
                    )

        member this.EnableRaisingEvents
            with get () =
                lock
                    resetLocker
                    (fun () ->
                        current
                        |> Option.map (fun x -> x.Watcher.EnableRaisingEvents)
                        |> Option.defaultValue false
                    )
            and set (value) =
                lock
                    resetLocker
                    (fun () ->
                        if current.IsSome then
                            current.Value.Watcher.EnableRaisingEvents <- value
                    )

        member this.GlobFilters = fileNameGlobFilters

        member this.Dispose() =
            lock
                resetLocker
                (fun () ->
                    if current.IsSome then
                        (current.Value.Watcher :> IDisposable).Dispose()
                        current.Value.FileChangeSubscription.Dispose()
                        disposed <- true
                )

/// A FileSystemWatcher wrapper that implements the IFileSystemWatcher interface.
type DotnetFileWatcher(globFilters: string list) =
    let fileSystemWatcher = new FileSystemWatcher()

    let onFileChange = new Event<string>()
    let onError = new Event<ErrorEventArgs>()

    do
        for filter in globFilters do
            fileSystemWatcher.Filters.Add(filter)

        let watcherChangeHandler (e: FileSystemEventArgs) =
            onFileChange.Trigger(e.FullPath)

        let watcherRenameHandler (e: RenamedEventArgs) =
            onFileChange.Trigger(e.OldFullPath)
            onFileChange.Trigger(e.FullPath)

        let watcherErrorHandler e = onError.Trigger(e)

        fileSystemWatcher.Created.Subscribe(watcherChangeHandler) |> ignore
        fileSystemWatcher.Deleted.Subscribe(watcherChangeHandler) |> ignore
        fileSystemWatcher.Changed.Subscribe(watcherChangeHandler) |> ignore
        fileSystemWatcher.Renamed.Subscribe(watcherRenameHandler) |> ignore
        fileSystemWatcher.Error.Subscribe(watcherErrorHandler) |> ignore

        fileSystemWatcher.IncludeSubdirectories <- true

    interface IFileSystemWatcher with
        [<CLIEvent>]
        member this.OnFileChange = onFileChange.Publish

        [<CLIEvent>]
        member this.OnError = onError.Publish

        member this.BasePath
            with get () = fileSystemWatcher.Path
            and set (value) = fileSystemWatcher.Path <- value

        member this.EnableRaisingEvents
            with get () = fileSystemWatcher.EnableRaisingEvents
            and set (value) = fileSystemWatcher.EnableRaisingEvents <- value

        member this.GlobFilters = fileSystemWatcher.Filters |> List.ofSeq
        member this.Dispose() = fileSystemWatcher.Dispose()
