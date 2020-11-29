// Ported from C# to F#
// from https://github.com/dotnet/aspnetcore/blob/5fd4f879779b131bf841c95d2a783d7f7742d2fa/src/Tools/dotnet-watch/src/Internal/FileWatcher/PollingFileWatcher.cs
// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

module PollingFileWatcher

open System
open System.IO
open System.Runtime.CompilerServices
open System.Threading
open System.Collections.Generic
open System.Diagnostics

[<IsReadOnly; Struct>]
type FileMeta = {
    FileInfo: FileSystemInfo
    FoundAgain: bool
    }

type PollingFileWatcher (watchedDirectoryPath: string, EnableRaisingEvents) =
    // The minimum interval to rerun the scan
    let minRunInternal = TimeSpan.FromSeconds(0.5)

    let watchedDirectory = new DirectoryInfo(watchedDirectoryPath)

    let mutable knownEntities = new Dictionary<string, FileMeta>()
    let mutable tempDictionary = new Dictionary<string, FileMeta>()
    let changes = new HashSet<string>()
    let mutable knownEntitiesCount = 0

    let onFileChange = new Event<string>()
    let mutable raiseEvents = EnableRaisingEvents
    let mutable disposed = false

    let notifyChanges () =
        for path in changes do
            if not disposed && raiseEvents
            then onFileChange.Trigger(path)

    let rec foreachEntityInDirectory (dirInfo: DirectoryInfo) fileAction =
        if dirInfo.Exists then
            let entitiesOption =
                // If the directory is deleted after the exists check
                // this will throw and could crash the process
                try Some (dirInfo.EnumerateFileSystemInfos("*.*"))
                with | :? DirectoryNotFoundException -> None
            if Option.isSome entitiesOption then
                for entity in entitiesOption.Value do
                    fileAction entity
                    match entity with
                    | :? DirectoryInfo as subdirInfo -> foreachEntityInDirectory subdirInfo fileAction
                    | _ -> ()

    let rec recordChange (fileInfo: FileSystemInfo) =
        if not (isNull fileInfo)
            && not (changes.Contains(fileInfo.Name))
            && not (fileInfo.FullName.Equals(watchedDirectory.FullName, StringComparison.Ordinal))
        then
            changes.Add(fileInfo.FullName) |> ignore
            if fileInfo.FullName <> watchedDirectory.FullName then
                match fileInfo with
                | :? FileInfo as file -> recordChange (file.Directory)
                | :? DirectoryInfo as dir -> recordChange (dir.Parent)
                | _ -> ()

    let checkForChangedFiles () =
        changes.Clear()
        foreachEntityInDirectory watchedDirectory (fun f ->
            let fullFilePath = f.FullName
            if not <| knownEntities.ContainsKey(fullFilePath)
            then recordChange f // New file
            else
                let fileMeta = knownEntities.[fullFilePath]
                try
                    if fileMeta.FileInfo.LastWriteTime <> f.LastWriteTime
                    then recordChange f // File changed

                    knownEntities.[fullFilePath] <- { fileMeta with FoundAgain = true }
                with
                    | :? FileNotFoundException ->
                        knownEntities.[fullFilePath] <- { fileMeta with FoundAgain = false }
            tempDictionary.Add(f.FullName, { FileInfo = f; FoundAgain = false }))

        for file in knownEntities do
            if not (file.Value.FoundAgain)
            then recordChange (file.Value.FileInfo) // File deleted

        notifyChanges ()

        // Swap the two dictionaries
        let swap = knownEntities
        knownEntities <- tempDictionary
        tempDictionary <-swap

        tempDictionary.Clear()

    let createKnownFilesSnapshot () =
        knownEntities.Clear()
        foreachEntityInDirectory watchedDirectory (fun f ->
            knownEntities.Add(f.FullName, { FileInfo = f; FoundAgain = false }))
        Volatile.Write(&knownEntitiesCount, knownEntities.Count)

    let pollingLoop () =
        let stopWatch = Stopwatch.StartNew()
        stopWatch.Start() // Present in the C# code but it looks like an oversight
        while not disposed do
            // Don't run too often
            // The min wait time here can be double
            // the value of the variable (FYI)
            if stopWatch.Elapsed < minRunInternal
            then Thread.Sleep(minRunInternal)
            stopWatch.Reset()
            if raiseEvents
            then checkForChangedFiles ()
        stopWatch.Stop()

    do
        if isNull watchedDirectoryPath
        then failwith $"{nameof watchedDirectoryPath} must not be null."

        let pollingThread = new Thread(new ThreadStart(pollingLoop))
        pollingThread.IsBackground <- true
        pollingThread.Name <- nameof PollingFileWatcher

        createKnownFilesSnapshot ()

        pollingThread.Start()

    [<CLIEvent>]
    member this.OnFileChange = onFileChange.Publish
    member this.BasePath = watchedDirectory.FullName
    member this.KnownEntitiesCount = Volatile.Read(&knownEntitiesCount)
    member this.EnableRaisingEvents
        with get () = raiseEvents
        and set (value) =
            if disposed then raise (ObjectDisposedException(nameof PollingFileWatcher))
            else raiseEvents <- value

    interface IDisposable with
        member this.Dispose () =
            this.EnableRaisingEvents <- false
            disposed <- true
