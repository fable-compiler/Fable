module Fable.Tests.FileWatcher

open Util.Testing
open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Linq
open Fable.Cli.FileWatcher
open System.Runtime.CompilerServices
open Expecto
open Fable.Tests

// Many of these tests are inspired from https://github.com/dotnet/aspnetcore/blob/1830f4f0ddc4aa3234e0dd7b08b31ab984e2b714/src/Tools/dotnet-watch/test/FileWatcherTests.cs

// Warning: most of these tests are actually integration tests because they touch the file system and
// can be relatively slow as some of them rely on timeouts on their happy path... (see comments)

let defaultTimeout = TimeSpan.FromMilliseconds(5000.0) // Might be a bit generous...
let negativeTimeout = TimeSpan.FromMilliseconds(2000.0) // Might be a bit tight...
let oneSecond = TimeSpan.FromSeconds(1.0)

let usingTempDirectoryAsync f =
    async {
        let folderName = $"FileWatcherTests-{DateTime.UtcNow:o}-{Guid.NewGuid()}"
        let tempFolder = Path.GetFullPath(Path.Combine(Path.GetTempPath(), folderName))
        if Directory.Exists(tempFolder)
        then Directory.Delete(tempFolder, true (*recursive*))

        Directory.CreateDirectory(tempFolder) |> ignore

        try
            return! f tempFolder
        finally Directory.Delete(tempFolder, true (*recursive*))
    }

let createWatcherWithoutPath usePolling globFilters =
    let watcher: IFileSystemWatcher =
        match usePolling with
        | false -> upcast new DotnetFileWatcher(globFilters)
        | true -> upcast new ResetablePollingFileWatcher(globFilters, [])
    watcher

let createWatcher path usePolling globFilters =
    let watcher = createWatcherWithoutPath usePolling globFilters
    watcher.BasePath <- path
    watcher

type Async() =
    /// Inspired from https://github.com/dotnet/aspnetcore/blob/0517745c08d1bc85009f1ca41a41d77176eeea60/src/SignalR/server/Specification.Tests/src/Internal/TaskExtensions.cs
    /// Using a (hot) Task has the advantage of always working, without requiring Async.StartChild to be used by the caller.
    /// Unfortunately, it means an AggregateException has to be expected by the caller...
    static member TimeoutAfter(timeout: TimeSpan, task: Task<'T>, [<CallerFilePath>] ?filePath: string, [<CallerLineNumber>] ?lineNumber: int) =
        let createErrorMessage () =
            if filePath.IsSome && lineNumber.IsSome
            then $"The operation at {filePath.Value}:{lineNumber.Value} timed out after reaching the limit of {timeout.TotalMilliseconds}ms.";
            else $"The operation timed out after reaching the limit of {timeout.TotalMilliseconds}ms."
        let asyncComputation =
            async {
                use cts = new CancellationTokenSource()
                let! firstToComplete = Task.WhenAny(task, Task.Delay(timeout, cts.Token)) |> Async.AwaitTask
                if firstToComplete = (task :> Task) then
                    cts.Cancel()
                    return! task |> Async.AwaitTask
                else
                    return raise (TimeoutException(createErrorMessage()))
            }
        asyncComputation |> Async.StartImmediateAsTask
    /// Await task, and returns only the inner exception when it throws (instead of an AggregateException)
    static member AwaitTaskUnwrappingException task =
        async {
            try
                let! result = task |> Async.AwaitTask
                return result
            with | :? AggregateException as aggrEx -> return raise aggrEx.InnerException
        }

/// Applied to a list of (test name * partial test),
/// generates tests with a first value from a list of fixed named values,
/// and a second value from a setup function.
/// Each test name is concatenated to the name of the value to generate the full name of the test.
let inline testFixtureForValuesAsync setup namedValues =
    Seq.collect (fun (name, partialTest) ->
        namedValues
        |> Seq.map (fun (valueName, value) ->
            let partiallyAppliedTest = partialTest value
            let fullName = $"{name} ({valueName})"
            testCaseAsync fullName (setup partiallyAppliedTest)))

let tests =
  testList "FileWatchers" [

    testCaseAsync "Timeout does not throw if the work completes before it expires" <|
        async {
            let tcs = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously)
            // Start a timeout task/async operation, but do not wait for its completion right now
            let timeoutTask = Async.TimeoutAfter(TimeSpan.FromMilliseconds(200.0), tcs.Task)
            // Complete the work task
            tcs.TrySetResult(42) |> ignore
            let! timeoutTaskResult = timeoutTask |> Async.AwaitTaskUnwrappingException

            timeoutTaskResult |> equal 42
        }

    testCaseAsync "Timeout throws if it expires before the work is done" <|
        async {
            let tcs = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously)
            // Start a timeout task/async operation, but do not wait for its completion right now
            let timeoutTask = Async.TimeoutAfter(TimeSpan.FromMilliseconds(200.0), tcs.Task)
            // Wait for expiration
            do! Async.Sleep(400)
            // Complete the work task after the timeout expired (too late)
            tcs.TrySetResult(42) |> ignore
            let mutable error = None
            let mutable timeoutTaskResult = None
            try
                let! result = timeoutTask |> Async.AwaitTaskUnwrappingException
                timeoutTaskResult <- Some result
            with | ex -> error <- Some ex

            if timeoutTaskResult.IsSome then failtest $"Expected a TimeoutException but got a result ({timeoutTaskResult.Value})."
            if error.IsNone then failtest $"Expected a TimeoutException but got no exception at all."
            match error.Value with
            | :? TimeoutException -> () // Expected
            | ex -> failtest $"Expected TimeoutException but got {ex}"
        }

    yield! testFixtureForValuesAsync usingTempDirectoryAsync [ ("Polling", true); ("FSW", false) ] [
        
        "New file",
        fun usePolling tempFolder ->
            async {
                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()
               
                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true
               
                let testFilePath = Path.Combine(tempFolder, "testFile")
                File.WriteAllText(testFilePath, "")
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
               
                testFilePath |> equal <| filesChanged.Single()
            }

        "New directory",
        fun usePolling tempFolder ->
            async {
                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()
               
                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true
               
                let testDirectoryPath = Path.Combine(tempFolder, "testDirectory")
                Directory.CreateDirectory(testDirectoryPath) |> ignore
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
               
                testDirectoryPath |> equal <| filesChanged.Single()
            }

        "Change file",
        fun usePolling tempFolder ->
            async {
                let testFilePath = Path.Combine(tempFolder, "testFile")
                File.WriteAllText(testFilePath, "content")

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()

                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true
               
                // On Unix the file write time is in 1s increments;
                // if we don't wait, there's a chance that the polling
                // watcher will not detect the change.
                //
                // FIXME: Asynchronously waiting with `do! Async.Sleep(1000)`
                //        results in the FSW not reporting our events anymmore (?!)
                //        (I tested this on Ubuntu WSL2 only)
                System.Threading.Thread.Sleep(oneSecond)

                File.WriteAllText(testFilePath, "changed content")
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
               
                testFilePath |> equal <| filesChanged.Single()
            }

        "Delete file",
        fun usePolling tempFolder ->
            async {
                let testFilePath = Path.Combine(tempFolder, "testFile")
                File.WriteAllText(testFilePath, "content")

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()

                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true
               
                File.Delete(testFilePath)
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
               
                testFilePath |> equal <| filesChanged.Single()
            }

        "Rename file",
        fun usePolling tempFolder ->
            async {
                let srcFile = Path.Combine(tempFolder, "srcFile")
                let dstFile = Path.Combine(tempFolder, "dstFile")

                File.WriteAllText(srcFile, "content")

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()

                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    if filesChanged.Count >= 2
                    then tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true
               
                File.Move(srcFile, dstFile)
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
               
                Expect.containsAll filesChanged [ srcFile; dstFile ] ""
            }

        "Files in sub-directory",
        fun usePolling tempFolder ->
            async {
                let subDirPath = Path.Combine(tempFolder, "subdir")
                Directory.CreateDirectory(subDirPath) |> ignore

                let newFilePath = Path.Combine(subDirPath, "newFile")
                let changedFilePath = Path.Combine(subDirPath, "changedFile")
                let deletedFilePath = Path.Combine(subDirPath, "deletedFile")
                let srcForRenamedFilePath = Path.Combine(subDirPath, "srcForRenamedFile")
                let dstForRenamedFilePath = Path.Combine(subDirPath, "dstForRenamedFile")

                File.WriteAllText(changedFilePath, "content")
                File.WriteAllText(deletedFilePath, "content")
                File.WriteAllText(srcForRenamedFilePath, "content")

                let allExpectedChanges = [
                    newFilePath
                    changedFilePath
                    deletedFilePath
                    srcForRenamedFilePath
                    dstForRenamedFilePath
                    ]

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()

                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    if filesChanged.Count >= allExpectedChanges.Length
                    then tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true

                // On Unix the file write time is in 1s increments;
                // if we don't wait, there's a chance that the polling
                // watcher will not detect the change.
                //
                // FIXME: Asynchronously waiting with `do! Async.Sleep(1000)`
                //        results in the FSW not reporting our events anymmore (?!)
                //        (I tested this on Ubuntu WSL2 only)
                System.Threading.Thread.Sleep(oneSecond)

                File.WriteAllText(newFilePath, "")
                File.WriteAllText(newFilePath + "2", "")
                File.WriteAllText(changedFilePath, "changed content")
                File.Delete(deletedFilePath)
                File.Move(srcForRenamedFilePath, dstForRenamedFilePath)

                try
                    do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
                with | :? TimeoutException as ex -> failtest $"Got {filesChanged.Count} of the expected {allExpectedChanges.Length} events. {ex.Message}"

                // Note: the polling watcher currently returns changes for directories when
                // their last write time is modified. This means we get a change for the sub-directory itself 
                // (creating a file inside a directory changes its last write time)
                // This does not seem to be a problem...
                Expect.containsAll filesChanged allExpectedChanges ""
            }

        "No event if disabled",
        fun usePolling tempFolder ->
            async {
                let newFilePath = Path.Combine(tempFolder, "newFile")
                let changedFilePath = Path.Combine(tempFolder, "changedFile")
                let deletedFilePath = Path.Combine(tempFolder, "deletedFile")
                let srcForRenamedFilePath = Path.Combine(tempFolder, "srcForRenamedFile")
                let dstForRenamedFilePath = Path.Combine(tempFolder, "dstForRenamedFile")

                File.WriteAllText(changedFilePath, "content")
                File.WriteAllText(deletedFilePath, "content")
                File.WriteAllText(srcForRenamedFilePath, "content")

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()
               
                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true

                // Disable
                watcher.EnableRaisingEvents <- false
               
                // On Unix the file write time is in 1s increments;
                // if we don't wait, there's a chance that the polling
                // watcher will not detect the change.
                //
                // FIXME: Asynchronously waiting with `do! Async.Sleep(1000)`
                //        results in the FSW not reporting our events anymmore (?!)
                //        (I tested this on Ubuntu WSL2 only)
                System.Threading.Thread.Sleep(oneSecond)

                File.WriteAllText(newFilePath, "content")
                File.WriteAllText(changedFilePath, "new content")
                File.Delete(deletedFilePath)
                File.Move(srcForRenamedFilePath, dstForRenamedFilePath)
                
                let mutable error = None
                try
                    do! Async.TimeoutAfter(negativeTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
                with | ex -> error <- Some ex

                match error with
                | None -> failtest $"The watcher detected {filesChanged.Count} change(s)!"
                | Some ex ->
                    match ex with
                    | :? TimeoutException -> () // Expected
                    | ex -> failtest $"Expected a timeout but got {ex}"
            }

        "No event if disposed",
        fun usePolling tempFolder ->
            async {
                let newFilePath = Path.Combine(tempFolder, "newFile")
                let changedFilePath = Path.Combine(tempFolder, "changedFile")
                let deletedFilePath = Path.Combine(tempFolder, "deletedFile")
                let srcForRenamedFilePath = Path.Combine(tempFolder, "srcForRenamedFile")
                let dstForRenamedFilePath = Path.Combine(tempFolder, "dstForRenamedFile")

                File.WriteAllText(changedFilePath, "content")
                File.WriteAllText(deletedFilePath, "content")
                File.WriteAllText(srcForRenamedFilePath, "content")

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()
               
                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true

                // Dispose
                watcher.Dispose()
               
                // On Unix the file write time is in 1s increments;
                // if we don't wait, there's a chance that the polling
                // watcher will not detect the change.
                //
                // FIXME: Asynchronously waiting with `do! Async.Sleep(1000)`
                //        results in the FSW not reporting our events anymmore (?!)
                //        (I tested this on Ubuntu WSL2 only)
                System.Threading.Thread.Sleep(oneSecond)

                File.WriteAllText(newFilePath, "content")
                File.WriteAllText(changedFilePath, "new content")
                File.Delete(deletedFilePath)
                File.Move(srcForRenamedFilePath, dstForRenamedFilePath)
                
                let mutable error = None
                try
                    do! Async.TimeoutAfter(negativeTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
                with | ex -> error <- Some ex

                match error with
                | None -> failtest $"The watcher detected {filesChanged.Count} change(s)!"
                | Some ex ->
                    match ex with
                    | :? TimeoutException -> () // Expected
                    | ex -> failtest $"Expected a timeout but got {ex}"
            }

        "Delete sub-directory",
        fun usePolling tempFolder ->
            async {
                let subDirPath = Path.Combine(tempFolder, "subdir")
                Directory.CreateDirectory(subDirPath) |> ignore

                let testFilePath1 = Path.Combine(subDirPath, "testFile1")
                let testFilePath2 = Path.Combine(subDirPath, "testFile2")
                let testFilePath3 = Path.Combine(subDirPath, "testFile3")

                File.WriteAllText(testFilePath1, "content")
                File.WriteAllText(testFilePath2, "content")
                File.WriteAllText(testFilePath3, "content")

                let allExpectedChanges = [
                    subDirPath
                    testFilePath1
                    testFilePath2
                    testFilePath3
                    ]

                let watcher = createWatcher tempFolder usePolling []
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()

                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    if filesChanged.Count >= allExpectedChanges.Length
                    then tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true

                Directory.Delete(subDirPath, true (*recursive*))

                try
                    do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore
                with | :? TimeoutException as ex -> failtest $"Got {filesChanged.Count} of the expected {allExpectedChanges.Length} events. {ex.Message}"

                Expect.containsAll filesChanged allExpectedChanges ""
            }

        "Filters work",
        fun usePolling tempFolder ->
            async {
                let filters = [ "*.test"; ".*" ]

                let shouldMatchPath1 = Path.Combine(tempFolder, "endswith.test")
                let shouldMatchPath2 = Path.Combine(tempFolder, ".startsWithDot")
                let shouldIgnorePath1 = Path.Combine(tempFolder, "almostendswith.testbutnot")
                let shouldIgnorePath2 = Path.Combine(tempFolder, "noDots")

                let expectedChanges = [ shouldMatchPath1; shouldMatchPath2 ]
                let ignoredChanges = [ shouldIgnorePath1; shouldIgnorePath2 ]

                let watcher = createWatcher tempFolder usePolling filters
                let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
                let filesChanged = new HashSet<string>()
               
                let onFileChange = fun path ->
                    filesChanged.Add(path) |> ignore
                    if filesChanged.Count >= expectedChanges.Length
                    then tcs.TrySetResult(true) |> ignore
               
                watcher.OnFileChange.Subscribe(onFileChange) |> ignore
                watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                watcher.EnableRaisingEvents <- true

                File.WriteAllText(shouldMatchPath1, "")
                File.WriteAllText(shouldMatchPath2, "")
                File.WriteAllText(shouldIgnorePath1, "")
                File.WriteAllText(shouldIgnorePath2, "")
               
                do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore

                Expect.containsAll filesChanged expectedChanges ""

                let ignoredThatShouldNotBeThere =
                    filesChanged 
                    |> Seq.where(fun x -> ignoredChanges |> List.contains x)

                Expect.isEmpty ignoredThatShouldNotBeThere
                    $"""The list of changed files contains items matching ignored patterns: {String.Join("\n", ignoredThatShouldNotBeThere)}"""
            }

        $"Changing {nameof Unchecked.defaultof<IFileSystemWatcher>.BasePath } works",
        fun usePolling tempFolder ->
            async {
                let subdirPath1 = Path.Combine(tempFolder, "subdir1")
                let subdirPath2 = Path.Combine(tempFolder, "subdir2")
                let subdirPath3 = Path.Combine(tempFolder, "subdir3")
                
                let testFilePath1 = Path.Combine(subdirPath1, "testFile1")
                let testFilePath2 = Path.Combine(subdirPath2, "testFile2")
                let testFilePath3 = Path.Combine(subdirPath3, "testFile3")

                let subDirs = [ subdirPath1; subdirPath2; subdirPath3 ]
                let testFilePaths = [ testFilePath1; testFilePath2; testFilePath3 ]

                let results = [ new HashSet<string>(); new HashSet<string>(); new HashSet<string>() ]

                let watcher = createWatcherWithoutPath usePolling []

                for i in 0..2 do
                    let dir = subDirs.[i]
                    let file = testFilePaths.[i]
                    let filesChanged = results.[i]

                    Directory.CreateDirectory(dir) |> ignore

                    watcher.BasePath <- dir

                    let tcs = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
               
                    let onFileChange = fun path ->
                        filesChanged.Add(path) |> ignore
                        tcs.TrySetResult(true) |> ignore
               
                    let eventSubscription = watcher.OnFileChange.Subscribe(onFileChange)
                    watcher.OnError.Subscribe(fun err -> printfn "Error: %A" <| err.GetException()) |> ignore
                    watcher.EnableRaisingEvents <- true

                    File.WriteAllText(file, "")
                    
                    do! Async.TimeoutAfter(defaultTimeout, tcs.Task) |> Async.AwaitTaskUnwrappingException |> Async.Ignore

                    eventSubscription.Dispose()
            
                Expect.sequenceEqual results.[0] [ testFilePath1 ] "Expected one change in the first directory"
                Expect.sequenceEqual results.[1] [ testFilePath2 ] "Expected one change in the second directory"
                Expect.sequenceEqual results.[2] [ testFilePath3 ] "Expected one change in the third directory"
            }
    ]
  ]