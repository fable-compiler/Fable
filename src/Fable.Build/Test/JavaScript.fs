module Build.Test.JavaScript

open Build.FableLibrary
open System.IO
open System
open BlackFox.CommandLine
open Build.Utils
open Build
open SimpleExec

let private mainTestSourceDir = Path.Resolve("tests", "Js", "Main")

let private mainTestProject =
    Path.Resolve("tests", "Js", "Main", "Fable.Tests.fsproj")

let private testReact (isWatch: bool) =
    let workingDirectory = Path.Resolve("tests", "React")

    Command.Run("npm", "install", workingDirectory = workingDirectory)

    if isWatch then
        Async.Parallel
            [
                Command.WatchFableAsync(
                    CmdLine.appendRaw "--noCache",
                    workingDirectory = workingDirectory
                )
                |> Async.AwaitTask

                Command.RunAsync(
                    "npx",
                    "jest --watch",
                    workingDirectory = workingDirectory
                )
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Fable(
            CmdLine.appendRaw "--noCache",
            workingDirectory = workingDirectory
        )

        Command.Run("npx", "jest", workingDirectory = workingDirectory)

let private testAdaptive (isWatch: bool) =
    let folderName = "Adaptive"
    let sourceDir = Path.Resolve("tests", "Js", folderName)

    let destinationDir = Path.Resolve("temp", "tests", "JavaScript", folderName)

    let mochaCommand =
        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "mocha"
        |> CmdLine.appendRaw destinationDir
        |> CmdLine.appendPrefix "--reporter" "dot"
        |> CmdLine.appendPrefix "-t" "10000"
        |> CmdLine.toString

    Directory.clean destinationDir

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw sourceDir
                |> CmdLine.appendPrefix "--outDir" destinationDir
                |> CmdLine.appendPrefix "--lang" "javascript"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw mochaCommand
                else
                    CmdLine.empty
                    |> CmdLine.appendRaw "--run"
                    |> CmdLine.appendRaw mochaCommand
            ]

    if isWatch then
        Command.WatchFable(fableArgs, workingDirectory = destinationDir)
    else
        Command.Fable(fableArgs, workingDirectory = destinationDir)

let private handleMainTests (isWatch: bool) (noDotnet: bool) =
    let folderName = "Main"
    let sourceDir = Path.Resolve("tests", "Js", folderName)

    let destinationDir = Path.Resolve("temp", "tests", "JavaScript", folderName)

    let mochaCommand =
        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "mocha"
        |> CmdLine.appendRaw destinationDir
        |> CmdLine.appendPrefix "--reporter" "dot"
        |> CmdLine.appendPrefix "-t" "10000"
        |> CmdLine.toString

    Directory.clean destinationDir

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw sourceDir
                |> CmdLine.appendPrefix "--outDir" destinationDir
                |> CmdLine.appendPrefix "--lang" "javascript"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw mochaCommand
                else
                    CmdLine.empty
                    |> CmdLine.appendRaw "--run"
                    |> CmdLine.appendRaw mochaCommand
            ]

    if isWatch then
        // In watch mode, we only test the Main tests to not pollute the logs too much
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch run -c Release",
                        workingDirectory = Path.Combine("tests", "Js", "Main")
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(
                    fableArgs,
                    workingDirectory = destinationDir
                )
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Run(
            "dotnet",
            "run -c Release",
            workingDirectory = Path.Combine("tests", "Js", "Main")
        )

        // Test the Main tests against JavaScript
        Command.Fable(fableArgs, workingDirectory = destinationDir)

        testReact false
        testAdaptive false

// let isCI = Environment.GetEnvironmentVariable("CI") |> Option.ofObj

// standalone will be tested by a separate CI job
// if isCI.IsSome then
//     Standalone.handleStandaloneFast ()

let handle (args: string list) =
    let isReactOnly = args |> List.contains "--react-only"
    let isStandaloneOnly = args |> List.contains "--standalone-only"
    let isAdaptiveOnly = args |> List.contains "--adaptive-only"
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    match (isReactOnly, isStandaloneOnly, isAdaptiveOnly) with
    | (true, true, _)
    | (true, _, true)
    | (_, true, true) ->
        failwith
            "Cannot use '--react-only', '--standalone-only' and '--adaptive-only' at the same time"

    | _ -> ()

    BuildFableLibraryJavaScript().Run(skipFableLibrary)

    if isReactOnly then
        testReact isWatch
    else if isStandaloneOnly then
        Standalone.handleStandaloneFast ()
    else if isAdaptiveOnly then
        testAdaptive isWatch
    else
        handleMainTests isWatch noDotnet
