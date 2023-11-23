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
    let workingDirectoy = Path.Resolve("tests", "React")

    Command.Run("npm", "install", workingDirectory = workingDirectoy)

    if isWatch then
        Async.Parallel
            [
                Command.WatchFableAsync(
                    CmdLine.appendRaw "--noCache",
                    workingDirectory = workingDirectoy
                )
                |> Async.AwaitTask

                Command.RunAsync(
                    "npx",
                    "jest --watch",
                    workingDirectory = workingDirectoy
                )
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Fable(
            CmdLine.appendRaw "--noCache",
            workingDirectory = workingDirectoy
        )

        Command.Run("npx", "jest", workingDirectory = workingDirectoy)

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

// let isCI = Environment.GetEnvironmentVariable("CI") |> Option.ofObj

// standalone will be tested by a separate CI job
// if isCI.IsSome then
//     Standalone.handleStandaloneFast ()

let handle (args: string list) =
    let isReactOnly = args |> List.contains "--react-only"
    let isStandaloneOnly = args |> List.contains "--standalone-only"
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    if isStandaloneOnly && isReactOnly then
        failwith
            "Cannot use --react-only and --standalone-only at the same time"

    BuildFableLibraryJavaScript().Run(skipFableLibrary)

    if isReactOnly then
        testReact isWatch
    else if isStandaloneOnly then
        Standalone.handleStandaloneFast ()
    else
        handleMainTests isWatch noDotnet
