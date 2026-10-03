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
                    CmdLine.appendRaw "watch"
                    >> CmdLine.appendRaw "--noCache"
                    // There seems to be some strange console Log writting
                    >> CmdLine.appendRaw "--verbose"
                    >> CmdLine.appendRaw "--runWatch"
                    >> CmdLine.appendRaw "npx jest",
                    workingDirectory = workingDirectory
                )
                |> Async.AwaitTask

            // Running both command in the same shell don't seems to be working as expected.

            // For now, we expect the user to use `./build.sh test javascript --react-only --watch`
            // and `npx jest --watch` in a second terminal
            // Command.RunAsync("npx", "jest --watch", workingDirectory = workingDirectory)
            // |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Fable(CmdLine.appendRaw "--noCache", workingDirectory = workingDirectory)

        Command.Run("npx", "jest", workingDirectory = workingDirectory)

let private testAdaptive (isWatch: bool) =
    let folderName = "Adaptive"
    let sourceDir = Path.Resolve("tests", "Js", folderName)

    let destinationDir = Path.Resolve("temp", "tests", "JavaScript", folderName)

    let testCommand =
        CmdLine.empty
        |> CmdLine.appendRaw "node"
        |> CmdLine.appendPrefix "--test-reporter" "spec"
        |> CmdLine.appendPrefix "--test-timeout" "20000"
        |> CmdLine.appendPrefix "--test" (destinationDir </> "Main.js")
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
                    |> CmdLine.appendRaw testCommand
                else
                    CmdLine.empty |> CmdLine.appendRaw "--run" |> CmdLine.appendRaw testCommand
            ]

    if isWatch then
        Command.WatchFable(fableArgs, workingDirectory = destinationDir)
    else
        Command.Fable(fableArgs, workingDirectory = destinationDir)

type private DateTimeRepresentation =
    | JsDate
    | Temporal

let private runMainTests (representation: DateTimeRepresentation) (isWatch: bool) (noDotnet: bool) =
    let folderName = "Main"
    let sourceDir = Path.Resolve("tests", "Js", folderName)

    let outputFolder =
        match representation with
        | JsDate -> "JavaScript"
        | Temporal -> "JavaScriptTemporal"

    let destinationDir = Path.Resolve("temp", "tests", outputFolder, folderName)

    let testCommand =
        CmdLine.empty
        |> CmdLine.appendRaw "node"
        |> CmdLine.appendPrefix "--test-reporter" "spec"
        |> CmdLine.appendPrefix "--test-timeout" "20000"
        |> CmdLine.appendPrefix "--test" (destinationDir </> "Main.js")
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
                |> CmdLine.appendIf (representation = Temporal) "--test:js-temporal"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw testCommand
                else
                    CmdLine.empty |> CmdLine.appendRaw "--run" |> CmdLine.appendRaw testCommand
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch run -c Release",
                        workingDirectory = Path.Combine("tests", "Js", "Main")
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = destinationDir)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Fable(fableArgs, workingDirectory = destinationDir)

let private handleMainTests (isWatch: bool) (noDotnet: bool) =
    if isWatch then
        // In watch mode, we only test the Main tests to not pollute the logs too much
        runMainTests JsDate isWatch noDotnet
    else
        Command.Run("dotnet", "run -c Release", workingDirectory = Path.Combine("tests", "Js", "Main"))

        // Test the Main tests against JavaScript
        runMainTests JsDate false noDotnet

        // Re-run them with the Temporal date/time representation enabled
        runMainTests Temporal false noDotnet

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
    let isTemporalOnly = args |> List.contains "--temporal-only"
    let forceFableLibrary = args |> List.contains "--force-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    let exclusiveArgs =
        [
            "--react-only", isReactOnly
            "--standalone-only", isStandaloneOnly
            "--adaptive-only", isAdaptiveOnly
            "--temporal-only", isTemporalOnly
        ]
        |> List.filter snd
        |> List.map (fun (name, _) -> $"'%s{name}'")

    if exclusiveArgs.Length > 1 then
        failwith $"""Cannot use %s{String.Join(", ", exclusiveArgs)} at the same time"""

    BuildFableLibraryJavaScript().Run(forceFableLibrary)

    if isReactOnly then
        testReact isWatch
    else if isStandaloneOnly then
        Standalone.handleStandaloneFast ()
    else if isAdaptiveOnly then
        testAdaptive isWatch
    else if isTemporalOnly then
        runMainTests Temporal isWatch noDotnet
    else
        handleMainTests isWatch noDotnet
