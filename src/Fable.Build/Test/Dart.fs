module Build.Test.Dart

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec
open Fake.IO

let private buildDir = Path.Resolve("temp", "tests", "Dart")
let private testsFolder = Path.Resolve("tests", "Dart")
let private testsFsprojFolder = Path.Resolve("tests", "Dart", "src")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    BuildFableLibraryDart().Run(skipFableLibrary)

    Directory.clean buildDir

    Directory.GetFiles(testsFolder, "pubspec.*")
    |> Seq.iter (Shell.copyFile buildDir)

    Shell.copyFile buildDir (testsFolder </> "analysis_options.yaml")

    Directory.GetFiles(testsFolder, "*.dart")
    |> Seq.iter (Shell.copyFile buildDir)

    let testCmd = $"dart test {buildDir}/main.dart"

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw testsFsprojFolder
                |> CmdLine.appendPrefix "--outDir" (buildDir </> "src")
                |> CmdLine.appendPrefix "--lang" "dart"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw testCmd
                else
                    CmdLine.empty
                    |> CmdLine.appendRaw "--run"
                    |> CmdLine.appendRaw testCmd
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch test -c Release",
                        workingDirectory = testsFsprojFolder
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = buildDir)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Run(
            "dotnet",
            "test -c Release",
            workingDirectory = testsFsprojFolder
        )

        Command.Fable(fableArgs, workingDirectory = buildDir)
