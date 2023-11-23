module Build.Test.Python

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec

let private buildDir = Path.Resolve("temp", "tests", "Python")
let private sourceDir = Path.Resolve("tests", "Python")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    BuildFableLibraryPython().Run(skipFableLibrary)

    Directory.clean buildDir

    Command.Run("poetry", "install")

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw sourceDir
                |> CmdLine.appendPrefix "--outDir" buildDir
                |> CmdLine.appendPrefix "--lang" "python"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw $"poetry run pytest {buildDir} -x"
                else
                    CmdLine.empty
                    |> CmdLine.appendRaw "--run"
                    |> CmdLine.appendRaw $"poetry run pytest {buildDir} -x"
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch test -c Release",
                        workingDirectory = sourceDir
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = buildDir)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else

        // Test against .NET
        Command.Run("dotnet", "test -c Release", workingDirectory = sourceDir)

        // Test against Python
        Command.Fable(fableArgs, workingDirectory = buildDir)
