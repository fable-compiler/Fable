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
    let runTyping = args |> List.contains "--typing"

    BuildFableLibraryPython().Run(skipFableLibrary)

    Directory.clean buildDir

    Command.Run("uv", "sync")

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
                    |> CmdLine.appendRaw $"uv run pytest {buildDir} -x"
                else
                    CmdLine.empty
                    |> CmdLine.appendRaw "--run"
                    |> CmdLine.appendRaw $"uv run pytest {buildDir} -x"
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = sourceDir)
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

        // Count the number of typing errors (so we can keep an eye on them)
        if runTyping then
            printfn "Running type checking ..."

            let output =
                let struct (output, _) =
                    Command.ReadAsync(
                        "uv",
                        "run pyright",
                        workingDirectory = buildDir,
                        handleExitCode = fun _ -> true
                    )
                    |> Async.AwaitTask
                    |> Async.RunSynchronously

                output

            // Extract and display only the summary line (last non-empty line)
            let summaryLine =
                output.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.tryLast
                |> Option.defaultValue "No output"

            printfn "Pyright summary: %s" summaryLine
        else
            printfn "Skipping type checking (use --typing to enable)"
