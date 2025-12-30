module Build.Test.Python

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec

let private buildDir = Path.Resolve("temp", "tests", "Python")
let private sourceDir = Path.Resolve("tests", "Python")
let private fableLibraryBuildDir = Path.Resolve("temp", "fable-library-py")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"
    let runTyping = args |> List.contains "--typing"
    let runFormat = args |> List.contains "--format"

    BuildFableLibraryPython().Run(skipFableLibrary)

    Directory.clean buildDir

    Command.Run("uv", "sync")

    // Install local fable-library as editable package for testing
    // This ensures tests use the locally built version, not PyPI
    Command.Run("uv", $"pip install -e {fableLibraryBuildDir}")

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
                    let ruffCmd =
                        if runFormat then
                            $"uv run ruff check --select I --fix {buildDir} && uv run ruff format {buildDir} && "
                        else
                            ""

                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw $"{ruffCmd}uv run pytest {buildDir} -x"
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

        if runFormat then
            // Run Ruff linter checking import sorting and fix any issues
            Command.Run("uv", $"run ruff check --select I --fix {buildDir}")
            // Run Ruff formatter on all generated files
            Command.Run("uv", $"run ruff format {buildDir}")

        // Run pytest
        Command.Run("uv", $"run pytest {buildDir} -x")

        // Count the number of typing errors (so we can keep an eye on them)
        if runTyping then
            printfn "Running type checking ..."

            let output =
                let struct (output, _) =
                    Command.ReadAsync(
                        "uv",
                        "run pyright .",
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
