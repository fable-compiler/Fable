module Build.Test.Beam

open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec

let private buildDir = Path.Resolve("temp", "tests", "Beam")
let private sourceDir = Path.Resolve("tests", "Beam")
let private testRunnerSrc = Path.Resolve("tests", "Beam", "erl_test_runner.erl")

let handle (args: string list) =
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    // No fable-library for Beam yet, skip that step

    Directory.clean buildDir

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw sourceDir
                |> CmdLine.appendPrefix "--outDir" buildDir
                |> CmdLine.appendPrefix "--lang" "beam"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty |> CmdLine.appendRaw "--watch"
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

        // Compile tests to Erlang
        Command.Fable(fableArgs, workingDirectory = buildDir)

        // Copy test runner and fable-library-beam files, then compile all .erl files with erlc
        File.Copy(testRunnerSrc, Path.Combine(buildDir, "erl_test_runner.erl"), overwrite = true)

        let libDir = Path.Resolve("src", "fable-library-beam")

        if Directory.Exists(libDir) then
            for erlFile in Directory.GetFiles(libDir, "*.erl") do
                File.Copy(erlFile, Path.Combine(buildDir, Path.GetFileName(erlFile)), overwrite = true)

        // Copy Fable-compiled library files (e.g., system_text.erl from System.Text.fs)
        let compiledLibDir = Path.Resolve("temp", "fable-library-beam")

        if Directory.Exists(compiledLibDir) then
            for erlFile in Directory.GetFiles(compiledLibDir, "*.erl") do
                File.Copy(erlFile, Path.Combine(buildDir, Path.GetFileName(erlFile)), overwrite = true)

        let erlFiles = Directory.GetFiles(buildDir, "*.erl")
        let mutable compileErrors = 0

        for erlFile in erlFiles do
            let fileName = Path.GetFileName(erlFile)

            try
                Command.Run("erlc", fileName, workingDirectory = buildDir)
            with _ ->
                printfn $"WARNING: erlc failed for {fileName}, skipping"
                compileErrors <- compileErrors + 1

        if compileErrors > 0 then
            printfn $"WARNING: {compileErrors} file(s) failed to compile with erlc"

        // Run Erlang test runner
        Command.Run(
            "erl",
            "-noshell -pa . -eval \"erl_test_runner:main([\\\".\\\"])\" -s init stop",
            workingDirectory = buildDir
        )
