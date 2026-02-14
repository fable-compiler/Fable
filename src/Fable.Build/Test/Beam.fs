module Build.Test.Beam

open Build.FableLibrary
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
    let skipFableLibrary = args |> List.contains "--skip-fable-library"

    BuildFableLibraryBeam().Run(skipFableLibrary)

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

        // Copy test runner to build dir
        File.Copy(testRunnerSrc, Path.Combine(buildDir, "erl_test_runner.erl"), overwrite = true)

        // fable-library-beam files are auto-copied to fable_modules/fable-library-beam/ by the compiler
        let fableModulesLibDir =
            Path.Combine(buildDir, "fable_modules", "fable-library-beam")

        // Compile fable-library-beam .erl files first
        let mutable compileErrors = 0

        if Directory.Exists(fableModulesLibDir) then
            for erlFile in Directory.GetFiles(fableModulesLibDir, "*.erl") do
                let fileName = Path.GetFileName(erlFile)

                try
                    Command.Run("erlc", fileName, workingDirectory = fableModulesLibDir)
                with _ ->
                    printfn $"WARNING: erlc failed for {fileName} (library), skipping"
                    compileErrors <- compileErrors + 1

        // Compile test .erl files with library on code path
        let erlFiles = Directory.GetFiles(buildDir, "*.erl")

        for erlFile in erlFiles do
            let fileName = Path.GetFileName(erlFile)

            try
                Command.Run("erlc", $"-pa fable_modules/fable-library-beam {fileName}", workingDirectory = buildDir)
            with _ ->
                printfn $"WARNING: erlc failed for {fileName}, skipping"
                compileErrors <- compileErrors + 1

        if compileErrors > 0 then
            printfn $"WARNING: {compileErrors} file(s) failed to compile with erlc"

        // Run Erlang test runner with library on code path
        Command.Run(
            "erl",
            "-noshell -pa . -pa fable_modules/fable-library-beam -eval \"erl_test_runner:main([\\\".\\\"])\" -s init stop",
            workingDirectory = buildDir
        )
