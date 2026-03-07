module Build.Test.Beam

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec

let private buildDir = Path.Resolve("temp", "tests", "Beam")
let private sourceDir = Path.Resolve("tests", "Beam")
let private testRunnerSrc = Path.Resolve("tests", "Beam", "erl_test_runner.erl")

// OTP app name derived from Fable.Tests.Beam.fsproj — must match
// what generateBeamScaffold produces from the project file name.
let private testProjectName = "fable_tests_beam"

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

        // Compile tests to Erlang.
        // Fable automatically generates the rebar3 scaffold (rebar.config, .app.src,
        // per-dep rebar.config and ebin/*.app) alongside the .erl files in src/.
        Command.Fable(fableArgs, workingDirectory = buildDir)

        // Copy test runner and native Erlang helpers into src/ so rebar3 compiles them
        // as part of the project.
        let buildSrcDir = Path.Combine(buildDir, "src")

        File.Copy(testRunnerSrc, Path.Combine(buildSrcDir, "erl_test_runner.erl"), overwrite = true)

        let nativeErlDir = Path.Resolve("tests", "Beam", "erl")

        if Directory.Exists(nativeErlDir) then
            for erlFile in Directory.GetFiles(nativeErlDir, "*.erl") do
                File.Copy(erlFile, Path.Combine(buildSrcDir, Path.GetFileName(erlFile)), overwrite = true)

        // Compile everything with rebar3 using the generated scaffold.
        Command.Run("rebar3", "compile", workingDirectory = buildDir)

        // Collect all ebin directories produced by rebar3 for the -pa flags.
        let buildDefaultLib = Path.Combine(buildDir, "_build", "default", "lib")

        let ebinDirs =
            if Directory.Exists(buildDefaultLib) then
                Directory.GetDirectories(buildDefaultLib)
                |> Array.map (fun d -> Path.Combine(d, "ebin"))
                |> Array.filter Directory.Exists
            else
                [||]

        let paArgs = ebinDirs |> Array.map (fun d -> $"-pa \"{d}\"") |> String.concat " "

        // The test runner scans this directory for *_tests.beam modules.
        let projectEbinDir = Path.Combine(buildDefaultLib, testProjectName, "ebin")

        // Run Erlang test runner
        Command.Run(
            "erl",
            $"-noshell {paArgs} -eval \"erl_test_runner:main([\\\"{projectEbinDir}\\\"])\" -s init stop",
            workingDirectory = buildDir
        )
