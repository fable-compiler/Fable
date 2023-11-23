module Build.Test.Rust

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open SimpleExec

let private testAst isWatch =

    let projectDir =
        Path.Resolve("src", "Fable.Transforms", "Rust", "AST", "Tests")

    if isWatch then
        Command.RunAsync(
            "dotnet",
            "watch test -c Release",
            workingDirectory = projectDir
        )
        |> Async.AwaitTask
        |> ignore
    else
        Command.Run("dotnet", "test -c Release", workingDirectory = projectDir)

let mainTestsDestinationDir = Path.Resolve("temp", "tests", "Rust")
let mainTestsProjectDir = Path.Resolve("tests", "Rust")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let astOnly = args |> List.contains "--ast-only"
    let noStd = args |> List.contains "--no_std"
    let threaded = args |> List.contains "--threaded"
    let noDotnet = args |> List.contains "--no-dotnet"

    if noStd && threaded then
        failwith "Cannot use --no-std and --threaded at the same time"

    BuildFableLibraryRust().Run(skipFableLibrary)

    if astOnly then
        testAst isWatch
    else
        // limited cleanup to reduce IO churn, speed up rebuilds,
        // and save the ssd (target folder can get huge)
        Directory.clean (mainTestsDestinationDir </> "tests" </> "src")
        Directory.clean (mainTestsDestinationDir </> "tests" </> "tests")
        Directory.clean (mainTestsDestinationDir </> "tests" </> ".fable")

        // copy rust only tests files (these must be present when running dotnet test as import expr tests for file presence)
        Directory.CreateDirectory(mainTestsDestinationDir </> "tests" </> "src")
        |> ignore

        Shell.copyFile
            mainTestsDestinationDir
            (mainTestsProjectDir </> "Cargo.toml")

        !!(mainTestsProjectDir </> "tests" </> "src" </> "*.rs")
        |> Seq.iter (fun file ->
            let destination = mainTestsDestinationDir </> "tests" </> "src"

            Shell.copyFile destination file
        )

        let cargoTestArgs =
            if noStd then
                "cargo test --features no_std"
            elif threaded then
                "cargo test --features threaded"
            else
                "cargo test"

        let fableArgs =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw mainTestsProjectDir
                    |> CmdLine.appendPrefix "--outDir" mainTestsDestinationDir
                    |> CmdLine.appendPrefix "--lang" "rust"
                    |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                    |> CmdLine.appendRaw "--noCache"
                    |> CmdLine.appendPrefixIf
                        noStd
                        "--define"
                        "NO_STD_NO_EXCEPTIONS"

                    if isWatch then
                        CmdLine.empty
                        |> CmdLine.appendRaw "--watch"
                        |> CmdLine.appendRaw "--runWatch"
                        |> CmdLine.appendRaw cargoTestArgs
                    else
                        CmdLine.empty
                        |> CmdLine.appendRaw "--run"
                        |> CmdLine.appendRaw cargoTestArgs
                ]

        if isWatch then
            Async.Parallel
                [
                    if not noDotnet then
                        Command.RunAsync(
                            "dotnet",
                            "watch test -c Release",
                            workingDirectory = mainTestsProjectDir
                        )
                        |> Async.AwaitTask

                    Command.WatchFableAsync(
                        fableArgs,
                        workingDirectory = mainTestsDestinationDir
                    )
                    |> Async.AwaitTask
                ]
            |> Async.RunSynchronously
            |> ignore
        else
            Command.Run(
                "dotnet",
                "test -c Release",
                workingDirectory = mainTestsProjectDir
            )

            Command.Fable(fableArgs, workingDirectory = mainTestsDestinationDir)

// Old build system was running cargo fmt and cargo build
// Is it still needed?
// Command.Run(
//     "cargo",
//     "fmt",
//     workingDirectory = mainTestsDestinationDir
// )

// Command.Run(
//     "cargo",
//     "temp",
//     workingDirectory = mainTestsDestinationDir
// )
