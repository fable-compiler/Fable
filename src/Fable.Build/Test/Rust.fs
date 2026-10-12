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

    let projectDir = Path.Resolve("src", "Fable.Transforms", "Rust", "AST", "Tests")

    if isWatch then
        Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = projectDir)
        |> Async.AwaitTask
        |> ignore
    else
        Command.Run("dotnet", "test -c Release", workingDirectory = projectDir)

let mainTestsDestinationDir = Path.Resolve("temp", "tests", "Rust")
let mainTestsProjectDir = Path.Resolve("tests", "Rust")

let handle (args: string list) =
    let forceFableLibrary = args |> List.contains "--force-fable-library"
    let isWatch = args |> List.contains "--watch"
    let astOnly = args |> List.contains "--ast-only"
    let noDotnet = args |> List.contains "--no-dotnet"

    let requestedFeatures =
        match args |> List.tryFindIndex ((=) "--features") with
        | Some index ->
            args
            |> List.tryItem (index + 1)
            |> Option.defaultValue ","
            |> fun features ->
                features.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun feature -> feature.Trim())
                |> Array.filter (fun feature -> feature <> "")
                |> Array.toList
        | None -> []

    let legacyFeatures =
        [ "--no_std", "no_std"; "--threaded", "threaded" ]
        |> List.choose (fun (flag, feature) ->
            if args |> List.contains flag then
                Some feature
            else
                None
        )

    let features = requestedFeatures @ legacyFeatures |> List.distinct

    let noStd = features |> List.contains "no_std"
    let threaded = features |> List.contains "threaded"

    if noStd && threaded then
        failwith "Cannot use no_std and threaded features together"

    BuildFableLibraryRust().Run(forceFableLibrary)

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

        Shell.copyFile mainTestsDestinationDir (mainTestsProjectDir </> "Cargo.toml")

        !!(mainTestsProjectDir </> "tests" </> "src" </> "*.rs")
        |> Seq.iter (fun file ->
            let destination = mainTestsDestinationDir </> "tests" </> "src"

            Shell.copyFile destination file
        )

        let cargoFeatures = features |> String.concat ","

        let cargoTestArgs =
            let cargoTest =
                if List.isEmpty features then
                    "cargo test"
                else
                    $"cargo test --features {cargoFeatures}"

            if threaded then
                cargoTest
            else
                $"{cargoTest} -- --test-threads=1"

        let fableArgs =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw mainTestsProjectDir
                    |> CmdLine.appendPrefix "--outDir" mainTestsDestinationDir
                    |> CmdLine.appendPrefix "--lang" "rust"
                    |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                    |> CmdLine.appendRaw "--noCache"
                    |> CmdLine.appendPrefixIf noStd "--define" "NO_STD_NO_EXCEPTIONS"

                    if isWatch then
                        CmdLine.empty
                        |> CmdLine.appendRaw "--watch"
                        |> CmdLine.appendRaw "--runWatch"
                        |> CmdLine.appendRaw cargoTestArgs
                    else
                        CmdLine.empty |> CmdLine.appendRaw "--run" |> CmdLine.appendRaw cargoTestArgs
                ]

        if isWatch then
            Async.Parallel
                [
                    if not noDotnet then
                        Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = mainTestsProjectDir)
                        |> Async.AwaitTask

                    Command.WatchFableAsync(fableArgs, workingDirectory = mainTestsDestinationDir)
                    |> Async.AwaitTask
                ]
            |> Async.RunSynchronously
            |> ignore
        else
            Command.Run("dotnet", "test -c Release", workingDirectory = mainTestsProjectDir)

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
