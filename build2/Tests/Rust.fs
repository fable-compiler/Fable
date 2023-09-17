module Build.Tests.Rust

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open SimpleExec

// module private Commands =

//     let watch =
//         CmdArg.Create(
//             "-w",
//             "--watch",
//             "Watch for changes and recompile",
//             isOptional = true
//         )

//     let noStd =
//         CmdArg.Create(
//             "-n",
//             "--no-std",
//             "Do not use the Rust standard library",
//             isOptional = true
//         )

//     let threaded =
//         CmdArg.Create(
//             "-t",
//             "--threaded",
//             "Use the Rust threaded runtime",
//             isOptional = true
//         )

//     let isWatch (ctx: StageContext) = ctx.TryGetCmdArg watch |> Option.isSome

// module Stages =

//     let testAST =
//         let projectDir =
//             Path.Resolve("src", "Fable.Transforms", "Rust", "AST", "Tests")

//         stage "Test AST" {
//             workingDir projectDir

//             run "dotnet test -c Release"
//         }

//     let mainTests =
//         let testsDestinationDir = Path.Resolve("build", "tests", "Rust")
//         let testsProjectDir = Path.Resolve("tests", "Rust")

//         stage "Main tests" {

//             stage "Clean up" {
//                 run (fun _ ->
//                     // limited cleanup to reduce IO churn, speed up rebuilds,
//                     // and save the ssd (target folder can get huge)
//                     let cleanUp dirName =
//                         if
//                             Directory.Exists(testsDestinationDir </> dirName)
//                         then
//                             Directory.Delete(
//                                 testsDestinationDir </> dirName,
//                                 true
//                             )

//                         Directory.CreateDirectory(
//                             testsDestinationDir </> dirName
//                         )
//                         |> ignore

//                     cleanUp "src"
//                     cleanUp "tests"
//                     cleanUp ".fable"
//                 )
//             }

//             stage "Copy required files" {
//                 // copy rust only tests files (these must be present when running dotnet test as import expr tests for file presence)
//                 run (fun _ ->
//                     Directory.CreateDirectory(
//                         testsDestinationDir </> "tests" </> "src"
//                     )
//                     |> ignore

//                     Shell.copyFile
//                         testsDestinationDir
//                         (testsProjectDir </> "cargo.toml")

//                     !!(testsProjectDir </> "tests" </> "src" </> "*.rs")
//                     |> Seq.iter (fun file ->
//                         let destionation =
//                             testsDestinationDir </> "tests" </> "src"

//                         Shell.copyFile destionation file
//                     )
//                 )
//             }

//             stage "Main tests" {
//                 stage ".NET" {
//                     workingDir testsProjectDir

//                     run "dotnet test -c Release"
//                 }

//                 run (
//                     Cmd.fable (
//                         CmdLine.appendRaw testsProjectDir
//                         >> CmdLine.appendPrefix "--outDir" testsDestinationDir
//                         >> CmdLine.appendPrefix "--exclude" "Fable.Core"
//                         >> CmdLine.appendPrefix "--lang" "rust"
//                         >> CmdLine.appendPrefix
//                             "--fableLib"
//                             "fable-library-rust"
//                         >> CmdLine.appendRaw "--noCache"
//                     // >> CmdLine.appendIf (Commands.isWatch) "--watch"
//                     // >> CmdLine.appendIf (Commands.noStd) "--noStd"
//                     // >> CmdLine.appendIf (Commands.threaded) "--threaded"
//                     )
//                     |> CmdLine.toString
//                 )

//                 stage "Rust" {
//                     workingDir testsDestinationDir

//                     // run "cargo fmt"
//                     // run "cargo build"
//                     run "cargo test"
//                 }

//             }

//         }

// let registerPipelines () =

//     pipeline $"test-rust" {
//         BuildFableLibraryRust().Stage()

//         whenCmdArg Commands.noStd
//         whenCmdArg Commands.threaded

//         Stages.testAST
//         Stages.mainTests

//         runIfOnlySpecified
//     }

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

let mainTestsDestinationDir = Path.Resolve("build", "tests", "Rust")
let mainTestsProjectDir = Path.Resolve("tests", "Rust")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--fast"
    let isWatch = args |> List.contains "--watch"
    let astOnly = args |> List.contains "--ast-only"
    let noStd = args |> List.contains "--no-std"
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
        Directory.clean "src"
        Directory.clean "tests"
        Directory.clean ".fable"

        // copy rust only tests files (these must be present when running dotnet test as import expr tests for file presence)
        Directory.CreateDirectory(mainTestsDestinationDir </> "tests" </> "src")
        |> ignore

        Shell.copyFile
            mainTestsDestinationDir
            (mainTestsProjectDir </> "cargo.toml")

        !!(mainTestsProjectDir </> "tests" </> "src" </> "*.rs")
        |> Seq.iter (fun file ->
            let destionation = mainTestsDestinationDir </> "tests" </> "src"

            Shell.copyFile destionation file
        )

        let cargoTestArgs =
            if noStd then
                "test --features no_std"
            elif threaded then
                "test --features threaded"
            else
                "test"

        let fableArgs =
            CmdLine.concat [
                CmdLine.empty
                |> CmdLine.appendRaw mainTestsProjectDir
                |> CmdLine.appendPrefix "--outDir" mainTestsDestinationDir
                |> CmdLine.appendPrefix "--lang" "python"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"
                |> CmdLine.appendPrefixIf noStd "--define" "NO_STD_NO_EXCEPTIONS"

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
            Async.Parallel [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch test -c Release",
                        workingDirectory = mainTestsProjectDir
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(
                    fableArgs, workingDirectory = mainTestsDestinationDir
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
            //     "build",
            //     workingDirectory = mainTestsDestinationDir
            // )
