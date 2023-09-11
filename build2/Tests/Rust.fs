module Build.Rust.Tests

open Fun.Build
open Fun.Build.Internal
open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open Fake.IO
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators

module private Commands =

    let watch =
        CmdArg.Create(
            "-w",
            "--watch",
            "Watch for changes and recompile",
            isOptional = true
        )

    let noStd =
        CmdArg.Create(
            "-n",
            "--no-std",
            "Do not use the Rust standard library",
            isOptional = true
        )

    let threaded =
        CmdArg.Create(
            "-t",
            "--threaded",
            "Use the Rust threaded runtime",
            isOptional = true
        )

    let isWatch (ctx : StageContext) =
        ctx.TryGetCmdArg watch
        |> Option.isSome

module Stages =

    let testAST =
        let projectDir = Path.Resolve("src", "Fable.Transforms", "Rust", "AST", "Tests")

        stage "Test AST" {
            workingDir projectDir

            run "dotnet test -c Release"
        }

    let mainTests =
        let testsDestinationDir = Path.Resolve("build", "tests", "Rust")
        let testsProjectDir = Path.Resolve("tests", "Rust")

        stage "Main tests" {

            stage "Clean up" {
                run (fun _ ->
                    // limited cleanup to reduce IO churn, speed up rebuilds,
                    // and save the ssd (target folder can get huge)
                    let cleanUp dirName =
                        if Directory.Exists (testsDestinationDir </> dirName) then
                            Directory.Delete(testsDestinationDir </> dirName, true)
                        Directory.CreateDirectory(testsDestinationDir </> dirName) |> ignore

                    cleanUp "src"
                    cleanUp "tests"
                    cleanUp ".fable"
                )
            }

            stage "Copy required files" {
                // copy rust only tests files (these must be present when running dotnet test as import expr tests for file presence)
                run (fun _ ->
                    Directory.CreateDirectory(testsDestinationDir </> "tests" </> "src") |> ignore
                    Shell.copyFile testsDestinationDir (testsProjectDir </> "cargo.toml")

                    !! (testsProjectDir </> "tests" </> "src" </> "*.rs")
                    |> Seq.iter (fun file ->
                        let destionation = testsDestinationDir </> "tests" </> "src"
                        Shell.copyFile destionation file
                    )
                )
            }

            stage "Main tests" {
                stage ".NET" {
                    workingDir testsProjectDir

                    run "dotnet test -c Release"
                }

                run (
                    Cmd.fable (
                        CmdLine.appendRaw testsProjectDir
                        >> CmdLine.appendPrefix "--outDir" testsDestinationDir
                        >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                        >> CmdLine.appendPrefix "--lang" "rust"
                        >> CmdLine.appendPrefix "--fableLib" "fable-library-rust"
                        >> CmdLine.appendRaw "--noCache"
                        // >> CmdLine.appendIf (Commands.isWatch) "--watch"
                        // >> CmdLine.appendIf (Commands.noStd) "--noStd"
                        // >> CmdLine.appendIf (Commands.threaded) "--threaded"
                    )
                    |> CmdLine.toString
                )

                stage "Rust" {
                    workingDir testsDestinationDir

                    // run "cargo fmt"
                    // run "cargo build"
                    run "cargo test"
                }


            }

        }

let registerPipelines () =

    pipeline $"test-rust" {
        BuildFableLibraryRust().Stage()

        whenCmdArg Commands.noStd
        whenCmdArg Commands.threaded

        Stages.testAST
        Stages.mainTests

        runIfOnlySpecified
    }