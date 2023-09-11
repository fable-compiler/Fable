module Build.JavaScript.Tests

open Fun.Build
open Fun.Build.Internal
open Build.FableLibrary
open System.IO
open BlackFox.CommandLine
open Build.Utils

module private Commands =

    let watch =
        CmdArg.Create(
            "-w",
            "--watch",
            "Watch for changes and recompile",
            isOptional = true
        )

    let isWatch (ctx : StageContext) =
        ctx.TryGetCmdArg watch
        |> Option.isSome

module private Stages =

    let testReact (allowWatchMode : bool) =
        stage "Test react" {
            workingDir "tests/React"

            stage "Restore" {
                run "npm install"
            }

            stage "Processes" {
                paralle (fun ctx ->
                    allowWatchMode && Commands.isWatch ctx
                )

                run (fun ctx ->
                    let isWatch = Commands.isWatch ctx
                    let args = CmdLine.appendRaw "--noCache"

                    Cmd.fable (args, isWatch)
                    |> CmdLine.toString
                )

                run (fun ctx ->
                    let isWatch = Commands.isWatch ctx

                    Cmd.jest (
                        CmdLine.appendIf isWatch "--watch"
                    )
                    |> CmdLine.toString
                )
            }
        }

    /// <summary>Create a mocha tests stage</summary>
    /// <param name="folderName">Name of the main folder for the tests</param>
    /// <param name="isWatchable">Does the stage supports watch mode</param>
    /// <returns></returns>
    let createMochaTestStage (folderName : string) (isWatchable : bool) =
        let sourceDir = Path.Resolve("tests", "Js", folderName)
        let destinationDir = Path.Resolve("build", "tests", "JavaScript", folderName)

        stage $"Js/{folderName}" {
            run (fun _ ->
                if Directory.Exists destinationDir then
                    Directory.Delete(destinationDir, true)

                // Ensure the directory exists, so nodemon can watch it
                Directory.CreateDirectory(destinationDir) |> ignore
            )

            // We need a sub-stage to make sure the clean up is done
            // Before starting long running processes
            stage "Processes" {
                paralle (fun ctx ->
                    isWatchable && Commands.isWatch ctx
                )

                run (fun ctx ->
                    let isWatch =
                        Commands.isWatch ctx

                    let args =
                        CmdLine.appendRaw sourceDir
                        >> CmdLine.appendPrefix "--outDir" destinationDir
                        >> CmdLine.appendPrefix "--lang" "javascript"
                        >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                        >> CmdLine.appendRaw "--noCache"
                        >> CmdLine.appendIf isWatch "--watch"
                        // This improve the logs readability in watch mode
                        // by forcing Fable to not write on the same line its progress
                        >> CmdLine.appendIf isWatch "--verbose"

                    Cmd.fable(args, isWatch)
                    |> CmdLine.toString
                )

                run (fun ctx ->
                    let isWatch =
                        Commands.isWatch ctx

                    let mochaCommand =
                        Cmd.mocha (
                            CmdLine.appendRaw destinationDir
                            >> CmdLine.appendPrefix "--reporter" "dot"
                            >> CmdLine.appendPrefix "-t" "10000"
                        )

                    // Mocha doesn't support watch ESM files
                    // So we use nodemon as the watcher instead
                    let nodemonCommand =
                        CmdLine.concat [
                            Cmd.nodemon (
                                CmdLine.appendPrefix "-e" "js"
                                >> CmdLine.appendPrefix "--watch" destinationDir
                                // Avoid polluting the logs when Fable generates a lot of files
                                >> CmdLine.appendPrefix "--delay" "1s"
                                >> CmdLine.appendRaw "--exec"
                            )
                            mochaCommand
                        ]

                    if isWatch then
                        nodemonCommand
                        |> CmdLine.toString
                    else
                        mochaCommand
                        |> CmdLine.toString
                )
            }
        }

    let testMain =
        createMochaTestStage "Main" true

    let testAdaptive =
        createMochaTestStage "Adaptive" false

    let testStandalone =
        let fableJs = Path.Resolve("src", "fable-compiler-js", "src", "app.fs.js")
        let testProj = Path.Resolve("tests", "Js", "Main", "Fable.Tests.fsproj")
        let buildDir = Path.Resolve("build", "tests", "JavaScript", "Standalone")

        stage "Standalone" {
            whenEnvVar "CI"

            run (
                Cmd.fable (
                    CmdLine.appendRaw "src/fable-standalone/src"
                    >> CmdLine.appendRaw "--noCache"
                )
                |> CmdLine.toString
            )

            run (
                Cmd.fable (
                    CmdLine.appendRaw "src/fable-compiler-js/src"
                    >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                    >> CmdLine.appendPrefix "--define" "LOCAL_TEST"
                    >> CmdLine.appendRaw "--noCache"
                )
                |> CmdLine.toString
            )

            run (
                Cmd.node (
                    CmdLine.appendRaw fableJs
                    >> CmdLine.appendRaw testProj
                    >> CmdLine.appendRaw buildDir
                )
                |> CmdLine.toString
            )

            run (
                Cmd.mocha (
                    CmdLine.appendRaw buildDir
                    >> CmdLine.appendPrefix "--reporter" "dot"
                    >> CmdLine.appendPrefix "-t" "10000"
                )
                |> CmdLine.toString
            )
        }

let registerPipelines () =
    pipeline "test-javascript" {
        BuildFableLibraryJavaScript().Stage()
        whenCmdArg Commands.watch

        let mainTestsDir = Path.Combine("tests", "Js", "Main")

        stage "Main tests" {
            paralle Commands.isWatch

            stage "Run tests against .NET" {
                workingDir mainTestsDir

                run (fun ctx ->
                    let isWatch = Commands.isWatch ctx

                    if isWatch then
                        "dotnet watch run -c Release"
                    else
                        "dotnet run -c Release"
                )
            }

            // Note: We only support watch mode for the Main tests
            // otherwise the logs is way too verbose and processes
            // erase each other making it impossible to read
            Stages.testMain
        }

        // The stages below will be run when not in watch mode
        // Because watch mode will be captured above and
        // the user will press Ctrl+C to stop it stopping the pipeline too
        Stages.testReact false
        Stages.testAdaptive
        Stages.testStandalone

        runIfOnlySpecified
    }

    pipeline "test-javascript-react" {
        BuildFableLibraryJavaScript().Stage()
        whenCmdArg Commands.watch

        Stages.testReact true

        runIfOnlySpecified
    }