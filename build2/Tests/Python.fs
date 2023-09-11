module Build.Python.Tests

open Fun.Build
open Fun.Build.Internal
open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine

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

let private buildDir = Path.Resolve("build", "tests", "Python")
let private sourceDir = Path.Resolve("tests", "Python")

let registerPipelines () =

    pipeline $"test-python" {
        BuildFableLibraryPython().Stage()

        stage "Clean" {
            run (fun _ ->
                if Directory.Exists buildDir then
                    Directory.Delete(buildDir, true)
                Directory.CreateDirectory(buildDir) |> ignore
            )
        }

        stage "Build" {
            run (fun _ ->
                let args =
                    CmdLine.appendRaw sourceDir
                    >> CmdLine.appendPrefix "--outDir" buildDir
                    >> CmdLine.appendPrefix "--lang" "python"
                    >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                    >> CmdLine.appendRaw "--noCache"

                Cmd.fable args
                |> CmdLine.toString
            )
        }

        stage "Run tests against .NET" {
            workingDir sourceDir

            run "dotnet test -c Release"
        }

        stage "Run tests against Python" {
            workingDir buildDir
            whenCmdArg Commands.watch

            run (fun ctx ->
                let isWatch = Commands.isWatch ctx

                let poetryCommand =
                    "poetry run pytest -x"

                if isWatch then
                    Cmd.nodemon (
                        CmdLine.appendPrefix "-e" "py"
                        >> CmdLine.appendPrefix "-w" buildDir
                        // Avoid polluting the logs when Fable generates a lot of files
                        >> CmdLine.appendPrefix "--delay" "1s"
                        >> CmdLine.appendRaw "--exec"
                        // We need to wrap the command in quotes
                        >> CmdLine.appendRaw "\""
                        >> CmdLine.appendRaw poetryCommand
                        >> CmdLine.appendRaw "\""
                    )
                    |> CmdLine.toString
                else
                    poetryCommand
            )
        }

        runIfOnlySpecified
    }