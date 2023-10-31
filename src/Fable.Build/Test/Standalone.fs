module Build.Test.Standalone

open Build.FableLibrary
open System.IO
open System
open BlackFox.CommandLine
open Build.Utils
open Build
open SimpleExec

let private mainTestProject =
    Path.Resolve("tests", "Js", "Main", "Fable.Tests.fsproj")

let private mochaCommand = "npx mocha . --reporter dot -t 10000"

let handleStandaloneFast () =
    let fableCompilerJsDir = Path.Resolve("src", "fable-compiler-js", "src")
    let fableCompilerJsEntry = Path.Combine(fableCompilerJsDir, "app.fs.js")
    let standaloneBuildDest = Path.Resolve("temp", "tests", "Standalone")

    Command.Fable(
        CmdLine.appendRaw "--noCache",
        workingDirectory = Path.Resolve("src", "fable-standalone", "src")
    )

    Command.Fable(
        CmdLine.appendPrefix "--exclude" "Fable.Core"
        >> CmdLine.appendPrefix "--define" "LOCAL_TEST"
        >> CmdLine.appendRaw "--noCache",
        workingDirectory = fableCompilerJsDir
    )

    // Make sure the projects are restored
    // Otherwise, on a first VM run dependencies can be missing
    Command.Run("dotnet", $"restore {mainTestProject}")

    Command.Run(
        "node",
        CmdLine.empty
        |> CmdLine.appendRaw fableCompilerJsEntry
        |> CmdLine.appendRaw mainTestProject
        |> CmdLine.appendRaw standaloneBuildDest
        |> CmdLine.toString,
        workingDirectory = fableCompilerJsDir
    )

    Command.Run("npx", mochaCommand, workingDirectory = standaloneBuildDest)

let handle (args: string list) =
    BuildFableLibraryJavaScript().Run()
    handleStandaloneFast ()
