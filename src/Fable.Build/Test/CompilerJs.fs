module Build.Test.CompilerJs


open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec
open Fake.IO

let private fableCompilerJsDir = Path.Resolve("src", "fable-compiler-js")

let private quicktestProject =
    Path.Resolve("src", "quicktest", "Quicktest.fsproj")

let private quicktestBuildDir =
    Path.Resolve("temp", "tests", "fable-compiler-js", "quicktest")

let handle (args: string list) =
    Command.Run(
        "npm",
        "link ../fable-standalone ../fable-metadata",
        workingDirectory = fableCompilerJsDir
    )

    Build.CompilerJs.handle args

    Command.Run(
        "node",
        CmdLine.empty
        |> CmdLine.appendRaw "index.js"
        |> CmdLine.appendRaw quicktestProject
        |> CmdLine.appendPrefix "--outDir" quicktestBuildDir
        |> CmdLine.toString,
        workingDirectory = fableCompilerJsDir
    )

    Command.Run(
        "node",
        quicktestBuildDir </> "QuickTest.js",
        workingDirectory = fableCompilerJsDir
    )

    // TODO: Add test against an fsx file but for now it seems broken

    Command.Run(
        "npm",
        "unlink ../fable-standalone ../fable-metadata",
        workingDirectory = fableCompilerJsDir
    )
