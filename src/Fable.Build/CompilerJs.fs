module Build.CompilerJs

open Build.FableLibrary
open System.IO
open System
open BlackFox.CommandLine
open Build.Utils
open Build
open SimpleExec
open Fake.IO

let private fableCompilerJsDir = Path.Resolve("src", "fable-compiler-js")
let private buildDir = Path.Combine(fableCompilerJsDir, "temp")
let private distDir = Path.Combine(fableCompilerJsDir, "dist")

module private FableLib =

    let source = Path.Resolve("temp", "fable-library")
    let destination = Path.Combine(distDir, "fable-library")

module private FableMetadata =

    let source = Path.Resolve("src", "fable-metadata", "lib")
    let destination = Path.Combine(distDir, "fable-metadata")

let handle (args: string list) =
    let minify = args |> List.contains "--no-minify" |> not
    // Note:
    // We don't need to build fable-library, because it will be
    // build as part of Standalone.build

    Command.Run("npm", "install", workingDirectory = fableCompilerJsDir)

    Standalone.handle args

    // Clean up temp folders
    Directory.clean buildDir
    Directory.clean distDir
    // Clean up destination folders and make sure they exist
    Directory.clean FableLib.destination
    Directory.clean FableMetadata.destination

    Command.Fable(
        CmdLine.empty
        |> CmdLine.appendRaw "src"
        |> CmdLine.appendPrefix "--outDir" buildDir
        |> CmdLine.appendPrefix "--exclude" "Fable.Core",
        workingDirectory = fableCompilerJsDir
    )

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "rollup"
        |> CmdLine.appendRaw (buildDir </> "app.js")
        |> CmdLine.appendPrefix "-o" (buildDir </> "app.rollup.js")
        |> CmdLine.appendPrefix "--format" "esm"
        |> CmdLine.appendPrefix "--name" "Fable"
        |> CmdLine.toString,
        workingDirectory = fableCompilerJsDir
    )

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "esbuild"
        |> CmdLine.appendRaw (buildDir </> "app.rollup.js")
        // When bundling with esbuild, there are errors about:
        // Cannot assign to "XXX" because it is a constant
        // Beause this is a CLI tool, I don't think bundling is important here
        // |> CmdLine.appendRaw "--bundle"
        |> CmdLine.appendRaw $"""--outfile={(distDir </> "app.min.js")}"""
        |> CmdLine.appendRaw "--format=esm"
        |> CmdLine.appendRaw "--platform=node"
        |> CmdLine.appendIf minify "--minify"
        |> CmdLine.toString,
        workingDirectory = fableCompilerJsDir
    )

    Shell.copyRecursive FableLib.source FableLib.destination true |> ignore

    Shell.copyRecursive FableMetadata.source FableMetadata.destination true
    |> ignore
