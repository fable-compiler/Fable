module Build.Test.CompilerJs

open Build.FableLibrary
open System.IO
open System
open BlackFox.CommandLine
open Build.Utils
open Build
open SimpleExec
open Fake.IO


let private mainTestProject =
    Path.Resolve("tests", "Js", "Main", "Fable.Tests.fsproj")

let private fableCompilerJsDir = Path.Resolve("src", "fable-compiler-js")
let private buildDir = Path.Combine(fableCompilerJsDir, "temp")
let private distDir = Path.Combine(fableCompilerJsDir, "dist")

module private FableLib =

    let source = Path.Resolve("temp", "fable-library")
    let destination = Path.Combine(fableCompilerJsDir, "fable-library")

module private FableMetadata =

    let source = Path.Resolve("src", "fable-metadata", "lib")
    let destination = Path.Combine(fableCompilerJsDir, "fable-metadata")

let handle (args: string list) =
    // BuildFableLibraryJavaScript().Run()

    // Standalone.build ()

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

    Command.Run("npx", "vite build", workingDirectory = fableCompilerJsDir)

    Shell.copyRecursive FableLib.source FableLib.destination true |> ignore

    Shell.copyRecursive FableMetadata.source FableMetadata.destination true
    |> ignore
