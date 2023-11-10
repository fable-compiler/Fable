module Build.WorkerJs

open SimpleExec
open Build.Utils
open BlackFox.CommandLine
open System.IO
open Build.FableLibrary
open Fake.IO
open Build.Workspace

let private projectDir = Path.Resolve("src", "fable-standalone")

let private buildDir = Path.Combine(projectDir, "temp")

let private distDir = Path.Combine(projectDir, "dist")

let handle (args: string list) =
    let minify = args |> List.contains "--no-minify" |> not
    let skipFableLibrary = args |> List.contains "--skip-fable-library"

    BuildFableLibraryJavaScript().Run(skipFableLibrary)

    let fableArgs =
        CmdLine.empty
        |> CmdLine.appendRaw "src/Worker"
        |> CmdLine.appendPrefix "--outDir" (buildDir </> "worker")
        |> CmdLine.appendPrefix "--lang" "javascript"

    Command.Fable(fableArgs, workingDirectory = projectDir)

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "rollup"
        |> CmdLine.appendRaw (buildDir </> "worker" </> "Worker.js")
        |> CmdLine.appendPrefix "-o" (buildDir </> "worker" </> "bundle.js")
        |> CmdLine.appendPrefix "--format" "iife"
        |> CmdLine.toString,
        workingDirectory = projectDir
    )

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "esbuild"
        |> CmdLine.appendRaw (buildDir </> "worker" </> "bundle.js")
        |> CmdLine.appendRaw $"""--outfile={(distDir </> "worker.min.js")}"""
        |> CmdLine.appendRaw "--format=iife"
        |> CmdLine.appendIf minify "--minify"
        |> CmdLine.toString,
        workingDirectory = projectDir
    )

    let fableLibraryDist = (distDir </> "fable-library")

    Directory.ensure fableLibraryDist

    // Copy Fable library to dist folder
    Shell.copyRecursive
        (Path.Resolve("temp", "fable-library"))
        fableLibraryDist
        true
    |> ignore
