module Build.Standalone

open SimpleExec
open Build.Utils
open BlackFox.CommandLine
open System.IO
open Build.FableLibrary
open Fake.IO

let private projectDir = Path.Resolve("src", "fable-standalone")

let private buildDir = Path.Combine(projectDir, "temp")

let private distDir = Path.Combine(projectDir, "dist")

let private buildWorker (minify: bool) =

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

let build (minify: bool) =

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw "src"
                |> CmdLine.appendPrefix "--outDir" buildDir
                |> CmdLine.appendPrefix "--lang" "javascript"

            // if isWatch then
            //     CmdLine.empty
            //     |> CmdLine.appendRaw "--watch"
            //     |> CmdLine.appendPrefix "--run" "rollup"
            //     // |> CmdLine.appendRaw rollupArgs
            //     |> CmdLine.appendRaw "--watch"
            ]

    // Clean destination folders and ensure they exist
    Directory.clean buildDir
    Directory.clean distDir

    // Build standalone bundle
    Command.Fable(fableArgs, workingDirectory = projectDir)

    // Compile project as a library using Vite
    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "rollup"
        |> CmdLine.appendRaw (buildDir </> "Main.js")
        |> CmdLine.appendPrefix "-o" (buildDir </> "bundle.js")
        // We need to use umd format so it can be used inside of web workers
        // Web workers don't support ES modules (yet)
        |> CmdLine.appendPrefix "--format" "umd"
        |> CmdLine.appendPrefix "--name" "__FABLE_STANDALONE__"
        |> CmdLine.toString,
        workingDirectory = projectDir
    )

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "esbuild"
        |> CmdLine.appendRaw (buildDir </> "bundle.js")
        |> CmdLine.appendRaw $"""--outfile={(distDir </> "bundle.min.js")}"""
        |> CmdLine.appendRaw "--format=esm"
        |> CmdLine.appendIf minify "--minify"
        |> CmdLine.toString,
        workingDirectory = projectDir
    )

let handle (args: string list) =
    let minify = args |> List.contains "--no-minify" |> not
    let isWatch = args |> List.contains "--watch"
    let skipFableLibrary = args |> List.contains "--skip-fable-library"

    Command.Run("npm", "install", workingDirectory = projectDir)

    BuildFableLibraryJavaScript().Run(skipFableLibrary)

    build minify
    // Force skip fable library for worker, because it's already built
    WorkerJs.handle ("--skip-fable-library" :: args)
