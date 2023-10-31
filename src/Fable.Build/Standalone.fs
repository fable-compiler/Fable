module Build.Standalone

open SimpleExec
open Build.Utils
open BlackFox.CommandLine
open System.IO
open Build.FableLibrary

let private projectDir = Path.Resolve("src", "fable-standalone")

let private buildDir = Path.Combine(projectDir, "temp")

let private distDir = Path.Combine(projectDir, "dist")

let build () =

    let fableArgs =
        CmdLine.concat [
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
        |> CmdLine.appendPrefix "-o" (distDir </> "bundle.js")
        |> CmdLine.appendPrefix "--format" "umd"
        |> CmdLine.appendPrefix "--name" "__FABLE_STANDALONE__"
        |> CmdLine.toString,
        workingDirectory = projectDir
    )

    Command.Run(
        "npx",
        CmdLine.empty
        |> CmdLine.appendRaw "esbuild"
        |> CmdLine.appendRaw (distDir </> "bundle.js")
        |> CmdLine.appendRaw "--bundle"
        |> CmdLine.appendRaw $"""--outfile={(distDir </> "bundle.js")}"""
        |> CmdLine.appendRaw "--format=esm"
        |> CmdLine.appendRaw "--platform=node"
        |> CmdLine.appendRaw "--minify"
        |> CmdLine.appendRaw "--overwrite"
        |> CmdLine.toString,
            workingDirectory = projectDir
    )

let handle (args: string list) =
    let minify = args |> List.contains "--no-minify" |> not
    let isWatch = args |> List.contains "--watch"

    build ()
