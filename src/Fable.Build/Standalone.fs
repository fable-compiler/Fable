module Build.Standalone

open SimpleExec
open Build.Utils
open BlackFox.CommandLine
open System.IO

let private projectDir = Path.Resolve("src", "fable-standalone", "src")

let private buildDir = Path.Resolve("temp", "fable-standalone")

let private distDir = Path.Resolve("src", "fable-standalone", "dist")

let handle (args: string list) =
    let minify = args |> List.contains "--no-minify" |> not
    let isWatch = args |> List.contains "--watch"

    let rollupOutput =
        match isWatch, minify with
        | true, _ -> failwith "Not supported yet"
        | false, true -> distDir </> "bundle.min.js"
        | false, false -> buildDir </> "bundle.js"

    let rollupArgs =
        CmdLine.empty
        |> CmdLine.appendRaw (buildDir </> "bundle/Main.js")
        |> CmdLine.appendPrefix "--output" rollupOutput
        |> CmdLine.appendPrefix "--format" "umd"
        |> CmdLine.appendPrefix "--name" "__FABLE_STANDALONE__"
        |> CmdLine.toString

    let fableArgs =
        CmdLine.concat [
            CmdLine.empty
            |> CmdLine.appendPrefix "--outDir" (buildDir </> "bundle")
            |> CmdLine.appendPrefix "--lang" "javascript"

            if isWatch then
                CmdLine.empty
                |> CmdLine.appendRaw "--watch"
                |> CmdLine.appendPrefix "--run" "rollup"
                |> CmdLine.appendRaw rollupArgs
                |> CmdLine.appendRaw "--watch"
        ]

    // Clean destination folders and ensure they exist
    Directory.clean buildDir
    Directory.clean distDir

    if isWatch then
        failwith "Not supported yet"
    else
        // Build standalone bundle
        Command.Fable(fableArgs, workingDirectory = projectDir)

        Command.Run("npx", $"rollup {rollupArgs}")

        if minify then
            Command.Run(
                "npx",
                CmdLine.empty
                |> CmdLine.appendRaw "terser"
                |> CmdLine.appendPrefix "--output" (distDir </> "bundle.min.js")
                |> CmdLine.appendRaw "--mangle"
                |> CmdLine.appendRaw "--compress"
                |> CmdLine.toString
            )

        // let printFileSize (fileName: string) =
        //     let fileInfo = FileInfo(distDir </> fileName)

        //     let size =
        //         float (fileInfo.Length) / 1000.0

        //     printfn $"Bundle size: %.2f{size} KB"

        // printFileSize "bundle.min.js"
        // printFileSize "working.min.js"
