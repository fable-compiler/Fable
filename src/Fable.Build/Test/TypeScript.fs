module Build.Test.TypeScript

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec
open Fake.IO

let private projectDir = Path.Resolve("tests", "TypeScript")
let private fableDest = Path.Resolve("temp", "tests", "TypeScript")
let private tscDest = Path.Resolve("temp", "tests", "TypeScriptCompiled")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    BuildFableLibraryTypeScript().Run(skipFableLibrary)

    Directory.clean fableDest
    Directory.clean tscDest

    Shell.copyFile fableDest (projectDir </> "tsconfig.json")

    let tscArgs = $"tsc --outDir {tscDest}"
    let mochaArgs = "mocha temp/tests/TypeScript --reporter dot -t 10000"

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw projectDir
                |> CmdLine.appendPrefix "--outDir" fableDest
                |> CmdLine.appendPrefix "--lang" "typescript"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                // Let Fable handle the TypeScript invocation
                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw $"npx {tscArgs}"
            ]

    let nodemonArgs =
        CmdLine.empty
        |> CmdLine.appendRaw "nodemon"
        |> CmdLine.appendPrefix "-e" "js"
        |> CmdLine.appendPrefix "--watch" tscDest
        // Avoid polluting the logs when a lot of files change at once
        |> CmdLine.appendPrefix "--delay" "1s"
        |> CmdLine.appendRaw "--exec"
        |> CmdLine.appendRaw "\""
        |> CmdLine.appendRaw mochaArgs
        |> CmdLine.appendRaw "\""
        |> CmdLine.toString

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync(
                        "dotnet",
                        "watch test -c Release",
                        workingDirectory = projectDir
                    )
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = fableDest)
                |> Async.AwaitTask

                Command.RunAsync("npx", nodemonArgs, workingDirectory = tscDest)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Run("dotnet", "test -c Release", workingDirectory = projectDir)

        Command.Fable(fableArgs, workingDirectory = fableDest)

        Command.Run("npx", tscArgs, workingDirectory = fableDest)

        Command.Run("npx", mochaArgs, workingDirectory = tscDest)
