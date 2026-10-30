module Build.Test.TypeScript

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec
open Fake.IO

let private projectDir = Path.Resolve("tests", "TypeScript")

/// Typed arrays change the type annotations, so tsc checks the tests with them off and on
type private TestRun(typedArrays: bool) =
    let name =
        if typedArrays then
            "TypeScriptTypedArrays"
        else
            "TypeScript"

    member _.TypedArraysArg = (string typedArrays).ToLowerInvariant()
    member _.FableDest = Path.Resolve("temp", "tests", name)
    member _.TscDest = Path.Resolve("temp", "tests", $"{name}Compiled")
    member this.TscArgs = $"tsc --outDir {this.TscDest}"

    member _.TestArgs =
        $"--test-reporter spec --test-timeout 20000 --test temp/tests/{name}/Main.js"

let handle (args: string list) =
    let forceFableLibrary = args |> List.contains "--force-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    BuildFableLibraryTypeScript().Run(forceFableLibrary)

    let prepare (run: TestRun) =
        Directory.clean run.FableDest
        Directory.clean run.TscDest

        Shell.copyFile run.FableDest (projectDir </> "tsconfig.json")

    let fableArgs (run: TestRun) =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw projectDir
                |> CmdLine.appendPrefix "--outDir" run.FableDest
                |> CmdLine.appendPrefix "--lang" "typescript"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendPrefix "--typedArrays" run.TypedArraysArg
                |> CmdLine.appendRaw "--noCache"

                // Let Fable handle the TypeScript invocation
                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw $"npx {run.TscArgs}"
            ]

    if isWatch then
        // Watch only the mode the JavaScript tests don't cover
        let run = TestRun(typedArrays = false)
        prepare run

        let nodemonArgs =
            CmdLine.empty
            |> CmdLine.appendRaw "nodemon"
            |> CmdLine.appendPrefix "-e" "js"
            |> CmdLine.appendPrefix "--watch" run.TscDest
            // Avoid polluting the logs when a lot of files change at once
            |> CmdLine.appendPrefix "--delay" "1s"
            |> CmdLine.appendRaw "--exec"
            |> CmdLine.appendRaw "\"node"
            |> CmdLine.appendRaw run.TestArgs
            |> CmdLine.appendRaw "\""
            |> CmdLine.toString

        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = projectDir)
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs run, workingDirectory = run.FableDest)
                |> Async.AwaitTask

                Command.RunAsync("npx", nodemonArgs, workingDirectory = run.TscDest)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Run("dotnet", "test -c Release", workingDirectory = projectDir)

        for run in [ TestRun(typedArrays = false); TestRun(typedArrays = true) ] do
            prepare run

            Command.Fable(fableArgs run, workingDirectory = run.FableDest)

            Command.Run("npx", run.TscArgs, workingDirectory = run.FableDest)

            Command.Run("node", run.TestArgs, workingDirectory = run.TscDest)
