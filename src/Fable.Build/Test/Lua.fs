module Build.Test.Lua

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec
open Fake.IO

let private buildDir = Path.Resolve("temp", "tests", "Lua")
let private testsFolder = Path.Resolve("tests", "Lua")
let private testsFsprojFolder = Path.Resolve("tests", "Lua")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"

    BuildFableLibraryLua().Run(skipFableLibrary)

    Directory.clean buildDir

    Directory.GetFiles(testsFolder, "*") |> Seq.iter (Shell.copyFile buildDir)

    Directory.GetFiles(testsFolder, "*.lua") |> Seq.iter (Shell.copyFile buildDir)

    let testCmd = $"lua test {buildDir}/main.lua"

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw testsFsprojFolder
                |> CmdLine.appendPrefix "--outDir" (buildDir </> "src")
                |> CmdLine.appendPrefix "--lang" "lua"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty
                    |> CmdLine.appendRaw "--watch"
                    |> CmdLine.appendRaw "--runWatch"
                    |> CmdLine.appendRaw testCmd
                else
                    CmdLine.empty |> CmdLine.appendRaw "--run" |> CmdLine.appendRaw testCmd
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = testsFsprojFolder)
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = buildDir)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        Command.Run("dotnet", "test -c Release", workingDirectory = testsFsprojFolder)

        Command.Fable(fableArgs, workingDirectory = buildDir)
