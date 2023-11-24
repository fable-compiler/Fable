module Build.Test.Integration

open Build.FableLibrary
open System.IO
open System
open BlackFox.CommandLine
open Build.Utils
open Build
open SimpleExec

let private integrationProjectDir =
    Path.Resolve("tests", "Integration", "Integration")

let private compilerProjectDir =
    Path.Resolve("tests", "Integration", "Compiler")

let private testProjectConfig
    (projectDirName: string)
    (configuration: string option)
    =
    let projectDir =
        Path.Resolve("tests", "Integration", "ProjectConfigs", projectDirName)

    let destinationDir =
        Path.Resolve(
            "temp",
            "tests",
            "Integration",
            "ProjectConfigs",
            projectDirName
        )

    Directory.clean destinationDir

    let fableArgs =
        CmdLine.empty
        |> CmdLine.appendRaw projectDir
        |> CmdLine.appendPrefix "--outDir" destinationDir
        |> CmdLine.appendPrefix "--exclude" "Fable.Core"
        |> CmdLine.appendPrefixIfSome "--configuration" configuration

    Command.Fable(fableArgs)

    Command.Run(
        "npx",
        "npx mocha . --reporter dot -t 10000",
        workingDirectory = destinationDir
    )

let handle (args: string list) =
    BuildFableLibraryJavaScript().Run()

    Command.Run(
        "dotnet",
        "run -c Release",
        workingDirectory = integrationProjectDir
    )

    Command.Run(
        "dotnet",
        "run -c Release",
        workingDirectory = compilerProjectDir
    )

    testProjectConfig "DebugWithExtraDefines" (Some "Debug")
    testProjectConfig "CustomConfiguration" (Some "Test")
    testProjectConfig "ReleaseNoExtraDefines" None
    testProjectConfig "ConsoleApp" None
