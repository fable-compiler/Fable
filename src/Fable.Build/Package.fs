module Build.Package

open Build.Utils
open Build.FableLibrary
open Octokit
open System
open Build.Workspace
open SimpleExec
open BlackFox.CommandLine
open System.IO

let private packageDestination = Path.Resolve("temp", "packages")

let handle (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"
    // Build all the fable-libraries
    BuildFableLibraryDart().Run(skipFableLibrary)
    BuildFableLibraryJavaScript().Run(skipFableLibrary)
    BuildFableLibraryPython().Run(skipFableLibrary)
    BuildFableLibraryRust().Run(skipFableLibrary)
    BuildFableLibraryTypeScript().Run(skipFableLibrary)

    Directory.clean packageDestination

    let fableCliVersion =
        "900.0.0-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHhmm")

    let compilerFsPath =
        Path.Resolve("src", "Fable.Transforms", "Global", "Compiler.fs")

    let compilerFsOriginalContent = File.ReadAllText compilerFsPath

    Publish.updateLibraryVersionInFableTransforms
        fableCliVersion
        {|
            JavaScript =
                Npm.getVersionFromProjectDir ProjectDir.temp_fable_library
        |}

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "pack"
        |> CmdLine.appendRaw Fsproj.fableCli
        |> CmdLine.appendPrefix "-c" "Release"
        // By pass the PackageVersion in the fsproj, without having to modify it on the disk
        |> CmdLine.appendRaw $"-p:PackageVersion={fableCliVersion}"
        |> CmdLine.appendPrefix "-o" packageDestination
        |> CmdLine.toString
    )

    // This avoid common error of comitting the modified file
    File.WriteAllText(compilerFsPath, compilerFsOriginalContent)

    let fableCoreVersion =
        "900.0.0-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHhmm")

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "pack"
        |> CmdLine.appendRaw Fsproj.fableCore
        |> CmdLine.appendPrefix "-c" "Release"
        // By pass the PackageVersion in the fsproj, without having to modify it on the disk
        |> CmdLine.appendRaw $"-p:PackageVersion={fableCoreVersion}"
        |> CmdLine.appendPrefix "-o" packageDestination
        |> CmdLine.toString
    )

    printfn
        $"""Local packages created.

Use the following commands to install them:

- Fable.Cli: dotnet tool update fable --version {fableCliVersion} --add-source {packageDestination}
- Fable.Core: dotnet add package Fable.Core --version {fableCoreVersion} --source {packageDestination}
    """
