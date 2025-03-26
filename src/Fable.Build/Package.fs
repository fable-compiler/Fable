module Build.Package

open Build.Utils
open Build.FableLibrary
open System
open Build.Workspace
open SimpleExec
open BlackFox.CommandLine
open System.IO
open EasyBuild.Tools.PackageJson

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

    let tempVersion = "5.999.0-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHmmss")

    let compilerFsPath =
        Path.Resolve("src", "Fable.Transforms", "Global", "Compiler.fs")

    let compilerFsOriginalContent = File.ReadAllText compilerFsPath

    Publish.updateLibraryVersionInFableTransforms
        tempVersion
        {|
            JavaScript = PackageJson.tempFableLibraryJs |> PackageJson.getVersion
            TypeScript = PackageJson.tempFableLibraryTs |> PackageJson.getVersion
        |}

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "pack"
        |> CmdLine.appendRaw Fsproj.fableCli
        |> CmdLine.appendPrefix "-c" "Release"
        // By pass the PackageVersion in the fsproj, without having to modify it on the disk
        |> CmdLine.appendRaw $"-p:PackageVersion={tempVersion}"
        |> CmdLine.appendPrefix "-o" packageDestination
        |> CmdLine.toString
    )

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "pack"
        |> CmdLine.appendRaw Fsproj.fableCompiler
        |> CmdLine.appendPrefix "-c" "Release"
        // By pass the PackageVersion in the fsproj, without having to modify it on the disk
        |> CmdLine.appendRaw $"-p:PackageVersion={tempVersion}"
        |> CmdLine.appendPrefix "-o" packageDestination
        |> CmdLine.toString
    )

    // This avoid common error of comitting the modified file
    File.WriteAllText(compilerFsPath, compilerFsOriginalContent)

    Command.Run(
        "dotnet",
        CmdLine.empty
        |> CmdLine.appendRaw "pack"
        |> CmdLine.appendRaw Fsproj.fableCore
        |> CmdLine.appendPrefix "-c" "Release"
        // By pass the PackageVersion in the fsproj, without having to modify it on the disk
        |> CmdLine.appendRaw $"-p:PackageVersion={tempVersion}"
        |> CmdLine.appendPrefix "-o" packageDestination
        |> CmdLine.toString
    )

    printfn
        $"""Local packages created.

Use the following commands to install them:

- Fable.Cli: dotnet tool update fable --version {tempVersion} --add-source {packageDestination}
- Fable.Compiler: dotnet add package Fable.Compiler --version {tempVersion} --source {packageDestination}
- Fable.Core: dotnet add package Fable.Core --version {tempVersion} --source {packageDestination}
    """
