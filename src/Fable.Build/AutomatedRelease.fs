module Build.AutomatedRelease

open System.IO
open Build.Workspace
open SimpleExec
open Build.FableLibrary

let handle (args: string list) =
    let struct (currentBranch, _) =
        Command.ReadAsync("git", "rev-parse --abbrev-ref HEAD")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if currentBranch.Trim() <> "main" then
        failwith "You must be on the main branch to release"

    let struct (lastCommitMessage, _) =
        Command.ReadAsync("git", "log -1 --pretty=%B")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if not (lastCommitMessage.TrimStart().StartsWith("chore: release")) then
        failwith "The last commit message must start with 'chore: release'"

    // Build all the fable-libraries
    // Force rebuld of fable-libraries to make sure they are generated with the latest
    // version of the compiler
    //
    // NOTE:
    //      Python libraries are distributed using PyPi (pip install fable-library)
    //      So we don't need Python runtime for publishing Fable packages

    BuildFableLibraryBeam().Run(true)
    BuildFableLibraryDart().Run(true)
    BuildFableLibraryJavaScript().Run(true)
    BuildFableLibraryRust().Run(true)
    BuildFableLibraryTypeScript().Run(true)

    Publish.publishNpm ProjectDir.temp_fable_library_js
    Publish.publishNpm ProjectDir.temp_fable_library_ts

    Publish.publishNpm ProjectDir.fable_metadata

    Publish.publishNuget ProjectDir.fableAst false
    Publish.publishNuget ProjectDir.fableCore false
    Publish.publishNuget ProjectDir.fableCompiler true
    Publish.publishNuget ProjectDir.fableCli false
    Publish.publishNuget ProjectDir.fablePublishUtils false

    CompilerJs.handle []

    Publish.publishNpm ProjectDir.fable_standalone
    Publish.publishNpm ProjectDir.fable_compiler_js


    let changelogContent = File.ReadAllText(Changelog.fableCLi)

    match LastVersionFinder.tryFindLastVersion changelogContent with
    | Ok version -> GithubRelease.createGithubRelease version
    | Error err -> err.ToText() |> failwith
