module Build.GithubRelease

open System.IO
open Build.Workspace
open SimpleExec
open BlackFox.CommandLine
open EasyBuild.Tools.Git

let private createGithubRelease (version: LastVersionFinder.Version) =

    let struct (lastestTag, _) =
        Command.ReadAsync("git", "describe --abbrev=0 --tags")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let versionText = version.Version.ToString()

    // Only create a Github release if the tag doesn't exist
    // It can happens that we trigger a release where Fable.Cli
    // is already up to date.
    if lastestTag.Trim() <> versionText then
        Command.Run(
            "gh",
            CmdLine.empty
            |> CmdLine.appendRaw "release"
            |> CmdLine.appendRaw "create"
            |> CmdLine.appendRaw versionText
            |> CmdLine.appendPrefix "--title" versionText
            |> CmdLine.appendPrefix "--notes" version.Body
            |> CmdLine.appendIf version.Version.IsPrerelease "--prerelease"
            |> CmdLine.toString
        )

let private createReleaseCommitAndPush (version: LastVersionFinder.Version) =
    let versionText = version.Version.ToString()

    Git.addAll ()
    Git.commit ($"Release {versionText}")
    Git.push ()

let handle (args: string list) =
    let struct (currentBranch, _) =
        Command.ReadAsync("git", "rev-parse --abbrev-ref HEAD")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if currentBranch.Trim() <> "main" then
        failwith "You must be on the main branch to release"

    // Check if the user is authenticated
    Command.Run("gh", "auth status")

    Publish.handle args

    let changelogContent = File.ReadAllText(Changelog.fableCLi)

    match LastVersionFinder.tryFindLastVersion changelogContent with
    | Ok version ->
        createReleaseCommitAndPush version
        createGithubRelease version
    | Error err -> err.ToText() |> failwith
