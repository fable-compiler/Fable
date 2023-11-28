module Build.GithubRelease

open Build.Utils
open Octokit
open System
open Build.Workspace
open SimpleExec
open BlackFox.CommandLine

let private createGithubRelease
    (githubToken: string)
    (version: ChangelogParser.Types.Version)
    =

    let struct (lastestTag, _) =
        Command.ReadAsync("git", "describe --abbrev=0 --tags")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    // Only create a Github release if the tag doesn't exist
    // It can happens that we trigger a release whre Fable.Cli
    // is already up to date.
    if lastestTag.Trim() <> version.Version.ToString() then
        let githubClient =
            GitHubClient(ProductHeaderValue("fable-release-tool"))

        githubClient.Credentials <- Credentials(githubToken)

        let newRelease = NewRelease(version.Version.ToString())
        newRelease.Name <- version.Version.ToString()
        newRelease.Body <- ChangelogParser.Version.bodyAsMarkdown version
        newRelease.Draft <- false
        newRelease.Prerelease <- false // TODO: Detect if this is a prerelease

        githubClient.Repository.Release.Create(
            "fable-compiler",
            "Fable",
            newRelease
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> ignore

let private createReleaseCommitAndPush
    (version: ChangelogParser.Types.Version)
    =
    let versionText = version.Version.ToString()

    Command.Run(
        "git",
        CmdLine.empty
        |> CmdLine.appendRaw "commit"
        |> CmdLine.appendPrefix "-am" $"Release {versionText}"
        |> CmdLine.toString
    )

    Command.Run("git", "push")


let handle (args: string list) =
    let struct (currentBranch, _) =
        Command.ReadAsync("git", "rev-parse --abbrev-ref HEAD")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if currentBranch.Trim() <> "main" then
        failwith "You must be on the main branch to release"

    Publish.handle args

    let githubToken =
        Environment.GetEnvironmentVariable("GITHUB_TOKEN_FABLE_ORG")

    if isNull githubToken then
        failwith "Missing GITHUB_TOKEN_FABLE_ORG environment variable"

    let versionInfo = Changelog.getLastVersion Changelog.fableCLi

    createReleaseCommitAndPush versionInfo

    createGithubRelease githubToken versionInfo
