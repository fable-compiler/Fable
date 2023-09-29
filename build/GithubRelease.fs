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

    let githubClient = GitHubClient(ProductHeaderValue("fable-release-tool"))
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

let private createTag (version: ChangelogParser.Types.Version) =
    let versionText = version.Version.ToString()

    Command.Run(
        "git",
        CmdLine.empty
        |> CmdLine.appendRaw "commit"
        |> CmdLine.appendPrefix "-am" $"Release {versionText}"
        |> CmdLine.toString
    )

    // Command.Run(
    //     "git",
    //     CmdLine.empty
    //     |> CmdLine.appendRaw "tag"
    //     |> CmdLine.appendPrefix "-a" (versionText)
    //     |> CmdLine.appendPrefix "-m" $"Release {versionText}"
    //     |> CmdLine.toString
    // )

    Command.Run(
        "git",
        "push"
    )


let handle (args: string list) =
    Publish.handle args

    let githubToken = Environment.GetEnvironmentVariable("GITHUB_TOKEN_FABLE_ORG")

    if githubToken = null then
        failwith "Missing GITHUB_TOKEN_FABLE_ORG environment variable"

    let versionInfo = Changelog.getLastVersion Changelog.fableCLi

    createTag versionInfo
    createGithubRelease githubToken versionInfo
