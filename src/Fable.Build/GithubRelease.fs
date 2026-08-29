module Build.GithubRelease

open System
open System.IO
open Build.Workspace
open SimpleExec
open BlackFox.CommandLine
open EasyBuild.Tools.Git

let createGithubRelease (version: LastVersionFinder.Version) =

    let versionText = version.Version.ToString()

    let releaseExists =
        try
            Command.Run("gh", $"release view {versionText}")
            true
        with :? ExitCodeException ->
            false

    // Only create a Github release if the tag doesn't exist
    // It can happens that we trigger a release where Fable.Cli
    // is already up to date.
    if not releaseExists then
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

        // Creating the release above with the default GITHUB_TOKEN does not
        // trigger the `release: published` event in other workflows (GitHub
        // suppresses it to avoid recursive runs). `workflow_dispatch` is one of
        // the two exceptions that CAN be triggered by GITHUB_TOKEN, so we
        // explicitly dispatch the PyPI publish workflow for the freshly created tag.
        Command.Run(
            "gh",
            CmdLine.empty
            |> CmdLine.appendRaw "workflow"
            |> CmdLine.appendRaw "run"
            |> CmdLine.appendRaw "publish-pypi.yml"
            |> CmdLine.appendPrefix "--ref" versionText
            |> CmdLine.toString
        )

let private createReleaseCommitAndPush (version: LastVersionFinder.Version) =
    let versionText = version.Version.ToString()

    Git.addAll ()
    Git.commit ($"Release %s{versionText}")
    Git.push ()

let handle (args: string list) =
    let struct (currentBranch, _) =
        Command.ReadAsync("git", "rev-parse --abbrev-ref HEAD")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if currentBranch.Trim() <> "main" then
        failwith "You must be on the main branch to release"

    Command.Run("gh", "auth status")

    let skipPublish = args |> List.contains "--skip-publish"

    if not skipPublish then
        Publish.handle args

    let changelogContent = File.ReadAllText(Changelog.fableCLi)

    match LastVersionFinder.tryFindLastVersion changelogContent with
    | Ok version ->
        // createReleaseCommitAndPush version
        createGithubRelease version
    | Error err -> err.ToText() |> failwith
