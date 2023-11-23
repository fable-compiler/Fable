module Build.Quicktest.Core

open Build.FableLibrary
open SimpleExec
open BlackFox.CommandLine
open Build.Utils

type RunMode =
    | RunScript
    | RunCommand of string

type QuicktestConfig =
    {
        Language: string
        FableLibBuilder: BuildFableLibrary
        ProjectDir: string
        Extension: string
        RunMode: RunMode
    }

let genericQuicktest (config: QuicktestConfig) (args: string list) =
    let skipFableLibrary = args |> List.contains "--skip-fable-library"

    config.FableLibBuilder.Run(skipFableLibrary)

    let appendRunMode (cmdLine: CmdLine) =
        match config.RunMode with
        | RunScript -> cmdLine |> CmdLine.appendRaw "--runScript"
        | RunCommand command ->
            cmdLine
            // Use appendRaw to avoid quoting the command
            |> CmdLine.appendRaw "--run"
            |> CmdLine.appendRaw command

    let projectDir = Path.Resolve config.ProjectDir

    Command.Fable(
        CmdLine.empty
        |> CmdLine.appendRaw "clean"
        |> CmdLine.appendRaw projectDir
        |> CmdLine.appendPrefix "--lang" config.Language
        |> CmdLine.appendPrefix "--extension" config.Extension
        |> CmdLine.appendRaw "--yes",
        workingDirectory = projectDir
    )

    Command.WatchFableAsync(
        CmdLine.empty
        |> CmdLine.appendRaw projectDir
        |> CmdLine.appendPrefix "--lang" config.Language
        |> CmdLine.appendPrefix "--extension" config.Extension
        |> CmdLine.appendPrefix "--exclude" "Fable.Core"
        |> CmdLine.appendRaw "--noCache"
        |> CmdLine.appendRaw "--watch"
        |> appendRunMode,
        workingDirectory = projectDir
    )
    |> Async.AwaitTask
    |> Async.RunSynchronously
