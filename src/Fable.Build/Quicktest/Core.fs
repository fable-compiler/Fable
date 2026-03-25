module Build.Quicktest.Core

open Build.FableLibrary
open SimpleExec
open BlackFox.CommandLine
open Build.Utils

type RunMode =
    | RunScript
    | RunCommand of string
    | NoRun

type QuicktestConfig =
    {
        Language: string
        FableLibBuilder: BuildFableLibrary
        ProjectDir: string
        Extension: string
        RunMode: RunMode
    }

let genericQuicktest (config: QuicktestConfig) (args: string list) =
    let forceFableLibrary = args |> List.contains "--force-fable-library"
    let isWatch = args |> List.contains "--watch"

    config.FableLibBuilder.Run(forceFableLibrary)

    let appendRunMode (cmdLine: CmdLine) =
        match config.RunMode with
        | RunScript -> cmdLine |> CmdLine.appendRaw "--runScript"
        | RunCommand command ->
            cmdLine
            // Use appendRaw to avoid quoting the command
            |> CmdLine.appendRaw "--run"
            |> CmdLine.appendRaw command
        | NoRun -> cmdLine

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

    let fableArgs =
        CmdLine.empty
        |> CmdLine.appendRaw projectDir
        |> CmdLine.appendPrefix "--lang" config.Language
        |> CmdLine.appendPrefix "--extension" config.Extension
        |> CmdLine.appendPrefix "--exclude" "Fable.Core"
        |> CmdLine.appendRaw "--noCache"
        |> (fun cmdLine ->
            if isWatch then
                cmdLine |> CmdLine.appendRaw "--watch"
            else
                cmdLine
        )
        |> CmdLine.appendRaw "--verbose"
        |> appendRunMode

    if isWatch then
        Command.WatchFableAsync(fableArgs, workingDirectory = projectDir)
        |> Async.AwaitTask
        |> Async.RunSynchronously
    else
        Command.Fable(fableArgs, workingDirectory = projectDir)
