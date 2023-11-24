module SimpleExec

open SimpleExec
open BlackFox.CommandLine
open Build.Utils

type Command with

    static member Fable
        (
            args: CmdLine,
            ?workingDirectory: string,
            ?noEcho,
            ?echoPrefix
        )
        =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let args =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw "run"
                    |> CmdLine.appendPrefix "-c" "Release"
                    |> CmdLine.appendPrefix "--project" localFableDir
                    |> CmdLine.appendRaw "--"

                    args

                ]
            |> CmdLine.toString

        Command.Run(
            "dotnet",
            args,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member Fable
        (
            ?argsBuilder: CmdLine -> CmdLine,
            ?workingDirectory: string,
            ?noEcho,
            ?echoPrefix
        )
        =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder = defaultArg argsBuilder id

        let args =
            CmdLine.empty
            |> CmdLine.appendRaw "run"
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "--"
            |> argsBuilder
            |> CmdLine.toString

        Command.Run(
            "dotnet",
            args,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member FableAsync
        (
            argsBuilder: CmdLine -> CmdLine,
            ?workingDirectory,
            ?noEcho,
            ?echoPrefix
        )
        =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder =
            CmdLine.empty
            |> CmdLine.appendRaw "run"
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "--"
            |> argsBuilder
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            argsBuilder,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member WatchFableAsync
        (
            argsBuilder: CmdLine -> CmdLine,
            ?workingDirectory,
            ?noEcho,
            ?echoPrefix
        )
        =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let argsBuilder =
            CmdLine.empty
            |> CmdLine.appendRaw "watch"
            |> CmdLine.appendPrefix "--project" localFableDir
            |> CmdLine.appendRaw "run"
            // Without the release mode, Fable stack overflow when compiling the tests
            |> CmdLine.appendPrefix "-c" "Release"
            |> CmdLine.appendRaw "--"
            |> argsBuilder
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            argsBuilder,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )

    static member WatchFableAsync
        (
            args: CmdLine,
            ?workingDirectory,
            ?noEcho,
            ?echoPrefix
        )
        =
        let localFableDir = __SOURCE_DIRECTORY__ </> ".." </> "Fable.Cli"

        let args =
            CmdLine.concat
                [
                    CmdLine.empty
                    |> CmdLine.appendRaw "watch"
                    |> CmdLine.appendPrefix "--project" localFableDir
                    |> CmdLine.appendRaw "run"
                    // Without the release mode, Fable stack overflow when compiling the tests
                    |> CmdLine.appendPrefix "-c" "Release"
                    |> CmdLine.appendRaw "--"

                    args
                ]
            |> CmdLine.toString

        Command.RunAsync(
            "dotnet",
            args,
            ?workingDirectory = workingDirectory,
            ?noEcho = noEcho,
            ?echoPrefix = echoPrefix
        )
