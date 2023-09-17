module Fun.Build

open Fun.Build
open Fun.Build.Internal
open Fun.Build.BuiltinCmdsInternal
open BlackFox.CommandLine

type StageBuilder with

    /// Add a step to run command. This will not encrypt any sensitive information when print to console.
    [<CustomOperation("run")>]
    member inline _.run
        (
            [<InlineIfLambda>] build: BuildStage,
            command: CmdLine
        ) =
        BuildStage(fun ctx ->
            build
                .Invoke(ctx)
                .AddCommandStep(fun _ ->
                    async {
                        let command = command |> CmdLine.toString
                        return command
                    }
                )
        )
