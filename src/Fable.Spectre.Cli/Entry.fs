open Fable
open Fable.Spectre.Cli.Commands.Clean
open Fable.Spectre.Cli.Commands.Compile
open Fable.Spectre.Cli.Settings.Compile
open Fable.Spectre.Cli.Settings.Spec.Output
open Spectre.Console
open Spectre.Console.Cli

[<EntryPoint>]
let main argv =
    let app = CommandApp()

    app.Configure(fun config ->
        config.Settings.ShowOptionDefaultValues <- false
        config.Settings.HelpProviderStyles.Options.RequiredOption <- Style(foreground = Color.Blue)
        // NOTE: The order the branches are added here determines the order
        //       in the help printout. We order this by popularity.
        config
            .AddBranch(
                "javascript",
                (fun (branchConfig: IConfigurator<JavaScriptSettings>) ->
                    branchConfig.SetDefaultCommand<JavaScriptCommand>()

                    branchConfig.SetDescription(
                        "[dim]Fable for javascript[/] [grey][underline]DEFAULT[/] (alias js)[/]"
                    )

                    branchConfig
                        .AddCommand<JavaScriptWatchCommand>("watch")
                        .WithAlias("w")
                        .WithDescription(dim "Fable for javascript in watch mode")
                    |> ignore
                )
            )
            .WithAlias("js")
        |> ignore

        config
            .AddBranch(
                "typescript",
                (fun (branchConfig: IConfigurator<TypeScriptSettings>) ->
                    branchConfig.SetDefaultCommand<TypeScriptCommand>()
                    branchConfig.SetDescription("[dim]Fable for typescript[/] [grey](alias ts)[/]")

                    branchConfig
                        .AddCommand<TypeScriptWatchCommand>("watch")
                        .WithAlias("w")
                        .WithDescription(dim "Fable for typescript in watch mode")
                    |> ignore
                )
            )
            .WithAlias("ts")
        |> ignore

        config
            .AddBranch(
                "python",
                (fun (branchConfig: IConfigurator<PythonSettings>) ->
                    branchConfig.SetDefaultCommand<PythonCommand>()
                    branchConfig.SetDescription("[dim]Fable for python[/] [grey](alias py)[/]")

                    branchConfig
                        .AddCommand<PythonWatchCommand>("watch")
                        .WithAlias("w")
                        .WithDescription(dim "Fable for python in watch mode")
                    |> ignore
                )
            )
            .WithAlias("py")
        |> ignore

        config
            .AddBranch(
                "rust",
                (fun (branchConfig: IConfigurator<RustSettings>) ->
                    branchConfig.SetDefaultCommand<RustCommand>()
                    branchConfig.SetDescription("[dim]Fable for rust[/] [grey](alias rs)[/]")

                    branchConfig
                        .AddCommand<RustWatchCommand>("watch")
                        .WithAlias("w")
                        .WithDescription(dim "Fable for rust in watch mode")
                    |> ignore
                )
            )
            .WithAlias("rs")
        |> ignore

        config.AddBranch(
            "php",
            (fun (branchConfig: IConfigurator<PhpSettings>) ->
                branchConfig.SetDefaultCommand<PhpCommand>()
                branchConfig.SetDescription(dim "Fable for php.")

                branchConfig
                    .AddCommand<PhpWatchCommand>("watch")
                    .WithAlias("w")
                    .WithDescription(dim "Fable for php in watch mode")
                |> ignore
            )
        )
        |> ignore

        config.AddBranch(
            "dart",
            (fun (branchConfig: IConfigurator<DartSettings>) ->
                branchConfig.SetDefaultCommand<DartCommand>()
                branchConfig.SetDescription(dim "Fable for dart.")

                branchConfig
                    .AddCommand<DartWatchCommand>("watch")
                    .WithAlias("w")
                    .WithDescription(dim "Fable for dart in watch mode")
                |> ignore
            )
        )
        |> ignore

        config
            .AddCommand<CleanCommand>("clean")
            .WithDescription(dim "Remove fable_modules folders and files with specified extension (default is .fs.js)")
        |> ignore

        config
            .AddCommand<WatchCommand>("watch")
            .WithAlias("w")
            .WithDescription("[dim]Fable in watch mode[/] [grey](alias 'w')[/]")
        |> ignore

        config
            .AddCommand<FablePrecompileCommand>("precompile")
            // TODO - provide documentation
            // .WithDescription(dim "")
            .IsHidden()
        |> ignore

        config.UseStrictParsing() |> ignore
    )

    app
        .SetDefaultCommand<FableCommand>()
        .WithDescription(
            """[bold]Fable compiler[/].

All flags and options are available, with defaults set for [italic]JavaScript[/].
Language specific commands will set defaults for file extensions, and/or restrict/add the available flags for that language."""
        )
    |> ignore

    app.Run(argv)
