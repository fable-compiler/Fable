module Fable.Spectre.Cli.Settings.Clean

open System.ComponentModel
open Fable
open Spectre.Console.Cli

type CleanSettings() =
    inherit FableSettingsBase()

    [<Description("[dim]Extension of generated files[/] [grey](default depends on language) (lower precedence than --extension flag)[/]")>]
    [<CommandArgument(1, "[EXTENSION]")>]
    member this.extensionArg
        with get () = this.extension
        and set value = this.extension <- value

    override val language = JavaScript with get, set

    [<CommandOption(Commands.legacyCracker)>]
    override val legacyCracker = false with get, set

    [<CommandOption(Commands.version)>]
    override val version = false with get, set

    [<CommandOption(Commands.language)>]
    override val langString = "javascript" with get, set

    [<CommandOption(Commands.excludePatterns)>]
    override val excludePatterns = [||] with get, set

    [<CommandOption(Commands.yes)>]
    override val yes = false with get, set
    // [<CommandOption(Commands.runFast)>]
    override val runCommandFast = "" with get, set
    // [<CommandOption(Commands.config)>]
    override val config = "Release" with get, set
    // [<CommandOption(Commands.definition)>]
    override val definitions = [||] with get, set
    // [<CommandOption(Commands.sourceMapsRoot)>]
    override val sourceMapsRoot = "" with get, set
    // [<CommandOption(Commands.sourceMaps)>]
    override val sourceMaps = false with get, set
    // [<CommandOption(Commands.watch)>]
    override val watch = false with get, set
    // [<CommandOption(Commands.typedArrays)>]
    override val typedArrays = true with get, set
    // [<CommandOption(Commands.optimize)>]
    override val optimize = false with get, set
    // [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set
    // [<CommandOption(Commands.runScript)>]
    override val runScript = "" with get, set
    // [<CommandOption(Commands.run)>]
    override val runCommand = "" with get, set
    // [<CommandOption(Commands.noRestore)>]
    override val noRestore = false with get, set
    // [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set
    // [<CommandOption(Commands.output)>]
    override val outputString = "" with get, set
    // [<CommandOption(Commands.noCache)>]
    override val noCache = false with get, set
