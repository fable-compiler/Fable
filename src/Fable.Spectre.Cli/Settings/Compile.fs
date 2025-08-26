module Fable.Spectre.Cli.Settings.Compile

open Fable
open Spectre.Console.Cli

[<AbstractClass>]
type CompileSettingsBase(language: Language) =
    inherit FableSettingsBase()
    override val language = language with get, set
    override val langString = language.ToString() with get, set

    [<CommandOption(Commands.runFast)>]
    override val runCommandFast = "" with get, set

    [<CommandOption(Commands.config)>]
    override val config = "Release" with get, set

    [<CommandOption(Commands.definition)>]
    override val definitions = [||] with get, set

    [<CommandOption(Commands.legacyCracker)>]
    override val legacyCracker = false with get, set

    [<CommandOption(Commands.optimize)>]
    override val optimize = false with get, set

    [<CommandOption(Commands.run)>]
    override val runCommand = "" with get, set

    [<CommandOption(Commands.version)>]
    override val version = false with get, set

    [<CommandOption(Commands.noRestore)>]
    override val noRestore = false with get, set

    [<CommandOption(Commands.output)>]
    override val outputString = "" with get, set

    [<CommandOption(Commands.excludePatterns)>]
    override val excludePatterns = [||] with get, set

    [<CommandOption(Commands.noCache)>]
    override val noCache = false with get, set
// [<CommandOption(Commands.yes)>]
// override val yes = false with get,set
// [<CommandOption(Commands.sourceMapsRoot)>]
// override val sourceMapsRoot = "" with get,set
// [<CommandOption(Commands.sourceMaps)>]
// override val sourceMaps = false with get,set
// [<CommandOption(Commands.typedArrays)>]
// override val typedArrays = true with get,set
// [<CommandOption(Commands.runScript)>]
// override val runScript = "" with get,set


type WatchSettings() =
    inherit FableSettingsBase()
    override val language = JavaScript with get, set
    override val watch = true with get, set
    override val yes = true with get, set

    [<CommandOption(Commands.language)>]
    override val langString = "javascript" with get, set

    [<CommandOption(Commands.runFast)>]
    override val runCommandFast = "" with get, set

    [<CommandOption(Commands.config)>]
    override val config = "Debug" with get, set

    [<CommandOption(Commands.definition)>]
    override val definitions = [||] with get, set

    [<CommandOption(Commands.legacyCracker)>]
    override val legacyCracker = false with get, set

    [<CommandOption(Commands.optimize)>]
    override val optimize = false with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

    [<CommandOption(Commands.run)>]
    override val runCommand = "" with get, set

    [<CommandOption(Commands.version)>]
    override val version = false with get, set

    [<CommandOption(Commands.noRestore)>]
    override val noRestore = false with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.output)>]
    override val outputString = "" with get, set

    [<CommandOption(Commands.excludePatterns)>]
    override val excludePatterns = [||] with get, set

    [<CommandOption(Commands.noCache)>]
    override val noCache = false with get, set

    [<CommandOption(Commands.sourceMapsRoot)>]
    override val sourceMapsRoot = "" with get, set

    [<CommandOption(Commands.sourceMaps)>]
    override val sourceMaps = false with get, set

    [<CommandOption(Commands.typedArrays)>]
    override val typedArrays = true with get, set

    [<CommandOption(Commands.runScript)>]
    override val runScript = "" with get, set

type JavaScriptSettings() =
    inherit CompileSettingsBase(JavaScript)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set
    override val runCommandWatch = "" with get, set

    [<CommandOption(Commands.yes)>]
    override val yes = false with get, set

    [<CommandOption(Commands.sourceMapsRoot)>]
    override val sourceMapsRoot = "" with get, set

    [<CommandOption(Commands.sourceMaps)>]
    override val sourceMaps = false with get, set

    [<CommandOption(Commands.typedArrays)>]
    override val typedArrays = true with get, set

    [<CommandOption(Commands.runScript)>]
    override val runScript = "" with get, set

type JavaScriptWatchSettings() =
    inherit JavaScriptSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

type TypeScriptSettings() =
    inherit CompileSettingsBase(TypeScript)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.yes)>]
    override val yes = false with get, set

    [<CommandOption(Commands.sourceMapsRoot)>]
    override val sourceMapsRoot = "" with get, set

    [<CommandOption(Commands.sourceMaps)>]
    override val sourceMaps = false with get, set
    // false in typescript
    [<CommandOption(Commands.typedArrays)>]
    override val typedArrays = false with get, set

    [<CommandOption(Commands.runScript)>]
    override val runScript = "" with get, set

    override val runCommandWatch = "" with get, set

type TypeScriptWatchSettings() =
    inherit TypeScriptSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

type PythonSettings() =
    inherit CompileSettingsBase(Python)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set
    override val yes = false with get, set
    override val sourceMapsRoot = "" with get, set
    override val sourceMaps = false with get, set
    override val typedArrays = false with get, set
    override val runScript = "" with get, set
    override val runCommandWatch = "" with get, set

type PythonWatchSettings() =
    inherit PythonSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

type DartSettings() =
    inherit CompileSettingsBase(Dart)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set
    override val yes = false with get, set
    override val sourceMapsRoot = "" with get, set
    override val sourceMaps = false with get, set
    override val typedArrays = false with get, set
    override val runScript = "" with get, set
    override val runCommandWatch = "" with get, set

type DartWatchSettings() =
    inherit DartSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

type RustSettings() =
    inherit CompileSettingsBase(Rust)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set
    override val yes = false with get, set
    override val sourceMapsRoot = "" with get, set
    override val sourceMaps = false with get, set
    override val typedArrays = false with get, set
    override val runScript = "" with get, set
    override val runCommandWatch = "" with get, set

type RustWatchSettings() =
    inherit RustSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set

type PhpSettings() =
    inherit CompileSettingsBase(Php)
    override val watch = false with get, set
    override val watchDelay = 200 with get, set
    override val yes = false with get, set
    override val sourceMapsRoot = "" with get, set
    override val sourceMaps = false with get, set
    override val typedArrays = false with get, set
    override val runScript = "" with get, set
    override val runCommandWatch = "" with get, set

type PhpWatchSettings() =
    inherit PhpSettings()
    override val watch = true with get, set

    [<CommandOption(Commands.watchDelay)>]
    override val watchDelay = 200 with get, set

    [<CommandOption(Commands.runWatch)>]
    override val runCommandWatch = "" with get, set
