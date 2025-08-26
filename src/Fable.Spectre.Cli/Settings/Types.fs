namespace Fable.Spectre.Cli.Settings

open System
open System.ComponentModel
open Fable
open Fable.Compiler.Util
open Fable.Spectre.Cli.Settings.Spec
open Spectre.Console
open Spectre.Console.Cli
open Spectre.Console.Cli.Help

module internal Commands =
    module Deprecated =
        // Deprecated // TODO - remove next version
        [<Literal>]
        let verbose = "--verbose"

        [<Literal>]
        let silent = "--silent"

    module Hidden =
        // Hidden
        [<Literal>]
        let precompiledLib = "--precompiledLib <PATH>"

        [<Literal>]
        let replace = "--replace <KEYVALUE>"

        [<Literal>]
        let printAst = "--printAst"

        [<Literal>]
        let noReflection = "--noReflection"

        [<Literal>]
        let noParallelTypeCheck = "--noParallelTypeCheck"

        [<Literal>]
        let trimRootModule = "--trimRootModule"

        [<Literal>]
        let fableLib = "--fableLib <PATH>"
    // Standard
    [<Literal>]
    let language = "-l|--language <LANGUAGE>"

    [<Literal>]
    let version = "--version"

    [<Literal>]
    let cwd = "--cwd <PATH>"

    [<Literal>]
    let verbosity = "--verbosity <ADJ>"

    [<Literal>]
    let extension = "-e|--extension"

    [<Literal>]
    let yes = "--yes"

    [<Literal>]
    let output = "-o|--output <DIRECTORY>"

    [<Literal>]
    let definition = "--define <SYMBOL>"

    [<Literal>]
    let config = "-c|--configuration <MODE>"

    [<Literal>]
    let watch = "--watch"

    [<Literal>]
    let watchDelay = "--watchDelay <INT>"

    [<Literal>]
    let run = "--run <COMMAND>"

    [<Literal>]
    let runFast = "--runFast <COMMAND>"

    [<Literal>]
    let runWatch = "--runWatch <COMMAND>"

    [<Literal>]
    let noRestore = "--noRestore"

    [<Literal>]
    let noCache = "--noCache"

    [<Literal>]
    let excludePatterns = "--exclude <Pattern>"

    [<Literal>]
    let optimize = "--optimize"

    [<Literal>]
    let legacyCracker = "--legacyCracker"

    [<Literal>]
    let runScript = "--runScript <COMMAND>"

    [<Literal>]
    let typedArrays = "--typedArrays <BOOL>"

    [<Literal>]
    let sourceMaps = "-s|--sourceMaps"

    [<Literal>]
    let sourceMapsRoot = "--sourceMapsRoot <VALUE>"

[<AbstractClass>]
type FableSettingsBase() =
    inherit CommandSettings()
    // Fields that we map to from 'input' properties
    member val replace: Map<string, string> = Map([]) with get, set
    member val verbosity: Verbosity = Verbosity.Normal with get, set
    abstract language: Language with get, set

    [<Description """[dim]Choose which language to compile to.[/]

[dim]Available options:[/]
[dim] - javascript | js (default)    [/][grey](default ext .fs.js)[/]
[dim] - typescript | ts              [/][grey](default ext .fs.ts)[/]
[dim] - python | py                  [/][grey](default ext .py)[/]
[dim] - rust | rs                    [/][grey](default ext .rs)[/]
[dim] - php                          [/][grey](default ext .php)[/]
[dim] - dart                         [/][grey](default ext .dart)[/]""">]
    abstract langString: string with get, set

    [<Description "[dim]Print the version.[/]">]
    abstract version: bool with get, set

    [<Description "[dim]Automatically respond 'yes' to prompts.[/] [grey](default false)[/]">]
    abstract yes: bool with get, set

    [<Description("[dim]Directory for generated files.[/]")>]
    abstract outputString: string with get, set

    [<Description("[dim]Defines a symbol for use in conditional compilation[/]")>]
    abstract definitions: string[] with get, set

    [<Description("[dim]The configuration to use when parsing .fsproj with MsBuild.[/] [grey](default 'Release'; default 'Debug' in watch mode)[/]")>]
    abstract config: string with get, set

    [<Description("[dim]Alias of watch command[/]")>]
    abstract watch: bool with get, set

    [<Description("[dim]Delay in ms before recompiling after a file changes[/] [grey](default 200)[/]")>]
    abstract watchDelay: int with get, set

    [<Description("[dim]A string containing a command after the argument that will be executed after compilation[/]")>]
    abstract runCommand: string with get, set

    [<Description("[dim]The command string after the argument will be executed BEFORE compilation.[/]")>]
    abstract runCommandFast: string with get, set

    [<Description("[dim]The command after the argument will be executed AFTER each watch compilation.[/]")>]
    abstract runCommandWatch: string with get, set

    [<Description("[dim]Skip [italic]`dotnet restore`[/][/]")>]
    abstract noRestore: bool with get, set

    [<Description("[dim]Recompile all files, including sources from packages.[/]")>]
    abstract noCache: bool with get, set

    [<Description("[dim]Don't merge sources of referenced projects with specified pattern (Intended for plugin development).[/]")>]
    abstract excludePatterns: string[] with get, set

    [<Description("[dim]Compile with optimized F# AST (experimental)[/]")>]
    abstract optimize: bool with get, set

    [<Description("[dim]Use this if you have issues with the new MSBuild Cracker released in Fable 5.[/]")>]
    abstract legacyCracker: bool with get, set

    [<Description("[dim]Runs the generated script for last file with node.[/]\
[grey](Requires `\"type\": \"module\"` in package.json and at minimum Node.js 12.20, 14.14, or 16.0.0)[/]")>]
    abstract runScript: string with get, set

    [<Description("[dim]Compile numeric arrays as JS typed arrays [/][grey](default is true for JS, false for TS)[/]")>]
    abstract typedArrays: bool with get, set

    [<Description("[dim]Enable source maps.[/]")>]
    abstract sourceMaps: bool with get, set

    [<Description("[dim]Set the value of the `sourceRoot` property in generated source map files.[/]")>]
    abstract sourceMapsRoot: string with get, set

    // Default/common options
    [<CommandOption(Commands.Deprecated.verbose, IsHidden = true)>]
    member val verbose = false with get, set

    [<CommandOption(Commands.Deprecated.silent, IsHidden = true)>]
    member val silent = false with get, set

    [<Description "[dim]Extension for generated files.[/] [grey](default depends on language)[/]">]
    [<CommandOption(Commands.extension)>]
    member val extension = ".fs.js" with get, set

    [<Description "[dim]Set the working directory.[/]">]
    [<CommandOption(Commands.cwd)>]
    member val cwd = Environment.CurrentDirectory with get, set

    [<Description "[dim]Set the amount of information printed.[/] [grey][[v|verbose|d|debug]] or [[n|normal]] or [[s|silent]][/]">]
    [<CommandOption(Commands.verbosity)>]
    member val verbosityString = "normal" with get, set

    [<Description "[dim]Path to project[/] [grey](default cwd)[/]">]
    [<CommandArgument(0, "[PATH]")>]
    member val projPath = Environment.CurrentDirectory with get, set

    // hidden options
    [<CommandOption(Commands.Hidden.replace, IsHidden = true)>]
    member val replaceStrings = [||] with get, set

    [<CommandOption(Commands.Hidden.noParallelTypeCheck, IsHidden = true)>]
    member val noParallelTypeCheck = false with get, set

    [<CommandOption(Commands.Hidden.precompiledLib, IsHidden = true)>]
    member val precompiledLib = "" with get, set

    [<CommandOption(Commands.Hidden.printAst, IsHidden = true)>]
    member val printAst = false with get, set

    [<CommandOption(Commands.Hidden.trimRootModule, IsHidden = true)>]
    member val trimRootModule = false with get, set

    [<CommandOption(Commands.Hidden.fableLib, IsHidden = true)>]
    member val fableLib = "" with get, set

    [<CommandOption(Commands.Hidden.noReflection, IsHidden = true)>]
    member val noReflection = false with get, set

    interface ICommonArgs with
        member this.extension =
            let prependDotIfRequired: string -> string =
                function
                | input when input.StartsWith('.') -> input
                | input -> "." + input

            match this.language with
            | JavaScript -> this.extension |> prependDotIfRequired
            | _ when this.extension <> ".fs.js" -> this.extension |> prependDotIfRequired
            | TypeScript -> ".fs.ts"
            | Python -> ".py"
            | Php -> ".php"
            | Dart -> ".dart"
            | Rust -> ".rs"

        member this.projPath = this.projPath
        member this.language = this.language
        member this.silent = this.silent
        member this.uncaughtArgs = []
        member this.verbosity = this.verbosity
        member this.version = this.version
        member this.workingDirectory = this.cwd
        member this.yes = this.yes

    interface ICompilingArgs with
        member this.configuration = this.config
        member this.exclude = this.excludePatterns

        member this.fableLib =
            if this.fableLib |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.fableLib

        member this.legacyCracker = this.legacyCracker
        member this.noCache = this.noCache
        member this.noParallelTypeCheck = this.noParallelTypeCheck
        member this.noReflection = this.noReflection
        member this.noRestore = this.noRestore
        member this.optimize = this.optimize

        member this.precompiledLib =
            if this.precompiledLib |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.precompiledLib

        member this.printAst = this.printAst
        member this.replace = this.replace

        member this.run =
            if this.runCommand |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.runCommand

        member this.runFast =
            if this.runCommandFast |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.runCommandFast

        member this.runWatch =
            if this.runCommandWatch |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.runCommandWatch

        member this.definitions =
            [
                "FABLE_COMPILER"
                "FABLE_COMPILER_5"
                this.language.ToString().ToUpper() |> sprintf "FABLE_COMPILER_%s"
                yield! this.definitions
            ]
            |> List.distinct

        member this.trimRootModule = this.trimRootModule
        member this.watch = this.watch
        member this.watchDelay = this.watchDelay

        member this.outputDirectory =
            if this.outputString |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.outputString

    interface IJavaScriptArgs with
        member this.runScript =
            if this.runScript |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.runScript

        member this.sourceMap = this.sourceMaps

        member this.sourceMapRoot =
            if this.sourceMapsRoot |> String.IsNullOrWhiteSpace then
                None
            else
                Some this.sourceMapsRoot

        member this.typedArrays = this.typedArrays

    interface ITypeScriptArgs
    interface IDartArgs
    interface IRustArgs
    interface IPhpArgs
    interface IPythonArgs
    interface ICliArgs

    member this.NormalizeAbsolutePath(path: string) =
        (if IO.Path.IsPathRooted(path) then
             path
         else
             IO.Path.Combine(this.cwd, path))
        // Use getExactFullPath to remove things like: myrepo/./build/
        // and get proper casing (see `getExactFullPath` comment)
        |> File.getExactFullPath
        |> Path.normalizePath

    // Prefix validation functions with 'validateAndSet' if they
    // mutate a property on validation.
    override this.Validate() =
        let validatePrecompiledLibCompatibility acc =
            match this.fableLib, this.precompiledLib with
            | "", _
            | _, "" -> acc
            | _, _ -> "[--precompiledLib] & [--fableLib] are incompatible." :: acc

        let validateAndSetCwd acc = // impure
            if System.IO.Path.Exists(this.cwd) then
                this.cwd <- this.NormalizeAbsolutePath this.cwd
                acc
            else
                $"[--cwd] Working directory does not exist: {this.cwd}" :: acc

        let validateAndSetProjPath acc = // impure
            if System.IO.Path.Exists(this.projPath) then
                this.projPath <- this.NormalizeAbsolutePath this.projPath
                acc
            else
                $"[PATH] Path does not exists: " + this.projPath :: acc

        let validateAndSetVerbosity acc = // impure
            match this.verbosityString.ToLower() with
            | "normal" when this.verbose ->
                this.verbosity <- Verbosity.Verbose
                acc
            | "normal" when this.silent ->
                this.verbosity <- Verbosity.Silent
                acc
            | "n"
            | "normal" ->
                this.verbosity <- Verbosity.Normal
                acc
            | "d"
            | "debug"
            | "v"
            | "verbose" ->
                this.verbosity <- Verbosity.Verbose
                acc
            | "s"
            | "silent" ->
                this.verbosity <- Verbosity.Silent
                acc
            | value ->
                $"[--verbosity] Verbosity can be one of [(verbose|debug)|silent|normal], but got '{value}'"
                :: acc

        let validateAndSetReplace acc = // impure
            this.replaceStrings
            |> Array.filter (String.exists ((=) ':') >> not)
            |> function
                | [||] ->
                    this.replace <-
                        this.replaceStrings
                        |> Array.map (_.Split(':') >> fun arr -> arr[0], arr[1])
                        |> Map

                    acc
                | arr ->
                    arr
                    |> Array.map (sprintf "[--replace] Expected '<KEY>:<VALUE>' pair but got: %s")
                    |> Array.toList
                    |> (@) acc

        let validateConfiguration acc =
            match this.config with
            | "Debug" // TODO should this ignore case?
            | "Release" -> acc
            | value -> $"[--configuration] Expected one of [ Debug | Release ] but got: {value}" :: acc

        let validateAndSetLanguage (input: string) acc = // impure
            match input.ToLower() with
            | "js"
            | "javascript" ->
                this.language <- JavaScript
                acc
            | "py"
            | "python" ->
                this.language <- Python
                acc
            | "rs"
            | "rust" ->
                this.language <- Rust
                acc
            | "ts"
            | "typescript" ->
                this.language <- TypeScript
                acc
            | "php" ->
                this.language <- Php
                acc
            | "dart" ->
                this.language <- Dart
                acc
            | value ->
                $"[-lang|--language] Unknown language target: '{value}'

    Available Options:
        - js | javascript
        - ts | typescript
        - py | python
        - rs | rust
        - php
        - dart"
                :: acc

        let validateOutputDirectoryCompatibility acc =
            Path.GetDirectoryName this.outputString
            |> function
                | "obj"
                | Naming.fableModules as dir ->
                    $"[-o|--output] {dir} is a reserved directory, please use another directory."
                    :: acc
                | _ -> acc

        let validation = // impure
            []
            |> validatePrecompiledLibCompatibility
            |> validateOutputDirectoryCompatibility
            |> validateConfiguration
            |> validateAndSetCwd
            |> validateAndSetVerbosity
            |> validateAndSetReplace
            |> validateAndSetProjPath
            |> validateAndSetLanguage this.langString

        if validation.IsEmpty then
            ValidationResult.Success()
        else
            ValidationResult.Error(validation |> String.concat "\n")
