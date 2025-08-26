module Fable.Spectre.Cli.Commands.Compile

open System
open Fable
open Fable.Cli
open Fable.Cli.Main
open Fable.Spectre.Cli.Settings
open Fable.Spectre.Cli.Settings.Compile
open Fable.Spectre.Cli.Settings.Root
open Fable.Spectre.Cli.Settings.Spec
open Fable.Spectre.Cli.SpectreOutput
open Microsoft.Extensions.Logging
open Spectre.Console
open Spectre.Console.Cli
open SpectreCoff
open Fable.Compiler.Util
open CustomLogging

let private logPrelude (settings: ICliArgs) =
    if not settings.version then
        Many
            [
                MarkupCD(Color.DodgerBlue1, [ Decoration.Bold ], "Fable")
                E $"{Literals.VERSION}:"
                Dim $"F# to {settings.language} compiler"
                match getStatus settings.language with
                | "stable"
                | "" -> ()
                | status ->
                    MarkupD([ Decoration.Dim ], $"(status: {markupString None [ Decoration.Italic ] status})")
                    |> padLeft 2
                BL
                V "Thanks to the contributor!"
                E $"@{Contributors.getRandom ()}"
                BL
                E "Stand with"
                MarkupCD(Color.Aqua, [ Decoration.Bold ], "Ukraine!")
                Link "https://standwithukraine.com.ua/"
            ]
        |> customPanel HeaderPanel ""
        |> toConsoleInline

        match getLibPkgVersion settings.language with
        | Some(repository, pkgName, version) ->
            MarkupD([ Decoration.Dim ], $"Minimum {pkgName} version (when installed from {repository}): {version}")
            |> toConsole
        | None -> ()

let executeCompileCommon (args: ICliArgs) =
    let createLogger level =
        use factory =
            LoggerFactory.Create(fun builder ->
                builder.SetMinimumLevel(level).AddCustomConsole(fun options -> options.UseNoPrefixMsgStyle <- true)
                |> ignore
            )

        factory.CreateLogger("")

    Compiler.SetLanguageUnsafe args.language

    match args.verbosity with
    | Verbosity.Normal -> LogLevel.Information
    | Verbosity.Verbose -> LogLevel.Debug
    | Verbosity.Silent -> LogLevel.Warning
    |> createLogger
    |> Log.setLogger args.verbosity

    CompilerOptionsHelper.Make(
        language = args.language,
        typedArrays = args.typedArrays,
        fileExtension = args.extension,
        define = args.definitions,
        debugMode = (args.configuration = "Debug"),
        optimizeFSharpAst = args.optimize,
        noReflection = args.noReflection,
        verbosity = args.verbosity
    )

let private createCliArgs
    (targetPath: string)
    (isPrecompile: bool)
    (compileOptionsHelper: CompilerOptions)
    (cliArgs: ICliArgs)
    =
    {
        ProjectFile = targetPath
        RootDir = cliArgs.workingDirectory
        OutDir = cliArgs.outputDirectory
        IsWatch = cliArgs.watch
        Precompile = isPrecompile
        PrecompiledLib = cliArgs.precompiledLib
        PrintAst = cliArgs.printAst
        FableLibraryPath = cliArgs.fableLib
        Configuration = cliArgs.configuration
        NoRestore = cliArgs.noRestore
        NoCache = cliArgs.noCache
        NoParallelTypeCheck = cliArgs.noParallelTypeCheck
        SourceMaps = cliArgs.sourceMap
        SourceMapsRoot = cliArgs.sourceMapRoot
        Exclude = cliArgs.exclude |> Array.toList
        Replace = cliArgs.replace
        RunProcess = None // TODO
        CompilerOptions = compileOptionsHelper
        Verbosity = cliArgs.verbosity
    }

let validateTargetFile (args: ICommonArgs) =
    let files =
        args.workingDirectory |> IO.Directory.EnumerateFileSystemEntries |> Seq.toList

    files
    |> List.filter _.EndsWith(".fsproj", StringComparison.Ordinal)
    |> function
        | [] -> files |> List.filter _.EndsWith(".fsx", StringComparison.Ordinal)
        | candidates -> candidates
    |> function
        | [] -> "Cannot find .fsproj/.fsx in dir: " + args.workingDirectory |> Error
        | [ fsproj ] -> Ok fsproj
        | _ -> "Found multiple .fsproj/.fsx in dir: " + args.workingDirectory |> Error

let runCompilation isPrecompiled targetFile cliArgs =
    let compilerOptions = cliArgs |> executeCompileCommon
    let cliArgs' = createCliArgs targetFile isPrecompiled compilerOptions cliArgs

    let startCompilation () =
        State.Create(cliArgs', watchDelay = cliArgs.watchDelay, useMSBuildForCracking = cliArgs.legacyCracker)
        |> startCompilationAsync
        |> Async.RunSynchronously

    match cliArgs.outputDirectory with
    | Some dir when isPrecompiled -> File.withLock dir startCompilation
    | _ -> startCompilation ()
    |> Result.mapEither ignore fst
    |> function
        | Ok _ -> 0
        | Error msg ->
            Log.error msg
            1

[<AbstractClass>]
type CompilingCommand<'T when 'T :> FableSettingsBase>() =
    inherit Command<'T>()
    member val targetFile: string = "" with get, set
    // IMPURE
    member private this.ValidateAndSetTargetFile(settings: 'T) =
        settings
        |> validateTargetFile
        |> Result.map (fun targetFile -> this.targetFile <- targetFile)

    override this.Validate(context, settings) =
        if not settings.version then
            match this.ValidateAndSetTargetFile(settings) with
            | Error errorMsg -> ValidationResult.Error(errorMsg)
            | _ -> base.Validate(context, settings)
        else
            base.Validate(context, settings)

type FableCommand() =
    inherit CompilingCommand<FableSettings>()

    override this.Execute(context, settings) =
        settings :> ICliArgs |> logPrelude

        if settings.version then
            0
        else
            runCompilation false this.targetFile settings

type WatchCommand() =
    inherit CompilingCommand<WatchSettings>()

    override this.Execute(_context, settings) =
        settings :> ICliArgs |> logPrelude
        runCompilation false this.targetFile settings

type CompileCommandBase<'T when 'T :> FableSettingsBase>() =
    inherit CompilingCommand<'T>()

    override this.Execute(_context, settings) =
        settings :> ICliArgs |> logPrelude
        runCompilation false this.targetFile settings

type CompileWatchCommandBase<'T, 'Base when 'T :> FableSettingsBase and 'Base :> FableSettingsBase>() =
    inherit CompilingCommand<'T>()
    interface ICommandLimiter<'Base>

    override this.Execute(_context, settings) =
        settings :> ICliArgs |> logPrelude
        runCompilation false this.targetFile settings

type FablePrecompileCommand() =
    inherit CompilingCommand<FablePrecompileSettings>()

    override this.Execute(_context, settings) =
        settings :> ICliArgs |> logPrelude
        runCompilation true this.targetFile settings

type JavaScriptCommand = CompileCommandBase<JavaScriptSettings>
type TypeScriptCommand = CompileCommandBase<TypeScriptSettings>
type PythonCommand = CompileCommandBase<PythonSettings>
type RustCommand = CompileCommandBase<RustSettings>
type DartCommand = CompileCommandBase<DartSettings>
type PhpCommand = CompileCommandBase<PhpSettings>
type JavaScriptWatchCommand = CompileWatchCommandBase<JavaScriptWatchSettings, JavaScriptSettings>
type TypeScriptWatchCommand = CompileWatchCommandBase<TypeScriptWatchSettings, TypeScriptSettings>
type PythonWatchCommand = CompileWatchCommandBase<PythonWatchSettings, PythonSettings>
type RustWatchCommand = CompileWatchCommandBase<RustWatchSettings, RustSettings>
type DartWatchCommand = CompileWatchCommandBase<DartWatchSettings, DartSettings>
type PhpWatchCommand = CompileWatchCommandBase<PhpWatchSettings, PhpSettings>
