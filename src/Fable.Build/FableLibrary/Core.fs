namespace Build.FableLibrary

open BlackFox.CommandLine
open Fake.IO
open System.IO
open Build.Utils
open Build.Utils
open System.Diagnostics
open SimpleExec
open Spectre.Console
open SpectreCoff

/// <summary>
/// Building fable-library is similar enough for all the targets
/// that we can use this class to standardise the process.
/// </summary>
type BuildFableLibrary
    (
        language: string,
        libraryDir: string,
        sourceDir: string,
        buildDir: string,
        outDir: string,
        ?fableLibArg: string
    )
    =

    // It seems like the different target have a different way of supporting
    // --fableLib argument.
    // For example,
    // If we provide `--fableLib < out dir path>`, then the Dart target
    // generates import './Option.dart' as option;
    // Bt if we provides `--fableLib .`, then the Dart target
    // generates import '../../src/fable-library-dart/Option.dart' as option;
    // Python seems to ignore completely the --fableLib argument.
    // Investigation are required to make all the targets behave the same way.
    // For now, I am providing a way to override the --fableLib argument in the
    // build system but it should be removed once the issue is fixed.
    let fableLibArg = defaultArg fableLibArg "."

    member val LibraryDir = libraryDir
    member val SourceDir = sourceDir
    member val BuildDir = buildDir
    member val OutDir = outDir

    // Allow language to be orverriden from "do constructor"
    // Useful for JavaScript target which is a specialisation of the TypeScript
    member val Language = language with get, set

    abstract member FableArgsBuilder: (CmdLine -> CmdLine)
    default _.FableArgsBuilder = id

    abstract member PostFableBuildStage: unit -> unit
    default _.PostFableBuildStage() = ()

    abstract member CopyStage: unit -> unit
    default _.CopyStage() = ()

    member this.Run(?skipIfExist: bool) =
        let skipIfExist = defaultArg skipIfExist false

        if skipIfExist && Directory.Exists outDir then
            Calm "Skipping Fable build stage" |> toConsole

        else

            Calm "Cleaning build directory" |> toConsole

            if Directory.Exists buildDir then
                Directory.Delete(buildDir, true)

            Calm "Building Fable.Library" |> toConsole

            let args =
                CmdLine.appendRaw sourceDir
                >> CmdLine.appendPrefix "--outDir" outDir
                >> CmdLine.appendPrefix "--fableLib" fableLibArg
                >> CmdLine.appendPrefix "--lang" language
                >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                >> CmdLine.appendPrefix "--define" "FABLE_LIBRARY"
                >> CmdLine.appendRaw "--noCache"
                // Target implementation can require additional arguments
                >> this.FableArgsBuilder

            Command.Fable(args)

            Calm "Copy stage" |> toConsole
            this.CopyStage()

            Calm "Post Fable build stage" |> toConsole
            this.PostFableBuildStage()
