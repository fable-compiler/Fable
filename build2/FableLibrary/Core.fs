namespace Build.FableLibrary

open Fun.Build
open BlackFox.CommandLine
open Fake.IO
open System.IO
open Build.Utils
open Build.Utils

type BuildFableLibrary
    (
        language : string,
        libraryDir : string,
        sourceDir : string,
        buildDir : string,
        outDir : string,
        ?fableLibArg : string
    ) =

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

    abstract member FableArgsBuilder : (CmdLine -> CmdLine)
    default _.FableArgsBuilder = id

    abstract member PostFableBuildStage : Internal.StageContext
    default _.PostFableBuildStage =
        stage "Post fable build" {
            echo "Nothing to do"
        }

    abstract member CopyStageRunner : Internal.StageContext -> unit
    default _.CopyStageRunner _ =
        ()

    member this.Pipeline =
        pipeline $"fable-library-{this.Language}" {
            this.Stage()

            runIfOnlySpecified
        }

    member this.Stage () : Internal.StageContext =

        stage $"Build fable-library-{this.Language}" {
            whenCmd {
                longName "--fast"
                description $"Skip building fable-library-{this.Language} if the build directory already exists"
                optional
            }

            run (fun ctx ->
                let isFast = ctx.TryGetCmdArg "--fast" |> ValueOption.isSome
                if isFast && Directory.Exists buildDir then
                    ctx.SoftCancelStage()
            )

            run (fun _ ->
                // Make sure to work with a clean build directory
                if Directory.Exists buildDir then
                    Directory.Delete(buildDir, true)
            )

            stage "Build library with Fable" {
                run (fun _ ->
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

                    // Compile F# files of Fable library to TypeScript
                    Cmd.fable args
                    |> CmdLine.toString
                )
            }

            stage "Copy" {
                run this.CopyStageRunner
            }

            this.PostFableBuildStage

        }