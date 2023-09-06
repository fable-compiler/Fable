namespace Build.Tests

open Build.FableLibrary
open Fun.Build
open System.IO
open BlackFox.CommandLine
open Build.Utils

[<AbstractClass>]
type TestsTarget
    (
        language : string,
        buildFableLibrary: BuildFableLibrary,
        sourceDir : string,
        buildDir : string
    ) =

    member val Language = language with get, set
    member val SourceDir = sourceDir with get, set
    member val BuildDir = buildDir with get, set

    abstract member TestsAgainstTargetStage : Internal.StageContext

    member this.Pipeline =
        pipeline $"test-{this.Language}" {
            buildFableLibrary.Stage()

            stage "Clean" {
                run (fun _ ->
                    if Directory.Exists this.BuildDir then
                        Directory.Delete(this.BuildDir, true)
                )
            }

            stage "Build" {
                run (fun _ ->
                    let args =
                        CmdLine.appendRaw this.SourceDir
                        >> CmdLine.appendPrefix "--outDir" this.BuildDir
                        >> CmdLine.appendPrefix "--lang" this.Language
                        >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                        >> CmdLine.appendRaw "--noCache"

                    Cmd.fable args
                    |> CmdLine.toString
                )
            }

            stage "Run tests against .NET" {
                workingDir this.SourceDir

                run "dotnet test -c Release"
            }

            this.TestsAgainstTargetStage

            runIfOnlySpecified
        }
