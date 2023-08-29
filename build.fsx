#r "nuget: Fun.Build, 0.4.2"
#r "nuget: Fake.IO.FileSystem, 5.23.1"
#r "nuget: Fake.Core.Environment, 5.23.1"
#r "nuget: Fake.Tools.Git, 5.23.1"
#r "nuget: Fake.Api.GitHub, 5.23.1"
#r "nuget: BlackFox.CommandLine, 1.0.0"
#r "nuget: FsToolkit.ErrorHandling, 4.9.0"

open Fun.Build
open System.IO
open Fake.IO
open BlackFox.CommandLine
open FsToolkit.ErrorHandling

let (</>) (p1: string) (p2: string): string =
    Path.Combine(p1, p2)

type Cmd =

    static member fable (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id
        // Use absolute path so we can invoke the command from anywhere
        let localFableDir = __SOURCE_DIRECTORY__ </> "src" </> "Fable.Cli"

        CmdLine.empty
        |> CmdLine.appendRaw "dotnet"
        |> CmdLine.appendRaw "run"
        |> CmdLine.appendPrefix "-c" "Release"
        |> CmdLine.appendPrefix "--project" localFableDir
        |> CmdLine.appendRaw "--"
        |> argsBuilder


    static member node (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "node"
        |> argsBuilder

    static member tsc (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "tsc"
        |> argsBuilder

    static member mocha (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "npx"
        |> CmdLine.appendRaw "mocha"
        |> argsBuilder

    static member dotnet (?argsBuilder : CmdLine -> CmdLine) : CmdLine =
        let argsBuilder = defaultArg argsBuilder id

        CmdLine.empty
        |> CmdLine.appendRaw "dotnet"
        |> argsBuilder

module Stages =

    module JavaScript =

        let testWithMocha projectName =
            let outDir = Path.Combine("build", "tests", "Js", projectName)
            let projectDir = Path.Combine("tests", "Js", projectName)

            stage $"Run test using mocha for project: {projectDir}" {

                run (fun _ ->
                    Shell.cleanDir outDir
                )

                run (fun _ ->
                    let args =
                        CmdLine.appendRaw projectDir
                        >> CmdLine.appendPrefix "--outDir" outDir
                        >> CmdLine.appendPrefix "--exclude" "Fable.Core"

                    Cmd.fable args
                    |> CmdLine.toString
                )

                run (fun _ ->
                    let args =
                        CmdLine.appendRaw outDir
                        >> CmdLine.appendPrefix "--reporter" "dot"
                        >> CmdLine.appendPrefix "-t" "10000"

                    Cmd.mocha args
                    |> CmdLine.toString
                )
            }

        let testReact =
            stage "Test react" {
                workingDir "tests/React"

                run (fun _ ->
                    Cmd.fable (
                        CmdLine.appendRaw "--noCache"
                    )
                    |> CmdLine.toString
                )

                run "npm install"
                run "npm run test"
            }


    let testStandalone =
        stage "Standalone" {
            whenEnvVar "CI"

            // Compile Fable standalone to JavaScript
            run (fun _ ->
                let args =
                    CmdLine.appendRaw "src/fable-standalone/src/"
                    >> CmdLine.appendRaw "--noCache"

                Cmd.fable args
                |> CmdLine.toString
            )

            // Compile Fable compiler to JavaScript
            run (fun _ ->
                let args =
                    CmdLine.appendRaw "src/fable-compiler-js/src"
                    >> CmdLine.appendPrefix "--exclude" "Fable.Core"
                    >> CmdLine.appendPrefix "--define" "LOCAL_TEST"

                Cmd.fable args
                |> CmdLine.toString
            )

            // Compile test project using JavaScript fable compiler
            run (fun _ ->
                let fableJs = "./src/fable-compiler-js/src/app.fs.js"
                let testProj = "tests/Js/Main/Fable.Tests.fsproj"
                let buildDir = "build/tests/Standalone"

                let args =
                    CmdLine.appendRaw fableJs
                    >> CmdLine.appendRaw testProj
                    >> CmdLine.appendRaw buildDir

                Cmd.node args
                |> CmdLine.toString
            )

            run (fun _ ->
                let args =
                    CmdLine.appendRaw "build/tests/Standalone"
                    >> CmdLine.appendPrefix "--reporter" "dot"
                    >> CmdLine.appendPrefix "-t" "10000"

                Cmd.fable args
                |> CmdLine.toString
            )
        }

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

    member this.Pipeline () =
        pipeline $"fable-library-{this.Language}" {
            this.Stage()

            runIfOnlySpecified
        }

    member this.Stage () : Internal.StageContext =

        stage $"Build fable-library-{this.Language}" {
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

type BuildFableLibraryRust() =
    inherit BuildFableLibrary(
        "rust",
        Path.Combine("src", "fable-library-rust"),
        Path.Combine("src", "fable-library-rust", "src"),
        Path.Combine("build", "fable-library-rust"),
        Path.Combine("build", "fable-library-rust", "src")
    )

    override this.PostFableBuildStage =
        stage "Post fable build" {
            workingDir this.BuildDir
            run "cargo fmt"
            run "cargo fix --allow-no-vcs"
            run "cargo build"
        }

    override this.CopyStageRunner _ =
        // Copy all *.rs files to the build directory
        Directory.GetFiles(this.SourceDir, "*.rs")
        |> Shell.copyFiles this.OutDir

        Shell.copyFile
            this.BuildDir
            (Path.Combine(this.LibraryDir, "Cargo.toml"))

        Shell.copyDir
            (Path.Combine(this.BuildDir, "vendored"))
            (Path.Combine(this.LibraryDir, "vendored"))
            FileFilter.allFiles

type BuildFableLibraryPython() =
    inherit BuildFableLibrary(
        "python",
        Path.Combine("src", "fable-library-py"),
        Path.Combine("src", "fable-library-py", "fable_library"),
        Path.Combine("build", "fable-library-py"),
        Path.Combine("build", "fable-library-py", "fable_library")
    )

    override this.CopyStageRunner _ =
        // Copy all *.rs files to the build directory
        Directory.GetFiles(this.LibraryDir, "*")
        |> Shell.copyFiles this.BuildDir

        Directory.GetFiles(this.SourceDir, "*.py")
        |> Shell.copyFiles this.OutDir

    override this.PostFableBuildStage =
        stage "Post fable build" {
            run (fun _ ->
                // Fix issues with Fable .fsproj not supporting links
                let linkedFileFolder = Path.Combine(this.BuildDir, "fable_library", "fable-library")
                Directory.GetFiles(linkedFileFolder, "*")
                |> Shell.copyFiles this.OutDir

                Shell.deleteDir (this.BuildDir </> "fable_library/fable-library")
            )
        }

type BuildFableLibraryDart() =
    inherit BuildFableLibrary(
        "dart",
        Path.Combine("src", "fable-library-dart"),
        Path.Combine("src", "fable-library-dart"),
        Path.Combine("build", "fable-library-dart"),
        Path.Combine("build", "fable-library-dart"),
        Path.Combine("." , "build", "fable-library-dart")
    )

    override this.CopyStageRunner _ =
        Directory.GetFiles(this.SourceDir, "pubspec.*")
        |> Shell.copyFiles this.BuildDir

        Shell.copyFile
            this.BuildDir
            (Path.Combine(this.LibraryDir, "analysis_options.yaml"))

        Directory.GetFiles(this.SourceDir, "*.dart")
        |> Shell.copyFiles this.OutDir

type BuildFableLibraryTypeScript() =
    inherit BuildFableLibrary(
        "typescript",
        Path.Combine("src", "fable-library"),
        Path.Combine("src", "fable-library"),
        Path.Combine("build", "fable-library-ts"),
        Path.Combine("build", "fable-library-ts"),
        Path.Combine(".", "build", "fable-library-ts")
    )

    // TODO add pre stage runner

    override _.FableArgsBuilder =
        CmdLine.appendPrefix "--typedArrays" "false"
        >> CmdLine.appendPrefix "--define" "FX_NO_BIGINT"

    override this.CopyStageRunner _ =
        // Copy all *.ts files to the build directory from source directory
        Directory.GetFiles(this.SourceDir, "*.ts")
        |> Shell.copyFiles this.OutDir

        // Copy the tsconfig.json file to the build directory
        let typeScriptConfig = Path.Combine(this.SourceDir, "ts", "tsconfig.json")
        Shell.copyFile this.OutDir typeScriptConfig

        // Copy the lib folder to the build directory
        let libSourceFolder = Path.Combine(this.SourceDir, "lib")
        let libDestinationFolder = Path.Combine(this.OutDir, "lib")
        Shell.copyDir libDestinationFolder libSourceFolder FileFilter.allFiles

        // Copy the package.json file to the build directory
        let packageJson = Path.Combine(this.SourceDir, "package.json")
        Shell.copyFile this.OutDir packageJson

        // Copy the README.md file to the build directory
        let readme = Path.Combine(this.SourceDir, "README.md")
        Shell.copyFile this.OutDir readme

type BuildFableLibraryJavaScript() =
    // JavaScript is a specialisation of the TypeScript target
    inherit BuildFableLibraryTypeScript()

    let jsOutDir = Path.Combine("build", "fable-library")
    do
        base.Language <- "javascript"

    override this.PostFableBuildStage =
        // Alias to make it clear which directory is referred to
        let tsBuildDir = this.BuildDir

        stage "Post Build" {

            // Make sure to work with a clean build directory
            // We need to delete the directy here because JavaScript is
            // a bit special compared to other targets.
            // JavaScript things happens after the Fable.Library to TypeScript compilation
            run (fun _ ->
                if Directory.Exists jsOutDir then
                    Directory.Delete(jsOutDir, true)
            )

            run (fun _ ->
                // Compile the library to JavaScript using the TypeScript compiler
                let args =
                    CmdLine.appendPrefix "--project" tsBuildDir
                    >> CmdLine.appendPrefix "--outDir" jsOutDir

                Cmd.tsc args
                |> CmdLine.toString
            )

            run (fun _ ->
                // Copy lib/big.d.ts to the JavaScript build directory
                //             // Copy lib/big.d.ts to the JavaScript build directory
            let bigDts = Path.Combine(tsBuildDir, "lib", "big.d.ts")
            Shell.copyFile bigDts jsOutDir

            Shell.copyFile
                jsOutDir
                (Path.Combine(tsBuildDir, "package.json"))

            Shell.copyFile
                jsOutDir
                (Path.Combine(this.SourceDir, "README.md"))
            )
        }

pipeline "Standalone" {
    // Make the pipeline think it is running on CI
    // This is to activate the condition on the "Standalone" stage
    envVars ["CI", "true"]

    Stages.testStandalone

    runIfOnlySpecified
}

module Instances =

    let buildFableLibraryRust = BuildFableLibraryRust()
    let buildFableLibraryPython = BuildFableLibraryPython()
    let buildFableLibraryDart = BuildFableLibraryDart()
    let buildFableLibraryTypeScript = BuildFableLibraryTypeScript()
    let buildFableLibraryJavaScript = BuildFableLibraryJavaScript()

pipeline "test-javascript" {
    Instances.buildFableLibraryJavaScript.Stage()

    // Test against .NET

    Stages.JavaScript.testWithMocha "Main"
    Stages.JavaScript.testWithMocha "Adaptive"
    Stages.JavaScript.testReact

    runIfOnlySpecified
}

pipeline "test-react" {
    Instances.buildFableLibraryJavaScript.Stage()

    Stages.JavaScript.testReact

    runIfOnlySpecified
}

// Register the build fable-library pipelines
Instances.buildFableLibraryRust.Pipeline()
Instances.buildFableLibraryPython.Pipeline()
Instances.buildFableLibraryDart.Pipeline()
Instances.buildFableLibraryTypeScript.Pipeline()
Instances.buildFableLibraryJavaScript.Pipeline()

tryPrintPipelineCommandHelp()