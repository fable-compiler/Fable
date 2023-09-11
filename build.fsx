// #r "nuget: Fun.Build, 0.5.1"
#r "nuget: Fake.IO.FileSystem, 5.23.1"
#r "nuget: Fake.Core.Environment, 5.23.1"
#r "nuget: Fake.Tools.Git, 5.23.1"
#r "nuget: Fake.Api.GitHub, 5.23.1"
#r "nuget: BlackFox.CommandLine, 1.0.0"
#r "nuget: FsToolkit.ErrorHandling, 4.9.0"
#r "nuget: Fli, 1.10.1"

// open Fun.Build
// open System.IO
// open Fake.IO
// open BlackFox.CommandLine
// open FsToolkit.ErrorHandling

// module Commands =

//     let isWatch =
//         CmdArg.Create(
//             "-w",
//             "--watch",
//             "Watch for changes and recompile",
//             isOptional = true
//         )

// module TypeScript =

//     let fableBuildDest = "build/tests/TypeScript"
//     let typeScriptBuildDest = "build/tests/TypeScriptCompiled"
//     let projectDir = "tests/TypeScript"

//     module Stages =

//         let compileAndTestInWatchMode =
//             stage "" {
//                 whenCmdArg Commands.isWatch

//                 stage "Pre-compile" {
//                     // We need to first pre-compile for TypeScript and Mocha to
//                     // start in watch mode
//                     // Could use NPM script, but doing the pre-compilation allow
//                     // to have the full build system in a single place
//                     run (fun _ ->
//                         let args =
//                             CmdLine.appendRaw projectDir
//                             >> CmdLine.appendPrefix "--outDir" fableBuildDest
//                             >> CmdLine.appendPrefix "--lang" "typescript"
//                             >> CmdLine.appendPrefix "--exclude" "Fable.Core"

//                         Cmd.fable args
//                     )

//                     run (fun _ ->
//                         let args =
//                             CmdLine.appendPrefix "-p" fableBuildDest
//                             >> CmdLine.appendPrefix "--outDir" typeScriptBuildDest

//                         Cmd.tsc args
//                         |> CmdLine.toString
//                     )
//                 }

//                 stage "Watch, compile and test" {
//                     paralle

//                     run (fun _ ->
//                         let args =
//                             CmdLine.appendRaw projectDir
//                             >> CmdLine.appendPrefix "--outDir" fableBuildDest
//                             >> CmdLine.appendPrefix "--lang" "typescript"
//                             >> CmdLine.appendPrefix "--exclude" "Fable.Core"
//                             >> CmdLine.appendRaw "--watch"

//                         Cmd.fable args
//                         |> CmdLine.toString
//                     )

//                     run (fun _ ->
//                         let args =
//                             CmdLine.appendPrefix "-p" fableBuildDest
//                             >> CmdLine.appendPrefix "--outDir" typeScriptBuildDest
//                             >> CmdLine.appendRaw "--watch"

//                         Cmd.tsc args
//                         |> CmdLine.toString
//                     )

//                     run (fun _ ->
//                         // Mocha `--watch` doesn't support for ESM modules
//                         // So we use nodemon as the watcher
//                         let args =
//                             CmdLine.appendPrefix "-e" "js"
//                             >> CmdLine.appendRaw "--watch"
//                             >> CmdLine.appendRaw typeScriptBuildDest
//                             >> CmdLine.appendPrefix "--delay" "200ms"
//                             >> CmdLine.appendPrefix "--exec" "mocha"
//                             >> CmdLine.appendRaw $"{typeScriptBuildDest}/{fableBuildDest}"
//                             >> CmdLine.appendPrefix "--reporter" "dot"
//                             >> CmdLine.appendPrefix "-t" "10000"

//                         Cmd.nodemon args
//                         |> CmdLine.toString
//                     )
//                 }

//             }

//         let compileAndTest =
//             stage "Compile and tests" {
//                 whenNot {
//                     cmdArg Commands.isWatch
//                 }

//                 run (fun _ ->
//                     let args =
//                         CmdLine.appendRaw projectDir
//                         >> CmdLine.appendPrefix "--outDir" fableBuildDest
//                         >> CmdLine.appendPrefix "--lang" "typescript"
//                         >> CmdLine.appendPrefix "--exclude" "Fable.Core"

//                     Cmd.fable args
//                     |> CmdLine.toString
//                 )

//                 run (fun _ ->
//                     let args =
//                         CmdLine.appendPrefix "-p" fableBuildDest
//                         >> CmdLine.appendPrefix "--outDir" typeScriptBuildDest

//                     Cmd.tsc args
//                     |> CmdLine.toString
//                 )

//                 run (fun _ ->
//                     let args =
//                         CmdLine.appendRaw $"{typeScriptBuildDest}/{fableBuildDest}"
//                         >> CmdLine.appendPrefix "--reporter" "dot"
//                         >> CmdLine.appendPrefix "-t" "10000"

//                     Cmd.mocha args
//                     |> CmdLine.toString
//                 )
//             }

// module Stages =

//     module JavaScript =

//         let testWithMocha projectName =
//             let outDir = Path.Combine("build", "tests", "Js", projectName)
//             let projectDir = Path.Combine("tests", "Js", projectName)

//             stage $"Run test using mocha for project: {projectDir}" {

//                 run (fun _ ->
//                     Shell.cleanDir outDir
//                 )

//                 run (fun _ ->
//                     let args =
//                         CmdLine.appendRaw projectDir
//                         >> CmdLine.appendPrefix "--outDir" outDir
//                         >> CmdLine.appendPrefix "--exclude" "Fable.Core"

//                     Cmd.fable args
//                     |> CmdLine.toString
//                 )

//                 run (fun _ ->
//                     let args =
//                         CmdLine.appendRaw outDir
//                         >> CmdLine.appendPrefix "--reporter" "dot"
//                         >> CmdLine.appendPrefix "-t" "10000"

//                     Cmd.mocha args
//                     |> CmdLine.toString
//                 )
//             }


//     let testStandalone =
//         stage "Standalone" {
//             whenEnvVar "CI"

//             // Compile Fable standalone to JavaScript
//             run (fun _ ->
//                 let args =
//                     CmdLine.appendRaw "src/fable-standalone/src/"
//                     >> CmdLine.appendRaw "--noCache"

//                 Cmd.fable args
//                 |> CmdLine.toString
//             )

//             // Compile Fable compiler to JavaScript
//             run (fun _ ->
//                 let args =
//                     CmdLine.appendRaw "src/fable-compiler-js/src"
//                     >> CmdLine.appendPrefix "--exclude" "Fable.Core"
//                     >> CmdLine.appendPrefix "--define" "LOCAL_TEST"

//                 Cmd.fable args
//                 |> CmdLine.toString
//             )

//             // Compile test project using JavaScript fable compiler
//             run (fun _ ->
//                 let fableJs = "./src/fable-compiler-js/src/app.fs.js"
//                 let testProj = "tests/Js/Main/Fable.Tests.fsproj"
//                 let buildDir = "build/tests/Standalone"

//                 let args =
//                     CmdLine.appendRaw fableJs
//                     >> CmdLine.appendRaw testProj
//                     >> CmdLine.appendRaw buildDir

//                 Cmd.node args
//                 |> CmdLine.toString
//             )

//             run (fun _ ->
//                 let args =
//                     CmdLine.appendRaw "build/tests/Standalone"
//                     >> CmdLine.appendPrefix "--reporter" "dot"
//                     >> CmdLine.appendPrefix "-t" "10000"

//                 Cmd.mocha args
//                 |> CmdLine.toString
//             )
//         }


// pipeline "Standalone" {
//     // Make the pipeline think it is running on CI
//     // This is to activate the condition on the "Standalone" stage
//     envVars ["CI", "true"]

//     Stages.testStandalone

//     runIfOnlySpecified
// }

// type TestsPython () =
//     inherit TestsTarget(
//         "python",
//         BuildFableLibraryPython(),
//         Path.Combine("tests", "Python"),
//         Path.Combine("build", "tests", "Python")
//     )

//     override this.TestsAgainstTargetStage =
//         stage "Run tests against Python" {
//             workingDir this.BuildDir

//             run "poetry run pytest -x"
//         }

// type TestsRust() =
//     inherit TestsTarget(
//         "rust",
//         BuildFableLibraryRust(),
//         Path.Combine("tests", "Rust"),
//         Path.Combine("build", "tests", "Rust")
//     )

//     override this.TestsAgainstTargetStage =
//         stage "Run tests against Rust" {
//             workingDir this.BuildDir

//             run "cargo test"
//         }

// module Test =

//     let python = TestsPython()

// pipeline "test-javascript" {
//     FableLibrary.javaScript.Stage()

//     // Test against .NET
//     stage "Run tests against .NET" {
//         workingDir "tests/Js/Main"

//         run "dotnet run -c Release"
//     }

//     Stages.JavaScript.testWithMocha "Main"
//     Stages.JavaScript.testWithMocha "Adaptive"
//     Stages.JavaScript.testReact
//     Stages.testStandalone

//     runIfOnlySpecified
// }

// pipeline "test-typescript" {
//     FableLibrary.typeScript.Stage()

//     // Stages below use a flag to determine which stage should
//     TypeScript.Stages.compileAndTest
//     TypeScript.Stages.compileAndTestInWatchMode

//     runIfOnlySpecified
// }

// pipeline "test-react" {
//     FableLibrary.javaScript.Stage()
//     Stages.JavaScript.testReact

//     runIfOnlySpecified
// }

// // Register the build fable-library pipelines
// FableLibrary.rust.Pipeline()
// FableLibrary.python.Pipeline()
// FableLibrary.dart.Pipeline()
// FableLibrary.typeScript.Pipeline()
// FableLibrary.javaScript.Pipeline()

// // Register the tests pipelines
// Test.python.Pipeline()

// tryPrintPipelineCommandHelp()




open Fli

cli {
    Shell CMD
    Command "echo Hello World!"
}
|> Command.execute