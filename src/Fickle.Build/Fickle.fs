/// Fickle's own build commands.
module Fickle.Build.Fickle

open System.IO
open Partas.Build
open Fickle.Build.Forwarded

let private cliProject = Path.Combine(Paths.root, "src", "Fable.Cli")

let private cliDll =
    Path.Combine(cliProject, "bin", "Release", "net10.0", "fable.dll")

let private buildCli =
    stage "build compiler" { run (cmd $"dotnet build {cliProject} -c Release") }

/// Compiles a project with the Release compiler and prints its --timings report.
let timings =
    let project =
        Input.argument<string> "project"
        |> Input.description "The F# project or directory to compile"
        |> Input.def "tests/Js/Main"

    let language =
        Input.option<string> "--lang"
        |> Input.description "The target language"
        |> Input.def "javascript"
        |> Input.acceptOnlyFromAmong [ "javascript"; "typescript"; "python"; "dart"; "rust"; "php"; "beam" ]

    let sequential =
        Input.option<bool> "--no-parallel-type-check"
        |> Input.description "Type-check the whole project before Fable starts, so the phases do not overlap"

    let runs =
        Input.option<int> "--runs"
        |> Input.description "How many times to compile, each in a fresh process"
        |> Input.def 1
        |> Input.validate (fun runs ->
            if runs >= 1 then
                Ok()
            else
                Error "--runs must be at least 1"
        )

    command "timings" {
        description "Compile a project with --timings to see where the time goes"

        input {
            let! project = project
            and! language = language
            and! sequential = sequential
            and! runs = runs

            return
                stage "timings" {
                    buildCli

                    for i in 1..runs do
                        stage $"compile (run {i})" {
                            run (
                                cmd
                                    $"dotnet {cliDll} {project} --lang {language} --outDir temp/timings/{language} --exclude Fable.Core --noCache --timings"
                                |> Cmd.argIf sequential [ "--noParallelTypeCheck" ]
                            )
                        }
                }
        }
    }
