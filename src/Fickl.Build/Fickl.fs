/// Fickl's own build commands.
module Fickl.Build.Fickl

open System.IO
open Partas.Build
open Fickl.Build.Forwarded

let private cliProject = Path.Combine(Paths.root, "src", "Fable.Cli")

let private cliDll =
    Path.Combine(cliProject, "bin", "Release", "net10.0", "fable.dll")

let private buildCli =
    stage "build compiler" { run (cmd $"dotnet build {cliProject} -c Release") }

let private fcsForkUrl = "https://github.com/shayanhabibi/fickle-fsharp.git"

/// The files Fable references from lib/fcs, as produced by the FCS build.
let private fcsFiles =
    [
        "FSharp.Compiler.Service.dll"
        "FSharp.Compiler.Service.xml"
        "FSharp.Compiler.Service.deps.json"
        "FSharp.DependencyManager.Nuget.dll"
        "FSharp.DependencyManager.Nuget.xml"
        "FSharp.Core.dll"
        "FSharp.Core.xml"
    ]

/// Builds the FCS fork and copies its binaries into lib/fcs, replacing upstream's.
let fcsFork =
    let repo =
        Input.option<string> "--repo"
        |> Input.description "Path to the fickle-fsharp checkout; cloned there if missing"
        |> Input.def (Path.GetFullPath(Path.Combine(Paths.root, "..", "fickle-fsharp")))

    let branch =
        Input.option<string> "--branch"
        |> Input.description "Branch to clone when the checkout is missing"
        |> Input.def "fickle_slim"

    let framework =
        Input.option<string> "--framework"
        |> Input.description "The FCS target framework to copy from"
        |> Input.def "net10.0"

    command "fcs-fork" {
        description "Build the FCS fork (fickle-fsharp) and copy its binaries into lib/fcs"

        input {
            let! repo = repo
            and! branch = branch
            and! framework = framework

            let repo = Path.GetFullPath repo

            let output =
                Path.Combine(repo, "artifacts", "bin", "FSharp.Compiler.Service", "Release", framework)

            let libFcs = Path.Combine(Paths.root, "lib", "fcs")

            return
                stage "fcs-fork" {
                    if not (Directory.Exists repo) then
                        stage "clone" {
                            run (cmd $"git clone --filter=blob:none --single-branch -b {branch} {fcsForkUrl} {repo}")
                        }

                    stage "build" {
                        workingDir repo
                        // BUILDING_USING_DOTNET: outside the FCS solution the build otherwise expects the bootstrap compiler.
                        // FSHARPCORE_USE_PACKAGE=false: build FSharp.Core from the fork's source, as upstream's lib/fcs does.
                        run (
                            cmd
                                $"dotnet build -c Release src/Compiler -p:BUILDING_USING_DOTNET=true -p:FSHARPCORE_USE_PACKAGE=false"
                        )
                    }

                    stage "copy to lib/fcs" {
                        run (fun _ ->
                            for file in fcsFiles do
                                File.Copy(Path.Combine(output, file), Path.Combine(libFcs, file), true)
                                printfn $"lib/fcs/{file}"

                            0
                        )
                    }
                }
        }
    }

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
