module Build.Main

open Build.FableLibrary

// This is a basic help message, as the CLI parser is not a "real" CLI parser
// For now, it is enough as this is just a dev tool
let printHelp () =
    let helpText =
        """
Usage: dotnet run <command> [<args>]

Available commands:
    fable-library

        Options:
            --javascript            Build fable-library for JavaScript
            --typescript            Build fable-library for TypeScript
            --python                Build fable-library for Python
            --dart                  Build fable-library for Dart
            --rust                  Build fable-library for Rust

    quicktest                       Watch for changes and re-run the quicktest
                                    This is useful to work on a feature in an isolated
                                    manner to avoid all the noise coming from the main tests

        Subcommands:
            javascript              Run for JavaScript
            typescript              Run for TypeScript
            python                  Run for Python
            dart                    Run for Dart
            rust                    Run for Rust

        Options:
            --skip-fable-library    Skip building fable-library if folder already exists

    test                            Run the main tests suite
        Subcommands:
            javascript              Run the tests for JavaScript
            typescript              Run the tests for TypeScript
            python                  Run the tests for Python
            dart                    Run the tests for Dart
            rust                    Run the tests for Rust
            integration             Run the integration test suite
            standalone              Tests the standalone version of Fable
                                    (Fable running on top of Node.js)

        Options for all except integration and standalone:
            --watch                 Watch for changes and re-run the tests
            --skip-fable-library    Skip building fable-library if folder already exists
            --no-dotnet             When in watch mode, do not run the .NET tests

        Options for JavaScript:
            --reat-only             Run only the tests for React (can be run in watch mode)

        Options for Rust:
            --ast-only              Run only the tests for the AST (can be run in watch mode)
            --no_std                Compile and run the tests without the standard library
            --threaded              Compile and run the tests with the threaded runtime

    standalone                      Compile standalone + worker version of Fable running
                                    on top of of Node.js

        Options:
            --skip-fable-library    Skip building fable-library if folder already exists
            --no-minify             Don't minify the JavaScript output
            --watch                 Watch for changes and recompile

    worker-js                       Compile the worker for the standalone version of Fable

        Options:
            --skip-fable-library    Skip building fable-library if folder already exists
            --no-minify             Don't minify the JavaScript output

    compiler-js                     Compile the Fable compiler to JavaScript

        Options:
            --skip-fable-library    Skip building fable-library if folder already exists
            --no-minify             Don't minify the JavaScript output

    package                         Generate local package for Fable.Cli and Fable.Core
                                    allowing to use this local package for testing
                                    inside of other projects

        Options:
            --skip-fable-library    Skip building fable-library if folder already exists

    publish                         Publish the different packages to NuGet and NPM
                                    based on the CHANGELOG.md files
                                    If the last version in the CHANGELOG.md is
                                    different from the version in the packages,
                                    the package will be published

    github-release                  Create a GitHub release based on the CHANGELOG.md
                                    file and the version in the package.json
                                    This will also invoke the publish command
        """

    printfn "%s" helpText

[<EntryPoint>]
let main argv =
    let argv = argv |> Array.map (fun x -> x.ToLower()) |> Array.toList

    SimpleExec.Command.Run(name = "dotnet", args = "tool restore")
    SimpleExec.Command.Run(name = "dotnet", args = "husky install")

    match argv with
    | "fable-library" :: args ->
        match args with
        | "--javascript" :: _ -> BuildFableLibraryJavaScript().Run()
        | "--typescript" :: _ -> BuildFableLibraryTypeScript().Run()
        | "--python" :: _ -> BuildFableLibraryPython().Run()
        | "--dart" :: _ -> BuildFableLibraryDart().Run()
        | "--rust" :: _ -> BuildFableLibraryRust().Run()
        | _ -> printHelp ()
    | "test" :: args ->
        match args with
        | "javascript" :: args -> Test.JavaScript.handle args
        | "typescript" :: args -> Test.TypeScript.handle args
        | "python" :: args -> Test.Python.handle args
        | "dart" :: args -> Test.Dart.handle args
        | "rust" :: args -> Test.Rust.handle args
        | "integration" :: args -> Test.Integration.handle args
        | "standalone" :: _ -> Test.Standalone.handle args
        // This test is using quicktest project for now,
        // because it can't compile (yet?) the Main JavaScript tests
        | "compiler-js" :: _ -> Test.CompilerJs.handle args
        | _ -> printHelp ()
    | "quicktest" :: args ->
        match args with
        | "javascript" :: _ -> Quicktest.JavaScript.handle args
        | "typescript" :: _ -> Quicktest.TypeScript.handle args
        | "python" :: _ -> Quicktest.Python.handle args
        | "dart" :: _ -> Quicktest.Dart.handle args
        | "rust" :: _ -> Quicktest.Rust.handle args
        | _ -> printHelp ()
    | "standalone" :: args -> Standalone.handle args
    | "compiler-js" :: args -> CompilerJs.handle args
    | "worker-js" :: args -> WorkerJs.handle args
    | "sync-fcs-repo" :: _ -> FcsRepo.sync ()
    | "copy-fcs-repo" :: _ -> FcsRepo.copy ()
    | "publish" :: args -> Publish.handle args
    | "github-release" :: args -> GithubRelease.handle args
    | "package" :: args -> Package.handle args
    | "help" :: _
    | "--help" :: _
    | _ -> printHelp ()

    0
