/// <summary>
/// Upstream's build commands, declared as typed Partas.Build commands.
/// Each command declares the flags its Fable.Build handler reads, so they appear in <c>--help</c>,
/// and runs the handler in-process through <c>Build.Main.main</c> with the same arguments
/// upstream's <c>./build.sh</c> would pass. Fable.Build itself stays untouched, so upstream merges stay cheap.
/// </summary>
module Fickl.Build.Forwarded

open System.IO
open Partas.Build

module Paths =
    /// The repository root, which Fable.Build's relative paths resolve against.
    let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

/// Boolean flags, each forwarded as-is when set.
let private flags (declared: (string * string) list) : InputSpec<string list> =
    declared
    |> List.map (fun (name, desc) ->
        Input.option<bool> name
        |> Input.description desc
        |> InputSpec.ofInput
        |> InputSpec.map (fun isSet ->
            if isSet then
                [ name ]
            else
                []
        )
    )
    |> InputSpec.sequence
    |> InputSpec.map List.concat

/// An option taking a value, forwarded as <c>name value</c> when given.
let private valued (name: string) (desc: string) : InputSpec<string list> =
    Input.optionMaybe<string> name
    |> Input.description desc
    |> InputSpec.ofInput
    |> InputSpec.map (
        function
        | Some value -> [ name; value ]
        | None -> []
    )

let private fableBuild (args: string list) =
    stage "fable-build" { run (fun _ -> Build.Main.main (Array.ofList args)) }

/// A command running Fable.Build with fixed leading arguments followed by the forwarded options.
let private forward (name: string) (desc: string) (prefix: string list) (options: InputSpec<string list>) =
    command name {
        description desc

        input {
            let! extra = options
            return fableBuild (prefix @ extra)
        }
    }

let private watch = "--watch", "Watch for changes and re-run"
let private forceLib = "--force-fable-library", "Force building fable-library"
let private noDotnet = "--no-dotnet", "In watch mode, do not run the .NET tests"
let private noMinify = "--no-minify", "Don't minify the JavaScript output"

let private skipLibCore =
    "--skip-fable-library-core", "Skip the slow Rust extension and .pyi rebuild"

let private none = InputSpec.ret []

let fableLibrary =
    let language =
        Input.argument<string> "language"
        |> Input.description "The target to build fable-library for"
        |> Input.acceptOnlyFromAmong [ "javascript"; "typescript"; "python"; "dart"; "rust"; "beam" ]

    command "fable-library" {
        description "Build fable-library for a target"

        input {
            let! language = language
            return fableBuild [ "fable-library"; $"--{language}" ]
        }
    }

let test =
    let common = [ watch; forceLib; noDotnet ]

    command "test" {
        description "Run the main test suites"

        addCommands
            [
                forward
                    "javascript"
                    "Run the tests for JavaScript"
                    [ "test"; "javascript" ]
                    (flags
                        [
                            yield! common
                            "--react-only", "Run only the tests for React"
                            "--standalone-only", "Run only the tests for the standalone version of Fable"
                            "--adaptive-only", "Run only the tests for the adaptive version of Fable"
                            "--temporal-only",
                            "Run only the main tests, compiled with the Temporal date/time representation"
                        ])
                forward "typescript" "Run the tests for TypeScript" [ "test"; "typescript" ] (flags common)
                forward
                    "python"
                    "Run the tests for Python"
                    [ "test"; "python" ]
                    (flags
                        [
                            yield! common
                            skipLibCore
                            "--type-check", "Type-check the generated code with Pyright"
                            "--format", "Format the generated code with the Ruff formatter"
                            "--compile-only", "Compile the tests without running them"
                        ])
                forward "dart" "Run the tests for Dart" [ "test"; "dart" ] (flags common)
                forward
                    "rust"
                    "Run the tests for Rust"
                    [ "test"; "rust" ]
                    (InputSpec.map2
                        (@)
                        (flags
                            [
                                yield! common
                                "--ast-only", "Run only the tests for the AST"
                                "--no_std", "Legacy alias for --features no_std"
                                "--threaded", "Legacy alias for --features threaded"
                            ])
                        (valued "--features" "Comma-separated features; omit or use \",\" for the defaults"))
                forward "beam" "Run the tests for BEAM (Erlang)" [ "test"; "beam" ] (flags common)
                forward "integration" "Run the integration test suite" [ "test"; "integration" ] none
                forward
                    "standalone"
                    "Test the standalone version of Fable (Fable on Node.js)"
                    [ "test"; "standalone" ]
                    none
                forward "compiler-js" "Test the JS-compiled Fable compiler" [ "test"; "compiler-js" ] none
                forward
                    "plugins"
                    "Run the plugin test suite (MemberDeclarationPluginAttribute)"
                    [ "test"; "plugins" ]
                    none
            ]
    }

let quicktest =
    let common = [ watch; forceLib ]

    let lang name desc extra =
        forward name desc [ "quicktest"; name ] (flags (common @ extra))

    command "quicktest" {
        description "Watch and re-run the quicktest project for one target"

        addCommands
            [
                lang "javascript" "Run for JavaScript" []
                lang "typescript" "Run for TypeScript" []
                lang "python" "Run for Python" [ skipLibCore ]
                lang "dart" "Run for Dart" []
                lang "rust" "Run for Rust" []
                lang "beam" "Run for BEAM (Erlang)" []
            ]
    }

let standalone =
    forward
        "standalone"
        "Compile the standalone + worker version of Fable for Node.js"
        [ "standalone" ]
        (flags [ forceLib; noMinify; watch ])

let workerJs =
    forward
        "worker-js"
        "Compile the worker for the standalone version of Fable"
        [ "worker-js" ]
        (flags [ forceLib; noMinify ])

let compilerJs =
    forward
        "compiler-js"
        "Compile the Fable compiler to JavaScript"
        [ "compiler-js" ]
        (flags
            [
                forceLib
                noMinify
                "--skip-fable-standalone", "Don't rebuild fable-standalone first"
            ])

let package =
    forward
        "package"
        "Generate local packages of the compiler and Fable.Core for testing in other projects"
        [ "package" ]
        (flags [ forceLib ])

/// Upstream's publish, github-release and automated-release push Fable's own package names,
/// so they are deliberately not exposed here.
let commands =
    [ fableLibrary; test; quicktest; standalone; workerJs; compilerJs; package ]
