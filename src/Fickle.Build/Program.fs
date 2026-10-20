module Fickle.Build.Program

open System
open Partas.Build
open Fickle.Build.Forwarded

[<EntryPoint>]
let main argv =
    // Fable.Build resolves relative paths against the current directory
    Environment.CurrentDirectory <- Paths.root

    rootCommand argv {
        name "fickle-build"
        description "Fickle build CLI. Run a command with --help for its options, or --explain to dry-run it"
        workingDir Paths.root
        addCommands [ yield! Forwarded.commands; Fickle.timings; Fickle.fcsFork ]
    }
