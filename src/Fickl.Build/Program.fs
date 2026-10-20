module Fickl.Build.Program

open System
open Partas.Build
open Fickl.Build.Forwarded

[<EntryPoint>]
let main argv =
    // Fable.Build resolves relative paths against the current directory
    Environment.CurrentDirectory <- Paths.root

    rootCommand argv {
        name "fickl-build"
        description "Fickl build CLI. Run a command with --help for its options, or --explain to dry-run it"
        workingDir Paths.root
        addCommands [ yield! Forwarded.commands; Fickl.timings; Fickl.fcsFork ]
    }
