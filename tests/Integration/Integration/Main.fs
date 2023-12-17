module Fable.Tests.Main
open System

let allTests =
  [
    Cli.tests
    FileWatcher.tests
    CompilationTests.tests
  ]

open Expecto

[<EntryPoint>]
let main args =
    testList "All" allTests
    |> runTestsWithCLIArgs [] args
