module Fable.Tests.Main
open System

let allTests =
  [
    Cli.tests
    FileWatcher.tests
  ]

open Expecto

[<EntryPoint>]
let main args =
    testList "All" allTests
    |> runTestsWithArgs defaultConfig args
