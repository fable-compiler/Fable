module Fable.Tests.Main
open System

let allTests =
  [|
    FileWatcher.tests
  |]

open Expecto

[<EntryPoint>]
let main args =
    Array.toList allTests
    |> testList "All"
    |> runTestsWithArgs defaultConfig args
