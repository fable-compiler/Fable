module Fable.Tests.Compiler.Main

open Fable.Tests.Compiler
open Expecto

let allTests =
    [
        CompilerMessages.tests
        AnonRecordInInterface.tests
    ]


[<EntryPoint>]
let main args =
    let config = { defaultConfig with ``parallel`` = false }

    allTests
    |> testList "All"
    |> runTestsWithArgs config args
