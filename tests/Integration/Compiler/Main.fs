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
    let config = [ Sequenced ]

    allTests
    |> testList "All"
    |> runTestsWithCLIArgs config args
