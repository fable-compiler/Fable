module Fable.Tests.Environment

open System
open Util.Testing

let tests =
    testList "Environment" [
        testCase "Environment.GetEnvironmentVariable returns null for a missing variable" <| fun () ->
            Environment.GetEnvironmentVariable("__FABLE_NON_EXISTENT_VAR__")
            |> equal null

        testCase "Environment.CurrentDirectory works" <| fun () ->
            Environment.CurrentDirectory.Length > 0
            |> equal true

        #if FABLE_COMPILER
        // These tests can sometimes get Expecto stuck
        // This happens if both Expecto and the test are trying to write the console at the same time
        testCase "Console.Out.WriteLine works" <| fun () -> Console.Out.WriteLine("Hello stdout")

        testCase "Console.Error.WriteLine works" <| fun () -> Console.Error.WriteLine("Hello stderr")
        #endif
    ]
