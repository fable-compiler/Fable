module Fable.Tests.Compiler.CompilerHelpers

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let tests =
    testList "Compiler Helpers" [
        testCase "expectedVersionMatchesActual works for same major version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "5.0.0" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "5.0.1" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "5.1.0" "5.0.0" |> equal true

        testCase "expectedVersionMatchesActual works if actual version is higher than expected version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.1" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "4.1.0" "5.0.0" |> equal true

        testCase "expectedVersionMatchesActual reject if actual version is lower than expected version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "3.0.0" |> equal false
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.1" "3.0.0" |> equal false
            Fable.CompilerExt.expectedVersionMatchesActual "4.1.0" "3.0.0" |> equal false
    ]
