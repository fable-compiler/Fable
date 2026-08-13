module Fable.Tests.Compiler.CompilerHelpers

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

open Fable.Transforms

let tests =
    testList "Compiler Helpers" [
        testCase "isSuppressible splits the code bands at FABLE0100" <| fun _ ->
            // Directive warnings sit below the boundary and can never be silenced by a directive.
            WarningCodes.isSuppressible (Some WarningCodes.UnknownSuppressionCode) |> equal false
            WarningCodes.isSuppressible (Some WarningCodes.UnusedSuppressionDirective) |> equal false
            WarningCodes.isSuppressible (Some WarningCodes.SuppressionBlockWithoutCode) |> equal false
            WarningCodes.isSuppressible (Some "FABLE0099") |> equal false
            WarningCodes.isSuppressible (Some "FABLE0100") |> equal true
            WarningCodes.isSuppressible (Some "FABLE9999") |> equal true
            // A warning with no code at all is still caught by a bare `// fable-disable-line`.
            WarningCodes.isSuppressible None |> equal true

        testCase "expectedVersionMatchesActual works for same major version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "5.0.0" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "5.0.0" "5.0.1" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "5.1.0" "5.1.0" |> equal true

        testCase "expectedVersionMatchesActual works if actual version is higher than expected version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "5.0.0" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "5.0.1" |> equal true
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "5.1.0" |> equal true

        testCase "expectedVersionMatchesActual reject if actual version is lower than expected version" <| fun _ ->
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.0" "3.0.0" |> equal false
            Fable.CompilerExt.expectedVersionMatchesActual "4.0.1" "3.0.0" |> equal false
            Fable.CompilerExt.expectedVersionMatchesActual "4.1.0" "3.0.0" |> equal false
    ]
