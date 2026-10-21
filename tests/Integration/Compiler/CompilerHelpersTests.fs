module Fable.Tests.Compiler.CompilerHelpers

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

open System
open System.Reflection
open Fable.Transforms

/// Every warning code `WarningCodes` can emit, read from the module itself so that this test
/// has no list of its own to fall out of date.
let private codesTheRegistryCanProduce () =
    let flags = BindingFlags.Public ||| BindingFlags.Static
    let moduleType = typeof<Fable.Transforms.State.LogEntry>.Assembly.GetType("Fable.Transforms.WarningCodes")
    let codeOf (pair: obj) = fst (pair :?> string * string)

    // The parameterised warnings interpolate their argument into the message, never into the
    // code, so anything of the right type will do.
    let throwaway (p: ParameterInfo) =
        if p.ParameterType = typeof<string> then box ""
        elif p.ParameterType.IsValueType then Activator.CreateInstance p.ParameterType
        else null

    // A warning is anything shaped `string * string`: plain ones compile to static properties,
    // parameterised ones to static methods. Nothing else in the module has that shape.
    let fromValues =
        moduleType.GetProperties(flags)
        |> Array.filter (fun p -> p.PropertyType = typeof<string * string>)
        |> Array.map (fun p -> codeOf (p.GetValue(null)))

    let fromFactories =
        moduleType.GetMethods(flags)
        |> Array.filter (fun m -> not m.IsSpecialName && m.ReturnType = typeof<string * string>)
        |> Array.map (fun m -> codeOf (m.Invoke(null, m.GetParameters() |> Array.map throwaway)))

    Set.ofArray (Array.append fromValues fromFactories)

let tests =
    testList "Compiler Helpers" [
        // The goal of this test is to act as a safe guard for us, so we remember to add
        // warning code to the knownCodes list.
        testCase "knownCodes lists every warning the registry can produce" <| fun _ ->
            // Forgetting to add a code is otherwise silent until somebody writes a directive for
            // it and gets told it's a typo - the one thing knownCodes exists to prevent.
            let produced = codesTheRegistryCanProduce ()

            // Guards against the reflection above quietly finding nothing, which would make every
            // assertion below pass for the wrong reason.
            if Set.isEmpty produced then
                failwith "Found no warnings in WarningCodes - the reflection in this test has gone stale"

            let missing = Set.difference produced WarningCodes.knownCodes
            let stale = Set.difference WarningCodes.knownCodes produced

            if not (Set.isEmpty missing) || not (Set.isEmpty stale) then
                failwithf "knownCodes is out of sync. Missing: %A. Listed but unused: %A" missing stale

        testCase "isSuppressible splits the code bands at FABLE0100" <| fun _ ->
            // Directive warnings sit below the boundary and can never be silenced by a directive.
            WarningCodes.isSuppressible (Some "FABLE0001") |> equal false
            WarningCodes.isSuppressible (Some "FABLE0002") |> equal false
            WarningCodes.isSuppressible (Some "FABLE0003") |> equal false
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
