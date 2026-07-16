module Fable.Tests.Environment

open System
open Util.Testing

[<Fact>]
let ``test Environment.GetEnvironmentVariable returns null for a missing variable`` () =
    Environment.GetEnvironmentVariable("__FABLE_NON_EXISTENT_VAR__")
    |> equal null

[<Fact>]
let ``test Environment.CurrentDirectory works`` () =
    Environment.CurrentDirectory.Length > 0
    |> equal true

[<Fact>]
let ``test Environment.GetEnvironmentVariable returns a value for an existing variable`` () =
    // PATH should be set in any environment (Linux/macOS/Windows CI runners included)
    Environment.GetEnvironmentVariable("PATH").Length > 0
    |> equal true
