
module Fable.Tests.Environment

open System
open Util.Testing

[<Fact>]
let ``test Environment.NewLine is non-empty`` () =
    Environment.NewLine.Length > 0 |> equal true

[<Fact>]
let ``test Environment.GetEnvironmentVariable returns null for a missing variable`` () =
    Environment.GetEnvironmentVariable("__FABLE_NON_EXISTENT_VAR__")
    |> equal null

[<Fact>]
let ``test Environment.CurrentDirectory works`` () =
    Environment.CurrentDirectory.Length > 0
    |> equal true

[<Fact>]
let ``test Environment.CurrentDirectory can be set and read back`` () =
    let original = Environment.CurrentDirectory

    try
        Environment.CurrentDirectory <- ".."
        let updated = Environment.CurrentDirectory
        updated <> original |> equal true
        original.StartsWith(updated) |> equal true
    finally
        Environment.CurrentDirectory <- original
