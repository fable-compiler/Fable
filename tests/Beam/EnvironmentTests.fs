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
