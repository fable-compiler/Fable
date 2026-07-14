// System.Environment reads process state, so the library module is std-only
// and these tests do not apply to no_std builds.
[<Fable.Core.Rust.OuterAttr("cfg", [| "not(feature = \"no_std\")" |])>]
module Fable.Tests.EnvironmentTests

open Util.Testing
open System

[<Fact>]
let ``Environment.NewLine works`` () =
    Environment.NewLine.Length > 0
    |> equal true

[<Fact>]
let ``Environment.GetEnvironmentVariable returns null for a missing variable`` () =
    Environment.GetEnvironmentVariable("__FABLE_NON_EXISTENT_VAR__")
    |> equal null

[<Fact>]
let ``Environment.CurrentDirectory works`` () =
    Environment.CurrentDirectory.Length > 0
    |> equal true
