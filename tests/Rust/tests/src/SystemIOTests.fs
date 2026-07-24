// System.IO needs a filesystem, so the library modules are std-only and
// these tests do not apply to no_std builds.
[<Fable.Core.Rust.OuterAttr("cfg", [| "not(feature = \"no_std\")" |])>]
module Fable.Tests.SystemIOTests

open Util.Testing
open System.IO

[<Fact>]
let ``Path.Combine works with two parts`` () =
    let combined = Path.Combine("foo", "bar")
    Path.GetFileName(combined) |> equal "bar"
    Path.GetDirectoryName(combined) |> equal "foo"

[<Fact>]
let ``Path.Combine works with three parts`` () =
    let combined = Path.Combine("foo", "bar", "baz.txt")
    Path.GetFileName(combined) |> equal "baz.txt"
    Path.GetDirectoryName(combined) |> equal (Path.Combine("foo", "bar"))

[<Fact>]
let ``Path.Combine with a rooted second part uses the second part`` () =
    // .NET: a rooted later argument discards the earlier one.
    Path.Combine("/foo", "/bar") |> equal "/bar"
    Path.Combine("foo", "/bar") |> equal "/bar"

[<Fact>]
let ``Path.GetDirectoryName works`` () =
    Path.GetDirectoryName("temp/test.txt")
    |> equal "temp"

[<Fact>]
let ``Path.GetExtension works`` () =
    Path.GetExtension("temp/test.txt")
    |> equal ".txt"

[<Fact>]
let ``Path.GetFileName works`` () =
    Path.GetFileName("temp/test.txt")
    |> equal "test.txt"

[<Fact>]
let ``Path.GetFileNameWithoutExtension works`` () =
    Path.GetFileNameWithoutExtension("temp/test.txt")
    |> equal "test"

[<Fact>]
let ``Path.HasExtension works`` () =
    Path.HasExtension("temp/test.txt")
    |> equal true

    Path.HasExtension("temp/test")
    |> equal false

[<Fact>]
let ``Path.GetTempPath works`` () =
    (Path.GetTempPath()).Length > 0
    |> equal true

[<Fact>]
let ``Path.GetRandomFileName works`` () =
    (Path.GetRandomFileName()).Length > 0
    |> equal true

[<Fact>]
let ``File round-trip works`` () =
    let file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    File.Exists(file) |> equal false

    File.WriteAllText(file, "Hello World")
    File.Exists(file) |> equal true

    File.ReadAllText(file)
    |> equal "Hello World"

    File.Delete(file)
    File.Exists(file) |> equal false

[<Fact>]
let ``File.Delete on a missing file is a no-op`` () =
    // .NET semantics: deleting a non-existent file does not throw.
    File.Delete("definitely-missing-file-xyz.tmp")
    true |> equal true
