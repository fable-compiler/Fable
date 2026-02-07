module Fable.Tests.String

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test String literal works`` () =
    "hello" |> equal "hello"

[<Fact>]
let ``test String concatenation with + works`` () =
    "hello" + " " + "world" |> equal "hello world"

[<Fact>]
let ``test String interpolation works`` () =
    let name = "world"
    $"hello {name}" |> equal "hello world"

[<Fact>]
let ``test String interpolation with expression works`` () =
    let x = 21
    $"the answer is {x * 2}" |> equal "the answer is 42"

[<Fact>]
let ``test String equality works`` () =
    ("hello" = "hello") |> equal true
    ("hello" = "world") |> equal false

[<Fact>]
let ``test String inequality works`` () =
    ("hello" <> "world") |> equal true
    ("hello" <> "hello") |> equal false

[<Fact>]
let ``test Empty string works`` () =
    "" |> equal ""

[<Fact>]
let ``test String.IsNullOrEmpty with empty string returns true`` () =
    System.String.IsNullOrEmpty("") |> equal true

[<Fact>]
let ``test String.IsNullOrEmpty with non-empty string returns false`` () =
    System.String.IsNullOrEmpty("hello") |> equal false

[<Fact>]
let ``test String concatenation with variable works`` () =
    let a = "hello"
    let b = " world"
    (a + b) |> equal "hello world"
