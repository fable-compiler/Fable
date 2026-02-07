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

[<Fact>]
let ``test String.Length works`` () =
    "hello".Length |> equal 5

[<Fact>]
let ``test String.Length of empty string works`` () =
    "".Length |> equal 0

[<Fact>]
let ``test String.ToUpper works`` () =
    "hello".ToUpper() |> equal "HELLO"

[<Fact>]
let ``test String.ToLower works`` () =
    "HELLO".ToLower() |> equal "hello"

[<Fact>]
let ``test String.Trim works`` () =
    "  hello  ".Trim() |> equal "hello"

[<Fact>]
let ``test String.TrimStart works`` () =
    "  hello  ".TrimStart() |> equal "hello  "

[<Fact>]
let ``test String.TrimEnd works`` () =
    "  hello  ".TrimEnd() |> equal "  hello"

[<Fact>]
let ``test String.StartsWith works`` () =
    "hello world".StartsWith("hello") |> equal true
    "hello world".StartsWith("world") |> equal false

[<Fact>]
let ``test String.EndsWith works`` () =
    "hello world".EndsWith("world") |> equal true
    "hello world".EndsWith("hello") |> equal false

[<Fact>]
let ``test String.Substring with start index works`` () =
    "hello world".Substring(6) |> equal "world"

[<Fact>]
let ``test String.Substring with start and length works`` () =
    "hello world".Substring(0, 5) |> equal "hello"

[<Fact>]
let ``test String.Replace works`` () =
    "hello world".Replace("world", "erlang") |> equal "hello erlang"

[<Fact>]
let ``test String.Contains works`` () =
    "hello world".Contains("world") |> equal true
    "hello world".Contains("xyz") |> equal false

[<Fact>]
let ``test String.IndexOf works`` () =
    "hello world".IndexOf("world") |> equal 6
    "hello world".IndexOf("xyz") |> equal -1
