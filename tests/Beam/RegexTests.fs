module Fable.Tests.Regex

open System
open System.Text.RegularExpressions
open Fable.Tests.Util
open Util.Testing

// --- Basic IsMatch ---

[<Fact>]
let ``test Regex.IsMatch works`` () =
    Regex.IsMatch("hello world", "hello") |> equal true

[<Fact>]
let ``test Regex.IsMatch with no match works`` () =
    Regex.IsMatch("hello world", "xyz") |> equal false

[<Fact>]
let ``test Regex.IsMatch with IgnoreCase works`` () =
    Regex.IsMatch("HELLO", "hello", RegexOptions.IgnoreCase) |> equal true

[<Fact>]
let ``test Regex.IsMatch with Multiline works`` () =
    Regex.IsMatch("line1\nline2", "^line2$", RegexOptions.Multiline) |> equal true

[<Fact>]
let ``test Regex.IsMatch with IgnoreCase and Multiline works`` () =
    let input = "ABC\ndef"
    Regex.IsMatch(input, "^def$", RegexOptions.IgnoreCase ||| RegexOptions.Multiline) |> equal true

// --- Instance IsMatch ---

[<Fact>]
let ``test Regex instance IsMatch works`` () =
    let r = Regex("\\d+")
    r.IsMatch("abc123") |> equal true

[<Fact>]
let ``test Regex instance IsMatch with offset works`` () =
    let r = Regex("\\d+")
    r.IsMatch("123abc", 3) |> equal false

// --- Match ---

[<Fact>]
let ``test Regex.Match works`` () =
    let m = Regex.Match("hello world", "\\w+")
    m.Success |> equal true
    m.Value |> equal "hello"

[<Fact>]
let ``test Match.Index works`` () =
    let m = Regex.Match("hello world", "world")
    m.Index |> equal 6

[<Fact>]
let ``test Match.Length works`` () =
    let m = Regex.Match("hello world", "world")
    m.Length |> equal 5

[<Fact>]
let ``test Match.Value works`` () =
    let m = Regex.Match("hello world", "world")
    m.Value |> equal "world"

[<Fact>]
let ``test Match.Groups indexer getter works`` () =
    let m = Regex.Match("John 30", "(\\w+) (\\d+)")
    m.Groups.[0].Value |> equal "John 30"
    m.Groups.[1].Value |> equal "John"
    m.Groups.[2].Value |> equal "30"

[<Fact>]
let ``test Match.Groups.Count works`` () =
    let m = Regex.Match("John 30", "(\\w+) (\\d+)")
    m.Groups.Count |> equal 3

[<Fact>]
let ``test Group values are correct when not matched`` () =
    let m = Regex.Match("foo", "(foo)(bar)?")
    m.Groups.[1].Value |> equal "foo"
    m.Groups.[1].Success |> equal true
    m.Groups.[2].Value |> equal ""
    m.Groups.[2].Success |> equal false

// --- Matches ---

[<Fact>]
let ``test Regex.Matches works`` () =
    let ms = Regex.Matches("a1b2c3", "\\d")
    ms.Count |> equal 3

[<Fact>]
let ``test Regex.Matches indexer getter works`` () =
    let ms = Regex.Matches("a1b2c3", "\\d")
    ms.[0].Value |> equal "1"
    ms.[1].Value |> equal "2"
    ms.[2].Value |> equal "3"

[<Fact>]
let ``test Regex instance Match and Matches work`` () =
    let r = Regex("\\d+")
    let m = r.Match("abc 123 def 456")
    m.Value |> equal "123"
    let ms = r.Matches("abc 123 def 456")
    ms.Count |> equal 2

[<Fact>]
let ``test Regex instance Match and Matches with offset work`` () =
    let r = Regex("\\d+")
    let m = r.Match("abc 123 def 456", 7)
    m.Value |> equal "456"

// --- Replace ---

[<Fact>]
let ``test Regex.Replace works`` () =
    let result = Regex.Replace("hello world", "world", "erlang")
    result |> equal "hello erlang"

[<Fact>]
let ``test Regex.Replace with macros works`` () =
    let result = Regex.Replace("John Smith", "(\\w+) (\\w+)", "$2 $1")
    result |> equal "Smith John"

[<Fact>]
let ``test Regex.Replace with evaluator works`` () =
    let result = Regex.Replace("abc123", "\\d+", fun m -> m.Value + m.Value)
    result |> equal "abc123123"

[<Fact>]
let ``test Regex.Replace with limit works`` () =
    let r = Regex("\\d")
    let result = r.Replace("a1b2c3", "x", 2)
    result |> equal "axbxc3"

[<Fact>]
let ``test Replacing with $0 works`` () =
    let result = Regex.Replace("abc", "(\\w)", "$0$0")
    result |> equal "aabbcc"

// --- Split ---

[<Fact>]
let ``test Regex.Split works`` () =
    let result = Regex.Split("a1b2c3", "\\d")
    result.Length |> equal 4
    result.[0] |> equal "a"
    result.[1] |> equal "b"
    result.[2] |> equal "c"
    result.[3] |> equal ""

// --- Escape / Unescape ---

[<Fact>]
let ``test Regex.Escape works`` () =
    let escaped = Regex.Escape("hello.world")
    escaped |> equal "hello\\.world"

[<Fact>]
let ``test Regex.Unescape works`` () =
    let unescaped = Regex.Unescape("hello\\.world")
    unescaped |> equal "hello.world"

// --- Options ---

[<Fact>]
let ``test Regex.Options works`` () =
    let r = Regex("\\d+", RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
    int r.Options |> equal 3

// --- Singleline ---

[<Fact>]
let ``test RegexOptions.Singleline works`` () =
    Regex.IsMatch("a\nb", "a.b", RegexOptions.Singleline) |> equal true
