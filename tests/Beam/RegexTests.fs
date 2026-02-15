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

// --- Iteration ---

[<Fact>]
let ``test Regex.Matches iteration works`` () =
    let ms = Regex.Matches("a1b2c3", "\\d")
    let count = ref 0
    for _m in ms do count.Value <- count.Value + 1
    equal 3 count.Value

[<Fact>]
let ``test Match.Groups iteration works`` () =
    let m = Regex.Match("John 30", "(\\w+) (\\d+)")
    let mutable count = 0
    for g in m.Groups do
        count <- count + g.Value.Length
    equal 13 count

[<Fact>]
let ``test Match.Groups iteration works II`` () =
    let m = Regex.Match("foo bar baz", @"(\w+) \w+ (\w+)")
    let li = [ for g in m.Groups -> g.Value ]
    let x = li |> List.skip 1 |> List.reduce (+)
    equal "foobaz" x

[<Fact>]
let ``test Group values can be converted to int`` () =
    let m = Regex.Match("ABC123", @"([A-Z]+)(\d+)")
    let group = m.Groups.[2]
    int (group.Value) |> equal 123

// --- Replace with offset ---

[<Fact>]
let ``test Regex.Replace with limit and offset works`` () =
    let str = "Too   much   space"
    let r = Regex("\\s+")
    r.Replace(str, " ", count = 20, startat = 0)
    |> equal "Too much space"
    r.Replace(str, " ", count = 20, startat = 5)
    |> equal "Too   much space"

[<Fact>]
let ``test Regex.Replace with evaluator and limit works`` () =
    let str = "abcabcabcabcabcabcabcabc"
    let r = Regex("c")
    let test count expected =
        r.Replace(str, (fun (m: Match) -> string m.Index), count=count)
        |> equal expected
    test 1 "ab2abcabcabcabcabcabcabc"
    test 3 "ab2ab5ab8abcabcabcabcabc"

// --- Split with limit ---

// TODO: Regex.Split with limit not yet supported
// [<Fact>]
// let ``test Regex.Split with limit works`` () =
//     let r = Regex("\\d")
//     let result = r.Split("a1b2c3", 2)
//     result.Length |> equal 2
//     result.[0] |> equal "a"
//     result.[1] |> equal "b2c3"

// --- Named groups ---

[<Fact>]
let ``test succeeds when match`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Number 12345 is positive"
    m.Success |> equal true

[<Fact>]
let ``test doesn't succeed when unmatched`` () =
    let r = Regex "(?<number>\\d+)"
    let m = r.Match "Hello World"
    m.Success |> equal false

// TODO: Accessing groups by string name (m.Groups.["name"]) not yet supported
// [<Fact>]
// let ``test can get value of existing group`` () =
//     let r = Regex "(?<number>\\d+)"
//     let m = r.Match "Number 12345 is positive"
//     m.Groups.["number"].Value |> equal "12345"
//
// [<Fact>]
// let ``test can get values of multiple existing groups`` () =
//     let r = Regex "\\+(?<country>\\d{1,3}) (?<num>\\d+)"
//     let m = r.Match "Number: +49 1234!"
//     m.Groups.["country"].Value |> equal "49"
//     m.Groups.["num"].Value |> equal "1234"
//
// [<Fact>]
// let ``test doesn't succeed for not existing named group`` () =
//     let r = Regex "(?<number>\\d+)"
//     let m = r.Match "Number 12345 is positive"
//     m.Groups.["nothing"].Success |> equal false
//
// [<Fact>]
// let ``test doesn't succeed for existing unmatched group`` () =
//     let r = Regex "(?<exact>42)|(?<number>\\d+)"
//     let m = r.Match "Number 12345 is positive"
//     m.Groups.["exact"].Success |> equal false
//
// [<Fact>]
// let ``test named capture group group name from variable`` () =
//     let r = Regex "(?<number>\\d+)"
//     let m = r.Match "Number 12345 is positive"
//     let g = "number"
//     m.Groups.[g].Value |> equal "12345"

[<Fact>]
let ``test Regex.Replace with evaluator works when regex has named capture group`` () =
    let r = Regex "0(?<number>\\d+)"
    let text = "Number 012345!"
    let replace (m: Match) =
        sprintf "%s" m.Groups.[1].Value
    let actual = r.Replace(text, replace)
    actual |> equal "Number 12345!"
