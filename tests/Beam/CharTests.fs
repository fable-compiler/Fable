module Fable.Tests.Chars

open System
open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Char.ToUpper works`` () =
    Char.ToUpper('b') |> equal 'B'

[<Fact>]
let ``test Char.ToLower works`` () =
    Char.ToLower('B') |> equal 'b'

[<Fact>]
let ``test Char.ToUpperInvariant works`` () =
    Char.ToUpperInvariant('b') |> equal 'B'

[<Fact>]
let ``test Char.ToLowerInvariant works`` () =
    Char.ToLowerInvariant('B') |> equal 'b'

[<Fact>]
let ``test Char.ToString works`` () =
    Char.ToString('b') |> equal "b"

[<Fact>]
let ``test Char.ToString type is casted correctly`` () =
    ('t'.ToString()) + "est" |> equal "test"

[<Fact>]
let ``test Char.IsLetter works`` () =
    Char.IsLetter('a') |> equal true
    Char.IsLetter('1') |> equal false
    Char.IsLetter('"') |> equal false

[<Fact>]
let ``test Char.IsLetter with two args works`` () =
    let str = "al1"
    Char.IsLetter(str, 0) |> equal true
    Char.IsLetter(str, 2) |> equal false

[<Fact>]
let ``test Char.IsDigit works`` () =
    Char.IsDigit('a') |> equal false
    Char.IsDigit('1') |> equal true

[<Fact>]
let ``test Char.IsDigit with two args works`` () =
    let str = "ba6"
    Char.IsDigit(str, 0) |> equal false
    Char.IsDigit(str, 2) |> equal true

[<Fact>]
let ``test Char.IsLetterOrDigit works`` () =
    Char.IsLetterOrDigit('a') |> equal true
    Char.IsLetterOrDigit('1') |> equal true
    Char.IsLetterOrDigit('-') |> equal false

[<Fact>]
let ``test Char.IsLetterOrDigit with two args works`` () =
    let str = "x-6"
    Char.IsLetterOrDigit(str, 0) |> equal true
    Char.IsLetterOrDigit(str, 1) |> equal false
    Char.IsLetterOrDigit(str, 2) |> equal true

[<Fact>]
let ``test Char.IsUpper works`` () =
    Char.IsUpper('A') |> equal true
    Char.IsUpper('b') |> equal false
    Char.IsUpper('2') |> equal false

[<Fact>]
let ``test Char.IsUpper with two args works`` () =
    let str = "Ab2"
    Char.IsUpper(str, 0) |> equal true
    Char.IsUpper(str, 1) |> equal false

[<Fact>]
let ``test Char.IsLower works`` () =
    Char.IsLower('A') |> equal false
    Char.IsLower('b') |> equal true
    Char.IsLower('2') |> equal false

[<Fact>]
let ``test Char.IsLower with two args works`` () =
    let str = "Ab2"
    Char.IsLower(str, 0) |> equal false
    Char.IsLower(str, 1) |> equal true

[<Fact>]
let ``test Char.IsNumber works`` () =
    Char.IsNumber('a') |> equal false
    Char.IsNumber('1') |> equal true

[<Fact>]
let ``test Char.IsNumber with two args works`` () =
    let str = "ba6"
    Char.IsNumber(str, 0) |> equal false
    Char.IsNumber(str, 2) |> equal true

[<Fact>]
let ``test Char.IsPunctuation works`` () =
    Char.IsPunctuation('a') |> equal false
    Char.IsPunctuation('.') |> equal true

[<Fact>]
let ``test Char.IsPunctuation with two args works`` () =
    let str = "ba,"
    Char.IsPunctuation(str, 0) |> equal false
    Char.IsPunctuation(str, 2) |> equal true

[<Fact>]
let ``test Char.IsSeparator works`` () =
    Char.IsSeparator('a') |> equal false
    Char.IsSeparator(' ') |> equal true

[<Fact>]
let ``test Char.IsSeparator with two args works`` () =
    let str = "ba "
    Char.IsSeparator(str, 0) |> equal false
    Char.IsSeparator(str, 2) |> equal true

[<Fact>]
let ``test Char.IsSymbol works`` () =
    Char.IsSymbol('a') |> equal false
    Char.IsSymbol('+') |> equal true

[<Fact>]
let ``test Char.IsSymbol with two args works`` () =
    let str = "ba+"
    Char.IsSymbol(str, 0) |> equal false
    Char.IsSymbol(str, 2) |> equal true

[<Fact>]
let ``test Char.IsWhitespace works`` () =
    Char.IsWhiteSpace(' ') |> equal true
    Char.IsWhiteSpace('\n') |> equal true
    Char.IsWhiteSpace('\t') |> equal true
    Char.IsWhiteSpace('-') |> equal false

[<Fact>]
let ``test Char.IsWhitespace works with two args`` () =
    let input = " \r"
    Char.IsWhiteSpace(input, 0) |> equal true
    Char.IsWhiteSpace(input, 1) |> equal true

[<Fact>]
let ``test Char.IsControl works`` () =
    Char.IsControl('a') |> equal false
    Char.IsControl('\u0000') |> equal true
    Char.IsControl('\u001F') |> equal true
    Char.IsControl('\u007F') |> equal true
    Char.IsControl('\u009F') |> equal true

[<Fact>]
let ``test Char.IsControl with two args works`` () =
    let str = "a\u0001\u0002\u0003"
    Char.IsControl(str, 0) |> equal false
    Char.IsControl(str, 1) |> equal true
    Char.IsControl(str, 2) |> equal true
    Char.IsControl(str, 3) |> equal true

[<Fact>]
let ``test Char.Parse works`` () =
    equal 'A' (Char.Parse "A")

[<Fact>]
let ``test Char.Parse fails with empty string`` () =
    throwsAnyError (fun () -> Char.Parse "")

[<Fact>]
let ``test Char.Parse fails with longer string`` () =
    throwsAnyError (fun () -> Char.Parse "AA")

[<Fact>]
let ``test Char.GetUnicodeCategory works`` () =
    Char.GetUnicodeCategory('a') |> int |> equal 1
    Char.GetUnicodeCategory('1') |> int |> equal 8

[<Fact>]
let ``test Char.GetUnicodeCategory with two args works`` () =
    let str = "Ba6"
    Char.GetUnicodeCategory(str, 0) |> int |> equal 0
    Char.GetUnicodeCategory(str, 1) |> int |> equal 1
    Char.GetUnicodeCategory(str, 2) |> int |> equal 8
