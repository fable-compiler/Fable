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

// A `char` is a plain integer at runtime on Beam, so every conversion path has to be told the type
// is a char — miss one and it prints the codepoint instead of the character.
[<Fact>]
let ``test char conversions to string agree`` () =
    let c = '2'
    string c |> equal "2"
    c.ToString() |> equal "2"
    "x" + string c |> equal "x2"
    Char.ToString c |> equal "2"
    $"{c}" |> equal "2"

[<Fact>]
let ``test char conversions to string work above the latin1 range`` () =
    let c = '✗' // U+2717 — needs real UTF-8 encoding, not a byte
    string c |> equal "✗"
    c.ToString() |> equal "✗"
    Char.ToString c |> equal "✗"
    $"{c}" |> equal "✗"
    $"a{c}b" |> equal "a✗b"

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

[<Fact>]
let ``test Char addition works`` () =
    'A' + 'B' |> int |> equal 131
    'A' + char 7 |> int |> equal 72

[<Fact>]
let ``test Char subtraction works`` () =
    'B' - 'A' |> int |> equal 1
    char 9 - char 7 |> int |> equal 2

[<Fact>]
let ``test Char.TryParse works`` () =
    let mutable c = ' '
    Char.TryParse("A", &c) |> equal true
    c |> equal 'A'

[<Fact>]
let ``test Char.TryParse fails with empty string`` () =
    let mutable c = ' '
    Char.TryParse("", &c) |> equal false

[<Fact>]
let ``test Char.TryParse fails with longer string`` () =
    let mutable c = ' '
    Char.TryParse("AA", &c) |> equal false

// TODO: Surrogate tests need UTF-16 codepoint handling.
// Erlang uses full Unicode codepoints, not UTF-16 encoding,
// so string indexing returns codepoints not UTF-16 code units.
// [<Fact>]
// let ``test Char.IsHighSurrogate works`` () =
//     Char.IsHighSurrogate('a') |> equal false
//     Char.IsHighSurrogate("\U00010F00".[0]) |> equal true
//
// [<Fact>]
// let ``test Char.IsHighSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsHighSurrogate(str,0) |> equal false
//     Char.IsHighSurrogate(str,1) |> equal true
//     Char.IsHighSurrogate(str,2) |> equal false
//     Char.IsHighSurrogate(str,3) |> equal false
//
// [<Fact>]
// let ``test Char.IsLowSurrogate works`` () =
//     Char.IsLowSurrogate('a') |> equal false
//     Char.IsLowSurrogate("\U00010F00".[1]) |> equal true
//
// [<Fact>]
// let ``test Char.IsLowSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsLowSurrogate(str,0) |> equal false
//     Char.IsLowSurrogate(str,1) |> equal false
//     Char.IsLowSurrogate(str,2) |> equal true
//     Char.IsLowSurrogate(str,3) |> equal false
//
// [<Fact>]
// let ``test Char.IsSurrogate works`` () =
//     Char.IsSurrogate('a') |> equal false
//     Char.IsSurrogate("\U00010F00".[1]) |> equal true
//
// [<Fact>]
// let ``test Char.IsSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsSurrogate(str,0) |> equal false
//     Char.IsSurrogate(str,1) |> equal true
//     Char.IsSurrogate(str,2) |> equal true
//     Char.IsSurrogate(str,3) |> equal false
//
// [<Fact>]
// let ``test Char.IsSurrogatePair works`` () =
//     Char.IsSurrogatePair('a', 'b') |> equal false
//     Char.IsSurrogatePair("\U00010F00".[0], "\U00010F00".[1]) |> equal true
//
// [<Fact>]
// let ``test Char.IsSurrogatePair with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsSurrogatePair(str,0) |> equal false
//     Char.IsSurrogatePair(str,1) |> equal true
//     Char.IsSurrogatePair(str,2) |> equal false

// --- Boxed chars reaching a string conversion ---
//
// A `char` is a plain `integer()` on Beam, so any conversion that dispatches on the runtime value
// alone renders the codepoint instead of the character. These sites see the static type through the
// boxing cast and encode the char up front. Where the cast is *not* visible — a boxed char piped
// into `string`, bound to an `obj` first, or reaching `%A`/`%O` through printf's curried runtime —
// the type is genuinely gone by then; see "char at a generic type" in FABLE-BEAM.md.

[<Fact>]
let ``test string of a directly boxed char works`` () =
    let c = '2'
    string (box c) |> equal "2"

[<Fact>]
let ``test String.Format of a char works`` () =
    let c = '2'
    String.Format("{0}", c) |> equal "2"
    String.Format("{0}", box c) |> equal "2"

[<Fact>]
let ``test String.Format of a non-ASCII char works`` () =
    String.Format("{0}", '✗') |> equal "✗"
