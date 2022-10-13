module Fable.Tests.CharTests

open System
open Util.Testing

// [<Fact>]
// let ``Char addition works`` () =
//     'A' + 'B' |> int |> equal 131
//     'A' + char 7 |> int |> equal 72

[<Fact>]
let ``Char.ToUpper works`` () =
    Char.ToUpper('b') |> equal 'B'

[<Fact>]
let ``Char.ToLower works`` () =
    Char.ToLower('B') |> equal 'b'

[<Fact>]
let ``Char.ToUpperInvariant works`` () =
    Char.ToUpperInvariant('b') |> equal 'B'

[<Fact>]
let ``Char.ToLowerInvariant works`` () =
    Char.ToLowerInvariant('B') |> equal 'b'

[<Fact>]
let ``Char.ToString works`` () =
    Char.ToString('b') |> equal "b"

// [<Fact>]
// let ``Char.GetUnicodeCategory works`` () =
//     Char.GetUnicodeCategory('a') |> equal UnicodeCategory.LowercaseLetter
//     Char.GetUnicodeCategory('1') |> equal UnicodeCategory.DecimalDigitNumber

// [<Fact>]
// let ``Char.GetUnicodeCategory with two args works`` () =
//     let str = "Ba6"
//     Char.GetUnicodeCategory(str,0) |> int |> equal 0 //UnicodeCategory.UppercaseLetter
//     Char.GetUnicodeCategory(str,1) |> int |> equal 1 //UnicodeCategory.LowercaseLetter
//     Char.GetUnicodeCategory(str,2) |> int |> equal 8 //UnicodeCategory.DecimalDigitNumber

[<Fact>]
let ``Char.IsControl works`` () =
    Char.IsControl('a') |> equal false
    Char.IsControl('\u0000') |> equal true
    Char.IsControl('\u001F') |> equal true
    Char.IsControl('\u007F') |> equal true
    Char.IsControl('\u009F') |> equal true

[<Fact>]
let ``Char.IsControl with two args works`` () =
    let str = "a\u0001\u0002\u0003"
    Char.IsControl(str,0) |> equal false
    Char.IsControl(str,1) |> equal true
    Char.IsControl(str,2) |> equal true
    Char.IsControl(str,3) |> equal true

[<Fact>]
let ``Char.IsLetter works`` () =
    Char.IsLetter('a') |> equal true
    Char.IsLetter('1') |> equal false
    Char.IsLetter('β') |> equal true
    Char.IsLetter('家') |> equal true
    Char.IsLetter('“') |> equal false
    Char.IsLetter('”') |> equal false

[<Fact>]
let ``Char.IsLetter with two args works`` () =
    let str = "al1"
    Char.IsLetter(str,0) |> equal true
    Char.IsLetter(str,2) |> equal false

[<Fact>]
let ``Char.IsDigit works`` () =
    Char.IsDigit('a') |> equal false
    Char.IsDigit('1') |> equal true

[<Fact>]
let ``Char.IsDigit with two args works`` () =
    let str = "ba6"
    Char.IsDigit(str,0) |> equal false
    Char.IsDigit(str,2) |> equal true

[<Fact>]
let ``Char.IsLetterOrDigit works`` () =
    Char.IsLetterOrDigit('a') |> equal true
    Char.IsLetterOrDigit('1') |> equal true
    Char.IsLetterOrDigit('β') |> equal true
    Char.IsLetterOrDigit('-') |> equal false

[<Fact>]
let ``Char.IsLetterOrDigit with two args works`` () =
    let str = "x-6"
    Char.IsLetterOrDigit(str,0) |> equal true
    Char.IsLetterOrDigit(str,1) |> equal false
    Char.IsLetterOrDigit(str,2) |> equal true

[<Fact>]
let ``Char.IsUpper works`` () =
    Char.IsUpper('A') |> equal true
    Char.IsUpper('b') |> equal false
    Char.IsUpper('2') |> equal false

[<Fact>]
let ``Char.IsUpper with two args works`` () =
    let str = "Ab2"
    Char.IsUpper(str,0) |> equal true
    Char.IsUpper(str,1) |> equal false

[<Fact>]
let ``Char.IsLower works`` () =
    Char.IsLower('A') |> equal false
    Char.IsLower('b') |> equal true
    Char.IsLower('2') |> equal false

[<Fact>]
let ``Char.IsLower with two args works`` () =
    let str = "Ab2"
    Char.IsLower(str,0) |> equal false
    Char.IsLower(str,1) |> equal true

[<Fact>]
let ``Char.IsNumber works`` () =
    Char.IsNumber('a') |> equal false
    Char.IsNumber('1') |> equal true

[<Fact>]
let ``Char.IsNumber with two args works`` () =
    let str = "ba6"
    Char.IsNumber(str,0) |> equal false
    Char.IsNumber(str,2) |> equal true

[<Fact>]
let ``Char.IsPunctuation works`` () =
    Char.IsPunctuation('a') |> equal false
    Char.IsPunctuation('.') |> equal true

[<Fact>]
let ``Char.IsPunctuation with two args works`` () =
    let str = "ba,"
    Char.IsPunctuation(str,0) |> equal false
    Char.IsPunctuation(str,2) |> equal true

[<Fact>]
let ``Char.IsSeparator works`` () =
    Char.IsSeparator('a') |> equal false
    Char.IsSeparator(' ') |> equal true

[<Fact>]
let ``Char.IsSeparator with two args works`` () =
    let str = "ba "
    Char.IsSeparator(str,0) |> equal false
    Char.IsSeparator(str,2) |> equal true

[<Fact>]
let ``Char.IsSymbol works`` () =
    Char.IsSymbol('a') |> equal false
    Char.IsSymbol('+') |> equal true

[<Fact>]
let ``Char.IsSymbol with two args works`` () =
    let str = "ba+"
    Char.IsSymbol(str,0) |> equal false
    Char.IsSymbol(str,2) |> equal true

[<Fact>]
let ``Char.IsWhitespace works`` () =
    Char.IsWhiteSpace(' ') |> equal true
    Char.IsWhiteSpace('\n') |> equal true
    Char.IsWhiteSpace('\t') |> equal true
    Char.IsWhiteSpace('\009') |> equal true
    Char.IsWhiteSpace('\013') |> equal true
    Char.IsWhiteSpace('\133') |> equal true
    Char.IsWhiteSpace('\160') |> equal true
    Char.IsWhiteSpace('-') |> equal false

[<Fact>]
let ``Char.IsWhitespace works with two args`` () =
    let input = " \r"
    Char.IsWhiteSpace(input, 0) |> equal true
    Char.IsWhiteSpace(input, 1) |> equal true

// [<Fact>]
// let ``Char.IsHighSurrogate works`` () =
//     Char.IsHighSurrogate('a') |> equal false
//     Char.IsHighSurrogate("\U00010F00"[0]) |> equal true

// [<Fact>]
// let ``Char.IsHighSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsHighSurrogate(str,0) |> equal false
//     Char.IsHighSurrogate(str,1) |> equal true
//     Char.IsHighSurrogate(str,2) |> equal false
//     Char.IsHighSurrogate(str,3) |> equal false

// [<Fact>]
// let ``Char.IsLowSurrogate works`` () =
//     Char.IsLowSurrogate('a') |> equal false
//     Char.IsLowSurrogate("\U00010F00"[1]) |> equal true

// [<Fact>]
// let ``Char.IsLowSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsLowSurrogate(str,0) |> equal false
//     Char.IsLowSurrogate(str,1) |> equal false
//     Char.IsLowSurrogate(str,2) |> equal true
//     Char.IsLowSurrogate(str,3) |> equal false

// [<Fact>]
// let ``Char.IsSurrogate works`` () =
//     Char.IsSurrogate('a') |> equal false
//     Char.IsSurrogate("\U00010F00"[1]) |> equal true

// [<Fact>]
// let ``Char.IsSurrogate with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsSurrogate(str,0) |> equal false
//     Char.IsSurrogate(str,1) |> equal true
//     Char.IsSurrogate(str,2) |> equal true
//     Char.IsSurrogate(str,3) |> equal false

// [<Fact>]
// let ``Char.IsSurrogatePair works`` () =
//     Char.IsSurrogatePair('a', 'b') |> equal false
//     Char.IsSurrogatePair("\U00010F00"[0], "\U00010F00"[1]) |> equal true

// [<Fact>]
// let ``Char.IsSurrogatePair with two args works`` () =
//     let str = "a\U00010F00z"
//     Char.IsSurrogatePair(str,0) |> equal false
//     Char.IsSurrogatePair(str,1) |> equal true
//     Char.IsSurrogatePair(str,2) |> equal false

// [<Fact>]
// let ``Char.Parse works`` () =
//     Char.Parse "A" |> equal 'A'

// [<Fact>]
// let ``Char.Parse fails if an empty string is given`` () =
//     try
//         Char.Parse ""
//         |> failwithf "Unexpected result '%c'"
//     with
//     | _ -> ()

// [<Fact>]
// let ``Char.Parse fails if a string with length > 1 is given`` () =
//     try
//         Char.Parse "Fable"
//         |> failwithf "Unexpected result '%c'"
//     with
//     | _ -> ()

// [<Fact>]
// let ``Adding char converted to string works`` () = // See #2832
//     let formatAmountNumber (d:decimal) =
//         let rounded = Math.Round(d,2)
//         if rounded = 0.M then "0,00" else
//         let number = if rounded < 0.M then ceil rounded else floor rounded
//         let decimalPart = abs(rounded - number)
//         let negative = d < 0.M
//         let number = string (abs number)
//         let mutable s = (sprintf "%.2f" decimalPart).Replace("0.",",")
//         for i in 0..number.Length-1 do
//             if i > 0 && i % 3 = 0 then
//                 s <- "." + s
//             let char = number[number.Length-1-i]
//             s <- char.ToString() + s
//         if negative then "-" + s else s
//     formatAmountNumber 50.M |> equal "50,00"
