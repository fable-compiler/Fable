module Fable.Tests.RuneTests

open System
open System.Globalization
open System.Text
open Util.Testing

[<Fact>]
let ``Rune(Char) constructor works`` () =
    Rune('A').Value |> equal 65

[<Fact>]
let ``Rune(Int32) constructor works`` () =
    Rune(0x1F600).Value |> equal 0x1F600

[<Fact>]
let ``Rune(Int32) constructor rejects invalid scalar values`` () =
    throwsAnyError (fun () -> Rune(-1) |> ignore)
    throwsAnyError (fun () -> Rune(0xD800) |> ignore)
    throwsAnyError (fun () -> Rune(0x110000) |> ignore)

[<Fact>]
let ``Rune(UInt32) constructor works`` () =
    Rune(uint32 0x10FFFF).Value |> equal 0x10FFFF

[<Fact>]
let ``Rune(UInt32) constructor rejects invalid scalar values`` () =
    throwsAnyError (fun () -> Rune(uint32 0xD800) |> ignore)

[<Fact>]
let ``Rune(Char, Char) constructor rejects non-surrogate input`` () =
    throwsAnyError (fun () -> Rune('a', 'b') |> ignore)

[<Fact>]
let ``Rune.TryCreate(Char) works`` () =
    Rune.TryCreate('A') |> equal (true, Rune('A'))

[<Fact>]
let ``Rune.TryCreate(Char, Char) rejects non-surrogate input`` () =
    Rune.TryCreate('a', 'b') |> equal (false, Rune(0))

[<Fact>]
let ``Rune.TryCreate(Int32) works`` () =
    Rune.TryCreate(0x1F600) |> equal (true, Rune(0x1F600))
    Rune.TryCreate(-1) |> equal (false, Rune(0))

[<Fact>]
let ``Rune.TryCreate(UInt32) works`` () =
    Rune.TryCreate(uint32 0x10FFFF) |> equal (true, Rune(0x10FFFF))
    Rune.TryCreate(uint32 0x110000) |> equal (false, Rune(0))

[<Fact>]
let ``Rune.IsValid(Int32) works`` () =
    Rune.IsValid(-1) |> equal false
    Rune.IsValid(0) |> equal true
    Rune.IsValid(0xD7FF) |> equal true
    Rune.IsValid(0xD800) |> equal false
    Rune.IsValid(0xDFFF) |> equal false
    Rune.IsValid(0xE000) |> equal true
    Rune.IsValid(0x10FFFF) |> equal true
    Rune.IsValid(0x110000) |> equal false

[<Fact>]
let ``Rune.IsValid(UInt32) works`` () =
    Rune.IsValid(uint32 0xD800) |> equal false
    Rune.IsValid(uint32 0x10FFFF) |> equal true
    Rune.IsValid(UInt32.MaxValue) |> equal false

[<Fact>]
let ``Rune.Value works`` () =
    Rune('A').Value |> equal 65
    Rune(0x1F600).Value |> equal 0x1F600
    Unchecked.defaultof<Rune>.Value |> equal 0

[<Fact>]
let ``Rune.Utf8SequenceLength works`` () =
    Rune('A').Utf8SequenceLength |> equal 1
    Rune('\u00E9').Utf8SequenceLength |> equal 2
    Rune('\u20AC').Utf8SequenceLength |> equal 3
    Rune(0x1F600).Utf8SequenceLength |> equal 4

[<Fact>]
let ``Rune.Utf16SequenceLength works`` () =
    Rune('A').Utf16SequenceLength |> equal 1
    Rune(0x1F600).Utf16SequenceLength |> equal 2

[<Fact>]
let ``Rune.IsAscii works`` () =
    Rune(0x7F).IsAscii |> equal true
    Rune(0x80).IsAscii |> equal false

[<Fact>]
let ``Rune.IsBmp works`` () =
    Rune(0xFFFF).IsBmp |> equal true
    Rune(0x10000).IsBmp |> equal false

[<Fact>]
let ``Rune.Plane works`` () =
    Rune(0xFFFF).Plane |> equal 0
    Rune(0x10000).Plane |> equal 1
    Rune(0x10FFFF).Plane |> equal 16

[<Fact>]
let ``Rune.ReplacementChar works`` () =
    Rune.ReplacementChar |> equal (Rune(0xFFFD))

[<Fact>]
let ``Rune.GetUnicodeCategory works`` () =
    Rune.GetUnicodeCategory(Rune('A')) |> equal UnicodeCategory.UppercaseLetter

[<Fact>]
let ``Rune.GetNumericValue works`` () =
    Rune.GetNumericValue(Rune('4')) |> equal 4.0

[<Fact>]
let ``Rune.IsControl works`` () =
    Rune.IsControl(Rune('\u0000')) |> equal true

[<Fact>]
let ``Rune.IsDigit works`` () =
    Rune.IsDigit(Rune('4')) |> equal true

[<Fact>]
let ``Rune.IsLetter works`` () =
    Rune.IsLetter(Rune('A')) |> equal true
    Rune.IsLetter(Rune('家')) |> equal true

[<Fact>]
let ``Rune.IsLetterOrDigit works`` () =
    Rune.IsLetterOrDigit(Rune('A')) |> equal true
    Rune.IsLetterOrDigit(Rune('-')) |> equal false

[<Fact>]
let ``Rune.IsLower works`` () =
    Rune.IsLower(Rune('a')) |> equal true

[<Fact>]
let ``Rune.IsNumber works`` () =
    Rune.IsNumber(Rune('4')) |> equal true

[<Fact>]
let ``Rune.IsPunctuation works`` () =
    Rune.IsPunctuation(Rune('.')) |> equal true

[<Fact>]
let ``Rune.IsSeparator works`` () =
    Rune.IsSeparator(Rune(' ')) |> equal true

[<Fact>]
let ``Rune.IsSymbol works`` () =
    Rune.IsSymbol(Rune('+')) |> equal true

[<Fact>]
let ``Rune.IsUpper works`` () =
    Rune.IsUpper(Rune('A')) |> equal true

[<Fact>]
let ``Rune.IsWhiteSpace works`` () =
    Rune.IsWhiteSpace(Rune('\n')) |> equal true

[<Fact>]
let ``Rune.ToLower works`` () =
    Rune.ToLower(Rune('A'), CultureInfo.InvariantCulture) |> equal (Rune('a'))

[<Fact>]
let ``Rune.ToLowerInvariant works`` () =
    Rune.ToLowerInvariant(Rune('A')) |> equal (Rune('a'))

[<Fact>]
let ``Rune.ToUpper works`` () =
    Rune.ToUpper(Rune('b'), CultureInfo.InvariantCulture) |> equal (Rune('B'))

[<Fact>]
let ``Rune.ToUpperInvariant works`` () =
    Rune.ToUpperInvariant(Rune('b')) |> equal (Rune('B'))

[<Fact>]
let ``Rune.ToString works`` () =
    Rune('A').ToString() |> equal "A"
    Rune(0x1F600).ToString() |> equal "\uD83D\uDE00"

[<Fact>]
let ``Rune string conversion works`` () =
    string (Rune(0x1F600)) |> equal "\uD83D\uDE00"

[<Fact>]
let ``Rune.GetRuneAt works`` () =
    let input = "A\uD83D\uDE00Z"

    Rune.GetRuneAt(input, 0) |> equal (Rune('A'))
    Rune.GetRuneAt(input, 1) |> equal (Rune(0x1F600))
    Rune.GetRuneAt(input, 3) |> equal (Rune('Z'))

[<Fact>]
let ``Rune.GetRuneAt rejects invalid positions`` () =
    let input = "A\uD83D\uDE00Z"

    throwsAnyError (fun () -> Rune.GetRuneAt(input, -1) |> ignore)
    throwsAnyError (fun () -> Rune.GetRuneAt(input, 2) |> ignore)
    throwsAnyError (fun () -> Rune.GetRuneAt(input, 4) |> ignore)

[<Fact>]
let ``Rune.TryGetRuneAt works`` () =
    let input = "A\uD83D\uDE00Z"

    Rune.TryGetRuneAt(input, 0) |> equal (true, Rune('A'))
    Rune.TryGetRuneAt(input, 1) |> equal (true, Rune(0x1F600))
    Rune.TryGetRuneAt(input, 2) |> equal (false, Rune(0))
    Rune.TryGetRuneAt(input, 3) |> equal (true, Rune('Z'))

[<Fact>]
let ``Rune.TryGetRuneAt rejects out of range positions`` () =
    let input = "A\uD83D\uDE00Z"

    throwsAnyError (fun () -> Rune.TryGetRuneAt(input, -1) |> ignore)
    throwsAnyError (fun () -> Rune.TryGetRuneAt(input, 4) |> ignore)

[<Fact>]
let ``Rune equality operator works`` () =
    Rune('a') = Rune('a') |> equal true
    Rune('a') = Rune('b') |> equal false

[<Fact>]
let ``Rune inequality operator works`` () =
    Rune('a') <> Rune('b') |> equal true

[<Fact>]
let ``Rune less than operator works`` () =
    Rune('a') < Rune('b') |> equal true

[<Fact>]
let ``Rune less than or equal operator works`` () =
    Rune('a') <= Rune('a') |> equal true

[<Fact>]
let ``Rune greater than operator works`` () =
    Rune('b') > Rune('a') |> equal true

[<Fact>]
let ``Rune greater than or equal operator works`` () =
    Rune('b') >= Rune('b') |> equal true

[<Fact>]
let ``Rune.CompareTo works`` () =
    Rune('a').CompareTo(Rune('b')) |> equal -1
    Rune('a').CompareTo(Rune('a')) |> equal 0
    Rune('b').CompareTo(Rune('a')) |> equal 1

[<Fact>]
let ``Rune comparison works`` () =
    compare (Rune('a')) (Rune('b')) |> equal -1
    compare (Rune('a')) (Rune('a')) |> equal 0
    compare (Rune('b')) (Rune('a')) |> equal 1

[<Fact>]
let ``Rune.Equals works`` () =
    Rune('a').Equals(Rune('a')) |> equal true
    Rune('a').Equals(Rune('b')) |> equal false

[<Fact>]
let ``Rune.GetHashCode works`` () =
    Rune('a').GetHashCode() |> equal 97
    Rune(0x1F600).GetHashCode() |> equal 0x1F600

[<Fact>]
let ``Rune.GetRuneAt works with BMP characters`` () =
    let rune = Rune.GetRuneAt("ABC", 1)
    rune.Value |> equal 0x42
    rune.ToString() |> equal "B"

[<Fact>]
let ``Rune.GetRuneAt works with supplementary characters`` () =
    let rune = Rune.GetRuneAt("𠀀", 0)
    rune.Value |> equal 0x20000
    rune.ToString() |> equal "𠀀"

[<Fact>]
let ``Rune.GetRuneAt uses UTF-16 code unit indexes`` () =
    Rune.GetRuneAt("A𠀀B", 0).ToString()
    |> equal "A"

    Rune.GetRuneAt("A𠀀B", 1).ToString()
    |> equal "𠀀"

    Rune.GetRuneAt("A𠀀B", 3).ToString()
    |> equal "B"

[<Fact>]
let ``Rune.GetRuneAt throws when index points inside a surrogate pair`` () =
    throwsAnyError <| fun () ->
        Rune.GetRuneAt("A𠀀B", 2)
        |> ignore

[<Fact>]
let ``Rune.GetRuneAt throws when index is out of range`` () =
    throwsAnyError (fun () -> Rune.GetRuneAt("ABC", -1) |> ignore)
    throwsAnyError (fun () -> Rune.GetRuneAt("ABC", 3) |> ignore)

[<Fact>]
let ``Rune.GetRuneAt returns ReplacementChar for unpaired surrogate characters`` () =
    Rune.GetRuneAt("\uD800", 0) |> equal Rune.ReplacementChar
    Rune.GetRuneAt("\uDC00", 0) |> equal Rune.ReplacementChar

[<Fact>]
let ``Rune.Value returns the Unicode scalar value`` () =
    Rune.GetRuneAt("A", 0).Value
    |> equal 0x41

    Rune.GetRuneAt("𠀀", 0).Value
    |> equal 0x20000

[<Fact>]
let ``Rune.Utf16SequenceLength returns the number of UTF-16 code units`` () =
    Rune.GetRuneAt("A", 0).Utf16SequenceLength
    |> equal 1

    Rune.GetRuneAt("𠀀", 0).Utf16SequenceLength
    |> equal 2

[<Fact>]
let ``Rune.Utf16SequenceLength works at the BMP boundary`` () =
    Rune.GetRuneAt("\uFFFF", 0).Utf16SequenceLength
    |> equal 1

    Rune.GetRuneAt("\U00010000", 0).Utf16SequenceLength
    |> equal 2

[<Fact>]
let ``Rune.ToString returns the represented Unicode scalar as a string`` () =
    Rune.GetRuneAt("A", 0).ToString()
    |> equal "A"

    Rune.GetRuneAt("𠀀", 0).ToString()
    |> equal "𠀀"

[<Fact>]
let ``F# string conversion works with Rune`` () =
    Rune.GetRuneAt("A", 0)
    |> string
    |> equal "A"

    Rune.GetRuneAt("𠀀", 0)
    |> string
    |> equal "𠀀"

[<Fact>]
let ``String.EnumerateRunes enumerates Unicode scalar values`` () =
    "A𠀀B".EnumerateRunes()
    |> Seq.map (fun rune -> rune.Value)
    |> Seq.toList
    |> equal [ 0x41; 0x20000; 0x42 ]

[<Fact>]
let ``String.EnumerateRunes works with Rune string conversion`` () =
    "A𠀀B".EnumerateRunes()
    |> Seq.map string
    |> Seq.toList
    |> equal [ "A"; "𠀀"; "B" ]

[<Fact>]
let ``String.EnumerateRunes replaces unpaired surrogates`` () =
    // .NET String.EnumerateRunes replaces invalid UTF-16 sequences with U+FFFD
    // https://learn.microsoft.com/en-us/dotnet/api/system.text.rune.replacementchar

    // Note: For Rust, this only works for string literals (replaced at compile time)
    let input = "\uD800A\uDC00"

    input.EnumerateRunes()
    |> Seq.map _.Value
    |> Seq.toList
    |> equal [ 0xFFFD; 0x41; 0xFFFD ]
