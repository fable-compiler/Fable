module Fable.Tests.Dart.Rune

open System.Text
open Fable.Core
open Util

[<Emit("String.fromCharCode($0)")>]
let private stringFromCharCode (_: int) : string =
    nativeOnly

let tests () =
    testCase "Rune.GetRuneAt works with BMP characters" <| fun () ->
        let rune = Rune.GetRuneAt("ABC", 1)

        rune.Value |> equal 0x42
        rune.ToString() |> equal "B"

    testCase "Rune.GetRuneAt works with supplementary characters" <| fun () ->
        let rune = Rune.GetRuneAt("𠀀", 0)

        rune.Value |> equal 0x20000
        rune.ToString() |> equal "𠀀"

    testCase "Rune.GetRuneAt uses UTF-16 code unit indexes" <| fun () ->
        Rune.GetRuneAt("A𠀀B", 0).ToString()
        |> equal "A"

        Rune.GetRuneAt("A𠀀B", 1).ToString()
        |> equal "𠀀"

        Rune.GetRuneAt("A𠀀B", 3).ToString()
        |> equal "B"

    testCase "Rune.GetRuneAt throws when index points inside a surrogate pair" <| fun () ->
        throwsAnyError <| fun () ->
            Rune.GetRuneAt("A𠀀B", 2)
            |> ignore

    testCase "Rune.GetRuneAt throws when index is out of range" <| fun () ->
        throwsAnyError <| fun () ->
            Rune.GetRuneAt("ABC", -1)
            |> ignore

        throwsAnyError <| fun () ->
            Rune.GetRuneAt("ABC", 3)
            |> ignore

    testCase "Rune.GetRuneAt throws for unpaired surrogate characters" <| fun () ->
        let highSurrogate = stringFromCharCode 0xD800
        let lowSurrogate = stringFromCharCode 0xDC00

        throwsAnyError <| fun () ->
            Rune.GetRuneAt(highSurrogate, 0)
            |> ignore

        throwsAnyError <| fun () ->
            Rune.GetRuneAt(lowSurrogate, 0)
            |> ignore

    testCase "Rune.Value returns the Unicode scalar value" <| fun () ->
        Rune.GetRuneAt("A", 0).Value
        |> equal 0x41

        Rune.GetRuneAt("𠀀", 0).Value
        |> equal 0x20000

    testCase "Rune.Utf16SequenceLength returns the number of UTF-16 code units" <| fun () ->
        Rune.GetRuneAt("A", 0).Utf16SequenceLength
        |> equal 1

        Rune.GetRuneAt("𠀀", 0).Utf16SequenceLength
        |> equal 2

    testCase "Rune.Utf16SequenceLength works at the BMP boundary" <| fun () ->
        Rune.GetRuneAt("\uFFFF", 0).Utf16SequenceLength
        |> equal 1

        Rune.GetRuneAt("\U00010000", 0).Utf16SequenceLength
        |> equal 2

    testCase "Rune.ToString returns the represented Unicode scalar as a string" <| fun () ->
        Rune.GetRuneAt("A", 0).ToString()
        |> equal "A"

        Rune.GetRuneAt("𠀀", 0).ToString()
        |> equal "𠀀"

    testCase "F# string conversion works with Rune" <| fun () ->
        Rune.GetRuneAt("A", 0)
        |> string
        |> equal "A"

        Rune.GetRuneAt("𠀀", 0)
        |> string
        |> equal "𠀀"

    testCase "String.EnumerateRunes enumerates Unicode scalar values" <| fun () ->
        "A𠀀B".EnumerateRunes()
        |> Seq.map (fun rune -> rune.Value)
        |> Seq.toList
        |> equal [ 0x41; 0x20000; 0x42 ]

    testCase "String.EnumerateRunes works with Rune string conversion" <| fun () ->
        "A𠀀B".EnumerateRunes()
        |> Seq.map string
        |> Seq.toList
        |> equal [ "A"; "𠀀"; "B" ]
