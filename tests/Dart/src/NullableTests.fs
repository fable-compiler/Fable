module Fable.Tests.Dart.Nullable

open System
open Util
open FSharp.Linq

let private none<'T when 'T: struct and 'T: (new: unit -> 'T) and 'T :> ValueType> = Nullable<'T>()

let tests() =
    testCase "Nullable.char works" <| fun () ->
        Nullable.char (Nullable 49uy) |> equal (Nullable '1')
        Nullable.char (Nullable 53) |> equal (Nullable '5')
        Nullable.char (Nullable 55L) |> equal (Nullable '7')
        Nullable.char (Nullable 58.0) |> equal (Nullable ':')
        Nullable.char (Nullable 'a') |> equal (Nullable 'a')
        Nullable.char none<int> |> equal none<char>

    testCase "Nullable.enum works" <| fun () ->
        (Nullable.enum (Nullable 2): Nullable<DayOfWeek>) |> equal (Nullable DayOfWeek.Tuesday)
        (Nullable.enum none<int>: Nullable<DayOfWeek>) |> equal none<DayOfWeek>

    testCase "Nullable.float and Nullable.float32 work" <| fun () ->
        Nullable.float (Nullable 1uy) |> equal (Nullable 1.0)
        Nullable.float (Nullable 5) |> equal (Nullable 5.0)
        Nullable.float (Nullable 7L) |> equal (Nullable 7.0)
        Nullable.float (Nullable 'c') |> equal (Nullable 99.0)
        Nullable.float none<int> |> equal none<float>
        Nullable.float32 (Nullable 5) |> equal (Nullable 5.0f)
        Nullable.float32 (Nullable 10.0) |> equal (Nullable 10.0f)
        Nullable.float32 none<int> |> equal none<float32>
        Nullable.double (Nullable 5) |> equal (Nullable 5.0)
        Nullable.single (Nullable 9.5) |> equal (Nullable 9.5f)

    testCase "Nullable integer conversions work" <| fun () ->
        Nullable.int (Nullable 1uy) |> equal (Nullable 1)
        Nullable.int (Nullable 7L) |> equal (Nullable 7)
        Nullable.int (Nullable 10.9) |> equal (Nullable 10)
        Nullable.int (Nullable -11.9) |> equal (Nullable -11)
        Nullable.int (Nullable 'c') |> equal (Nullable 99)
        Nullable.int none<float> |> equal none<int>
        Nullable.int16 (Nullable -7L) |> equal (Nullable -7s)
        Nullable.int32 (Nullable 5L) |> equal (Nullable 5)
        Nullable.int64 (Nullable -9.9) |> equal (Nullable -9L)
        Nullable.int64 none<int> |> equal none<int64>
        Nullable.int8 (Nullable 9.9) |> equal (Nullable 9y)
        Nullable.sbyte (Nullable -7.9) |> equal (Nullable -7y)
        Nullable.byte (Nullable 5) |> equal (Nullable 5uy)
        Nullable.uint8 (Nullable 7L) |> equal (Nullable 7uy)
        Nullable.uint16 (Nullable 9.9) |> equal (Nullable 9us)
        Nullable.uint (Nullable 5) |> equal (Nullable 5u)
        Nullable.uint32 (Nullable 7L) |> equal (Nullable 7u)
        Nullable.uint64 none<int> |> equal none<uint64>

    testCase "Nullable conversions evaluate their argument once" <| fun () ->
        let mutable calls = 0
        let next () =
            calls <- calls + 1
            Nullable 5
        Nullable.float (next ()) |> equal (Nullable 5.0)
        calls |> equal 1
