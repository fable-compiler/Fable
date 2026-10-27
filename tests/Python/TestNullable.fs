module Fable.Tests.Nullable

open System
open Util.Testing
open FSharp.Linq

[<Fact>]
let ``test Nullable.char works`` () =
    Nullable.char (Nullable 49uy) |> equal (Nullable '1')
    Nullable.char (Nullable 53) |> equal (Nullable '5')
    Nullable.char (Nullable 55L) |> equal (Nullable '7')
    Nullable.char (Nullable 57m) |> equal (Nullable '9')
    Nullable.char (Nullable 58.0) |> equal (Nullable ':')
    Nullable.char (Nullable 'a') |> equal (Nullable 'a')
    Nullable.char (Nullable<int>()) |> equal (Nullable<char>())

[<Fact>]
let ``test Nullable.decimal works`` () =
    Nullable.decimal (Nullable 5) |> equal (Nullable 5m)
    Nullable.decimal (Nullable 7L) |> equal (Nullable 7m)
    Nullable.decimal (Nullable 10.5) |> equal (Nullable 10.5m)
    Nullable.decimal (Nullable<float>()) |> equal (Nullable<decimal>())

[<Fact>]
let ``test Nullable.enum works`` () =
    (Nullable.enum (Nullable 2): Nullable<DayOfWeek>) |> equal (Nullable DayOfWeek.Tuesday)
    (Nullable.enum (Nullable<int>()): Nullable<DayOfWeek>) |> equal (Nullable<DayOfWeek>())

[<Fact>]
let ``test Nullable.float and Nullable.float32 work`` () =
    Nullable.float (Nullable 1uy) |> equal (Nullable 1.0)
    Nullable.float (Nullable 5) |> equal (Nullable 5.0)
    Nullable.float (Nullable 7L) |> equal (Nullable 7.0)
    Nullable.float (Nullable 9m) |> equal (Nullable 9.0)
    Nullable.float (Nullable 'c') |> equal (Nullable 99.0)
    Nullable.float (Nullable<int>()) |> equal (Nullable<float>())
    Nullable.float32 (Nullable 5) |> equal (Nullable 5.0f)
    Nullable.float32 (Nullable 10.0) |> equal (Nullable 10.0f)
    Nullable.float32 (Nullable<int>()) |> equal (Nullable<float32>())
    Nullable.double (Nullable 5) |> equal (Nullable 5.0)
    Nullable.single (Nullable 9.5) |> equal (Nullable 9.5f)

[<Fact>]
let ``test Nullable integer conversions work`` () =
    Nullable.int (Nullable 1uy) |> equal (Nullable 1)
    Nullable.int (Nullable 7L) |> equal (Nullable 7)
    Nullable.int (Nullable 9m) |> equal (Nullable 9)
    Nullable.int (Nullable 10.9) |> equal (Nullable 10)
    Nullable.int (Nullable -11.9) |> equal (Nullable -11)
    Nullable.int (Nullable 'c') |> equal (Nullable 99)
    Nullable.int (Nullable<float>()) |> equal (Nullable<int>())
    Nullable.int16 (Nullable -7L) |> equal (Nullable -7s)
    Nullable.int32 (Nullable 5L) |> equal (Nullable 5)
    Nullable.int64 (Nullable -9.9) |> equal (Nullable -9L)
    Nullable.int64 (Nullable<int>()) |> equal (Nullable<int64>())
    Nullable.int8 (Nullable 9.9) |> equal (Nullable 9y)
    Nullable.sbyte (Nullable -7.9) |> equal (Nullable -7y)
    Nullable.byte (Nullable 5) |> equal (Nullable 5uy)
    Nullable.uint8 (Nullable 7L) |> equal (Nullable 7uy)
    Nullable.uint16 (Nullable 9.9) |> equal (Nullable 9us)
    Nullable.uint (Nullable 5) |> equal (Nullable 5u)
    Nullable.uint32 (Nullable 7L) |> equal (Nullable 7u)
    Nullable.uint64 (Nullable<int>()) |> equal (Nullable<uint64>())

[<Fact>]
let ``test Nullable conversions evaluate their argument once`` () =
    let mutable calls = 0
    let next () =
        calls <- calls + 1
        Nullable 5
    Nullable.float (next ()) |> equal (Nullable 5.0)
    calls |> equal 1
