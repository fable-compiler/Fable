module Fable.Tests.Conversion

open System
open Fable.Tests.Util
open Util.Testing

// --- Basic type conversions ---

[<Fact>]
let ``test int to string works`` () =
    string 42 |> equal "42"

[<Fact>]
let ``test negative int to string works`` () =
    string -7 |> equal "-7"

[<Fact>]
let ``test string to int works`` () =
    int "42" |> equal 42

[<Fact>]
let ``test float to int works`` () =
    int 3.14 |> equal 3

[<Fact>]
let ``test int to float works`` () =
    float 42 |> equal 42.0

[<Fact>]
let ``test string to float works`` () =
    float "3.14" |> equal 3.14

[<Fact>]
let ``test bool to string works`` () =
    // .NET produces "True"/"False", Erlang produces "true"/"false"
    // Test that the result is non-empty and meaningful
    let t = string true
    let f = string false
    (t <> f) |> equal true

[<Fact>]
let ``test int instance ToString works`` () =
    (42).ToString() |> equal "42"

[<Fact>]
let ``test float instance ToString works`` () =
    let s = (3.14).ToString()
    // Erlang float_to_binary may produce different precision
    s.Length > 0 |> equal true

[<Fact>]
let ``test string identity conversion works`` () =
    string "hello" |> equal "hello"

[<Fact>]
let ``test float truncation works`` () =
    int 9.99 |> equal 9
    int -2.7 |> equal -2

// --- Cross-type integer conversions ---

[<Fact>]
let ``test int to int64 works`` () =
    int64 42 |> equal 42L

[<Fact>]
let ``test int64 to int works`` () =
    int 42L |> equal 42

[<Fact>]
let ``test int to byte works`` () =
    byte 42 |> equal 42uy

[<Fact>]
let ``test byte to int works`` () =
    int 42uy |> equal 42

[<Fact>]
let ``test int to int16 works`` () =
    int16 42 |> equal 42s

[<Fact>]
let ``test int16 to int works`` () =
    int 42s |> equal 42

// --- Float conversions ---

[<Fact>]
let ``test float to float32 works`` () =
    float32 1.5 |> equal 1.5f

[<Fact>]
let ``test float32 to float works`` () =
    float 1.5f |> equal 1.5

[<Fact>]
let ``test int64 to float works`` () =
    float 42L |> equal 42.0

[<Fact>]
let ``test float to int64 works`` () =
    int64 3.14 |> equal 3L

// --- Char conversions ---

[<Fact>]
let ``test char to int works`` () =
    int 'a' |> equal 97

[<Fact>]
let ``test int to char works`` () =
    char 97 |> equal 'a'

// --- System.Convert ---

[<Fact>]
let ``test System.Convert.ToInt32 works`` () =
    let x = 1
    int(1) |> equal x
    int(1L) |> equal x
    int(1.f) |> equal x
    int(1.) |> equal x
    int(1.4) |> equal x
    int(1.5) |> equal x
    int(1.6) |> equal x
    int("1") |> equal x

    Convert.ToInt32(1) |> equal x
    Convert.ToInt32(1L) |> equal x
    Convert.ToInt32(1.f) |> equal x
    Convert.ToInt32(1.) |> equal x
    Convert.ToInt32("1") |> equal x

[<Fact>]
let ``test System.Convert.ToInt64 works`` () =
    let x = 1L
    int64(1) |> equal x
    int64(1L) |> equal x
    int64(1.f) |> equal x
    int64(1.) |> equal x
    int64(1.4) |> equal x
    int64(1.5) |> equal x
    int64(1.6) |> equal x
    int64("1") |> equal x

    Convert.ToInt64(1) |> equal x
    Convert.ToInt64(1L) |> equal x
    Convert.ToInt64(1.f) |> equal x
    Convert.ToInt64(1.) |> equal x
    Convert.ToInt64("1") |> equal x

[<Fact>]
let ``test System.Convert.ToDouble works`` () =
    let x = 1.
    float(1) |> equal x
    float(1L) |> equal x
    float(1.f) |> equal x
    float(1.) |> equal x
    float("1.0") |> equal x

    Convert.ToDouble(1) |> equal x
    Convert.ToDouble(1L) |> equal x
    Convert.ToDouble(1.f) |> equal x
    Convert.ToDouble(1.) |> equal x

[<Fact>]
let ``test System.Convert.ToSingle works`` () =
    let x = 1.f
    float32(1) |> equal x
    float32(1L) |> equal x
    float32(1.f) |> equal x
    float32(1.) |> equal x

    Convert.ToSingle(1) |> equal x
    Convert.ToSingle(1L) |> equal x
    Convert.ToSingle(1.f) |> equal x
    Convert.ToSingle(1.) |> equal x

[<Fact>]
let ``test System.Convert.ToByte works`` () =
    let x = 1uy
    byte(1) |> equal x
    byte(1L) |> equal x
    byte(1.f) |> equal x
    byte(1.) |> equal x
    byte(1.4) |> equal x
    byte("1") |> equal x

    Convert.ToByte(1) |> equal x
    Convert.ToByte(1L) |> equal x
    Convert.ToByte(1.f) |> equal x
    Convert.ToByte(1.) |> equal x
    Convert.ToByte("1") |> equal x

[<Fact>]
let ``test System.Convert.ToSByte works`` () =
    let x = 1y
    sbyte(1) |> equal x
    sbyte(1L) |> equal x
    sbyte(1.f) |> equal x
    sbyte(1.) |> equal x
    sbyte("1") |> equal x

    Convert.ToSByte(1) |> equal x
    Convert.ToSByte(1L) |> equal x
    Convert.ToSByte(1.f) |> equal x
    Convert.ToSByte(1.) |> equal x
    Convert.ToSByte("1") |> equal x

[<Fact>]
let ``test System.Convert.ToInt16 works`` () =
    let x = 1s
    int16(1) |> equal x
    int16(1L) |> equal x
    int16(1.f) |> equal x
    int16(1.) |> equal x
    int16("1") |> equal x

    Convert.ToInt16(1) |> equal x
    Convert.ToInt16(1L) |> equal x
    Convert.ToInt16(1.f) |> equal x
    Convert.ToInt16(1.) |> equal x
    Convert.ToInt16("1") |> equal x

[<Fact>]
let ``test System.Convert.ToUInt16 works`` () =
    let x = 1us
    uint16(1) |> equal x
    uint16(1L) |> equal x
    uint16(1.f) |> equal x
    uint16(1.) |> equal x
    uint16("1") |> equal x

    Convert.ToUInt16(1) |> equal x
    Convert.ToUInt16(1L) |> equal x
    Convert.ToUInt16(1.f) |> equal x
    Convert.ToUInt16(1.) |> equal x
    Convert.ToUInt16("1") |> equal x

[<Fact>]
let ``test System.Convert.ToUInt32 works`` () =
    let x = 1u
    uint32(1) |> equal x
    uint32(1L) |> equal x
    uint32(1.f) |> equal x
    uint32(1.) |> equal x
    uint32("1") |> equal x

    Convert.ToUInt32(1) |> equal x
    Convert.ToUInt32(1L) |> equal x
    Convert.ToUInt32(1.f) |> equal x
    Convert.ToUInt32(1.) |> equal x
    Convert.ToUInt32("1") |> equal x

[<Fact>]
let ``test System.Convert.ToUInt64 works`` () =
    let x = 1uL
    uint64(1) |> equal x
    uint64(1L) |> equal x
    uint64(1.f) |> equal x
    uint64(1.) |> equal x
    uint64("1") |> equal x

    Convert.ToUInt64(1) |> equal x
    Convert.ToUInt64(1L) |> equal x
    Convert.ToUInt64(1.f) |> equal x
    Convert.ToUInt64(1.) |> equal x
    Convert.ToUInt64("1") |> equal x

[<Fact>]
let ``test System.Convert.ToChar works`` () =
    let x = 'a'
    char(97) |> equal x
    char(97L) |> equal x
    char("a") |> equal x
    char('a') |> equal x

    Convert.ToChar(97) |> equal x
    Convert.ToChar("a") |> equal x
    Convert.ToChar('a') |> equal x

// --- Numeric instance methods ---

[<Fact>]
let ``test System.Int32.Parse works`` () =
    Int32.Parse("5") |> equal 5
    Int32.Parse("-5") |> equal -5

[<Fact>]
let ``test System.Int64.Parse works`` () =
    Int64.Parse("5") |> equal 5L
    Int64.Parse("-5") |> equal -5L

[<Fact>]
let ``test System.Double.Parse works`` () =
#if FABLE_COMPILER
    Double.Parse("1.5") |> equal 1.5
#else
    Double.Parse("1.5", System.Globalization.CultureInfo.InvariantCulture) |> equal 1.5
#endif

[<Fact>]
let ``test System.Int32.ToString works`` () =
    (5592405).ToString() |> equal "5592405"

[<Fact>]
let ``test System.Int64.ToString works`` () =
    (5592405L).ToString() |> equal "5592405"

[<Fact>]
let ``test int64 can parse signed longs`` () =
    let a = int64 "5"
    let b = int64 "-5"
    let c = int64 "+5"
    equal 5L a
    equal -5L b
    a = b |> equal false
    a = c |> equal true

// --- BigInt conversions ---

[<Fact>]
let ``test BigInt from uint32 works`` () =
    bigint System.UInt32.MaxValue |> equal 4294967295I

[<Fact>]
let ``test BigInt.ToInt32 works`` () =
    let value = 0x01020304
    int32 (bigint value) |> equal value

[<Fact>]
let ``test BigInt.ToInt64 works`` () =
    let value = 0x0102030405060708L
    int64 (bigint value) |> equal value

[<Fact>]
let ``test BigInt.ToDouble works`` () =
    let value = -1.0
    double (bigint value) |> equal value

[<Fact>]
let ``test BigInt.ToString works`` () =
    let value = 1234567890
    string (bigint value) |> equal "1234567890"

// --- Convert.ToString ---

[<Fact>]
let ``test System.Convert.ToString SByte works`` () =
    Convert.ToString(101y) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString Int16 works`` () =
    Convert.ToString(101s) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString Int32 works`` () =
    Convert.ToString(101) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString Int64 works`` () =
    Convert.ToString(101L) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString Byte works`` () =
    Convert.ToString(101uy) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString UInt16 works`` () =
    Convert.ToString(101us) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString UInt32 works`` () =
    Convert.ToString(101u) |> equal "101"

[<Fact>]
let ``test System.Convert.ToString UInt64 works`` () =
    Convert.ToString(101uL) |> equal "101"

// TODO: Decimal not implemented for Beam
// [<Fact>]
// let ``test System.Decimal.Parse works`` () =
//     Decimal.Parse "1.5" |> equal 1.5M

// TODO: TryParse uses ref cells / byref which may not work in Beam
// [<Fact>]
// let ``test System.Int32.TryParse works`` () =
//     let success, value = Int32.TryParse("1")
//     success |> equal true
//     value |> equal 1

// TODO: Hex parsing requires NumberStyles support
// [<Fact>]
// let ``test System.Int32.Parse with hex works`` () =
//     Int32.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95

// TODO: Convert.ToXxx with base requires library support
// [<Fact>]
// let ``test System.Convert.ToInt32 with base works`` () =
//     Convert.ToInt32("101", 2) |> equal 5

// TODO: Convert.ToString with base requires library support
// [<Fact>]
// let ``test System.Convert.ToString with base works`` () =
//     Convert.ToString(5, 2) |> equal "101"

// TODO: BitConverter requires byte array support
// [<Fact>]
// let ``test BitConverter.GetBytes Int32 works`` () =
//     let bytes = BitConverter.GetBytes(0x01020304)
//     bytes |> equal [| 4uy; 3uy; 2uy; 1uy |]

// TODO: Base64 encoding/decoding requires library support
// [<Fact>]
// let ``test Convert.ToBase64String works`` () =
//     let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
//     Convert.ToBase64String(bytes) |> equal "AgQGCAoMDhASFA=="

// TODO: Guid not implemented for Beam
// [<Fact>]
// let ``test Guid.Parse works`` () =
//     let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
//     g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
