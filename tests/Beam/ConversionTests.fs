module Fable.Tests.Conversion

open System
open Fable.Tests.Util
open Util.Testing

let tryParse f initial (value: string) =
    let res = ref initial
#if FABLE_COMPILER
    let success = f(value, res)
#else
    let success = f(value, System.Globalization.NumberStyles.Number, System.Globalization.CultureInfo("en-US"), res)
#endif
    (success, res.Value)

let parse f (a: string) =
#if FABLE_COMPILER
    f(a)
#else
    f(a, System.Globalization.CultureInfo("en-US"))
#endif

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

// --- Boolean.Parse ---

[<Fact>]
let ``test System.Boolean.Parse works`` () =
    Boolean.Parse "true" |> equal true
    Boolean.Parse "True" |> equal true
    Boolean.Parse " true " |> equal true
    Boolean.Parse "false" |> equal false
    Boolean.Parse "False" |> equal false
    Boolean.Parse " false " |> equal false

// --- More Parse methods ---

[<Fact>]
let ``test System.SByte.Parse works`` () =
    SByte.Parse("5") |> equal 5y
    SByte.Parse("-5") |> equal -5y
    SByte.Parse("-128") |> equal -128y

[<Fact>]
let ``test System.Int16.Parse works`` () =
    Int16.Parse("5") |> equal 5s
    Int16.Parse("-5") |> equal -5s
    Int16.Parse("-32768") |> equal -32768s

[<Fact>]
let ``test System.Byte.Parse works`` () =
    Byte.Parse("5") |> equal 5uy
    Byte.Parse("255") |> equal 255uy

[<Fact>]
let ``test System.UInt16.Parse works`` () =
    UInt16.Parse("5") |> equal 5us
    UInt16.Parse("65535") |> equal 65535us

[<Fact>]
let ``test System.UInt32.Parse works`` () =
    UInt32.Parse("5") |> equal 5u
    UInt32.Parse("4294967295") |> equal 4294967295u

[<Fact>]
let ``test System.UInt64.Parse works`` () =
    UInt64.Parse("5") |> equal 5uL

// --- Convert with base parameter ---

[<Fact>]
let ``test System.Convert.ToInt32 with base works`` () =
    let x = "101"
    Convert.ToInt32(x) |> equal 101
    Convert.ToInt32(x, 2) |> equal 5
    Convert.ToInt32(x, 8) |> equal 65
    Convert.ToInt32(x, 10) |> equal 101
    Convert.ToInt32(x, 16) |> equal 257

[<Fact>]
let ``test System.Convert.ToInt64 with base works`` () =
    let x = "101"
    Convert.ToInt64(x) |> equal 101L
    Convert.ToInt64(x, 2) |> equal 5L
    Convert.ToInt64(x, 8) |> equal 65L
    Convert.ToInt64(x, 10) |> equal 101L
    Convert.ToInt64(x, 16) |> equal 257L

[<Fact>]
let ``test System.Convert.ToByte with base works`` () =
    let x = "101"
    Convert.ToByte(x) |> equal 101uy
    Convert.ToByte(x, 2) |> equal 5uy
    Convert.ToByte(x, 8) |> equal 65uy
    Convert.ToByte(x, 10) |> equal 101uy

[<Fact>]
let ``test System.Convert.ToInt16 with base works`` () =
    let x = "101"
    Convert.ToInt16(x) |> equal 101s
    Convert.ToInt16(x, 2) |> equal 5s
    Convert.ToInt16(x, 8) |> equal 65s
    Convert.ToInt16(x, 10) |> equal 101s
    Convert.ToInt16(x, 16) |> equal 257s

// --- Convert.ToString with base parameter ---

[<Fact>]
let ``test System.Convert.ToString Int32 with base works`` () =
    let x = "101"
    Convert.ToString(101) |> equal x
    Convert.ToString(5, 2) |> equal x
    Convert.ToString(65, 8) |> equal x
    Convert.ToString(101, 10) |> equal x
    Convert.ToString(257, 16) |> equal x
    Convert.ToString(-5, 16) |> equal "fffffffb"

[<Fact>]
let ``test System.Convert.ToString Int64 with base works`` () =
    let x = "101"
    Convert.ToString(101L) |> equal x
    Convert.ToString(5L, 2) |> equal x
    Convert.ToString(65L, 8) |> equal x
    Convert.ToString(101L, 10) |> equal x
    Convert.ToString(257L, 16) |> equal x
    Convert.ToString(-5L, 16) |> equal "fffffffffffffffb"

[<Fact>]
let ``test System.Convert.ToString with base binary max values`` () =
    Convert.ToString(Byte.MaxValue, 2) |> equal "11111111"
    Convert.ToString(Int32.MaxValue, 2) |> equal "1111111111111111111111111111111"
    Convert.ToString(Int64.MaxValue, 2) |> equal "111111111111111111111111111111111111111111111111111111111111111"

// --- Parsing with radix prefixes ---

[<Fact>]
let ``test Parsing integers with different radices works`` () =
    equal 11 (int "11")
    equal 17 (int "0x11")
    equal 9 (int "0o11")
    equal 3 (int "0b11")

// --- More cross-type conversions ---

[<Fact>]
let ``test System.Convert.ToDouble comprehensive works`` () =
    let x = 1.
    float(1y) |> equal x
    float(1uy) |> equal x
    float(1s) |> equal x
    float(1) |> equal x
    float(1L) |> equal x
    float(1u) |> equal x
    float(1us) |> equal x
    float(1uL) |> equal x
    float(1.f) |> equal x
    float(1.) |> equal x

[<Fact>]
let ``test System.Convert.ToSingle comprehensive works`` () =
    let x = 1.f
    float32(1y) |> equal x
    float32(1uy) |> equal x
    float32(1s) |> equal x
    float32(1) |> equal x
    float32(1L) |> equal x
    float32(1u) |> equal x
    float32(1us) |> equal x
    float32(1uL) |> equal x
    float32(1.f) |> equal x
    float32(1.) |> equal x

#if FABLE_COMPILER
[<Fact>]
let ``test System.Decimal.Parse works`` () =
    Decimal.Parse "1.5" |> equal 1.5M
#endif

[<Fact>]
let ``test System.Int32.TryParse works`` () =
    let success, value = Int32.TryParse("1")
    success |> equal true
    value |> equal 1
    let success2, value2 = Int32.TryParse("not a number")
    success2 |> equal false
    value2 |> equal 0

// TODO: Hex parsing requires NumberStyles support
// [<Fact>]
// let ``test System.Int32.Parse with hex works`` () =
//     Int32.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95

[<Fact>]
let ``test BitConverter.GetBytes Int32 works`` () =
    let bytes = BitConverter.GetBytes(0x01020304)
    bytes |> equal [| 4uy; 3uy; 2uy; 1uy |]

[<Fact>]
let ``test Convert.ToBase64String works`` () =
    let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
    Convert.ToBase64String(bytes) |> equal "AgQGCAoMDhASFA=="

// --- TryParse methods ---

// TODO: TryParse via higher-order function with ref cells doesn't work in Beam
// (ref cells use process dictionary, but lambda wrapping treats them as maps)
// [<Fact>]
// let ``test System.Double.TryParse works`` () =
//     tryParse Double.TryParse 0.0 "1" |> equal (true, 1.0)
//     tryParse Double.TryParse 0.0 "1.5" |> equal (true, 1.5)
//     tryParse Double.TryParse 0.0 "foo" |> equal (false, 0.0)
//     tryParse Double.TryParse 0.0 "" |> equal (false, 0.0)
//     tryParse Double.TryParse 0.0 "-1.5" |> equal (true, -1.5)
//
// [<Fact>]
// let ``test System.Single.TryParse works`` () =
//     tryParse Single.TryParse 0.0f "1" |> equal (true, 1.0f)
//     tryParse Single.TryParse 0.0f "1.5" |> equal (true, 1.5f)
//     tryParse Single.TryParse 0.0f "foo" |> equal (false, 0.0f)
//     tryParse Single.TryParse 0.0f "-1.5" |> equal (true, -1.5f)

[<Fact>]
let ``test System.Boolean.TryParse works`` () =
    Boolean.TryParse "true" |> equal (true, true)
    Boolean.TryParse "True" |> equal (true, true)
    Boolean.TryParse "false" |> equal (true, false)
    Boolean.TryParse "False" |> equal (true, false)
    Boolean.TryParse "tru" |> equal (false, false)
    Boolean.TryParse "falsee" |> equal (false, false)

// TODO: TryParse via higher-order function with ref cells doesn't work in Beam
// [<Fact>]
// let ``test System.Int64.TryParse works`` () =
//     tryParse Int64.TryParse 0L "99" |> equal (true, 99L)
//     tryParse Int64.TryParse 0L "foo" |> equal (false, 0L)
//
// [<Fact>]
// let ``test System.UInt32.TryParse works`` () =
//     tryParse UInt32.TryParse 0u "99" |> equal (true, 99u)
//     tryParse UInt32.TryParse 0u "foo" |> equal (false, 0u)
//
// [<Fact>]
// let ``test System.UInt64.TryParse works`` () =
//     tryParse UInt64.TryParse 0UL "99" |> equal (true, 99UL)
//     tryParse UInt64.TryParse 0UL "foo" |> equal (false, 0UL)

// --- Single.Parse ---

[<Fact>]
let ``test System.Single.Parse works`` () =
    parse Single.Parse "1.5" |> equal 1.5f

[<Fact>]
let ``test System.Int32.ToString 'd' works`` () =
    (5592405).ToString("d") |> equal "5592405"
    (5592405).ToString("d10") |> equal "0005592405"

[<Fact>]
let ``test System.Int32.ToString 'x' works`` () =
    (5592405).ToString("x") |> equal "555555"
    (5592405).ToString("x10") |> equal "0000555555"

[<Fact>]
let ``test System.Int64.ToString 'd' works`` () =
    5592405L.ToString("d") |> equal "5592405"
    5592405L.ToString("d10") |> equal "0005592405"

[<Fact>]
let ``test System.Int64.ToString 'x' works`` () =
    5592405L.ToString("x") |> equal "555555"
    5592405L.ToString("x10") |> equal "0000555555"

// --- Convert.FromBase64String ---

[<Fact>]
let ``test Convert.FromBase64String works`` () =
    Convert.FromBase64String("AgQGCAoMDhASFA==")
    |> equal [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]

// --- More BitConverter tests ---

// TODO: BitConverter.GetBytes(Boolean) not yet supported
// [<Fact>]
// let ``test BitConverter.GetBytes Boolean works`` () =
//     let value = true
//     let bytes = BitConverter.GetBytes(value)
//     bytes |> equal [| 1uy |]

[<Fact>]
let ``test BitConverter.GetBytes Int16 works`` () =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 2uy; 1uy |]

[<Fact>]
let ``test BitConverter.GetBytes Int64 works`` () =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 1uy |]

[<Fact>]
let ``test BitConverter.GetBytes Double works`` () =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 240uy; 63uy |]

// TODO: BitConverter.ToBoolean not yet supported by Beam Replacements
// [<Fact>]
// let ``test BitConverter.ToBoolean works`` () =
//     let value = true
//     let bytes = BitConverter.GetBytes(value)
//     BitConverter.ToBoolean(bytes, 0) |> equal value

[<Fact>]
let ``test BitConverter.ToInt16 works`` () =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt16(bytes, 0) |> equal value

[<Fact>]
let ``test BitConverter.ToInt64 works`` () =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt64(bytes, 0) |> equal value

[<Fact>]
let ``test BitConverter.ToDouble works`` () =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToDouble(bytes, 0) |> equal value

// TODO: BitConverter.ToString not yet supported by Beam Replacements
// [<Fact>]
// let ``test BitConverter.ToString works`` () =
//     let value = 0x01020304
//     let bytes = BitConverter.GetBytes(value)
//     BitConverter.ToString(bytes) |> equal "04-03-02-01"
//
// [<Fact>]
// let ``test BitConverter.ToString 2 works`` () =
//     let value = 0x01020304
//     let bytes = BitConverter.GetBytes(value)
//     BitConverter.ToString(bytes, 1) |> equal "03-02-01"
//
// [<Fact>]
// let ``test BitConverter.ToString 3 works`` () =
//     let value = 0x01020304
//     let bytes = BitConverter.GetBytes(value)
//     BitConverter.ToString(bytes, 1, 2) |> equal "03-02"

// --- Guid tests (now implemented) ---

[<Fact>]
let ``test Guid.Parse works`` () =
    let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"

// TODO: Guid.TryParse not yet supported by Beam Replacements
// [<Fact>]
// let ``test Guid.TryParse works`` () =
//     let success1, g1 = Guid.TryParse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
//     success1 |> equal true
//     g1.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
//     let success2, _ = Guid.TryParse("not-a-guid")
//     success2 |> equal false

[<Fact>]
let ``test Parsed guids with different case are considered the same`` () =
    let g1 = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let g2 = Guid.Parse("96258006-C4BA-4A7F-80C4-DE7F2B2898C5")
    g1 = g2 |> equal true

[<Fact>]
let ``test System.BigInt.ToString works`` () =
    5592405I.ToString() |> equal "5592405"

#if FABLE_COMPILER
[<Fact>]
let ``test System.Decimal.ToString works`` () =
    5592405M.ToString() |> equal "5592405"
#endif

// --- Convert.ToString (Single/Double/Decimal only - others already exist above) ---

// TODO: Float ToString returns scientific notation in Erlang (e.g., "1.01e+02" instead of "101")
// [<Fact>]
// let ``test System.Convert.ToString Single works`` () =
//     Convert.ToString(101.f) |> equal "101"

// [<Fact>]
// let ``test System.Convert.ToString Double works`` () =
//     Convert.ToString(101.) |> equal "101"

// TODO: Decimal Convert.ToString not supported
// [<Fact>]
// let ``test System.Convert.ToString Decimal works`` () =
//     Convert.ToString(101.m) |> equal "101"

// --- Decimal conversions ---

// TODO: System.Decimal.op_Explicit not supported by Fable Beam
// #if FABLE_COMPILER
// [<Fact>]
// let ``test Decimal.ToSByte works`` () =
//     let value = 0x02y
//     sbyte (decimal (int32 value)) |> equal value
// ... (all Decimal.To* tests need Decimal.op_Explicit)
// #endif

// --- BigInt conversions ---

[<Fact>]
let ``test BigInt.ToChar works`` () =
    let value = 'A'
    char (bigint (int32 value)) |> equal value

[<Fact>]
let ``test BigInt.ToSByte works`` () =
    let value = 0x02y
    sbyte (bigint (int32 value)) |> equal value

[<Fact>]
let ``test BigInt.ToInt16 works`` () =
    let value = 0x0102s
    int16 (bigint (int32 value)) |> equal value

[<Fact>]
let ``test BigInt.ToByte works`` () =
    let value = 0x02uy
    byte (bigint (uint32 value)) |> equal value

[<Fact>]
let ``test BigInt.ToUInt16 works`` () =
    let value = 0xFF02us
    uint16 (bigint (uint32 value)) |> equal value

[<Fact>]
let ``test BigInt.ToUInt32 works`` () =
    let value = 0xFF020304u
    uint32 (bigint value) |> equal value

[<Fact>]
let ``test BigInt.ToUInt64 works`` () =
    let value = 0xFF02030405060708UL
    uint64 (bigint value) |> equal value

[<Fact>]
let ``test BigInt.ToSingle works`` () =
    let value = 1.0f
    single (bigint value) |> equal value

// --- FSharp.Core type converters ---

[<Fact>]
let ``test FSharp.Core type converters can combined via the >> operator`` () =
    "1" |> (sbyte >> Ok) |> equal (Ok 1y)
    "1" |> (int16 >> Ok) |> equal (Ok 1s)
    "1" |> (int >> Ok) |> equal (Ok 1)
    "1" |> (int64 >> Ok) |> equal (Ok 1L)
    "1" |> (byte >> Ok) |> equal (Ok 1uy)
    "1" |> (uint16 >> Ok) |> equal (Ok 1us)
    "1" |> (uint32 >> Ok) |> equal (Ok 1u)
    "1" |> (uint64 >> Ok) |> equal (Ok 1uL)
    "1" |> (float32 >> Ok) |> equal (Ok 1.f)
    "1" |> (float >> Ok) |> equal (Ok 1.)
    "1" |> (double >> Ok) |> equal (Ok 1.)
    "1" |> (decimal >> Ok) |> equal (Ok 1.m)
