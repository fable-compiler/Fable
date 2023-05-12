module Fable.Tests.ConvertTests

open System
open System.Globalization
open Util.Testing
open Fable.Tests.Util

//-------------------------------------
// Parse and TryParse
//-------------------------------------

[<Fact>]
let ``System.Double.Parse works`` () =
    Double.Parse "1.5" |> equal 1.5

[<Fact>]
let ``System.Double.TryParse works`` () =
    Double.TryParse "1" |> equal (true, 1.0)
    Double.TryParse "    1     " |> equal (true, 1.0)
    Double.TryParse "1.5" |> equal (true, 1.5)
    Double.TryParse "    1.5     " |> equal (true, 1.5)
    Double.TryParse "foo" |> equal (false, 0.0)
    Double.TryParse "" |> equal (false, 0.0)
    Double.TryParse "      " |> equal (false, 0.0)
    Double.TryParse "9X" |> equal (false, 0.0)
    Double.TryParse "X9" |> equal (false, 0.0)
    Double.TryParse "X9TRE34" |> equal (false, 0.0)
    Double.TryParse "9SayWhat12Huh" |> equal (false, 0.0)
    Double.TryParse "-1.5" |> equal (true, -1.5)

[<Fact>]
let ``System.Decimal.Parse works`` () =
    Decimal.Parse "1.5" |> equal 1.5M

[<Fact>]
let ``System.Decimal.TryParse works`` () =
    Decimal.TryParse "1" |> equal (true, 1.0M)
    Decimal.TryParse "    1     " |> equal (true, 1.0M)
    Decimal.TryParse "1.5" |> equal (true, 1.5M)
    Decimal.TryParse "    1.5     " |> equal (true, 1.5M)
    Decimal.TryParse "foo" |> equal (false, 0.0M)
    Decimal.TryParse "9X" |> equal (false, 0.0M)
    Decimal.TryParse "X9" |> equal (false, 0.0M)
    Decimal.TryParse "X9TRE34" |> equal (false, 0.0M)
    Decimal.TryParse "9SayWhat12Huh" |> equal (false, 0.0M)
    Decimal.TryParse "-1.5" |> equal (true, -1.5M)

[<Fact>]
let ``System.Single.Parse works`` () =
    Single.Parse "1.5" |> equal 1.5f

[<Fact>]
let ``System.Single.TryParse works`` () =
    Single.TryParse "1" |> equal (true, 1.0f)
    Single.TryParse "    1     " |> equal (true, 1.0f)
    Single.TryParse "1.5" |> equal (true, 1.5f)
    Single.TryParse "    1.5     " |> equal (true, 1.5f)
    Single.TryParse "foo" |> equal (false, 0.0f)
    Single.TryParse "9X" |> equal (false, 0.0f)
    Single.TryParse "X9" |> equal (false, 0.0f)
    Single.TryParse "X9TRE34" |> equal (false, 0.0f)
    Single.TryParse "9SayWhat12Huh" |> equal (false, 0.0f)
    Single.TryParse "-1.5" |> equal (true, -1.5f)

[<Fact>]
let ``System.Boolean.Parse works`` () =
    Boolean.Parse "true" |> equal true
    Boolean.Parse "True" |> equal true
    Boolean.Parse " true " |> equal true
    Boolean.Parse "false" |> equal false
    Boolean.Parse "False" |> equal false
    Boolean.Parse " false " |> equal false
    throwsAnyError (fun () -> Boolean.Parse "tru")
    throwsAnyError (fun () -> Boolean.Parse "falsee")

[<Fact>]
let ``System.Boolean.TryParse works`` () =
    Boolean.TryParse "true" |> equal (true, true)
    Boolean.TryParse "True" |> equal (true, true)
    Boolean.TryParse " true " |> equal (true, true)
    Boolean.TryParse "false" |> equal (true, false)
    Boolean.TryParse "False" |> equal (true, false)
    Boolean.TryParse " false " |> equal (true, false)
    Boolean.TryParse "tru" |> equal (false, false)
    Boolean.TryParse "falsee" |> equal (false, false)

[<Fact>]
let ``System.SByte.Parse works`` () =
    SByte.Parse("5") |> equal 5y
    SByte.Parse("-5") |> equal -5y
    SByte.Parse("-128") |> equal -128y
    throwsAnyError (fun () -> SByte.Parse("128"))
    throwsAnyError (fun () -> SByte.Parse("5f"))
    throwsAnyError (fun () -> SByte.Parse("F"))
    throwsAnyError (fun () -> SByte.Parse("5o"))

[<Fact>]
let ``System.SByte.Parse with hex works`` () =
    SByte.Parse("55", NumberStyles.HexNumber) |> equal 85y
    SByte.Parse("5f", NumberStyles.HexNumber) |> equal 95y
    SByte.Parse("FF", NumberStyles.HexNumber) |> equal -1y
    throwsAnyError (fun () -> SByte.Parse("1FF", NumberStyles.HexNumber))
    throwsAnyError (fun () -> SByte.Parse("5o", NumberStyles.HexNumber))
    throwsAnyError (fun () -> SByte.Parse("o5", NumberStyles.HexNumber))

[<Fact>]
let ``System.Int16.Parse works`` () =
    Int16.Parse("5") |> equal 5s
    Int16.Parse("-5") |> equal -5s
    Int16.Parse("-32768") |> equal -32768s
    throwsAnyError (fun () -> Int16.Parse("32768"))
    throwsAnyError (fun () -> Int16.Parse("5f"))
    throwsAnyError (fun () -> Int16.Parse("FFF"))
    throwsAnyError (fun () -> Int16.Parse("5fo0"))

[<Fact>]
let ``System.Int16.Parse with hex works`` () =
    Int16.Parse("5555", NumberStyles.HexNumber) |> equal 21845s
    Int16.Parse("5f", NumberStyles.HexNumber) |> equal 95s
    Int16.Parse("FFFF", NumberStyles.HexNumber) |> equal -1s
    throwsAnyError (fun () -> Int16.Parse("1FFFF", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int16.Parse("5foo", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int16.Parse("foo5", NumberStyles.HexNumber))

[<Fact>]
let ``System.Int32.Parse works`` () =
    Int32.Parse("5") |> equal 5
    Int32.Parse("-5") |> equal -5
    Int32.Parse("-2147483648") |> equal -2147483648
    throwsAnyError (fun () -> Int32.Parse("2147483648"))
    throwsAnyError (fun () -> Int32.Parse("5f"))
    throwsAnyError (fun () -> Int32.Parse("f5"))
    throwsAnyError (fun () -> Int32.Parse("foo"))

[<Fact>]
let ``System.Int32.Parse with hex works`` () =
    Int32.Parse("555555", NumberStyles.HexNumber) |> equal 5592405
    Int32.Parse("5f", NumberStyles.HexNumber) |> equal 95
    Int32.Parse("FFFFFFFF", NumberStyles.HexNumber) |> equal -1
    throwsAnyError (fun () -> Int32.Parse("1FFFFFFFF", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int32.Parse("5foo", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int32.Parse("foo5", NumberStyles.HexNumber))

[<Fact>]
let ``System.Int64.Parse works`` () =
    Int64.Parse("5") |> equal 5L
    Int64.Parse("-5") |> equal -5L
    Int64.Parse("-9223372036854775808") |> equal -9223372036854775808L
    throwsAnyError (fun () -> Int64.Parse("9223372036854775808"))
    throwsAnyError (fun () -> Int64.Parse("5f"))
    throwsAnyError (fun () -> Int64.Parse("f5"))
    throwsAnyError (fun () -> Int64.Parse("foo"))

[<Fact>]
let ``System.Int64.Parse with hex works`` () =
    Int64.Parse("555555", NumberStyles.HexNumber) |> equal 5592405L
    Int64.Parse("5f", NumberStyles.HexNumber) |> equal 95L
    Int64.Parse("FFFFFFFFFFFFFFFF", NumberStyles.HexNumber) |> equal -1L
    throwsAnyError (fun () -> Int64.Parse("1FFFFFFFFFFFFFFFF", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int64.Parse("5foo", NumberStyles.HexNumber))
    throwsAnyError (fun () -> Int64.Parse("foo5", NumberStyles.HexNumber))

[<Fact>]
let ``System.Int64.TryParse works`` () =
    Int64.TryParse "99" |> equal (true, 99L)
    Int64.TryParse "foo" |> equal (false, 0L)

[<Fact>]
let ``System.UInt32.TryParse works`` () =
    UInt32.TryParse "99" |> equal (true, 99u)
    UInt32.TryParse "foo" |> equal (false, 0u)

[<Fact>]
let ``System.UInt64.TryParse works`` () =
    UInt64.TryParse "99" |> equal (true, 99UL)
    UInt64.TryParse "foo" |> equal (false, 0UL)

[<Fact>]
let ``Parsing integers with different radices works`` () =
    equal 11 (int "11")
    equal 17 (int "0x11")
    equal 9  (int "0o11")
    equal 3  (int "0b11")

[<Fact>]
let ``System.Int32.TryParse works`` () =
    Int32.TryParse "1" |> equal (true, 1)
    Int32.TryParse "    1     " |> equal (true, 1)
    Int32.TryParse "1.5" |> equal (false, 0)
    Int32.TryParse "    1.5     " |> equal (false, 0)
    Int32.TryParse "foo" |> equal (false, 0)
    Int32.TryParse "9X" |> equal (false, 0)
    Int32.TryParse "X9" |> equal (false, 0)
    Int32.TryParse "X9TRE34" |> equal (false, 0)
    Int32.TryParse "9SayWhat12Huh" |> equal (false, 0)
    Int32.TryParse "-1" |> equal (true, -1)

[<Fact>]
let ``BigInt.TryParse works`` () =
    bigint.TryParse "4234523548923954" |> equal (true, 4234523548923954I)
    bigint.TryParse "9SayWhat12Huh" |> equal (false, 0I)

//-------------------------------------
// ToString
//-------------------------------------

[<Fact>]
let ``System.SByte.ToString works`` () =
    (-55y).ToString() |> equal "-55"

[<Fact>]
let ``System.Byte.ToString works`` () =
    (205uy).ToString() |> equal "205"

[<Fact>]
let ``System.Int16.ToString works`` () =
    (25592s).ToString() |> equal "25592"

[<Fact>]
let ``System.UInt16.ToString works`` () =
    (45592us).ToString() |> equal "45592"

[<Fact>]
let ``System.Int32.ToString works`` () =
    (5592405).ToString() |> equal "5592405"

[<Fact>]
let ``System.UInt32.ToString works`` () =
    (5592405u).ToString() |> equal "5592405"

[<Fact>]
let ``System.Int64.ToString works`` () =
    (5592405L).ToString() |> equal "5592405"

[<Fact>]
let ``System.UInt64.ToString works`` () =
    (5592405UL).ToString() |> equal "5592405"

[<Fact>]
let ``System.Single.ToString works`` () =
    (5592405.f).ToString() |> equal "5592405"

[<Fact>]
let ``System.Double.ToString works`` () =
    (5592405.).ToString() |> equal "5592405"

[<Fact>]
let ``System.Decimal.ToString works`` () =
    (5592405M).ToString() |> equal "5592405"

//-------------------------------------
// ToString with format
//-------------------------------------

// [<Fact>]
// let ``System.Int32.ToString 'd' works`` () =
//     (5592405).ToString("d") |> equal "5592405"
//     (5592405).ToString("d10") |> equal "0005592405"

// [<Fact>]
// let ``System.Int32.ToString 'x' works`` () =
//     (5592405).ToString("x") |> equal "555555"
//     (5592405).ToString("x10") |> equal "0000555555"

// [<Fact>]
// let ``System.Int64.ToString 'd' works`` () =
//     (5592405L).ToString("d") |> equal "5592405"
//     (5592405L).ToString("d10") |> equal "0005592405"

// [<Fact>]
// let ``System.Int64.ToString 'x' works`` () =
//     (5592405L).ToString("x") |> equal "555555"
//     (5592405L).ToString("x10") |> equal "0000555555"

// [<Fact>]
// let ``System.BigInt.ToString works`` () =
//     (5592405I).ToString() |> equal "5592405"

//-------------------------------------
// System.Convert
//-------------------------------------

[<Fact>]
let ``System.Convert.ToSByte works`` () =
    let x = 1y
    sbyte(1y) |> equal x
    sbyte(1uy) |> equal x
    sbyte(257s) |> equal x
    sbyte(257) |> equal x
    sbyte(257L) |> equal x
    sbyte(257u) |> equal x
    sbyte(257us) |> equal x
    sbyte(257ul) |> equal x
    sbyte(257uL) |> equal x
    sbyte(1.f) |> equal x
    sbyte(1.) |> equal x
    sbyte(1.4) |> equal x
    sbyte(1.5) |> equal x
    sbyte(1.6) |> equal x
    sbyte(1.m) |> equal x
    sbyte("1") |> equal x

    Convert.ToSByte(1y) |> equal x
    Convert.ToSByte(1uy) |> equal x
    Convert.ToSByte(1s) |> equal x
    Convert.ToSByte(1) |> equal x
    Convert.ToSByte(1L) |> equal x
    Convert.ToSByte(1u) |> equal x
    Convert.ToSByte(1us) |> equal x
    Convert.ToSByte(1ul) |> equal x
    Convert.ToSByte(1uL) |> equal x
    Convert.ToSByte(1.f) |> equal x
    Convert.ToSByte(1.) |> equal x
    Convert.ToSByte(1.m) |> equal x
    Convert.ToSByte(1.4) |> equal x
    Convert.ToSByte(1.5) |> equal (x+x)
    Convert.ToSByte(1.6) |> equal (x+x)
    // Convert.ToSByte(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToSByte(2.6) |> equal (x+x+x)
    Convert.ToSByte(3.5) |> equal (x+x+x+x)
    Convert.ToSByte("1") |> equal x

[<Fact>]
let ``System.Convert.ToInt16 works`` () =
    let x = 1s
    int16(1y) |> equal x
    int16(1uy) |> equal x
    int16(1s) |> equal x
    int16(65537) |> equal x
    int16(65537L) |> equal x
    int16(65537u) |> equal x
    int16(1us) |> equal x
    int16(65537ul) |> equal x
    int16(65537uL) |> equal x
    int16(1.f) |> equal x
    int16(1.) |> equal x
    int16(1.4) |> equal x
    int16(1.5) |> equal x
    int16(1.6) |> equal x
    int16(1.m) |> equal x
    int16("1") |> equal x

    Convert.ToInt16(1y) |> equal x
    Convert.ToInt16(1uy) |> equal x
    Convert.ToInt16(1s) |> equal x
    Convert.ToInt16(1) |> equal x
    Convert.ToInt16(1L) |> equal x
    Convert.ToInt16(1u) |> equal x
    Convert.ToInt16(1us) |> equal x
    Convert.ToInt16(1ul) |> equal x
    Convert.ToInt16(1uL) |> equal x
    Convert.ToInt16(1.f) |> equal x
    Convert.ToInt16(1.) |> equal x
    Convert.ToInt16(1.m) |> equal x
    Convert.ToInt16(1.4) |> equal x
    Convert.ToInt16(1.5) |> equal (x+x)
    Convert.ToInt16(1.6) |> equal (x+x)
    // Convert.ToInt16(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToInt16(2.6) |> equal (x+x+x)
    Convert.ToInt16(3.5) |> equal (x+x+x+x)
    Convert.ToInt16("1") |> equal x

[<Fact>]
let ``System.Convert.ToInt32 works`` () =
    let x = 1
    int(1y) |> equal x
    int(1uy) |> equal x
    int(1s) |> equal x
    int(1) |> equal x
    int(1L) |> equal x
    int(1u) |> equal x
    int(1us) |> equal x
    int(1ul) |> equal x
    int(1uL) |> equal x
    int(1.f) |> equal x
    int(1.) |> equal x
    int(1.4) |> equal x
    int(1.5) |> equal x
    int(1.6) |> equal x
    int(1.m) |> equal x
    int("1") |> equal x

    Convert.ToInt32(1y) |> equal x
    Convert.ToInt32(1uy) |> equal x
    Convert.ToInt32(1s) |> equal x
    Convert.ToInt32(1) |> equal x
    Convert.ToInt32(1L) |> equal x
    Convert.ToInt32(1u) |> equal x
    Convert.ToInt32(1us) |> equal x
    Convert.ToInt32(1ul) |> equal x
    Convert.ToInt32(1uL) |> equal x
    Convert.ToInt32(1.f) |> equal x
    Convert.ToInt32(1.) |> equal x
    Convert.ToInt32(1.m) |> equal x
    Convert.ToInt32(1.4) |> equal x
    Convert.ToInt32(1.5) |> equal (x+x)
    Convert.ToInt32(1.6) |> equal (x+x)
    // Convert.ToInt32(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToInt32(2.6) |> equal (x+x+x)
    Convert.ToInt32(3.5) |> equal (x+x+x+x)
    Convert.ToInt32("1") |> equal x

[<Fact>]
let ``Special cases conversion to and from Int64 work`` () =
    let xn = -1L
    let xnu = 0xFFFFFFFFFFFFFFFFuL
    -1 |> uint64 |> equal xnu
    0xFFFFFFFFu |> int64 |> equal 0xFFFFFFFFL
    xn |> uint64 |> equal xnu
    xnu |> int64 |> equal -1L
    xn |> int32 |> equal -1
    xn |> uint32 |> equal 0xFFFFFFFFu
    xnu |> int32 |> equal -1
    xnu |> uint32 |> equal 0xFFFFFFFFu

[<Fact>]
let ``Special cases conversion to UInt64 work`` () = // See #1880
    uint64 "0x9fffffffffffffff" |> equal 11529215046068469759UL
    uint64 "0xafffffffffffffff" |> equal 12682136550675316735UL
    uint64 "0xAFFFFFFFFFFFFFFF" |> equal 12682136550675316735UL
    uint64 "0x9fffffff_ffffffff" |> equal 11529215046068469759UL
    uint64 "0x9fff_ffff_ffff_ffff" |> equal 11529215046068469759UL

[<Fact>]
let ``System.Convert.ToInt64 works`` () =
    let x = 1L
    int64(1y) |> equal x
    int64(1uy) |> equal x
    int64(1s) |> equal x
    int64(1) |> equal x
    int64(1L) |> equal x
    int64(1u) |> equal x
    int64(1us) |> equal x
    int64(1ul) |> equal x
    int64(1uL) |> equal x
    int64(1.f) |> equal x
    int64(1.) |> equal x
    int64(1.4) |> equal x
    int64(1.5) |> equal x
    int64(1.6) |> equal x
    int64(1.m) |> equal x
    int64("1") |> equal x

    Convert.ToInt64(1y) |> equal x
    Convert.ToInt64(1uy) |> equal x
    Convert.ToInt64(1s) |> equal x
    Convert.ToInt64(1) |> equal x
    Convert.ToInt64(1L) |> equal x
    Convert.ToInt64(1u) |> equal x
    Convert.ToInt64(1us) |> equal x
    Convert.ToInt64(1ul) |> equal x
    Convert.ToInt64(1uL) |> equal x
    Convert.ToInt64(1.f) |> equal x
    Convert.ToInt64(1.) |> equal x
    Convert.ToInt64(1.m) |> equal x
    Convert.ToInt64(1.4) |> equal x
    Convert.ToInt64(1.5) |> equal (x+x)
    Convert.ToInt64(1.6) |> equal (x+x)
    // Convert.ToInt64(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToInt64(2.6) |> equal (x+x+x)
    Convert.ToInt64(3.5) |> equal (x+x+x+x)
    Convert.ToInt64("1") |> equal x

[<Fact>]
let ``System.Convert.ToByte works`` () =
    let x = 1uy
    byte(1y) |> equal x
    byte(1uy) |> equal x
    byte(257s) |> equal x
    byte(257) |> equal x
    byte(257L) |> equal x
    byte(257u) |> equal x
    byte(257us) |> equal x
    byte(257ul) |> equal x
    byte(257uL) |> equal x
    byte(1.f) |> equal x
    byte(1.) |> equal x
    byte(1.4) |> equal x
    byte(1.5) |> equal x
    byte(1.6) |> equal x
    byte(1.m) |> equal x
    byte("1") |> equal x

    Convert.ToByte(1y) |> equal x
    Convert.ToByte(1uy) |> equal x
    Convert.ToByte(1s) |> equal x
    Convert.ToByte(1) |> equal x
    Convert.ToByte(1L) |> equal x
    Convert.ToByte(1u) |> equal x
    Convert.ToByte(1us) |> equal x
    Convert.ToByte(1ul) |> equal x
    Convert.ToByte(1uL) |> equal x
    Convert.ToByte(1.f) |> equal x
    Convert.ToByte(1.) |> equal x
    Convert.ToByte(1.m) |> equal x
    Convert.ToByte(1.4) |> equal x
    Convert.ToByte(1.5) |> equal (x+x)
    Convert.ToByte(1.6) |> equal (x+x)
    // Convert.ToByte(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToByte(2.6) |> equal (x+x+x)
    Convert.ToByte(3.5) |> equal (x+x+x+x)
    Convert.ToByte("1") |> equal x

[<Fact>]
let ``System.Convert.ToUInt16 works`` () =
    let x = 1us
    uint16(1y) |> equal x
    uint16(1uy) |> equal x
    uint16(1s) |> equal x
    uint16(65537) |> equal x
    uint16(65537L) |> equal x
    uint16(65537u) |> equal x
    uint16(1us) |> equal x
    uint16(65537ul) |> equal x
    uint16(65537uL) |> equal x
    uint16(1.f) |> equal x
    uint16(1.) |> equal x
    uint16(1.4) |> equal x
    uint16(1.5) |> equal x
    uint16(1.6) |> equal x
    uint16(1.m) |> equal x
    uint16("1") |> equal x

    Convert.ToUInt16(1y) |> equal x
    Convert.ToUInt16(1uy) |> equal x
    Convert.ToUInt16(1s) |> equal x
    Convert.ToUInt16(1) |> equal x
    Convert.ToUInt16(1L) |> equal x
    Convert.ToUInt16(1u) |> equal x
    Convert.ToUInt16(1us) |> equal x
    Convert.ToUInt16(1ul) |> equal x
    Convert.ToUInt16(1uL) |> equal x
    Convert.ToUInt16(1.f) |> equal x
    Convert.ToUInt16(1.) |> equal x
    Convert.ToUInt16(1.m) |> equal x
    Convert.ToUInt16(1.4) |> equal x
    Convert.ToUInt16(1.5) |> equal (x+x)
    Convert.ToUInt16(1.6) |> equal (x+x)
    // Convert.ToUInt16(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToUInt16(2.6) |> equal (x+x+x)
    Convert.ToUInt16(3.5) |> equal (x+x+x+x)
    Convert.ToUInt16("1") |> equal x

[<Fact>]
let ``System.Convert.ToUInt32 works`` () =
    let x = 1u
    uint32(1y) |> equal x
    uint32(1uy) |> equal x
    uint32(1s) |> equal x
    uint32(1) |> equal x
    uint32(1L) |> equal x
    uint32(1u) |> equal x
    uint32(1us) |> equal x
    uint32(1ul) |> equal x
    uint32(1uL) |> equal x
    uint32(1.f) |> equal x
    uint32(1.) |> equal x
    uint32(1.4) |> equal x
    uint32(1.5) |> equal x
    uint32(1.6) |> equal x
    uint32(1.m) |> equal x
    uint32("1") |> equal x

    Convert.ToUInt32(1y) |> equal x
    Convert.ToUInt32(1uy) |> equal x
    Convert.ToUInt32(1s) |> equal x
    Convert.ToUInt32(1) |> equal x
    Convert.ToUInt32(1L) |> equal x
    Convert.ToUInt32(1u) |> equal x
    Convert.ToUInt32(1us) |> equal x
    Convert.ToUInt32(1ul) |> equal x
    Convert.ToUInt32(1uL) |> equal x
    Convert.ToUInt32(1.f) |> equal x
    Convert.ToUInt32(1.) |> equal x
    Convert.ToUInt32(1.m) |> equal x
    Convert.ToUInt32(1.4) |> equal x
    Convert.ToUInt32(1.5) |> equal (x+x)
    Convert.ToUInt32(1.6) |> equal (x+x)
    // Convert.ToUInt32(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToUInt32(2.6) |> equal (x+x+x)
    Convert.ToUInt32(3.5) |> equal (x+x+x+x)
    Convert.ToUInt32("1") |> equal x

[<Fact>]
let ``System.Convert.ToUInt64 works`` () =
    let x = 1uL
    uint64(1y) |> equal x
    uint64(1uy) |> equal x
    uint64(1s) |> equal x
    uint64(1) |> equal x
    uint64(1L) |> equal x
    uint64(1u) |> equal x
    uint64(1us) |> equal x
    uint64(1ul) |> equal x
    uint64(1uL) |> equal x
    uint64(1.f) |> equal x
    uint64(1.) |> equal x
    uint64(1.4) |> equal x
    uint64(1.5) |> equal x
    uint64(1.6) |> equal x
    uint64(1.m) |> equal x
    uint64("1") |> equal x

    Convert.ToUInt64(1y) |> equal x
    Convert.ToUInt64(1uy) |> equal x
    Convert.ToUInt64(1s) |> equal x
    Convert.ToUInt64(1) |> equal x
    Convert.ToUInt64(1L) |> equal x
    Convert.ToUInt64(1u) |> equal x
    Convert.ToUInt64(1us) |> equal x
    Convert.ToUInt64(1ul) |> equal x
    Convert.ToUInt64(1uL) |> equal x
    Convert.ToUInt64(1.f) |> equal x
    Convert.ToUInt64(1.) |> equal x
    Convert.ToUInt64(1.m) |> equal x
    Convert.ToUInt64(1.4) |> equal x
    Convert.ToUInt64(1.5) |> equal (x+x)
    Convert.ToUInt64(1.6) |> equal (x+x)
    // Convert.ToUInt64(2.5) |> equal (x+x) // Rust rounds away from zero
    Convert.ToUInt64(2.6) |> equal (x+x+x)
    Convert.ToUInt64(3.5) |> equal (x+x+x+x)
    Convert.ToUInt64("1") |> equal x

[<Fact>]
let ``Convert between (un)signed long`` () = // See #1485
    int64 System.UInt64.MaxValue |> equal -1L
    uint64 -1L |> equal System.UInt64.MaxValue

[<Fact>]
let ``int64 can parse signed longs`` () = // See #1586
    let a = int64 "5"
    let b = int64 "-5"
    let c = int64 "+5"
    equal 5L a
    equal -5L b
    a = b |> equal false
    a = c |> equal true

[<Fact>]
let ``System.Convert.ToSingle works`` () =
    let x = 1.f
    float32(1y) |> equal x
    float32(1uy) |> equal x
    float32(1s) |> equal x
    float32(1) |> equal x
    float32(1L) |> equal x
    float32(1u) |> equal x
    float32(1us) |> equal x
    float32(1ul) |> equal x
    float32(1uL) |> equal x
    float32(1.f) |> equal x
    float32(1.) |> equal x
    float32(1.m) |> equal x
    float32("1.") |> equal x

    Convert.ToSingle(1y) |> equal x
    Convert.ToSingle(1uy) |> equal x
    Convert.ToSingle(1s) |> equal x
    Convert.ToSingle(1) |> equal x
    Convert.ToSingle(1L) |> equal x
    Convert.ToSingle(1u) |> equal x
    Convert.ToSingle(1us) |> equal x
    Convert.ToSingle(1ul) |> equal x
    Convert.ToSingle(1uL) |> equal x
    Convert.ToSingle(1.f) |> equal x
    Convert.ToSingle(1.) |> equal x
    Convert.ToSingle(1.m) |> equal x
    Convert.ToSingle("1.") |> equal x

[<Fact>]
let ``System.Convert.ToDouble works`` () =
    let x = 1.
    float(1y) |> equal x
    float(1uy) |> equal x
    float(1s) |> equal x
    float(1) |> equal x
    float(1L) |> equal x
    float(1u) |> equal x
    float(1us) |> equal x
    float(1ul) |> equal x
    float(1uL) |> equal x
    float(1.f) |> equal x
    float(1.) |> equal x
    float(1.m) |> equal x
    float("1.") |> equal x

    Convert.ToDouble(1y) |> equal x
    Convert.ToDouble(1uy) |> equal x
    Convert.ToDouble(1s) |> equal x
    Convert.ToDouble(1) |> equal x
    Convert.ToDouble(1L) |> equal x
    Convert.ToDouble(1u) |> equal x
    Convert.ToDouble(1us) |> equal x
    Convert.ToDouble(1ul) |> equal x
    Convert.ToDouble(1uL) |> equal x
    Convert.ToDouble(1.f) |> equal x
    Convert.ToDouble(1.) |> equal x
    Convert.ToDouble(1.m) |> equal x
    Convert.ToDouble("1.") |> equal x

[<Fact>]
let ``System.Convert.ToDecimal works`` () =
    let x = 1.m
    decimal(1y) |> equal x
    decimal(1uy) |> equal x
    decimal(1s) |> equal x
    decimal(1) |> equal x
    decimal(1L) |> equal x
    decimal(1u) |> equal x
    decimal(1us) |> equal x
    decimal(1ul) |> equal x
    decimal(1.f) |> equal x
    decimal(1.) |> equal x
    decimal(1.m) |> equal x
    decimal("1.") |> equal x

    Convert.ToDecimal(1y) |> equal x
    Convert.ToDecimal(1uy) |> equal x
    Convert.ToDecimal(1s) |> equal x
    Convert.ToDecimal(1) |> equal x
    Convert.ToDecimal(1L) |> equal x
    Convert.ToDecimal(1u) |> equal x
    Convert.ToDecimal(1us) |> equal x
    Convert.ToDecimal(1ul) |> equal x
    Convert.ToDecimal(1uL) |> equal x
    Convert.ToDecimal(1.f) |> equal x
    Convert.ToDecimal(1.) |> equal x
    Convert.ToDecimal(1.m) |> equal x
    Convert.ToDecimal("1.") |> equal x

//-------------------------------------
// Convert.ToString
//-------------------------------------

[<Fact>]
let ``System.Convert.ToString SByte works`` () =
    let x = "101"
    Convert.ToString(101y) |> equal x

[<Fact>]
let ``System.Convert.ToString Int16 works`` () =
    let x = "101"
    Convert.ToString(101s) |> equal x

[<Fact>]
let ``System.Convert.ToString Int32 works`` () =
    let x = "101"
    Convert.ToString(101) |> equal x

[<Fact>]
let ``System.Convert.ToString Int64 works`` () =
    let x = "101"
    Convert.ToString(101L) |> equal x

[<Fact>]
let ``System.Convert.ToString Byte works`` () =
    let x = "101"
    Convert.ToString(101uy) |> equal x

[<Fact>]
let ``System.Convert.ToString UInt16 works`` () =
    let x = "101"
    Convert.ToString(101us) |> equal x

[<Fact>]
let ``System.Convert.ToString UInt32 works`` () =
    let x = "101"
    Convert.ToString(101u) |> equal x

[<Fact>]
let ``System.Convert.ToString UInt64 works`` () =
    let x = "101"
    Convert.ToString(101uL) |> equal x

[<Fact>]
let ``System.Convert.ToString Single works`` () =
    let x = "101"
    Convert.ToString(101.f) |> equal x

[<Fact>]
let ``System.Convert.ToString Double works`` () =
    let x = "101"
    Convert.ToString(101.) |> equal x

[<Fact>]
let ``System.Convert.ToString Decimal works`` () =
    let x = "101"
    Convert.ToString(101.m) |> equal x

//-------------------------------------
// Invalid numeric conversions
//-------------------------------------

[<Fact>]
let ``Invalid numeric conversions will throw`` () =
    throwsAnyError (fun () -> sbyte("1.4"))
    throwsAnyError (fun () -> sbyte("foo"))
    throwsAnyError (fun () -> Convert.ToSByte("1.4"))
    throwsAnyError (fun () -> Convert.ToSByte("foo"))
    throwsAnyError (fun () -> int16("1.4"))
    throwsAnyError (fun () -> int16("foo"))
    throwsAnyError (fun () -> Convert.ToInt16("1.4"))
    throwsAnyError (fun () -> Convert.ToInt16("foo"))
    throwsAnyError (fun () -> int("1.4"))
    throwsAnyError (fun () -> int("foo"))
    throwsAnyError (fun () -> Convert.ToInt32("1.4"))
    throwsAnyError (fun () -> Convert.ToInt32("foo"))
    throwsAnyError (fun () -> int64("1.4"))
    throwsAnyError (fun () -> int64("foo"))
    throwsAnyError (fun () -> Convert.ToInt64("1.4"))
    throwsAnyError (fun () -> Convert.ToInt64("foo"))
    throwsAnyError (fun () -> byte("1.4"))
    throwsAnyError (fun () -> byte("foo"))
    throwsAnyError (fun () -> Convert.ToByte("1.4"))
    throwsAnyError (fun () -> Convert.ToByte("foo"))
    throwsAnyError (fun () -> uint16("1.4"))
    throwsAnyError (fun () -> uint16("foo"))
    throwsAnyError (fun () -> Convert.ToUInt16("1.4"))
    throwsAnyError (fun () -> Convert.ToUInt16("foo"))
    throwsAnyError (fun () -> uint32("1.4"))
    throwsAnyError (fun () -> uint32("foo"))
    throwsAnyError (fun () -> Convert.ToUInt32("1.4"))
    throwsAnyError (fun () -> Convert.ToUInt32("foo"))
    throwsAnyError (fun () -> uint64("1.4"))
    throwsAnyError (fun () -> uint64("foo"))
    throwsAnyError (fun () -> Convert.ToUInt64("1.4"))
    throwsAnyError (fun () -> Convert.ToUInt64("foo"))
    throwsAnyError (fun () -> float32("foo"))
    throwsAnyError (fun () -> Convert.ToSingle("foo"))
    throwsAnyError (fun () -> float("foo"))
    throwsAnyError (fun () -> Convert.ToDouble("foo"))
    throwsAnyError (fun () -> decimal("foo"))
    throwsAnyError (fun () -> Convert.ToDecimal("foo"))

//-------------------------------------
// String to number convertions (with base)
//-------------------------------------

[<Fact>]
let ``Invalid numeric conversions with base will throw`` () =
    throwsAnyError (fun () -> Convert.ToSByte("255", 2))
    throwsAnyError (fun () -> Convert.ToInt16("255", 2))
    throwsAnyError (fun () -> Convert.ToInt32("255", 2))
    throwsAnyError (fun () -> Convert.ToInt64("255", 2))
    throwsAnyError (fun () -> Convert.ToByte("255", 2))
    throwsAnyError (fun () -> Convert.ToUInt16("255", 2))
    throwsAnyError (fun () -> Convert.ToUInt32("255", 2))
    throwsAnyError (fun () -> Convert.ToUInt64("255", 2))

[<Fact>]
let ``System.Convert.ToSByte with base works`` () =
    let x = "101"
    Convert.ToSByte(x) |> equal 101y
    Convert.ToSByte(x, 2) |> equal 5y
    Convert.ToSByte(x, 8) |> equal 65y
    Convert.ToSByte(x, 10) |> equal 101y

[<Fact>]
let ``System.Convert.ToInt16 with base works`` () =
    let x = "101"
    Convert.ToInt16(x) |> equal 101s
    Convert.ToInt16(x, 2) |> equal 5s
    Convert.ToInt16(x, 8) |> equal 65s
    Convert.ToInt16(x, 10) |> equal 101s
    Convert.ToInt16(x, 16) |> equal 257s

[<Fact>]
let ``System.Convert.ToInt32 with base works`` () =
    let x = "101"
    Convert.ToInt32(x) |> equal 101
    Convert.ToInt32(x, 2) |> equal 5
    Convert.ToInt32(x, 8) |> equal 65
    Convert.ToInt32(x, 10) |> equal 101
    Convert.ToInt32(x, 16) |> equal 257

[<Fact>]
let ``System.Convert.ToInt64 with base works`` () =
    let x = "101"
    Convert.ToInt64(x) |> equal 101L
    Convert.ToInt64(x, 2) |> equal 5L
    Convert.ToInt64(x, 8) |> equal 65L
    Convert.ToInt64(x, 10) |> equal 101L
    Convert.ToInt64(x, 16) |> equal 257L

[<Fact>]
let ``System.Convert.ToByte with base works`` () =
    let x = "101"
    Convert.ToByte(x) |> equal 101uy
    Convert.ToByte(x, 2) |> equal 5uy
    Convert.ToByte(x, 8) |> equal 65uy
    Convert.ToByte(x, 10) |> equal 101uy

[<Fact>]
let ``System.Convert.ToUInt16 with base works`` () =
    let x = "101"
    Convert.ToUInt16(x) |> equal 101us
    Convert.ToUInt16(x, 2) |> equal 5us
    Convert.ToUInt16(x, 8) |> equal 65us
    Convert.ToUInt16(x, 10) |> equal 101us
    Convert.ToUInt16(x, 16) |> equal 257us

[<Fact>]
let ``System.Convert.ToUInt32 with base works`` () =
    let x = "101"
    Convert.ToUInt32(x) |> equal 101u
    Convert.ToUInt32(x, 2) |> equal 5u
    Convert.ToUInt32(x, 8) |> equal 65u
    Convert.ToUInt32(x, 10) |> equal 101u
    Convert.ToUInt32(x, 16) |> equal 257u

[<Fact>]
let ``System.Convert.ToUInt64 with base works`` () =
    let x = "101"
    Convert.ToUInt64(x) |> equal 101uL
    Convert.ToUInt64(x, 2) |> equal 5uL
    Convert.ToUInt64(x, 8) |> equal 65uL
    Convert.ToUInt64(x, 10) |> equal 101uL
    Convert.ToUInt64(x, 16) |> equal 257uL

//-------------------------------------
// Number to string convertions (with base)
//-------------------------------------

[<Fact>]
let ``System.Convert.ToString with base works`` () =
    Convert.ToString(Byte.MaxValue, 2) |> equal "11111111"
    Convert.ToString(Int16.MaxValue, 2) |> equal "111111111111111"
    Convert.ToString(Int32.MaxValue, 2) |> equal "1111111111111111111111111111111"
    Convert.ToString(Int64.MaxValue, 2) |> equal "111111111111111111111111111111111111111111111111111111111111111"

[<Fact>]
let ``System.Convert.ToString Int16 with base works`` () =
    let x = "101"
    Convert.ToString(5s, 2) |> equal x
    Convert.ToString(65s, 8) |> equal x
    Convert.ToString(101s, 10) |> equal x
    Convert.ToString(257s, 16) |> equal x
    Convert.ToString(-5s, 16) |> equal "fffb"

[<Fact>]
let ``System.Convert.ToString Int32 with base works`` () =
    let x = "101"
    Convert.ToString(5, 2) |> equal x
    Convert.ToString(65, 8) |> equal x
    Convert.ToString(101, 10) |> equal x
    Convert.ToString(257, 16) |> equal x
    Convert.ToString(-5, 16) |> equal "fffffffb"

[<Fact>]
let ``System.Convert.ToString Int64 with base works`` () =
    let x = "101"
    Convert.ToString(5L, 2) |> equal x
    Convert.ToString(65L, 8) |> equal x
    Convert.ToString(101L, 10) |> equal x
    Convert.ToString(257L, 16) |> equal x
    Convert.ToString(-5L, 16) |> equal "fffffffffffffffb"

[<Fact>]
let ``System.Convert.ToString Byte with base works`` () =
    let x = "101"
    Convert.ToString(5uy, 2) |> equal x
    Convert.ToString(65uy, 8) |> equal x
    Convert.ToString(101uy, 10) |> equal x

[<Fact>]
let ``FSharp.Core type converters can combined via the >> operator`` () =
    let res (x: 'T): Result<'T, string> = Ok x
    "1" |> (sbyte >> res) |> equal (Ok 1y)
    "1" |> (int16 >> res) |> equal (Ok 1s)
    "1" |> (int >> res) |> equal (Ok 1)
    "1" |> (int64 >> res) |> equal (Ok 1L)
    "1" |> (byte >> res) |> equal (Ok 1uy)
    "1" |> (uint16 >> res) |> equal (Ok 1us)
    "1" |> (uint32 >> res) |> equal (Ok 1u)
    "1" |> (uint64 >> res) |> equal (Ok 1uL)
    "1" |> (float32 >> res) |> equal (Ok 1.f)
    "1" |> (float >> res) |> equal (Ok 1.)
    "1" |> (double >> res) |> equal (Ok 1.)
    "1" |> (decimal >> res) |> equal (Ok 1.m)

[<Fact>]
let ``Convert.ToHexString works`` () =
    let bytes = [| 250uy; 251uy; 252uy; 253uy; 254uy |]
    Convert.ToHexString(bytes)
    |> equal "FAFBFCFDFE"

[<Fact>]
let ``Convert.FromHexString works`` () =
    Convert.FromHexString("FAFBFCFDFE")
    |> equal [| 250uy; 251uy; 252uy; 253uy; 254uy |]

[<Fact>]
let ``Convert.ToBase64String works`` () =
    let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
    Convert.ToBase64String(bytes)
    |> equal "AgQGCAoMDhASFA=="

[<Fact>]
let ``Convert.FromBase64String works`` () =
    Convert.FromBase64String("AgQGCAoMDhASFA==")
    |> equal [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]

//-------------------------------------
// System.BitConverter
//-------------------------------------

[<Fact>]
let ``BitConverter.IsLittleEndian works`` () =
    BitConverter.IsLittleEndian |> equal true

[<Fact>]
let ``BitConverter.GetBytes Boolean works`` () =
    let value = true
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 1uy |]

[<Fact>]
let ``BitConverter.GetBytes Char works`` () =
    let value = 'A'
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 65uy; 0uy |]

[<Fact>]
let ``BitConverter.GetBytes Int16 works`` () =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 2uy; 1uy |]

[<Fact>]
let ``BitConverter.GetBytes Int32 works`` () =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 4uy; 3uy; 2uy; 1uy |]

[<Fact>]
let ``BitConverter.GetBytes Int64 works`` () =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 1uy |]

[<Fact>]
let ``BitConverter.GetBytes UInt16 works`` () =
    let value = 0xFF02us
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 2uy; 255uy |]

[<Fact>]
let ``BitConverter.GetBytes UInt32 works`` () =
    let value = 0xFF020304u
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 4uy; 3uy; 2uy; 255uy |]

[<Fact>]
let ``BitConverter.GetBytes UInt64 works`` () =
    let value = 0xFF02030405060708UL
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 255uy |]

[<Fact>]
let ``BitConverter.GetBytes Single works`` () =
    let value = 1.0f
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 0uy; 0uy; 128uy; 63uy |]

[<Fact>]
let ``BitConverter.GetBytes Double works`` () =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 240uy; 63uy |]

[<Fact>]
let ``BitConverter.Int64BitsToDouble works`` () =
    let f = BitConverter.Int64BitsToDouble(1L)
    f |> equal 4.9406564584124654E-324

[<Fact>]
let ``BitConverter.DoubleToInt64Bits works`` () =
    let i = BitConverter.DoubleToInt64Bits(1.0)
    i |> equal 4607182418800017408L

[<Fact>]
let ``BitConverter.UInt64BitsToDouble works`` () =
    let f = BitConverter.UInt64BitsToDouble(1uL)
    f |> equal 4.9406564584124654E-324

[<Fact>]
let ``BitConverter.DoubleToUInt64Bits works`` () =
    let i = BitConverter.DoubleToUInt64Bits(1.0)
    i |> equal 4607182418800017408uL

[<Fact>]
let ``BitConverter.Int32BitsToSingle works`` () =
    let f = BitConverter.Int32BitsToSingle(1)
    f |> equal 1.40129846E-45f

[<Fact>]
let ``BitConverter.SingleToInt32Bits works`` () =
    let i = BitConverter.SingleToInt32Bits(1.0f)
    i |> equal 1065353216

[<Fact>]
let ``BitConverter.UInt32BitsToSingle works`` () =
    let f = BitConverter.UInt32BitsToSingle(1u)
    f |> equal 1.40129846E-45f

[<Fact>]
let ``BitConverter.SingleToUInt32Bits works`` () =
    let i = BitConverter.SingleToUInt32Bits(1.0f)
    i |> equal 1065353216u

[<Fact>]
let ``BitConverter.ToBoolean works`` () =
    let value = true
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToBoolean(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToChar works`` () =
    let value = 'A'
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToChar(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToInt16 works`` () =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt16(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToInt32 works`` () =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt32(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToInt64 works`` () =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt64(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToUInt16 works`` () =
    let value = 0xFF02us
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt16(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToUInt32 works`` () =
    let value = 0xFF020304u
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt32(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToUInt64 works`` () =
    let value = 0xFF02030405060708UL
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt64(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToSingle works`` () =
    let value = 1.0f
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToSingle(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToDouble works`` () =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToDouble(bytes, 0) |> equal value

[<Fact>]
let ``BitConverter.ToString works`` () =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes) |> equal "04-03-02-01"

[<Fact>]
let ``BitConverter.ToString 2 works`` () =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes, 1) |> equal "03-02-01"

[<Fact>]
let ``BitConverter.ToString 3 works`` () =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes, 1, 2) |> equal "03-02"

//-------------------------------------
// System.Numerics.BigInteger
//-------------------------------------

[<Fact>]
let ``BigInt from uint32 works`` () =
    bigint System.UInt32.MaxValue |> equal 4294967295I

[<Fact>]
let ``BigInt ToSByte works`` () =
    let value = 0x02y
    sbyte (bigint (int32 value)) |> equal value

[<Fact>]
let ``BigInt ToInt16 works`` () =
    let value = 0x0102s
    int16 (bigint (int32 value)) |> equal value

[<Fact>]
let ``BigInt ToInt32 works`` () =
    let value = 0x01020304
    int32 (bigint value) |> equal value

[<Fact>]
let ``BigInt ToInt64 works`` () =
    let value = 0x0102030405060708L
    int64 (bigint value) |> equal value

[<Fact>]
let ``BigInt ToByte works`` () =
    let value = 0x02uy
    byte (bigint (uint32 value)) |> equal value

[<Fact>]
let ``BigInt ToUInt16 works`` () =
    let value = 0xFF02us
    uint16 (bigint (uint32 value)) |> equal value

[<Fact>]
let ``BigInt ToUInt32 works`` () =
    let value = 0xFF020304u
    uint32 (bigint value) |> equal value

[<Fact>]
let ``BigInt ToUInt64 works`` () =
    let value = 0xFF02030405060708UL
    uint64 (bigint value) |> equal value

[<Fact>]
let ``BigInt ToSingle works`` () =
    let value = 1.0f
    single (bigint value) |> equal value

[<Fact>]
let ``BigInt ToDouble works`` () =
    let value = -1.0
    double (bigint value) |> equal value

[<Fact>]
let ``BigInt ToDecimal works`` () =
    let value = 1.0m
    decimal (bigint value) |> equal value

[<Fact>]
let ``BigInt ToString works`` () =
    let value = 1234567890
    string (bigint value) |> equal "1234567890"

//-------------------------------------
// System.Text.Encoding.Unicode
//-------------------------------------

[<Fact>]
let ``Encoding.Unicode.GetBytes for string works`` () =
    System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
    |> equal [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

[<Fact>]
let ``Encoding.Unicode.GetBytes for string range works`` () =
    System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF", 4, 3)
    |> equal [| 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

[<Fact>]
let ``Encoding.Unicode.GetBytes for chars works`` () =
    System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray())
    |> equal [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

[<Fact>]
let ``Encoding.Unicode.GetBytes for chars range works`` () =
    System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
    |> equal [| 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

[<Fact>]
let ``Encoding.Unicode.GetByteCount for string works`` () =
    System.Text.Encoding.Unicode.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
    |> equal 14

[<Fact>]
let ``Encoding.Unicode.GetByteCount for string range works`` () =
    System.Text.Encoding.Unicode.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF", 4, 3)
    |> equal 6

[<Fact>]
let ``Encoding.Unicode.GetByteCount for chars works`` () =
    System.Text.Encoding.Unicode.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray())
    |> equal 14

[<Fact>]
let ``Encoding.Unicode.GetByteCount for chars range works`` () =
    System.Text.Encoding.Unicode.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
    |> equal 6

[<Fact>]
let ``Encoding.Unicode.GetMaxByteCount works`` () =
    [| 0..10 |] |> Array.map System.Text.Encoding.Unicode.GetMaxByteCount
    |> equal [| 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 22 |]

[<Fact>]
let ``Encoding.Unicode.GetMaxCharCount works`` () =
    [| 0..10 |] |> Array.map System.Text.Encoding.Unicode.GetMaxCharCount
    |> equal [| 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6 |]

[<Fact>]
let ``Encoding.Unicode.GetString works`` () =
    let bytes = [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]
    System.Text.Encoding.Unicode.GetString(bytes)
    |> equal "za\u0306\u01FD\u03B2\uD8FF\uDCFF"

[<Fact>]
let ``Encoding.Unicode.GetString for range works`` () =
    let bytes = [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]
    System.Text.Encoding.Unicode.GetString(bytes, 8, 6)
    |> equal "\u03B2\uD8FF\uDCFF"

//-------------------------------------
// System.Text.Encoding.UTF8
//-------------------------------------

[<Fact>]
let ``Encoding.UTF8.GetBytes for string works`` () =
    System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
    |> equal [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

[<Fact>]
let ``Encoding.UTF8.GetBytes for string range works`` () =
    System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF", 4, 3)
    |> equal [| 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

[<Fact>]
let ``Encoding.UTF8.GetBytes for chars works`` () =
    System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray())
    |> equal [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

[<Fact>]
let ``Encoding.UTF8.GetBytes for chars range works`` () =
    System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
    |> equal [| 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

[<Fact>]
let ``Encoding.UTF8.GetByteCount for string works`` () =
    System.Text.Encoding.UTF8.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
    |> equal 12

[<Fact>]
let ``Encoding.UTF8.GetByteCount for string range works`` () =
    System.Text.Encoding.UTF8.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF", 4, 3)
    |> equal 6

[<Fact>]
let ``Encoding.UTF8.GetByteCount for chars works`` () =
    System.Text.Encoding.UTF8.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray())
    |> equal 12

[<Fact>]
let ``Encoding.UTF8.GetByteCount for chars range works`` () =
    System.Text.Encoding.UTF8.GetByteCount("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
    |> equal 6

[<Fact>]
let ``Encoding.UTF8.GetMaxByteCount works`` () =
    [| 0..10 |] |> Array.map System.Text.Encoding.UTF8.GetMaxByteCount
    |> equal [| 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33 |]

[<Fact>]
let ``Encoding.UTF8.GetMaxCharCount works`` () =
    [| 0..10 |] |> Array.map System.Text.Encoding.UTF8.GetMaxCharCount
    |> equal [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 |]

[<Fact>]
let ``Encoding.UTF8.GetString works`` () =
    let bytes = [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]
    System.Text.Encoding.UTF8.GetString(bytes)
    |> equal "za\u0306\u01FD\u03B2\uD8FF\uDCFF"

[<Fact>]
let ``Encoding.UTF8.GetString for range works`` () =
    let bytes = [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]
    System.Text.Encoding.UTF8.GetString(bytes, 6, 6)
    |> equal "\u03B2\uD8FF\uDCFF"
