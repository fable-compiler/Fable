[<Util.Testing.TestFixture>]
module Fable.Tests.Convert
open System
open System.Globalization
open Util.Testing
open Fable.Tests.Util

//-------------------------------------
// Parse and TryParse
//-------------------------------------

let tryParse f initial value =
    #if FABLE_COMPILER
    f(value)
    #else
    let res = ref initial
    let success = f(value, NumberStyles.Number, CultureInfo("en-US"), res)
    (success, !res)
    #endif

let parse f (a: string) =
    #if FABLE_COMPILER
    f(a)
    #else
    f(a, CultureInfo("en-US"))
    #endif

[<Test>]
let ``System.Double.Parse works``() =
    parse Double.Parse "1.5" |> equal 1.5

[<Test>]
let ``System.Double.TryParse works``() =
    tryParse Double.TryParse 0.0 "1" |> equal (true, 1.0)
    tryParse Double.TryParse 0.0 "    1     " |> equal (true, 1.0)
    tryParse Double.TryParse 0.0 "1.5" |> equal (true, 1.5)
    tryParse Double.TryParse 0.0 "    1.5     " |> equal (true, 1.5)
    tryParse Double.TryParse 0.0 "foo" |> equal (false, 0.0)
    tryParse Double.TryParse 0.0 "9X" |> equal (false, 0.0)
    tryParse Double.TryParse 0.0 "X9" |> equal (false, 0.0)
    tryParse Double.TryParse 0.0 "X9TRE34" |> equal (false, 0.0)
    tryParse Double.TryParse 0.0 "9SayWhat12Huh" |> equal (false, 0.0)
    tryParse Double.TryParse 0.0 "-1.5" |> equal (true, -1.5)

[<Test>]
let ``System.Single.Parse works``() =
    parse Single.Parse "1.5" |> equal 1.5f

[<Test>]
let ``System.Single.TryParse works``() =
    tryParse Single.TryParse 0.0f "1" |> equal (true, 1.0f)
    tryParse Single.TryParse 0.0f "    1     " |> equal (true, 1.0f)
    tryParse Single.TryParse 0.0f "1.5" |> equal (true, 1.5f)
    tryParse Single.TryParse 0.0f "    1.5     " |> equal (true, 1.5f)
    tryParse Single.TryParse 0.0f "foo" |> equal (false, 0.0f)
    tryParse Single.TryParse 0.0f "9X" |> equal (false, 0.0f)
    tryParse Single.TryParse 0.0f "X9" |> equal (false, 0.0f)
    tryParse Single.TryParse 0.0f "X9TRE34" |> equal (false, 0.0f)
    tryParse Single.TryParse 0.0f "9SayWhat12Huh" |> equal (false, 0.0f)
    tryParse Single.TryParse 0.0f "-1.5" |> equal (true, -1.5f)

[<Test>]
let ``System.Int32.Parse works``() =
    Int32.Parse("5") |> equal 5
    (fun () -> Int32.Parse("5f")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("f5")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Int32.Parse with hex works``() =
    Int32.Parse("555555", System.Globalization.NumberStyles.HexNumber) |> equal 5592405
    (fun () -> Int32.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Int32.ToString "x" works``() =
    (5592405).ToString("x") |> equal "555555"

[<Test>]
let ``System.Int32.ToString works``() =
    (5592405).ToString() |> equal "5592405"

[<Test>]
let ``System.Int32.TryParse works``() =
    tryParse Int32.TryParse 0 "1" |> equal (true, 1)
    tryParse Int32.TryParse 0 "    1     " |> equal (true, 1)
    tryParse Int32.TryParse 0 "1.5" |> equal (false, 0)
    tryParse Int32.TryParse 0 "    1.5     " |> equal (false, 0)
    tryParse Int32.TryParse 0 "foo" |> equal (false, 0)
    tryParse Int32.TryParse 0 "9X" |> equal (false, 0)
    tryParse Int32.TryParse 0 "X9" |> equal (false, 0)
    tryParse Int32.TryParse 0 "X9TRE34" |> equal (false, 0)
    tryParse Int32.TryParse 0 "9SayWhat12Huh" |> equal (false, 0)
    tryParse Int32.TryParse 0 "-1" |> equal (true, -1)

//-------------------------------------
// System.Convert
//-------------------------------------

[<Test>]
let ``System.Convert.ToSByte works``() =
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
    (fun () -> sbyte("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> sbyte("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToSByte(2.5) |> equal (x+x)
    Convert.ToSByte(2.6) |> equal (x+x+x)
    Convert.ToSByte(3.5) |> equal (x+x+x+x)
    Convert.ToSByte("1") |> equal x
    (fun () -> Convert.ToSByte("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToSByte("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToInt16 works``() =
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
    (fun () -> int16("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> int16("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToInt16(2.5) |> equal (x+x)
    Convert.ToInt16(2.6) |> equal (x+x+x)
    Convert.ToInt16(3.5) |> equal (x+x+x+x)
    Convert.ToInt16("1") |> equal x
    (fun () -> Convert.ToInt16("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToInt16("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToInt32 works``() =
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
    (fun () -> int("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> int("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToInt32(2.5) |> equal (x+x)
    Convert.ToInt32(2.6) |> equal (x+x+x)
    Convert.ToInt32(3.5) |> equal (x+x+x+x)
    Convert.ToInt32("1") |> equal x
    (fun () -> Convert.ToInt32("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToInt32("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToInt64 works``() =
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
    (fun () -> int64("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> int64("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToInt64(2.5) |> equal (x+x)
    Convert.ToInt64(2.6) |> equal (x+x+x)
    Convert.ToInt64(3.5) |> equal (x+x+x+x)
    Convert.ToInt64("1") |> equal x
    (fun () -> Convert.ToInt64("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToInt64("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToByte works``() =
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
    (fun () -> byte("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> byte("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToByte(2.5) |> equal (x+x)
    Convert.ToByte(2.6) |> equal (x+x+x)
    Convert.ToByte(3.5) |> equal (x+x+x+x)
    Convert.ToByte("1") |> equal x
    (fun () -> Convert.ToByte("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToByte("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToUInt16 works``() =
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
    (fun () -> uint16("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> uint16("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToUInt16(2.5) |> equal (x+x)
    Convert.ToUInt16(2.6) |> equal (x+x+x)
    Convert.ToUInt16(3.5) |> equal (x+x+x+x)
    Convert.ToUInt16("1") |> equal x
    (fun () -> Convert.ToUInt16("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToUInt16("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToUInt32 works``() =
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
    (fun () -> uint32("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> uint32("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToUInt32(2.5) |> equal (x+x)
    Convert.ToUInt32(2.6) |> equal (x+x+x)
    Convert.ToUInt32(3.5) |> equal (x+x+x+x)
    Convert.ToUInt32("1") |> equal x
    (fun () -> Convert.ToUInt32("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToUInt32("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToUInt64 works``() =
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
    (fun () -> uint64("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> uint64("foo")) |> throwsError "Input string was not in a correct format."

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
    Convert.ToUInt64(2.5) |> equal (x+x)
    Convert.ToUInt64(2.6) |> equal (x+x+x)
    Convert.ToUInt64(3.5) |> equal (x+x+x+x)
    Convert.ToUInt64("1") |> equal x
    (fun () -> Convert.ToUInt64("1.4")) |> throwsError "Input string was not in a correct format."
    (fun () -> Convert.ToUInt64("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToSingle works``() =
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
    (fun () -> float32("foo")) |> throwsError "Input string was not in a correct format."

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
    parse Convert.ToSingle "1." |> equal x
    (fun () -> Convert.ToSingle("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToDouble works``() =
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
    (fun () -> float("foo")) |> throwsError "Input string was not in a correct format."

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
    parse Convert.ToDouble "1." |> equal x
    (fun () -> Convert.ToDouble("foo")) |> throwsError "Input string was not in a correct format."

[<Test>]
let ``System.Convert.ToDecimal works``() =
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
    (fun () -> decimal("foo")) |> throwsError "Input string was not in a correct format."

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
    parse Convert.ToDecimal "1." |> equal x
    (fun () -> Convert.ToDecimal("foo")) |> throwsError "Input string was not in a correct format."

// String to number convertions (with base)

[<Test>]
let ``System.Convert.ToSByte with base works``() =
    let x = "101"
    Convert.ToSByte(x) |> equal 101y
    Convert.ToSByte(x, 2) |> equal 5y
    Convert.ToSByte(x, 8) |> equal 65y
    Convert.ToSByte(x, 10) |> equal 101y
    #if FABLE_COMPILER
    (fun () -> Convert.ToSByte("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToSByte("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToInt16 with base works``() =
    let x = "101"
    Convert.ToInt16(x) |> equal 101s
    Convert.ToInt16(x, 2) |> equal 5s
    Convert.ToInt16(x, 8) |> equal 65s
    Convert.ToInt16(x, 10) |> equal 101s
    Convert.ToInt16(x, 16) |> equal 257s
    #if FABLE_COMPILER
    (fun () -> Convert.ToInt16("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToInt16("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToInt32 with base works``() =
    let x = "101"
    Convert.ToInt32(x) |> equal 101
    Convert.ToInt32(x, 2) |> equal 5
    Convert.ToInt32(x, 8) |> equal 65
    Convert.ToInt32(x, 10) |> equal 101
    Convert.ToInt32(x, 16) |> equal 257
    #if FABLE_COMPILER
    (fun () -> Convert.ToInt32("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToInt32("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToInt64 with base works``() =
    let x = "101"
    Convert.ToInt64(x) |> equal 101L
    Convert.ToInt64(x, 2) |> equal 5L
    Convert.ToInt64(x, 8) |> equal 65L
    Convert.ToInt64(x, 10) |> equal 101L
    Convert.ToInt64(x, 16) |> equal 257L
    #if FABLE_COMPILER
    (fun () -> Convert.ToInt64("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToInt64("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToByte with base works``() =
    let x = "101"
    Convert.ToByte(x) |> equal 101uy
    Convert.ToByte(x, 2) |> equal 5uy
    Convert.ToByte(x, 8) |> equal 65uy
    Convert.ToByte(x, 10) |> equal 101uy
    #if FABLE_COMPILER
    (fun () -> Convert.ToByte("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToByte("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToUInt16 with base works``() =
    let x = "101"
    Convert.ToUInt16(x) |> equal 101us
    Convert.ToUInt16(x, 2) |> equal 5us
    Convert.ToUInt16(x, 8) |> equal 65us
    Convert.ToUInt16(x, 10) |> equal 101us
    Convert.ToUInt16(x, 16) |> equal 257us
    #if FABLE_COMPILER
    (fun () -> Convert.ToUInt16("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToUInt16("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToUInt32 with base works``() =
    let x = "101"
    Convert.ToUInt32(x) |> equal 101u
    Convert.ToUInt32(x, 2) |> equal 5u
    Convert.ToUInt32(x, 8) |> equal 65u
    Convert.ToUInt32(x, 10) |> equal 101u
    Convert.ToUInt32(x, 16) |> equal 257u
    #if FABLE_COMPILER
    (fun () -> Convert.ToUInt32("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToUInt32("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

[<Test>]
let ``System.Convert.ToUInt64 with base works``() =
    let x = "101"
    Convert.ToUInt64(x) |> equal 101uL
    Convert.ToUInt64(x, 2) |> equal 5uL
    Convert.ToUInt64(x, 8) |> equal 65uL
    Convert.ToUInt64(x, 10) |> equal 101uL
    Convert.ToUInt64(x, 16) |> equal 257uL
    #if FABLE_COMPILER
    (fun () -> Convert.ToUInt64("255", 2)) |> throwsError "Input string was not in a correct format."
    #else
    (fun () -> Convert.ToUInt64("255", 2)) |> throwsError "Could not find any recognizable digits."
    #endif

// Number to string convertions (with base)

[<Test>]
let ``System.Convert.ToString with base works``() =
    Convert.ToString(Byte.MaxValue,2) |> equal "11111111"
    Convert.ToString(Int16.MaxValue,2) |> equal "111111111111111"
    Convert.ToString(Int32.MaxValue,2) |> equal "1111111111111111111111111111111"
    Convert.ToString(Int64.MaxValue,2) |> equal "111111111111111111111111111111111111111111111111111111111111111"

[<Test>]
let ``System.Convert.ToString SByte works``() =
    let x = "101"
    Convert.ToString(101y) |> equal x

[<Test>]
let ``System.Convert.ToString Int16 works``() =
    let x = "101"
    Convert.ToString(101s) |> equal x
    Convert.ToString(5s, 2) |> equal x
    Convert.ToString(65s, 8) |> equal x
    Convert.ToString(101s, 10) |> equal x
    Convert.ToString(257s, 16) |> equal x
    Convert.ToString(-5s, 16) |> equal "fffb"

[<Test>]
let ``System.Convert.ToString Int32 works``() =
    let x = "101"
    Convert.ToString(101) |> equal x
    Convert.ToString(5, 2) |> equal x
    Convert.ToString(65, 8) |> equal x
    Convert.ToString(101, 10) |> equal x
    Convert.ToString(257, 16) |> equal x
    Convert.ToString(-5, 16) |> equal "fffffffb"

[<Test>]
let ``System.Convert.ToString Int64 works``() =
    let x = "101"
    Convert.ToString(101L) |> equal x
    Convert.ToString(5L, 2) |> equal x
    Convert.ToString(65L, 8) |> equal x
    Convert.ToString(101L, 10) |> equal x
    Convert.ToString(257L, 16) |> equal x
    Convert.ToString(-5L, 16) |> equal "fffffffffffffffb"

[<Test>]
let ``System.Convert.ToString Byte works``() =
    let x = "101"
    Convert.ToString(101uy) |> equal x
    Convert.ToString(5uy, 2) |> equal x
    Convert.ToString(65uy, 8) |> equal x
    Convert.ToString(101uy, 10) |> equal x

[<Test>]
let ``System.Convert.ToString UInt16 works``() =
    let x = "101"
    Convert.ToString(101us) |> equal x

[<Test>]
let ``System.Convert.ToString UInt32 works``() =
    let x = "101"
    Convert.ToString(101u) |> equal x

[<Test>]
let ``System.Convert.ToString UInt64 works``() =
    let x = "101"
    Convert.ToString(101uL) |> equal x

[<Test>]
let ``System.Convert.ToString Single works``() =
    let x = "101"
    Convert.ToString(101.f) |> equal x

[<Test>]
let ``System.Convert.ToString Double works``() =
    let x = "101"
    Convert.ToString(101.) |> equal x

[<Test>]
let ``System.Convert.ToString Decimal works``() =
    let x = "101"
    Convert.ToString(101.m) |> equal x

[<Test>]
let ``FSharp.Core type converters can combined via the >> operator``() =
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

//-------------------------------------
// System.BitConverter
//-------------------------------------

[<Test>]
let ``BitConverter.GetBytes Boolean works``() =
    let value = true
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 1uy |]

[<Test>]
let ``BitConverter.GetBytes Char works``() =
    let value = 'A'
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 65uy; 0uy |]

[<Test>]
let ``BitConverter.GetBytes Int16 works``() =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 2uy; 1uy |]

[<Test>]
let ``BitConverter.GetBytes Int32 works``() =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 4uy; 3uy; 2uy; 1uy |]

[<Test>]
let ``BitConverter.GetBytes Int64 works``() =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 1uy |]

[<Test>]
let ``BitConverter.GetBytes UInt16 works``() =
    let value = 0xFF02us
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 2uy; 255uy |]

[<Test>]
let ``BitConverter.GetBytes UInt32 works``() =
    let value = 0xFF020304u
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 4uy; 3uy; 2uy; 255uy |]

[<Test>]
let ``BitConverter.GetBytes UInt64 works``() =
    let value = 0xFF02030405060708UL
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 255uy |]

[<Test>]
let ``BitConverter.GetBytes Single works``() =
    let value = 1.0f
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 0uy; 0uy; 128uy; 63uy |]

[<Test>]
let ``BitConverter.GetBytes Double works``() =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    bytes |> equal [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 240uy; 63uy |]

[<Test>]
let ``BitConverter.Int64BitsToDouble works``() =
    let f = BitConverter.Int64BitsToDouble(1L)
    f |> equal 4.9406564584124654E-324

[<Test>]
let ``BitConverter.DoubleToInt64Bits works``() =
    let i = BitConverter.DoubleToInt64Bits(1.0)
    i |> equal 4607182418800017408L

[<Test>]
let ``BitConverter.ToBoolean works``() =
    let value = true
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToBoolean(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToChar works``() =
    let value = 'A'
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToChar(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToInt16 works``() =
    let value = 0x0102s
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt16(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToInt32 works``() =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt32(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToInt64 works``() =
    let value = 0x0102030405060708L
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToInt64(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToUInt16 works``() =
    let value = 0xFF02us
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt16(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToUInt32 works``() =
    let value = 0xFF020304u
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt32(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToUInt64 works``() =
    let value = 0xFF02030405060708UL
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToUInt64(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToSingle works``() =
    let value = 1.0f
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToSingle(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToDouble works``() =
    let value = 1.0
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToDouble(bytes, 0) |> equal value

[<Test>]
let ``BitConverter.ToString works``() =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes) |> equal "04-03-02-01"

[<Test>]
let ``BitConverter.ToString 2 works``() =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes, 1) |> equal "03-02-01"

[<Test>]
let ``BitConverter.ToString 3 works``() =
    let value = 0x01020304
    let bytes = BitConverter.GetBytes(value)
    BitConverter.ToString(bytes, 1, 2) |> equal "03-02"

//-------------------------------------
// System.Numerics.BigInteger
//-------------------------------------

[<Test>]
let ``BigInt ToSByte works``() =
    let value = 0x02y
    sbyte (bigint (int32 value)) |> equal value

[<Test>]
let ``BigInt ToInt16 works``() =
    let value = 0x0102s
    int16 (bigint (int32 value)) |> equal value

[<Test>]
let ``BigInt ToInt32 works``() =
    let value = 0x01020304
    int32 (bigint value) |> equal value

[<Test>]
let ``BigInt ToInt64 works``() =
    let value = 0x0102030405060708L
    int64 (bigint value) |> equal value

[<Test>]
let ``BigInt ToByte works``() =
    let value = 0x02uy
    byte (bigint (uint32 value)) |> equal value

[<Test>]
let ``BigInt ToUInt16 works``() =
    let value = 0xFF02us
    uint16 (bigint (uint32 value)) |> equal value

[<Test>]
let ``BigInt ToUInt32 works``() =
    //let value = 0xFF020304u //TODO: BigInt.fromUInt32 not implemented yet, so this will fail
    let value = 0x1F020304u
    uint32 (bigint value) |> equal value

[<Test>]
let ``BigInt ToUInt64 works``() =
    let value = 0xFF02030405060708UL
    uint64 (bigint value) |> equal value

[<Test>]
let ``BigInt ToSingle works``() =
    let value = 1.0f
    single (bigint value) |> equal value

[<Test>]
let ``BigInt ToDouble works``() =
    let value = -1.0
    double (bigint value) |> equal value

[<Test>]
let ``BigInt ToDecimal works``() =
    let value = 1.0m
    decimal (bigint value) |> equal value

[<Test>]
let ``BigInt ToString works``() =
    let value = 1234567890
    string (bigint value) |> equal "1234567890"

[<Test>]
let ``Convert.ToBase64String works``() =
    let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
    Convert.ToBase64String(bytes)
    |> equal "AgQGCAoMDhASFA=="

[<Test>]
let ``Convert.FromBase64String works``() =
    Convert.FromBase64String("AgQGCAoMDhASFA==")
    |> equal [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]

[<Test>]
let ``int64 can be JSON serialized forth and back``() =
    let val1 = 5348937298839933899L
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson val1
    let val2 = Fable.Core.JsInterop.ofJson<int64> json
    equal true (box val2 :? int64) // Type is kept
    equal val1 val2
    equal val2 5348937298839933899L
    let val2 = Fable.Core.JsInterop.ofJsonAsType json (val1.GetType())
    equal true (val2 :? int64) // Type is kept
    let val2 = val2 :?> int64
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject val1
    let val2 = Newtonsoft.Json.JsonConvert.DeserializeObject<int64> json
    #endif
    printfn "JSON: %s" json
    equal val1 val2
    equal val2 5348937298839933899L

[<Test>]
let ``uint64 can be JSON serialized forth and back``() =
    let val1 = 9348937298839933899UL
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson val1
    let val2 = Fable.Core.JsInterop.ofJson<uint64> json
    equal true (box val2 :? uint64) // Type is kept
    equal val1 val2
    equal val2 9348937298839933899UL
    let val2 = Fable.Core.JsInterop.ofJsonAsType json (val1.GetType())
    equal true (val2 :? uint64) // Type is kept
    let val2 = val2 :?> uint64
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject val1
    let val2 = Newtonsoft.Json.JsonConvert.DeserializeObject<uint64> json
    #endif
    printfn "JSON: %s" json
    equal val1 val2
    equal val2 9348937298839933899UL

[<Test>]
let ``BigInts can be JSON serialized forth and back``() =
    let val1 = 59823749821707124891298739821798327321028091380980I
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson val1
    let val2 = Fable.Core.JsInterop.ofJson<bigint> json
    equal true (box val2 :? bigint) // Type is kept
    equal val1 val2
    equal val2 59823749821707124891298739821798327321028091380980I
    let val2 = Fable.Core.JsInterop.ofJsonAsType json (val1.GetType())
    equal true (val2 :? bigint) // Type is kept
    let val2 = val2 :?> bigint
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject val1
    let val2 = Newtonsoft.Json.JsonConvert.DeserializeObject<bigint> json
    #endif
    // printfn "JSON: %s" json
    equal val1 val2
    equal val2 59823749821707124891298739821798327321028091380980I
