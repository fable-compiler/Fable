module Fable.Tests.Convert

open System
open System.Globalization
open Util.Testing
open Fable.Tests.Util

//-------------------------------------
// Parse and TryParse
//-------------------------------------

let tryParse f initial (value: string) =
    let res = ref initial
#if FABLE_COMPILER
    let success = f(value, res)
#else
    let success = f(value, NumberStyles.Number, CultureInfo("en-US"), res)
#endif
    (success, !res)

let parse f (a: string) =
#if FABLE_COMPILER
    f(a)
#else
    f(a, CultureInfo("en-US"))
#endif

let tests =
  testList "Convert" [
    testCase "System.Double.Parse works" <| fun () ->
        parse Double.Parse "1.5" |> equal 1.5

    testCase "System.Double.TryParse works" <| fun () ->
        tryParse Double.TryParse 0.0 "1" |> equal (true, 1.0)
        tryParse Double.TryParse 0.0 "    1     " |> equal (true, 1.0)
        tryParse Double.TryParse 0.0 "1.5" |> equal (true, 1.5)
        tryParse Double.TryParse 0.0 "    1.5     " |> equal (true, 1.5)
        tryParse Double.TryParse 0.0 "foo" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "      " |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "9X" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "X9" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "X9TRE34" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "9SayWhat12Huh" |> equal (false, 0.0)
        tryParse Double.TryParse 0.0 "-1.5" |> equal (true, -1.5)

    testCase "System.Decimal.Parse works" <| fun () ->
        parse Decimal.Parse "1.5" |> equal 1.5M

    testCase "System.Decimal.TryParse works" <| fun () ->
        let equal expected (success, actual) =
            match expected with
            | Some expected -> equal true success; equal expected actual
            | None -> equal false success
        tryParse Decimal.TryParse 0.0M "1" |> equal (Some 1.0M)
        tryParse Decimal.TryParse 0.0M "    1     " |> equal (Some 1.0M)
        tryParse Decimal.TryParse 0.0M "1.5" |> equal (Some 1.5M)
        tryParse Decimal.TryParse 0.0M "    1.5     " |> equal (Some 1.5M)
        tryParse Decimal.TryParse 0.0M "foo" |> equal None
        tryParse Decimal.TryParse 0.0M "9X" |> equal None
        tryParse Decimal.TryParse 0.0M "X9" |> equal None
        tryParse Decimal.TryParse 0.0M "X9TRE34" |> equal None
        tryParse Decimal.TryParse 0.0M "9SayWhat12Huh" |> equal None
        tryParse Decimal.TryParse 0.0M "-1.5" |> equal (Some -1.5M)

    testCase "System.Single.Parse works" <| fun () ->
        parse Single.Parse "1.5" |> equal 1.5f

    testCase "System.Single.TryParse works" <| fun () ->
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

    testCase "System.Boolean.Parse works" <| fun () ->
        Boolean.Parse "true" |> equal true
        Boolean.Parse "True" |> equal true
        Boolean.Parse " true " |> equal true
        Boolean.Parse "false" |> equal false
        Boolean.Parse "False" |> equal false
        Boolean.Parse " false " |> equal false

        throwsAnyError (fun () -> Boolean.Parse "tru")
        throwsAnyError (fun () -> Boolean.Parse "falsee")

    testCase "System.Boolean.TryParse works" <| fun () ->
        Boolean.TryParse "true" |> equal (true, true)
        Boolean.TryParse "True" |> equal (true, true)
        Boolean.TryParse " true " |> equal (true, true)
        Boolean.TryParse "false" |> equal (true, false)
        Boolean.TryParse "False" |> equal (true, false)
        Boolean.TryParse " false " |> equal (true, false)

        Boolean.TryParse "tru" |> equal (false, false)
        Boolean.TryParse "falsee" |> equal (false, false)

    testCase "System.SByte.Parse works" <| fun () ->
        SByte.Parse("5") |> equal 5y
        SByte.Parse("-5") |> equal -5y
        SByte.Parse("-128") |> equal -128y
        (fun () -> SByte.Parse("128")) |> throwsError ""
        (fun () -> SByte.Parse("5f")) |> throwsError "Input string was not in a correct format."
        (fun () -> SByte.Parse("F")) |> throwsError "Input string was not in a correct format."
        (fun () -> SByte.Parse("5o")) |> throwsError "Input string was not in a correct format."

    testCase "System.SByte.Parse with hex works" <| fun () ->
        SByte.Parse("55", System.Globalization.NumberStyles.HexNumber) |> equal 85y
        SByte.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95y
        SByte.Parse("FF", System.Globalization.NumberStyles.HexNumber) |> equal -1y
        (fun () -> SByte.Parse("1FF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
        (fun () -> SByte.Parse("5o", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
        (fun () -> SByte.Parse("o5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

    testCase "System.Int16.Parse works" <| fun () ->
        Int16.Parse("5") |> equal 5s
        Int16.Parse("-5") |> equal -5s
        Int16.Parse("-32768") |> equal -32768s
        (fun () -> Int16.Parse("32768")) |> throwsError ""
        (fun () -> Int16.Parse("5f")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int16.Parse("FFF")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int16.Parse("5fo0")) |> throwsError "Input string was not in a correct format."

    testCase "System.Int16.Parse with hex works" <| fun () ->
        Int16.Parse("5555", System.Globalization.NumberStyles.HexNumber) |> equal 21845s
        Int16.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95s
        Int16.Parse("FFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1s
        (fun () -> Int16.Parse("1FFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
        (fun () -> Int16.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
        (fun () -> Int16.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

    testCase "System.Int32.Parse works" <| fun () ->
        Int32.Parse("5") |> equal 5
        Int32.Parse("-5") |> equal -5
        Int32.Parse("-2147483648") |> equal -2147483648
        (fun () -> Int32.Parse("2147483648")) |> throwsError ""
        (fun () -> Int32.Parse("5f")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int32.Parse("f5")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int32.Parse("foo")) |> throwsError "Input string was not in a correct format."

    testCase "System.Int32.Parse with hex works" <| fun () ->
        Int32.Parse("555555", System.Globalization.NumberStyles.HexNumber) |> equal 5592405
        Int32.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95
        Int32.Parse("FFFFFFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1
        (fun () -> Int32.Parse("1FFFFFFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
        (fun () -> Int32.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
        (fun () -> Int32.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

    testCase "System.Int64.Parse works" <| fun () ->
        Int64.Parse("5") |> equal 5L
        Int64.Parse("-5") |> equal -5L
        Int64.Parse("-9223372036854775808") |> equal -9223372036854775808L
        (fun () -> Int64.Parse("9223372036854775808")) |> throwsError ""
        (fun () -> Int64.Parse("5f")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int64.Parse("f5")) |> throwsError "Input string was not in a correct format."
        (fun () -> Int64.Parse("foo")) |> throwsError "Input string was not in a correct format."

    testCase "System.Int64.Parse with hex works" <| fun () ->
        Int64.Parse("555555", System.Globalization.NumberStyles.HexNumber) |> equal 5592405L
        Int64.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95L
        Int64.Parse("FFFFFFFFFFFFFFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1L
        (fun () -> Int64.Parse("1FFFFFFFFFFFFFFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
        (fun () -> Int64.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
        (fun () -> Int64.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

    testCase "System.Int64.TryParse works" <| fun () ->
        tryParse Int64.TryParse 0L "99" |> equal (true, 99L)
        tryParse Int64.TryParse 0L "foo" |> equal (false, 0L)

    testCase "System.UInt32.TryParse works" <| fun () ->
        tryParse UInt32.TryParse 0u "99" |> equal (true, 99u)
        tryParse UInt32.TryParse 0u "foo" |> equal (false, 0u)

    testCase "System.UInt64.TryParse works" <| fun () ->
        tryParse UInt64.TryParse 0UL "99" |> equal (true, 99UL)
        tryParse UInt64.TryParse 0UL "foo" |> equal (false, 0UL)

    testCase "Parsing integers with different radices works" <| fun () ->
        equal 11 (int "11")
        equal 17 (int "0x11")
        equal 9  (int "0o11")
        equal 3  (int "0b11")

    testCase "System.Int32.TryParse works" <| fun () ->
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

    testCase "BigInt.TryParse works" <| fun () ->
        tryParse bigint.TryParse 0I "4234523548923954" |> equal (true, 4234523548923954I)
        tryParse bigint.TryParse 0I "9SayWhat12Huh" |> equal (false, 0I)

    testCase "System.Int32.ToString works" <| fun () ->
        (5592405).ToString() |> equal "5592405"

    testCase "System.Int32.ToString 'd' works" <| fun () ->
        (5592405).ToString("d") |> equal "5592405"
        (5592405).ToString("d10") |> equal "0005592405"

    testCase "System.Int32.ToString 'x' works" <| fun () ->
        (5592405).ToString("x") |> equal "555555"
        (5592405).ToString("x10") |> equal "0000555555"

    testCase "System.Int64.ToString works" <| fun () ->
        (5592405L).ToString() |> equal "5592405"

    testCase "System.Int64.ToString 'd' works" <| fun () ->
        (5592405L).ToString("d") |> equal "5592405"
        (5592405L).ToString("d10") |> equal "0005592405"

    testCase "System.Int64.ToString 'x' works" <| fun () ->
        (5592405L).ToString("x") |> equal "555555"
        (5592405L).ToString("x10") |> equal "0000555555"

    testCase "System.BigInt.ToString works" <| fun () ->
        (5592405I).ToString() |> equal "5592405"

    testCase "System.Decimal.ToString works" <| fun () ->
        (5592405M).ToString() |> equal "5592405"

    //-------------------------------------
    // System.Convert
    //-------------------------------------

    testCase "System.Convert.ToSByte works" <| fun () ->
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

    testCase "System.Convert.ToInt16 works" <| fun () ->
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

    testCase "System.Convert.ToInt32 works" <| fun () ->
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

    testCase "Special cases conversion to/from Int64 work" <| fun () ->
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

    testCase "Special cases conversion to UInt64 work" <| fun () -> // See #1880
        uint64 "0x9fffffffffffffff" |> equal 11529215046068469759UL
        uint64 "0xafffffffffffffff" |> equal 12682136550675316735UL
        uint64 "0xAFFFFFFFFFFFFFFF" |> equal 12682136550675316735UL
        uint64 "0x9fffffff_ffffffff" |> equal 11529215046068469759UL
        uint64 "0x9fff_ffff_ffff_ffff" |> equal 11529215046068469759UL

    testCase "System.Convert.ToInt64 works" <| fun () ->
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

    testCase "System.Convert.ToByte works" <| fun () ->
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

    testCase "System.Convert.ToUInt16 works" <| fun () ->
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

    testCase "System.Convert.ToUInt32 works" <| fun () ->
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

    testCase "System.Convert.ToUInt64 works" <| fun () ->
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

    testCase "Convert between (un)signed long" <| fun () -> // See #1485
        int64 System.UInt64.MaxValue |> equal -1L
        uint64 -1L |> equal System.UInt64.MaxValue

    testCase "int64 can parse signed longs" <| fun () -> // See #1586
        let a = int64 "5"
        let b = int64 "-5"
        let c = int64 "+5"
        equal 5L a
        equal -5L b
        a = b |> equal false
        a = c |> equal true

    testCase "System.Convert.ToSingle works" <| fun () ->
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

    testCase "System.Convert.ToDouble works" <| fun () ->
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

    testCase "System.Convert.ToDecimal works" <| fun () ->
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
        (fun () -> decimal("foo")) |> throwsError ""

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
        (fun () -> Convert.ToDecimal("foo")) |> throwsError ""

    // String to number convertions (with base)

    testCase "System.Convert.ToSByte with base works" <| fun () ->
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

    testCase "System.Convert.ToInt16 with base works" <| fun () ->
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

    testCase "System.Convert.ToInt32 with base works" <| fun () ->
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

    testCase "System.Convert.ToInt64 with base works" <| fun () ->
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

    testCase "System.Convert.ToByte with base works" <| fun () ->
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

    testCase "System.Convert.ToUInt16 with base works" <| fun () ->
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

    testCase "System.Convert.ToUInt32 with base works" <| fun () ->
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

    testCase "System.Convert.ToUInt64 with base works" <| fun () ->
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

    testCase "System.Convert.ToString with base works" <| fun () ->
        Convert.ToString(Byte.MaxValue,2) |> equal "11111111"
        Convert.ToString(Int16.MaxValue,2) |> equal "111111111111111"
        Convert.ToString(Int32.MaxValue,2) |> equal "1111111111111111111111111111111"
        Convert.ToString(Int64.MaxValue,2) |> equal "111111111111111111111111111111111111111111111111111111111111111"

    testCase "System.Convert.ToString SByte works" <| fun () ->
        let x = "101"
        Convert.ToString(101y) |> equal x

    testCase "System.Convert.ToString Int16 works" <| fun () ->
        let x = "101"
        Convert.ToString(101s) |> equal x
        Convert.ToString(5s, 2) |> equal x
        Convert.ToString(65s, 8) |> equal x
        Convert.ToString(101s, 10) |> equal x
        Convert.ToString(257s, 16) |> equal x
        Convert.ToString(-5s, 16) |> equal "fffb"

    testCase "System.Convert.ToString Int32 works" <| fun () ->
        let x = "101"
        Convert.ToString(101) |> equal x
        Convert.ToString(5, 2) |> equal x
        Convert.ToString(65, 8) |> equal x
        Convert.ToString(101, 10) |> equal x
        Convert.ToString(257, 16) |> equal x
        Convert.ToString(-5, 16) |> equal "fffffffb"

    testCase "System.Convert.ToString Int64 works" <| fun () ->
        let x = "101"
        Convert.ToString(101L) |> equal x
        Convert.ToString(5L, 2) |> equal x
        Convert.ToString(65L, 8) |> equal x
        Convert.ToString(101L, 10) |> equal x
        Convert.ToString(257L, 16) |> equal x
        // TODO long.js lib always use negative sign to convert negative longs to strings
        // Convert.ToString(-5L, 16) |> equal "fffffffffffffffb"

    testCase "System.Convert.ToString Byte works" <| fun () ->
        let x = "101"
        Convert.ToString(101uy) |> equal x
        Convert.ToString(5uy, 2) |> equal x
        Convert.ToString(65uy, 8) |> equal x
        Convert.ToString(101uy, 10) |> equal x

    testCase "System.Convert.ToString UInt16 works" <| fun () ->
        let x = "101"
        Convert.ToString(101us) |> equal x

    testCase "System.Convert.ToString UInt32 works" <| fun () ->
        let x = "101"
        Convert.ToString(101u) |> equal x

    testCase "System.Convert.ToString UInt64 works" <| fun () ->
        let x = "101"
        Convert.ToString(101uL) |> equal x

    testCase "System.Convert.ToString Single works" <| fun () ->
        let x = "101"
        Convert.ToString(101.f) |> equal x

    testCase "System.Convert.ToString Double works" <| fun () ->
        let x = "101"
        Convert.ToString(101.) |> equal x

    testCase "System.Convert.ToString Decimal works" <| fun () ->
        let x = "101"
        Convert.ToString(101.m) |> equal x

    testCase "FSharp.Core type converters can combined via the >> operator" <| fun () ->
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

    testCase "BitConverter.IsLittleEndian works" <| fun () ->
        BitConverter.IsLittleEndian |> equal true

    testCase "BitConverter.GetBytes Boolean works" <| fun () ->
        let value = true
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 1uy |]

    testCase "BitConverter.GetBytes Char works" <| fun () ->
        let value = 'A'
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 65uy; 0uy |]

    testCase "BitConverter.GetBytes Int16 works" <| fun () ->
        let value = 0x0102s
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 2uy; 1uy |]

    testCase "BitConverter.GetBytes Int32 works" <| fun () ->
        let value = 0x01020304
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 4uy; 3uy; 2uy; 1uy |]

    testCase "BitConverter.GetBytes Int64 works" <| fun () ->
        let value = 0x0102030405060708L
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 1uy |]

    testCase "BitConverter.GetBytes UInt16 works" <| fun () ->
        let value = 0xFF02us
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 2uy; 255uy |]

    testCase "BitConverter.GetBytes UInt32 works" <| fun () ->
        let value = 0xFF020304u
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 4uy; 3uy; 2uy; 255uy |]

    testCase "BitConverter.GetBytes UInt64 works" <| fun () ->
        let value = 0xFF02030405060708UL
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 8uy; 7uy; 6uy; 5uy; 4uy; 3uy; 2uy; 255uy |]

    testCase "BitConverter.GetBytes Single works" <| fun () ->
        let value = 1.0f
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 0uy; 0uy; 128uy; 63uy |]

    testCase "BitConverter.GetBytes Double works" <| fun () ->
        let value = 1.0
        let bytes = BitConverter.GetBytes(value)
        bytes |> equal [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 240uy; 63uy |]

    testCase "BitConverter.Int64BitsToDouble works" <| fun () ->
        let f = BitConverter.Int64BitsToDouble(1L)
        f |> equal 4.9406564584124654E-324

    testCase "BitConverter.DoubleToInt64Bits works" <| fun () ->
        let i = BitConverter.DoubleToInt64Bits(1.0)
        i |> equal 4607182418800017408L

    testCase "BitConverter.ToBoolean works" <| fun () ->
        let value = true
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToBoolean(bytes, 0) |> equal value

    testCase "BitConverter.ToChar works" <| fun () ->
        let value = 'A'
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToChar(bytes, 0) |> equal value

    testCase "BitConverter.ToInt16 works" <| fun () ->
        let value = 0x0102s
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToInt16(bytes, 0) |> equal value

    testCase "BitConverter.ToInt32 works" <| fun () ->
        let value = 0x01020304
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToInt32(bytes, 0) |> equal value

    testCase "BitConverter.ToInt64 works" <| fun () ->
        let value = 0x0102030405060708L
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToInt64(bytes, 0) |> equal value

    testCase "BitConverter.ToUInt16 works" <| fun () ->
        let value = 0xFF02us
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToUInt16(bytes, 0) |> equal value

    testCase "BitConverter.ToUInt32 works" <| fun () ->
        let value = 0xFF020304u
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToUInt32(bytes, 0) |> equal value

    testCase "BitConverter.ToUInt64 works" <| fun () ->
        let value = 0xFF02030405060708UL
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToUInt64(bytes, 0) |> equal value

    testCase "BitConverter.ToSingle works" <| fun () ->
        let value = 1.0f
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToSingle(bytes, 0) |> equal value

    testCase "BitConverter.ToDouble works" <| fun () ->
        let value = 1.0
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToDouble(bytes, 0) |> equal value

    testCase "BitConverter.ToString works" <| fun () ->
        let value = 0x01020304
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToString(bytes) |> equal "04-03-02-01"

    testCase "BitConverter.ToString 2 works" <| fun () ->
        let value = 0x01020304
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToString(bytes, 1) |> equal "03-02-01"

    testCase "BitConverter.ToString 3 works" <| fun () ->
        let value = 0x01020304
        let bytes = BitConverter.GetBytes(value)
        BitConverter.ToString(bytes, 1, 2) |> equal "03-02"

    //-------------------------------------
    // System.Numerics.BigInteger
    //-------------------------------------

    testCase "BigInt from uint32 works" <| fun () ->
        bigint System.UInt32.MaxValue |> equal 4294967295I

    testCase "BigInt ToSByte works" <| fun () ->
        let value = 0x02y
        sbyte (bigint (int32 value)) |> equal value

    testCase "BigInt ToInt16 works" <| fun () ->
        let value = 0x0102s
        int16 (bigint (int32 value)) |> equal value

    testCase "BigInt ToInt32 works" <| fun () ->
        let value = 0x01020304
        int32 (bigint value) |> equal value

    testCase "BigInt ToInt64 works" <| fun () ->
        let value = 0x0102030405060708L
        int64 (bigint value) |> equal value

    testCase "BigInt ToByte works" <| fun () ->
        let value = 0x02uy
        byte (bigint (uint32 value)) |> equal value

    testCase "BigInt ToUInt16 works" <| fun () ->
        let value = 0xFF02us
        uint16 (bigint (uint32 value)) |> equal value

    testCase "BigInt ToUInt32 works" <| fun () ->
        //let value = 0xFF020304u //TODO: BigInt.FromUInt32 not implemented yet, so this will fail
        let value = 0x1F020304u
        uint32 (bigint value) |> equal value

    testCase "BigInt ToUInt64 works" <| fun () ->
        let value = 0xFF02030405060708UL
        uint64 (bigint value) |> equal value

    testCase "BigInt ToSingle works" <| fun () ->
        let value = 1.0f
        single (bigint value) |> equal value

    testCase "BigInt ToDouble works" <| fun () ->
        let value = -1.0
        double (bigint value) |> equal value

    testCase "BigInt ToDecimal works" <| fun () ->
        let value = 1.0m
        decimal (bigint value) |> equal value

    testCase "BigInt ToString works" <| fun () ->
        let value = 1234567890
        string (bigint value) |> equal "1234567890"

    testCase "Convert.ToBase64String works" <| fun () ->
        let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
        Convert.ToBase64String(bytes)
        |> equal "AgQGCAoMDhASFA=="

    testCase "Convert.FromBase64String works" <| fun () ->
        Convert.FromBase64String("AgQGCAoMDhASFA==")
        |> equal [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]

    // id is prefixed for guid creation as we check at compile time (if able) to create a string const
    testCase "Guid.Parse works" <| fun () ->
        let guids = [
            Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid.Parse("96258006c4ba4a7f80c4de7f2b2898c5")
            Guid.Parse(id "96258006c4ba4a7f80c4de7f2b2898c5")
            Guid.Parse("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid.Parse(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid.Parse("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid.Parse(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid.Parse("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
            Guid.Parse(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
            Guid("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid("96258006c4ba4a7f80c4de7f2b2898c5")
            Guid(id "96258006c4ba4a7f80c4de7f2b2898c5")
            Guid("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
            Guid(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        ]

        guids
        |> List.iter (fun g -> g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5")

    // testCase "Guid.Parse fails if string is not well formed" <| fun () ->
    //     let success =
    //         try
    //             let g1 = Guid.Parse(id "foo")
    //             true
    //         with _ -> false
    //     equal false success

    testCase "Guid.TryParse works" <| fun () ->
        let successGuids = [
            Guid.TryParse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid.TryParse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
            Guid.TryParse("96258006c4ba4a7f80c4de7f2b2898c5")
            Guid.TryParse(id "96258006c4ba4a7f80c4de7f2b2898c5")
            Guid.TryParse("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid.TryParse(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
            Guid.TryParse("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid.TryParse(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
            Guid.TryParse("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
            Guid.TryParse(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        ]

        let failGuids = [
            Guid.TryParse("96258006-c4ba-4a7f-80c4")
            Guid.TryParse(id "96258006-c4ba-4a7f-80c4")
            Guid.TryParse("96258007f80c4de7f2b2898c5")
            Guid.TryParse(id "96258007f80c4de7f2b2898c5")
            Guid.TryParse("{96258006-c4ba-4a7f-80c4}")
            Guid.TryParse(id "{96258006-c4ba-4a7f-80c4}")
            Guid.TryParse("(96258006-c4ba-80c4-de7f2b2898c5)")
            Guid.TryParse(id "(96258006-c4ba-80c4-de7f2b2898c5)")
            Guid.TryParse("{0x96258006,0xc4ba,{0x80,0xc4,0xde,0x7f,0x28,0x98,0xc5}}")
            Guid.TryParse(id "{0x96258006,0xc4ba,{0x80,0xc4,0xde,0x7f,0x28,0x98,0xc5}}")
        ]

        successGuids
        |> List.iter (fst >> (equal true))

        failGuids
        |> List.iter (fst >> (equal false))

    testCase "Parsed guids with different case are considered the same" <| fun () -> // See #1718
        let aGuid = Guid.NewGuid()

        let lower = aGuid.ToString().ToLower()
        let upper = aGuid.ToString().ToUpper()
        lower = upper |> equal false

        let lowerGuid = Guid.Parse lower
        let upperGuid = Guid.Parse upper
        lowerGuid = upperGuid |> equal true

    testCase "Convert Guid to byte[] works" <| fun () ->
        let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        let g2 = Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")

        g.ToByteArray() |> equal [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]
        g2.ToByteArray() |> equal [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]

    testCase "Convert byte[] to Guid works" <| fun () ->
        let g = Guid [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]
        g.ToString() |> equal("96258006-c4ba-4a7f-80c4-de7f2b2898c5")

    testCase "Guid.ToString works with formats" <| fun () ->
        let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        let g2 = Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")

        let testGuid (g: Guid) =
            g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
            g.ToString("N") |> equal "96258006c4ba4a7f80c4de7f2b2898c5"
            g.ToString("D") |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
            g.ToString("B") |> equal "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}"
            g.ToString("P") |> equal "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)"
            g.ToString("X") |> equal "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}"

        testGuid g
        testGuid g2

    //-------------------------------------
    // System.Text.Encoding
    //-------------------------------------

    testCase "Encoding.Unicode.GetBytes works" <| fun () ->
        System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
        |> equal [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

    testCase "Encoding.Unicode.GetBytes for range works" <| fun () ->
        System.Text.Encoding.Unicode.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
        |> equal [| 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]

    testCase "Encoding.Unicode.GetString works" <| fun () ->
        let bytes = [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]
        System.Text.Encoding.Unicode.GetString(bytes)
        |> equal "za\u0306\u01FD\u03B2\uD8FF\uDCFF"

    testCase "Encoding.Unicode.GetString for range works" <| fun () ->
        let bytes = [| 0x7Auy; 0x00uy; 0x61uy; 0x00uy; 0x06uy; 0x03uy; 0xFDuy; 0x01uy; 0xB2uy; 0x03uy; 0xFFuy; 0xD8uy; 0xFFuy; 0xDCuy |]
        System.Text.Encoding.Unicode.GetString(bytes, 8, 6)
        |> equal "\u03B2\uD8FF\uDCFF"

    testCase "Encoding.UTF8.GetBytes works" <| fun () ->
        System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF")
        |> equal [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

    testCase "Encoding.UTF8.GetBytes for range works" <| fun () ->
        System.Text.Encoding.UTF8.GetBytes("za\u0306\u01FD\u03B2\uD8FF\uDCFF".ToCharArray(), 4, 3)
        |> equal [| 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]

    testCase "Encoding.UTF8.GetString works" <| fun () ->
        let bytes = [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]
        System.Text.Encoding.UTF8.GetString(bytes)
        |> equal "za\u0306\u01FD\u03B2\uD8FF\uDCFF"

    testCase "Encoding.UTF8.GetString for range works" <| fun () ->
        let bytes = [| 0x7Auy; 0x61uy; 0xCCuy; 0x86uy; 0xC7uy; 0xBDuy; 0xCEuy; 0xB2uy; 0xF1uy; 0x8Fuy; 0xB3uy; 0xBFuy |]
        System.Text.Encoding.UTF8.GetString(bytes, 6, 6)
        |> equal "\u03B2\uD8FF\uDCFF"

  ]
