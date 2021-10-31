module Fable.Tests.Convert

open System
open System.Globalization

open Util
open Util.Testing

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


[<Fact>]
let ``test System.Double.Parse works`` () =
    parse Double.Parse "1.5" |> equal 1.5

[<Fact>]
let ``test System.Double.TryParse works`` () =
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

[<Fact>]
let ``test System.Decimal.Parse works`` () =
    parse Decimal.Parse "1.5" |> equal 1.5M

[<Fact>]
let ``test System.Decimal.TryParse works`` () =
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

[<Fact>]
let ``test System.Single.Parse works`` () =
    parse Single.Parse "1.5" |> equal 1.5f

[<Fact>]
let ``test System.Single.TryParse works`` () =
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

[<Fact>]
let ``test System.Boolean.Parse works`` () =
    Boolean.Parse "true" |> equal true
    Boolean.Parse "True" |> equal true
    Boolean.Parse " true " |> equal true
    Boolean.Parse "false" |> equal false
    Boolean.Parse "False" |> equal false
    Boolean.Parse " false " |> equal false

    throwsAnyError (fun () -> Boolean.Parse "tru")
    throwsAnyError (fun () -> Boolean.Parse "falsee")

[<Fact>]
let ``test System.Boolean.TryParse works`` () =
    Boolean.TryParse "true" |> equal (true, true)
    Boolean.TryParse "True" |> equal (true, true)
    Boolean.TryParse " true " |> equal (true, true)
    Boolean.TryParse "false" |> equal (true, false)
    Boolean.TryParse "False" |> equal (true, false)
    Boolean.TryParse " false " |> equal (true, false)

    Boolean.TryParse "tru" |> equal (false, false)
    Boolean.TryParse "falsee" |> equal (false, false)

[<Fact>]
let ``test System.SByte.Parse works`` () =
    SByte.Parse("5") |> equal 5y
    SByte.Parse("-5") |> equal -5y
    SByte.Parse("-128") |> equal -128y
    (fun () -> SByte.Parse("128")) |> throwsError ""
    (fun () -> SByte.Parse("5f")) |> throwsError "Input string was not in a correct format."
    (fun () -> SByte.Parse("F")) |> throwsError "Input string was not in a correct format."
    (fun () -> SByte.Parse("5o")) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.SByte.Parse with hex works`` () =
    SByte.Parse("55", System.Globalization.NumberStyles.HexNumber) |> equal 85y
    SByte.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95y
    SByte.Parse("FF", System.Globalization.NumberStyles.HexNumber) |> equal -1y
    (fun () -> SByte.Parse("1FF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
    (fun () -> SByte.Parse("5o", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
    (fun () -> SByte.Parse("o5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int16.Parse works`` () =
    Int16.Parse("5") |> equal 5s
    Int16.Parse("-5") |> equal -5s
    Int16.Parse("-32768") |> equal -32768s
    (fun () -> Int16.Parse("32768")) |> throwsError ""
    (fun () -> Int16.Parse("5f")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int16.Parse("FFF")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int16.Parse("5fo0")) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int16.Parse with hex works`` () =
    Int16.Parse("5555", System.Globalization.NumberStyles.HexNumber) |> equal 21845s
    Int16.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95s
    Int16.Parse("FFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1s
    (fun () -> Int16.Parse("1FFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
    (fun () -> Int16.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
    (fun () -> Int16.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int32.Parse works`` () =
    Int32.Parse("5") |> equal 5
    Int32.Parse("-5") |> equal -5
    Int32.Parse("-2147483648") |> equal -2147483648
    (fun () -> Int32.Parse("2147483648")) |> throwsError ""
    (fun () -> Int32.Parse("5f")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("f5")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("foo")) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int32.Parse with hex works`` () =
    Int32.Parse("555555", System.Globalization.NumberStyles.HexNumber) |> equal 5592405
    Int32.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95
    Int32.Parse("FFFFFFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1
    (fun () -> Int32.Parse("1FFFFFFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
    (fun () -> Int32.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."
    (fun () -> Int32.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int64.Parse works`` () =
    Int64.Parse("5") |> equal 5L
    Int64.Parse("-5") |> equal -5L
    Int64.Parse("-9223372036854775808") |> equal -9223372036854775808L
    (fun () -> Int64.Parse("9223372036854775808")) |> throwsError ""
    (fun () -> Int64.Parse("5f")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int64.Parse("f5")) |> throwsError "Input string was not in a correct format."
    (fun () -> Int64.Parse("foo")) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int64.Parse with hex works``  () =
    Int64.Parse("555555", System.Globalization.NumberStyles.HexNumber) |> equal 5592405L
    Int64.Parse("5f", System.Globalization.NumberStyles.HexNumber) |> equal 95L
    Int64.Parse("FFFFFFFFFFFFFFFF", System.Globalization.NumberStyles.HexNumber) |> equal -1L
    (fun () -> Int64.Parse("1FFFFFFFFFFFFFFFF", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
    (fun () -> Int64.Parse("5foo", System.Globalization.NumberStyles.HexNumber)) |> throwsError ""
    (fun () -> Int64.Parse("foo5", System.Globalization.NumberStyles.HexNumber)) |> throwsError "Input string was not in a correct format."

[<Fact>]
let ``test System.Int64.TryParse works``  () =
    tryParse Int64.TryParse 0L "99" |> equal (true, 99L)
    tryParse Int64.TryParse 0L "foo" |> equal (false, 0L)

[<Fact>]
let ``test System.UInt32.TryParse works``  () =
    tryParse UInt32.TryParse 0u "99" |> equal (true, 99u)
    tryParse UInt32.TryParse 0u "foo" |> equal (false, 0u)

[<Fact>]
let ``test System.UInt64.TryParse works``  () =
    tryParse UInt64.TryParse 0UL "99" |> equal (true, 99UL)
    tryParse UInt64.TryParse 0UL "foo" |> equal (false, 0UL)

[<Fact>]
let ``test Parsing integers with different radices works``  () =
    equal 11 (int "11")
    equal 17 (int "0x11")
    equal 9  (int "0o11")
    equal 3  (int "0b11")

[<Fact>]
let ``test System.Int32.TryParse works`` () =
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
