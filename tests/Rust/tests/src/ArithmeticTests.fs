module Fable.Tests.ArithmeticTests

open System
open Util.Testing

let [<Literal>] posLiteral = 5
let [<Literal>] negLiteral = -345
let notALiteral = 5

let checkTo3dp (expected: float) actual =
    floor (actual * 1000.) |> equal expected

[<Fact>]
let ``Adding floats works`` () =
    3.141 + 2.85 |> equal 5.991

[<Fact>]
let ``Infix add can be generated`` () =
    4 + 2 |> equal 6

[<Fact>]
let ``Int32 literal addition is optimized`` () =
    posLiteral + 7 |> equal 12
    notALiteral + 7 |> equal 12

[<Fact>]
let ``Unary negation with negative literal values works`` () =
    -negLiteral |> equal 345

// [<Fact>]
// let ``Unary negation with integer MinValue works`` () =
//     -(-128y) |> equal SByte.MinValue
//     -(-32768s) |> equal Int16.MinValue
//     -(-2147483648) |> equal Int32.MinValue
//     -(-9223372036854775808L) |> equal Int64.MinValue

[<Fact>]
let ``Infix subtract can be generated`` () =
    4 - 2 |> equal 2

[<Fact>]
let ``Infix multiply can be generated`` () =
    4 * 2 |> equal 8

[<Fact>]
let ``Infix divide can be generated`` () =
    4 / 2 |> equal 2

[<Fact>]
let ``Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5 / 2 |> equal 2
    5 / 3 |> equal 1
    // float 5 / 2. |> equal 2.5 // TODO: Number conversion

[<Fact>]
let ``Infix modulo can be generated`` () =
    4 % 3 |> equal 1

[<Fact>]
let ``Evaluation order is preserved by generated code`` () =
    (4 - 2) * 2 + 1 |> equal 5

[<Fact>]
let ``Bitwise and can be generated`` () =
    6 &&& 2 |> equal 2

[<Fact>]
let ``Bitwise or can be generated`` () =
    4 ||| 2 |> equal 6

[<Fact>]
let ``Bitwise shift left can be generated`` () =
    4 <<< 2 |> equal 16

[<Fact>]
let ``Bitwise shift left with unsigned integer works`` () =
    1u <<< 31 |> equal 2147483648u

[<Fact>]
let ``Bitwise OR on large unsigned integer works`` () =
    0x80000000u ||| 0u |> equal (0x80000000u ||| 0u >>> 0)

[<Fact>]
let ``Bitwise AND on large unsigned integer works`` () =
    0x80000000u &&& 0xffffffffu |> equal (0x80000000u &&& 0xffffffffu >>> 0)

[<Fact>]
let ``Bitwise XOR on large unsigned integer works`` () =
    0x80000000u ^^^ 0u |> equal (0x80000000u ^^^ 0u >>> 0)

[<Fact>]
let ``Bitwise Invert on large unsigned integer works`` () =
    (~~~0x80000000u >>> 0) |> equal ~~~0x80000000u

[<Fact>]
let ``Bitwise shift right can be generated`` () = // See #1530
    4 >>> 2 |> equal 1

[<Fact>]
let ``Zero fill shift right (>>>) for uint32`` () = // See #646
    0x80000000 >>> 1 |> equal -1073741824
    0x80000000u >>> 1 |> equal 1073741824u

[<Fact>]
let ``UInt64 multiplication with 0 returns uint`` () = // See #1480
    0x0UL * 0x1UL |> equal 0x0UL

[<Fact>]
let ``Int64 Infix add can be generated`` () =
    4L + 2L |> equal 6L

[<Fact>]
let ``Int64 Infix subtract can be generated`` () =
    4L - 2L |> equal 2L

[<Fact>]
let ``Int64 Infix multiply can be generated`` () =
    4L * 2L |> equal 8L

[<Fact>]
let ``Int64 Infix divide can be generated`` () =
    4L / 2L |> equal 2L

[<Fact>]
let ``Int64 Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5L / 2L |> equal 2L
    5L / 3L |> equal 1L
    // float 5L / 2. |> equal 2.5 // TODO: Number conversion

[<Fact>]
let ``Int64 Infix modulo can be generated`` () =
    4L % 3L |> equal 1L

[<Fact>]
let ``Int64 Evaluation order is preserved by generated code`` () =
    (4L - 2L) * 2L + 1L |> equal 5L

[<Fact>]
let ``Int64 Bitwise and can be generated`` () =
    6L &&& 2L |> equal 2L

[<Fact>]
let ``Int64 Bitwise or can be generated`` () =
    4L ||| 2L |> equal 6L

[<Fact>]
let ``Int64 Bitwise shift left can be generated`` () =
    4L <<< 2 |> equal 16L

[<Fact>]
let ``Int64 Bitwise shift right can be generated`` () =
    4L >>> 2 |> equal 1L

[<Fact>]
let ``UInt64 Bitwise shift right can be generated`` () = // See #1482
    15210016002388773605UL >>> 33 |> equal 1770678907UL

[<Fact>]
let ``Int64 abs works`` () =
    abs -4L |> equal 4L

[<Fact>]
let ``abs works`` () =
    abs -4 |> equal 4

// [<Fact>]
// let ``round works`` () =
//     round -12.5 |> equal -12.
//     round 1.5 |> equal 2.
//     round 1.535 |> equal 2.
//     round 1.525 |> equal 2.

[<Fact>]
let ``ceil works`` () =
    ceil 11.25 |> equal 12.
    ceil -11.25 |> equal -11.
    Math.Ceiling 11.25 |> equal 12.

[<Fact>]
let ``floor works`` () =
    floor 11.75 |> equal 11.
    floor -11.75 |> equal -12.
    Math.Floor 11.25 |> equal 11.

// [<Fact>]
// let ``pown works`` () =
//     pown 2.2 3 |> checkTo3dp 10648.

[<Fact>]
let ``sqrt works`` () =
    sqrt 4.5 |> checkTo3dp 2121.

// As per https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
[<Fact>]
let ``sqrt matches .net core implementation`` () =
    checkTo3dp 1732. (sqrt 3.0)
    sqrt 0.0  |> equal 0.0
    Double.IsNaN (sqrt -3.0) |> equal true
    Double.IsNaN (sqrt Double.NaN) |> equal true
    Double.IsNaN (sqrt Double.NegativeInfinity) |> equal true
    sqrt Double.PositiveInfinity |> equal Double.PositiveInfinity

// [<Fact>]
// let ``Double.Parse works with IFormatProvider`` () =
//     // culture compiles to { } for now and it is ignored on the call-site
//     let culture = Globalization.CultureInfo.InvariantCulture
//     let result = Double.Parse(``10.5``, culture)
//     result |> equal 10.5

// [<Fact>]
// let ``Single.Parse works with IFormatProvider`` () =
//     // culture compiles to { } for now and it is ignored on the call-site
//     let culture = Globalization.CultureInfo.InvariantCulture
//     let result = Single.Parse(``10.5``, culture)
//     float result |> equal 10.5

[<Fact>]
let ``acos works`` () =
    acos 0.25 |> checkTo3dp 1318.

[<Fact>]
let ``asin works`` () =
    asin 0.25 |> checkTo3dp 252.

[<Fact>]
let ``atan works`` () =
    atan 0.25 |> checkTo3dp 244.

// [<Fact>]
// let ``atan2 works`` () =
//     atan2 90. 15. |> checkTo3dp 1405.

[<Fact>]
let ``cos works`` () =
    cos 0.25 |> checkTo3dp 968.

[<Fact>]
let ``sin works`` () =
    sin 0.25 |> checkTo3dp 247.

[<Fact>]
let ``tan works`` () =
    tan 0.25 |> checkTo3dp 255.

[<Fact>]
let ``cosh works`` () =
    cosh 0.25 |> checkTo3dp 1031.

[<Fact>]
let ``sinh works`` () =
    sinh 0.25 |> checkTo3dp 252.

[<Fact>]
let ``tanh works`` () =
    tanh 0.25 |> checkTo3dp 244.

[<Fact>]
let ``exp works`` () =
    exp 8.0 |> checkTo3dp 2980957.

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L228
[<Fact>]
let ``log works`` () =
    log 232.12 |> checkTo3dp 5447.
    checkTo3dp 1098. (log 3.0)
    log 0.0 |> equal Double.NegativeInfinity
    Double.IsNaN (log -2.0) |> equal true
    Double.IsNaN (log Double.NaN) |> equal true
    Double.IsNaN (log Double.NegativeInfinity) |> equal true
    log Double.PositiveInfinity |> equal Double.PositiveInfinity

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L239
[<Fact>]
let ``Math.Log(double, double) works`` () =
    Math.Log(8.0, 2.0) |> equal 3.0
    Math.Log(3.0, 3.0) |> equal 1.0
    Math.Log(14., 3.0) |> checkTo3dp 2402.
    Math.Log(0.0, 3.0) |> equal Double.NegativeInfinity
    Math.Log(Double.PositiveInfinity, 3.0) |> equal Double.PositiveInfinity
    Double.IsNaN (Math.Log(-3.0, 3.0)) |> equal true
    Double.IsNaN (Math.Log(Double.NaN, 3.0)) |> equal true
    Double.IsNaN (Math.Log(Double.NegativeInfinity, 3.0)) |> equal true

[<Fact>]
let ``log10 works`` () =
    log10 232.12 |> checkTo3dp 2365.

[<Fact>]
let ``PI works`` () =
    checkTo3dp 3141. Math.PI

[<Fact>]
let ``E works`` () =
    checkTo3dp 2718. Math.E

[<Fact>]
let ``Math.abs works`` () =
    Math.Abs -4 |> equal 4

// [<Fact>]
// let ``Math.pown works`` () =
//     Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

[<Fact>]
let ``Math.sqrt works`` () =
    Math.Sqrt 4.5 |> checkTo3dp 2121.

// [<Fact>]
// let ``Math.round works`` () =
//     Math.Round -12.5 |> equal -12.
//     Math.Round 1.425 |> equal 1.
//     Math.Round -1.425 |> equal -1.
//     Math.Round 1.546 |> equal 2.
//     Math.Round -1.546 |> equal -2.

// [<Fact>]
// let ``Math.round with digits works`` () =
//     Math.Round(1.426, 2) |> equal 1.43
//     Math.Round(1.426, 1) |> equal 1.4
//     Math.Round(-1.426, 2) |> equal -1.43
//     Math.Round(-1.426, 1) |> equal -1.4

[<Fact>]
let ``Math.truncate works`` () =
    Math.Truncate -12.5 |> equal -12.
    Math.Truncate 1.425 |> equal 1.
    Math.Truncate -1.425 |> equal -1.
    Math.Truncate 1.546 |> equal 1.
    Math.Truncate -1.546 |> equal -1.

[<Fact>]
let ``Math.ceil works`` () =
    Math.Ceiling 11.25 |> equal 12.

[<Fact>]
let ``Math.floor works`` () =
    Math.Floor 11.75 |> equal 11.

[<Fact>]
let ``Math.acos works`` () =
    Math.Acos 0.25 |> checkTo3dp 1318.

[<Fact>]
let ``Math.asin works`` () =
    Math.Asin 0.25 |> checkTo3dp 252.

[<Fact>]
let ``Math.atan works`` () =
    Math.Atan 0.25 |> checkTo3dp 244.

// [<Fact>]
// let ``Math.atan2 works`` () =
//     Math.Atan2(90., 15.) |> checkTo3dp 1405.

[<Fact>]
let ``Math.cos works`` () =
    Math.Cos(0.1 * Math.PI) |> checkTo3dp 951.

[<Fact>]
let ``Math.sin works`` () =
    Math.Sin(0.25 * Math.PI) |> checkTo3dp 707.

[<Fact>]
let ``Math.tan works`` () =
    Math.Tan(0.5) |> checkTo3dp 546.

[<Fact>]
let ``Math.exp works`` () =
    Math.Exp 8.0 |> checkTo3dp 2980957.

[<Fact>]
let ``Math.log works`` () =
    Math.Log 232.12 |> checkTo3dp 5447.

[<Fact>]
let ``Math.log10 works`` () =
    Math.Log10 232.12 |> checkTo3dp 2365.

[<Fact>]
let ``Long integers equality works`` () =
    let x = 5L
    let y = 5L
    let z = 6L
    (x = y) |> equal true
    (y = z) |> equal false

// [<Fact>]
// let ``Long integers comparison works`` () =
//     let x = 5L
//     let y = 5L
//     let z = 6L
//     compare x y |> equal 0
//     compare y z |> equal -1
//     compare z x |> equal 1
