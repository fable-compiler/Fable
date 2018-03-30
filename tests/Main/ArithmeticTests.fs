[<Util.Testing.TestFixture>]
module Fable.Tests.Arithmetic
open System
open Util.Testing

let inline equal (actual, expected) = Fable.Tests.Util.equal expected actual

[<Test>]
let ``Infix add can be generated``() =
    equal (4 + 2, 6)

let [<Literal>] aLiteral = 5
let notALiteral = 5

[<Test>]
let ``Int32 literal addition is optimized``() =
    equal (aLiteral + 7, 12)
    equal (notALiteral + 7, 12)

let [<Literal>] literalNegativeValue = -345

[<Test>]
let ``Unary negation with negative literal values works``() =
    equal(345, -literalNegativeValue)

[<Test>]
let ``Infix subtract can be generated``() =
    equal (4 - 2, 2)

[<Test>]
let ``Infix multiply can be generated``() =
    equal (4 * 2, 8)

[<Test>]
let ``Infix divide can be generated``() =
    equal (4 / 2, 2)

[<Test>]
let ``Integer division doesn't produce floats``() =
    equal (5. / 2., 2.5)
    equal (5 / 2, 2)
    equal (5 / 3, 1)
    equal (float 5 / 2., 2.5)

[<Test>]
let ``Decimal literals can be generated``() =
    equal (System.Decimal.Zero, 0.M)
    equal (System.Decimal.One, 1.M)

[<Test>]
let ``Infix modulo can be generated``() =
    equal (4 % 3, 1)

[<Test>]
let ``Evaluation order is preserved by generated code``() =
    equal ((4 - 2) * 2 + 1, 5)

[<Test>]
let ``Bitwise and can be generated``() =
    equal (6 &&& 2, 2)

[<Test>]
let ``Bitwise or can be generated``() =
    equal (4 ||| 2, 6)

[<Test>]
let ``Bitwise shift left can be generated``() =
    equal (4 <<< 2, 16)

[<Test>]
let ``Bitwise shift right can be generated``() =
    equal (4 >>> 2, 1)

[<Test>]
let ``Zero fill shift right (>>>) for uint32``() = // See #646
    equal (0x80000000 >>> 1, -1073741824)
    equal (0x80000000u >>> 1, 1073741824u)

[<Test>]
let ``Int64 Infix add can be generated``() =
    equal (4L + 2L, 6L)

[<Test>]
let ``Int64 Infix subtract can be generated``() =
    equal (4L - 2L, 2L)

[<Test>]
let ``Int64 Infix multiply can be generated``() =
    equal (4L * 2L, 8L)

[<Test>]
let ``Int64 Infix divide can be generated``() =
    equal (4L / 2L, 2L)

[<Test>]
let ``Int64 Integer division doesn't produce floats``() =
    equal (5. / 2., 2.5)
    equal (5L / 2L, 2L)
    equal (5L / 3L, 1L)
    equal (float 5L / 2., 2.5)

[<Test>]
let ``Int64 Infix modulo can be generated``() =
    equal (4L % 3L, 1L)

[<Test>]
let ``Int64 Evaluation order is preserved by generated code``() =
    equal ((4L - 2L) * 2L + 1L, 5L)

[<Test>]
let ``Int64 Bitwise and can be generated``() =
    equal (6L &&& 2L, 2L)

[<Test>]
let ``Int64 Bitwise or can be generated``() =
    equal (4L ||| 2L, 6L)

[<Test>]
let ``Int64 Bitwise shift left can be generated``() =
    equal (4L <<< 2, 16L)

[<Test>]
let ``Int64 Bitwise shift right can be generated``() =
    equal (4L >>> 2, 1L)

[<Test>]
let ``Int64 abs works``() =
    equal (abs -4L, 4L)

[<Test>]
let ``Big integers addition works``() =
    let x = 59823749821707124891298739821798327321028091380980I
    let y = bigint 1L
    let z = 1I
    equal(59823749821707124891298739821798327321028091380982I, (x + y + z))

[<Test>]
let ``BigInt Infix add can be generated``() =
    equal (4I + 2I, 6I)

[<Test>]
let ``BigInt Infix subtract can be generated``() =
    equal (4I - 2I, 2I)

[<Test>]
let ``BigInt Infix multiply can be generated``() =
    equal (4I * 2I, 8I)

[<Test>]
let ``BigInt Infix divide can be generated``() =
    equal (4I / 2I, 2I)

[<Test>]
let ``BigInt Integer division doesn't produce floats``() =
    equal (5. / 2., 2.5)
    equal (5I / 2I, 2I)
    equal (5I / 3I, 1I)
    // equal (float 5I / 2., 2.5)

[<Test>]
let ``BigInt Infix modulo can be generated``() =
    equal (4I % 3I, 1I)

[<Test>]
let ``BigInt Evaluation order is preserved by generated code``() =
    equal ((4I - 2I) * 2I + 1I, 5I)

[<Test>]
let ``BigInt Bitwise and can be generated``() =
    equal (6I &&& 2I, 2I)

[<Test>]
let ``BigInt Bitwise or can be generated``() =
    equal (4I ||| 2I, 6I)

[<Test>]
let ``BigInt Bitwise shift left can be generated``() =
    equal (4I <<< 2, 16I)

[<Test>]
let ``BigInt Bitwise shift right can be generated``() =
    equal (4I >>> 2, 1I)

[<Test>]
let ``BigInt abs works``() =
    equal (abs -4I, 4I)

[<Test>]
let ``abs works``() =
    equal (abs -4, 4)

[<Test>]
let ``round works``() =
    equal (round -12.5, -12.)
    equal (round 1.5, 2.)
    equal (round 1.535, 2.)
    equal (round 1.525, 2.)
    equal (System.Math.Round(1.55, 1), 1.6)

[<Test>]
let ``ceil works``() =
    equal (ceil 11.25, 12.)

[<Test>]
let ``floor works``() =
    equal (floor 11.75, 11.)

let checkTo3dp (expected: float) actual =
    equal (floor(actual * 1000.), expected)

[<Test>]
let ``pown works``() =
    pown 2.2 3 |> checkTo3dp 10648.

[<Test>]
let ``sqrt works``() =
    sqrt 4.5 |> checkTo3dp 2121.

let positiveInfinity = System.Double.PositiveInfinity
let negativeInfinity = System.Double.NegativeInfinity
let NaN = System.Double.NaN
let isNaN = fun x -> System.Double.IsNaN(x)

// As per
// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
[<Test>]
let ``sqrt matches .net core implementation``() =
    checkTo3dp 1732. (sqrt 3.0)
    equal (sqrt 0.0 , 0.0)
    equal (isNaN (sqrt -3.0), true)
    equal (isNaN (sqrt NaN), true)
    equal (isNaN (sqrt negativeInfinity), true)
    equal (sqrt positiveInfinity, positiveInfinity)


[<Test>]
let ``Double.Parse works with IFormatProvider``() =
    // culture compiles to { } for now and it is ignore on the call-site
    let culture = Globalization.CultureInfo.InvariantCulture
    let result = System.Double.Parse("10.5", culture)
    equal(10.5, result)

[<Test>]
let ``Single.Parse works with IFormatProvider``() =
    // culture compiles to { } for now and it is ignore on the call-site
    let culture = Globalization.CultureInfo.InvariantCulture
    let result = System.Single.Parse("10.5", culture)
    equal(10.5, float result)


[<Test>]
let ``acos works``() =
    acos 0.25 |> checkTo3dp 1318.

[<Test>]
let ``asin works``() =
    asin 0.25 |> checkTo3dp 252.

[<Test>]
let ``atan works``() =
    atan 0.25 |> checkTo3dp 244.

[<Test>]
let ``atan2 works``() =
    atan2 90. 15. |> checkTo3dp 1405.

[<Test>]
let ``cos works``() =
    cos 0.25 |> checkTo3dp 968.

[<Test>]
let ``sin works``() =
    sin 0.25 |> checkTo3dp 247.

[<Test>]
let ``tan works``() =
    tan 0.25 |> checkTo3dp 255.

[<Test>]
let ``exp works``() =
    exp 8.0 |> checkTo3dp 2980957.

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L228
[<Test>]
let ``log works``() =
    log 232.12 |> checkTo3dp 5447.
    checkTo3dp 1098. (log 3.0)
    equal (log 0.0, negativeInfinity)
    equal (isNaN (log -2.0), true)
    equal (isNaN (log NaN), true)
    equal (isNaN (log negativeInfinity), true)
    equal (log positiveInfinity, positiveInfinity)

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L239
let ``Math.Log(double, double) works``() =
    equal (3.0, Math.Log(8.0, 2.0))
    equal (1.0, Math.Log(3.0, 3.0))
    Math.Log(14., 3.0) |> checkTo3dp 2402.
    equal (negativeInfinity, Math.Log(0.0, 3.0))
    equal (positiveInfinity, Math.Log(positiveInfinity, 3.0))
    equal (true, isNaN (Math.Log(-3.0, 3.0)))
    equal (true, isNaN (Math.Log(NaN, 3.0)))
    equal (true, isNaN (Math.Log(negativeInfinity, 3.0)))


[<Test>]
let ``log10 works``() =
    log10 232.12 |> checkTo3dp 2365.

[<Test>]
let ``PI works``() =
    checkTo3dp 3141. Math.PI

[<Test>]
let ``E works``() =
    checkTo3dp 2718. Math.E

[<Test>]
let ``Math.abs works``() =
    equal(Math.Abs -4, 4)

[<Test>]
let ``Math.pown works``() =
    Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

[<Test>]
let ``Math.sqrt works``() =
    Math.Sqrt 4.5 |> checkTo3dp 2121.

[<Test>]
let ``Math.round works``() =
    equal(Math.Round -12.5, -12.)

[<Test>]
let ``Math.ceil works``() =
    equal(Math.Ceiling 11.25, 12.)

[<Test>]
let ``Math.floor works``() =
    equal(Math.Floor 11.75, 11.)

[<Test>]
let ``Math.acos works``() =
    Math.Acos 0.25 |> checkTo3dp 1318.

[<Test>]
let ``Math.asin works``() =
    Math.Asin 0.25 |> checkTo3dp 252.

[<Test>]
let ``Math.atan works``() =
    Math.Atan 0.25 |> checkTo3dp 244.

[<Test>]
let ``Math.atan2 works``() =
    Math.Atan2(90., 15.) |> checkTo3dp 1405.

[<Test>]
let ``Math.cos works``() =
    Math.Cos(0.1 * Math.PI) |> checkTo3dp 951.

[<Test>]
let ``Math.sin works``() =
    Math.Sin(0.25 * Math.PI) |> checkTo3dp 707.

[<Test>]
let ``Math.tan works``() =
    Math.Tan(0.5) |> checkTo3dp 546.

[<Test>]
let ``Math.exp works``() =
    Math.Exp 8.0 |> checkTo3dp 2980957.

[<Test>]
let ``Math.log works``() =
    Math.Log 232.12 |> checkTo3dp 5447.

[<Test>]
let ``Math.log10 works``() =
    Math.Log10 232.12 |> checkTo3dp 2365.

[<Test>]
let ``incr works``() =
    let i = ref 5
    incr i
    equal(!i, 6)

[<Test>]
let ``decr works``() =
    let i = ref 5
    decr i
    equal(!i, 4)

[<Test>]
let ``System.Random works``() =
    let rnd = System.Random()
    let x = rnd.Next(5)
    equal(true, x >= 0 && x < 5)

    let y = rnd.NextDouble()
    equal(true, y >= 0.0 && y < 1.0)

let equals (x:'a) (y:'a) = x = y
let compareTo (x:'a) (y:'a) = compare x y

[<Test>]
let ``Long integers equality works``() =
    let x = 5L
    let y = 5L
    let z = 6L
    equal(true, (x = y))
    equal(false, (y = z))
    equal(true, equals y x)
    equal(false, equals z x)

[<Test>]
let ``Long integers comparison works``() =
    let x = 5L
    let y = 5L
    let z = 6L
    equal(0, compare x y)
    equal(-1, compare y z)
    equal(0, compareTo y x)
    equal(1, compareTo z x)

[<Test>]
let ``bigint equality works``() =
    let a = 9007199254740992I
    let b = 9007199254740993I
    equal(false, (a = b))

let ``Big integers equality works``() =
    let x = 59823749821707124891298739821798327321028091380980I
    let y = 59823749821707124891298739821798327321028091380980I
    let z = 59823749821707124891298739821798327321028091380981I
    equal(true, (x = y))
    equal(false, (y = z))
    equal(true, equals y x)
    equal(false, equals z x)

[<Test>]
let ``Big integers comparison works``() =
    let x = 5I
    let y = 5I
    let z = 6I
    equal(0, compare x y)
    equal(-1, compare y z)
    equal(0, compareTo y x)
    equal(1, compareTo z x)

[<Test>]
let ``Big integers multiplication works``() =
    equal(0I, 0I * 1I)

let decimalOne = 1M
let decimalTwo = 2M

[<Test>]
let ``Member values of decimal type can be compared``() = // See #747
    equal(true, decimalOne < decimalTwo)
    equal(false, decimalOne > decimalTwo)

[<Test>]
let ``Sign operator works``() = // See #1311
    equal(1, sign 1)
    equal(1, sign 34)
    equal(1, sign 1L)
    equal(1, sign 36L)
    equal(1, sign 1.)
    equal(1, sign 89)
    equal(1, sign 1)
    equal(0, sign 0)
    equal(0, sign 0L)
    equal(0, sign 0.)
    equal(-1, sign -1)
    equal(-1, sign -56)
    equal(-1, sign -1L)
    equal(-1, sign -72L)
    equal(-1, sign -1.)
    equal(-1, sign -89.)
