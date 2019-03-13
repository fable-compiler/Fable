module Fable.Tests.Arithmetic

open System
open Util.Testing

// For historical reasons, this file uses `equal` with tuple arguments
let inline equal (actual, expected) = equal expected actual

let [<Literal>] aLiteral = 5
let notALiteral = 5
let [<Literal>] literalNegativeValue = -345

let checkTo3dp (expected: float) actual =
    equal (floor(actual * 1000.), expected)

let positiveInfinity = System.Double.PositiveInfinity
let negativeInfinity = System.Double.NegativeInfinity
let isNaN = fun x -> System.Double.IsNaN(x)

let equals (x:'a) (y:'a) = x = y
let compareTo (x:'a) (y:'a) = compare x y

let decimalOne = 1M
let decimalTwo = 2M

let tests =
  testList "Arithmetic" [
    testCase "Infix add can be generated" <| fun () ->
        equal (4 + 2, 6)

    testCase "Int32 literal addition is optimized" <| fun () ->
        equal (aLiteral + 7, 12)
        equal (notALiteral + 7, 12)

    testCase "Unary negation with negative literal values works" <| fun () ->
        equal(345, -literalNegativeValue)

    testCase "Unary negation with integer MinValue works" <| fun () ->
        equal(-(-128y), System.SByte.MinValue)
        equal(-(-32768s), System.Int16.MinValue)
        equal(-(-2147483648), System.Int32.MinValue)
        equal(-(-9223372036854775808L), System.Int64.MinValue)

    testCase "Infix subtract can be generated" <| fun () ->
        equal (4 - 2, 2)

    testCase "Infix multiply can be generated" <| fun () ->
        equal (4 * 2, 8)

    testCase "Infix divide can be generated" <| fun () ->
        equal (4 / 2, 2)

    testCase "Integer division doesn't produce floats" <| fun () ->
        equal (5. / 2., 2.5)
        equal (5 / 2, 2)
        equal (5 / 3, 1)
        // equal (float 5 / 2., 2.5) // TODO: Number conversion

    testCase "Infix modulo can be generated" <| fun () ->
        equal (4 % 3, 1)

    testCase "Evaluation order is preserved by generated code" <| fun () ->
        equal ((4 - 2) * 2 + 1, 5)

    testCase "Bitwise and can be generated" <| fun () ->
        equal (6 &&& 2, 2)

    testCase "Bitwise or can be generated" <| fun () ->
        equal (4 ||| 2, 6)

    testCase "Bitwise shift left can be generated" <| fun () ->
        equal (4 <<< 2, 16)

    testCase "Bitwise shift left with unsigned integer works" <| fun () ->
        equal (1u <<< 31, 2147483648u)

    testCase "Bitwise OR on large unsigned integer works" <| fun () ->
        equal (0x80000000u ||| 0u, (0x80000000u ||| 0u >>> 0))

    testCase "Bitwise AND on large unsigned integer works" <| fun () ->
        equal (0x80000000u &&& 0xffffffffu, (0x80000000u &&& 0xffffffffu >>> 0))

    testCase "Bitwise XOR on large unsigned integer works" <| fun () ->
        equal (0x80000000u ^^^ 0u, (0x80000000u ^^^ 0u >>> 0))

    testCase "Bitwise Invert on large unsigned integer works" <| fun () ->
        equal ( ~~~0x80000000u, (~~~0x80000000u >>> 0))

    testCase "Bitwise shift right can be generated" <| fun () -> // See #1530
        equal (4 >>> 2, 1)

    testCase "Zero fill shift right (>>>) for uint32" <| fun () -> // See #646
        equal (0x80000000 >>> 1, -1073741824)
        equal (0x80000000u >>> 1, 1073741824u)

    testCase "UInt64 multiplication with 0 returns uint" <| fun () -> // See #1480
        equal (0x0UL * 0x1UL, 0x0UL)

    testCase "Decimal literals can be generated" <| fun () ->
        equal (System.Decimal.Zero, 0M)
        equal (System.Decimal.One, 1M)
        equal (System.Decimal.MinusOne, -1M)
        equal (System.Decimal.MaxValue, 79228162514264337593543950335M)
        equal (System.Decimal.MinValue, -79228162514264337593543950335M)

    testCase "Decimal.ToString works" <| fun () ->
        equal (string 001.23456M, "1.23456")
        equal (string 1.23456M, "1.23456")
        equal (string 0.12345M, "0.12345")
        equal (string 0.01234M, "0.01234")
        equal (string 0.00123M, "0.00123")
        equal (string 0.00012M, "0.00012")
        equal (string 0.00001M, "0.00001")
        equal (string 0.00000M, "0.00000")
        equal (string 0.12300M, "0.12300")
        equal (string 0.0M, "0.0")
        equal (string 0M, "0")
        equal (string 1M, "1")
        equal (string -1M, "-1")
        equal (string 00000000000000000000000000000.M, "0")
        equal (string 0.0000000000000000000000000000M, "0.0000000000000000000000000000")
        equal (string 79228162514264337593543950335M, "79228162514264337593543950335")
        equal (string -79228162514264337593543950335M, "-79228162514264337593543950335")

    testCase "Decimal precision is kept" <| fun () ->
        let items = [ 290.8M
                      290.8M
                      337.12M
                      6.08M
                      -924.8M ]
        equal (List.sum items, 0M)

    testCase "Decimal Infix add can be generated" <| fun () ->
        equal (4.0868M + 2.289348M, 6.376148M)

    testCase "Decimal Infix subtract can be generated" <| fun () ->
        equal (4.0868M - 2.289348M, 1.797452M)

    testCase "Decimal Infix multiply can be generated" <| fun () ->
        equal (4.0868M * 2.289348M, 9.3561074064M)

    testCase "Decimal Infix divide can be generated" <| fun () ->
        equal (4M / 2M, 2M)

    testCase "Decimal Infix modulo can be generated" <| fun () ->
        equal (4.0868M % 2.289348M, 1.797452M)

    testCase "Decimal Evaluation order is preserved by generated code" <| fun () ->
        equal ((4.4567M - 2.2234M) * 2.6492M + 1.2493M, 7.16575836M)

    testCase "Decimal constructors work" <| fun () ->
        let d = 1.2493M
        let bits = Decimal.GetBits(d)
        let d2 = Decimal(bits)
        let d3 = Decimal(bits.[0], bits.[1], bits.[2], false, 4uy)
        equal (d, d2)
        equal (d, d3)

    testCase "Decimal GetBits works" <| fun () ->
        let d = Decimal([| -1; -1; -2; 0 |])
        let bits = Decimal.GetBits(d)
        let d2 = Decimal(bits)
        let d3 = Decimal(bits.[0], bits.[1], bits.[2], true, 0uy)
        equal (d, d2)
        equal (-d, d3)

    testCase "Decimal abs works" <| fun () ->
        equal (abs -4M, 4M)

    testCase "Decimal round works" <| fun () ->
        equal (round -12.5M, -12.M)
        equal (round 1.5M, 2.M)
        equal (round 1.535M, 2.M)
        equal (round 1.525M, 2.M)
        equal (Math.Round 1.425M, 1.M)
        equal (Math.Round -1.425M, -1.M)
        equal (Math.Round 1.546M, 2.M)
        equal (Math.Round -1.546M, -2.M)

    testCase "Decimal round with digits works" <| fun () ->
        equal(Math.Round(1.426M, 2), 1.43M)
        equal(Math.Round(1.426M, 1), 1.4M)
        equal(Math.Round(-1.426M, 2), -1.43M)
        equal(Math.Round(-1.426M, 1), -1.4M)

    testCase "Decimal truncate works" <| fun () ->
        equal(Math.Truncate -12.5M, -12.M)
        equal(Math.Truncate 1.425M, 1.M)
        equal(Math.Truncate -1.425M, -1.M)
        equal(Math.Truncate 1.546M, 1.M)
        equal(Math.Truncate -1.546M, -1.M)

    testCase "Decimal ceil works" <| fun () ->
        equal (ceil 11.25M, 12.M)
        equal (ceil -11.25M, -11.M)
        equal (Math.Ceiling 11.25M, 12.M)

    testCase "Decimal floor works" <| fun () ->
        equal (floor 11.75M, 11.M)
        equal (floor -11.75M, -12.M)
        equal (Math.Floor 11.25M, 11.M)

    testCase "Decimal pown works" <| fun () ->
        equal (pown 2.2M 3, 10.648M)

    testCase "Int64 Infix add can be generated" <| fun () ->
        equal (4L + 2L, 6L)

    testCase "Int64 Infix subtract can be generated" <| fun () ->
        equal (4L - 2L, 2L)

    testCase "Int64 Infix multiply can be generated" <| fun () ->
        equal (4L * 2L, 8L)

    testCase "Int64 Infix divide can be generated" <| fun () ->
        equal (4L / 2L, 2L)

    testCase "Int64 Integer division doesn't produce floats" <| fun () ->
        equal (5. / 2., 2.5)
        equal (5L / 2L, 2L)
        equal (5L / 3L, 1L)
        // equal (float 5L / 2., 2.5) // TODO: Number conversion

    testCase "Int64 Infix modulo can be generated" <| fun () ->
        equal (4L % 3L, 1L)

    testCase "Int64 Evaluation order is preserved by generated code" <| fun () ->
        equal ((4L - 2L) * 2L + 1L, 5L)

    testCase "Int64 Bitwise and can be generated" <| fun () ->
        equal (6L &&& 2L, 2L)

    testCase "Int64 Bitwise or can be generated" <| fun () ->
        equal (4L ||| 2L, 6L)

    testCase "Int64 Bitwise shift left can be generated" <| fun () ->
        equal (4L <<< 2, 16L)

    testCase "Int64 Bitwise shift right can be generated" <| fun () ->
        equal (4L >>> 2, 1L)

    testCase "UInt64 Bitwise shift right can be generated" <| fun () -> // See #1482
      equal (15210016002388773605UL >>> 33, 1770678907UL)

    testCase "Int64 abs works" <| fun () ->
        equal (abs -4L, 4L)

    testCase "Big integers addition works" <| fun () ->
        let x = 59823749821707124891298739821798327321028091380980I
        let y = bigint 1L
        let z = 1I
        equal(59823749821707124891298739821798327321028091380982I, (x + y + z))

    testCase "BigInt Infix add can be generated" <| fun () ->
        equal (4I + 2I, 6I)

    testCase "BigInt Infix subtract can be generated" <| fun () ->
        equal (4I - 2I, 2I)

    testCase "BigInt Infix multiply can be generated" <| fun () ->
        equal (4I * 2I, 8I)

    testCase "BigInt Infix divide can be generated" <| fun () ->
        equal (4I / 2I, 2I)

    testCase "BigInt Integer division doesn't produce floats" <| fun () ->
        equal (5. / 2., 2.5)
        equal (5I / 2I, 2I)
        equal (5I / 3I, 1I)
        // equal (float 5I / 2., 2.5)

    testCase "BigInt Infix modulo can be generated" <| fun () ->
        equal (4I % 3I, 1I)

    testCase "BigInt.DivRem works" <| fun () -> // See #1744
        let quotient,remainder = bigint.DivRem(5I,2I)
        equal(quotient, 2I)
        equal(remainder, 1I)

    testCase "BigInt Evaluation order is preserved by generated code" <| fun () ->
        equal ((4I - 2I) * 2I + 1I, 5I)

    testCase "BigInt Bitwise and can be generated" <| fun () ->
        equal (6I &&& 2I, 2I)

    testCase "BigInt Bitwise or can be generated" <| fun () ->
        equal (4I ||| 2I, 6I)

    testCase "BigInt Bitwise xor can be generated" <| fun () ->
        equal (6I ^^^ 2I, 4I)

    testCase "BigInt Bitwise shift left can be generated" <| fun () ->
        equal (4I <<< 2, 16I)

    testCase "BigInt Bitwise shift right can be generated" <| fun () ->
        equal (4I >>> 2, 1I)

    testCase "BigInt abs works" <| fun () ->
        equal (abs -4I, 4I)

    testCase "abs works" <| fun () ->
        equal (abs -4, 4)

    testCase "round works" <| fun () ->
        equal (round -12.5, -12.)
        equal (round 1.5, 2.)
        equal (round 1.535, 2.)
        equal (round 1.525, 2.)

    testCase "ceil works" <| fun () ->
        equal (ceil 11.25, 12.)
        equal (ceil -11.25, -11.)
        equal (Math.Ceiling 11.25, 12.)

    testCase "floor works" <| fun () ->
        equal (floor 11.75, 11.)
        equal (floor -11.75, -12.)
        equal (Math.Floor 11.25, 11.)

    testCase "pown works" <| fun () ->
        pown 2.2 3 |> checkTo3dp 10648.

    testCase "sqrt works" <| fun () ->
        sqrt 4.5 |> checkTo3dp 2121.

    // As per https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
    testCase "sqrt matches .net core implementation" <| fun () ->
        checkTo3dp 1732. (sqrt 3.0)
        equal (sqrt 0.0 , 0.0)
        equal (isNaN (sqrt -3.0), true)
        equal (isNaN (sqrt System.Double.NaN), true)
        equal (isNaN (sqrt negativeInfinity), true)
        equal (sqrt positiveInfinity, positiveInfinity)

    testCase "Double.Parse works with IFormatProvider" <| fun () ->
        // culture compiles to { } for now and it is ignored on the call-site
        let culture = Globalization.CultureInfo.InvariantCulture
        let result = System.Double.Parse("10.5", culture)
        equal(10.5, result)

    testCase "Single.Parse works with IFormatProvider" <| fun () ->
        // culture compiles to { } for now and it is ignored on the call-site
        let culture = Globalization.CultureInfo.InvariantCulture
        let result = System.Single.Parse("10.5", culture)
        equal(10.5, float result)

    testCase "acos works" <| fun () ->
        acos 0.25 |> checkTo3dp 1318.

    testCase "asin works" <| fun () ->
        asin 0.25 |> checkTo3dp 252.

    testCase "atan works" <| fun () ->
        atan 0.25 |> checkTo3dp 244.

    testCase "atan2 works" <| fun () ->
        atan2 90. 15. |> checkTo3dp 1405.

    testCase "cos works" <| fun () ->
        cos 0.25 |> checkTo3dp 968.

    testCase "sin works" <| fun () ->
        sin 0.25 |> checkTo3dp 247.

    testCase "tan works" <| fun () ->
        tan 0.25 |> checkTo3dp 255.

    testCase "exp works" <| fun () ->
        exp 8.0 |> checkTo3dp 2980957.

    // https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L228
    testCase "log works" <| fun () ->
        log 232.12 |> checkTo3dp 5447.
        checkTo3dp 1098. (log 3.0)
        equal (log 0.0, negativeInfinity)
        equal (isNaN (log -2.0), true)
        equal (isNaN (log System.Double.NaN), true)
        equal (isNaN (log negativeInfinity), true)
        equal (log positiveInfinity, positiveInfinity)

    // https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L239
    testCase "Math.Log(double, double) works" <| fun () ->
        equal (3.0, Math.Log(8.0, 2.0))
        equal (1.0, Math.Log(3.0, 3.0))
        Math.Log(14., 3.0) |> checkTo3dp 2402.
        equal (negativeInfinity, Math.Log(0.0, 3.0))
        equal (positiveInfinity, Math.Log(positiveInfinity, 3.0))
        equal (true, isNaN (Math.Log(-3.0, 3.0)))
        equal (true, isNaN (Math.Log(System.Double.NaN, 3.0)))
        equal (true, isNaN (Math.Log(negativeInfinity, 3.0)))

    testCase "log10 works" <| fun () ->
        log10 232.12 |> checkTo3dp 2365.

    testCase "PI works" <| fun () ->
        checkTo3dp 3141. Math.PI

    testCase "E works" <| fun () ->
        checkTo3dp 2718. Math.E

    testCase "Math.abs works" <| fun () ->
        equal(Math.Abs -4, 4)

    testCase "Math.pown works" <| fun () ->
        Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

    testCase "Math.sqrt works" <| fun () ->
        Math.Sqrt 4.5 |> checkTo3dp 2121.

    testCase "Math.round works" <| fun () ->
        equal(Math.Round -12.5, -12.)
        equal(Math.Round 1.425, 1.)
        equal(Math.Round -1.425, -1.)
        equal(Math.Round 1.546, 2.)
        equal(Math.Round -1.546, -2.)

    testCase "Math.round with digits works" <| fun () ->
        equal(Math.Round(1.426, 2), 1.43)
        equal(Math.Round(1.426, 1), 1.4)
        equal(Math.Round(-1.426, 2), -1.43)
        equal(Math.Round(-1.426, 1), -1.4)

    testCase "Math.truncate works" <| fun () ->
        equal(Math.Truncate -12.5, -12.)
        equal(Math.Truncate 1.425, 1.)
        equal(Math.Truncate -1.425, -1.)
        equal(Math.Truncate 1.546, 1.)
        equal(Math.Truncate -1.546, -1.)

    testCase "Math.ceil works" <| fun () ->
        equal(Math.Ceiling 11.25, 12.)

    testCase "Math.floor works" <| fun () ->
        equal(Math.Floor 11.75, 11.)

    testCase "Math.acos works" <| fun () ->
        Math.Acos 0.25 |> checkTo3dp 1318.

    testCase "Math.asin works" <| fun () ->
        Math.Asin 0.25 |> checkTo3dp 252.

    testCase "Math.atan works" <| fun () ->
        Math.Atan 0.25 |> checkTo3dp 244.

    testCase "Math.atan2 works" <| fun () ->
        Math.Atan2(90., 15.) |> checkTo3dp 1405.

    testCase "Math.cos works" <| fun () ->
        Math.Cos(0.1 * Math.PI) |> checkTo3dp 951.

    testCase "Math.sin works" <| fun () ->
        Math.Sin(0.25 * Math.PI) |> checkTo3dp 707.

    testCase "Math.tan works" <| fun () ->
        Math.Tan(0.5) |> checkTo3dp 546.

    testCase "Math.exp works" <| fun () ->
        Math.Exp 8.0 |> checkTo3dp 2980957.

    testCase "Math.log works" <| fun () ->
        Math.Log 232.12 |> checkTo3dp 5447.

    testCase "Math.log10 works" <| fun () ->
        Math.Log10 232.12 |> checkTo3dp 2365.

    testCase "incr works" <| fun () ->
        let i = ref 5
        incr i
        equal(!i, 6)

    testCase "decr works" <| fun () ->
        let i = ref 5
        decr i
        equal(!i, 4)

    testCase "System.Random works" <| fun () ->
        let rnd = System.Random()
        let x = rnd.Next(5)
        equal(true, x >= 0 && x < 5)

        let y = rnd.NextDouble()
        equal(true, y >= 0.0 && y < 1.0)

    // Note: Test could fail sometime during life of universe, if it picks all zeroes.
    testCase "System.Random.NextBytes works" <| fun () ->
        let buffer = Array.create 16 0uy // guid-sized buffer
        System.Random().NextBytes(buffer)
        equal(buffer = Array.create 16 0uy, false)

    testCase "Long integers equality works" <| fun () ->
        let x = 5L
        let y = 5L
        let z = 6L
        equal(true, (x = y))
        equal(false, (y = z))
        equal(true, equals y x)
        equal(false, equals z x)

    testCase "Long integers comparison works" <| fun () ->
        let x = 5L
        let y = 5L
        let z = 6L
        equal(0, compare x y)
        equal(-1, compare y z)
        equal(0, compareTo y x)
        equal(1, compareTo z x)

    testCase "bigint equality works" <| fun () ->
        let a = 9007199254740992I
        let b = 9007199254740993I
        equal(false, (a = b))

    testCase "Big integers equality works" <| fun () ->
        let x = 59823749821707124891298739821798327321028091380980I
        let y = 59823749821707124891298739821798327321028091380980I
        let z = 59823749821707124891298739821798327321028091380981I
        equal(true, (x = y))
        equal(false, (y = z))
        equal(true, equals y x)
        equal(false, equals z x)

    testCase "Big integers comparison works" <| fun () ->
        let x = 5I
        let y = 5I
        let z = 6I
        equal(0, compare x y)
        equal(-1, compare y z)
        equal(0, compareTo y x)
        equal(1, compareTo z x)

    testCase "Big integer to byte array works" <| fun () ->
        // values with high bit both 0 and 1 for different array lengths
        equal(32767I.ToByteArray(), [|255uy; 127uy|])
        equal(32768I.ToByteArray(), [|0uy; 128uy; 0uy|])
        equal(-32768I.ToByteArray(), [|0uy; 128uy|])
        equal(-32769I.ToByteArray(), [|255uy; 127uy; 255uy|])
        // large numbers
        equal(111222333444555666777888999I.ToByteArray(), [|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|])
        equal(-111222333444555666777888999I.ToByteArray(), [|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|])

    testCase "Big integer from byte array works" <| fun () ->
        // values with high bit both 0 and 1 for different array lengths
        equal(Numerics.BigInteger([|255uy; 127uy|]), 32767I)
        equal(Numerics.BigInteger([|0uy; 128uy; 0uy|]), 32768I)
        equal(Numerics.BigInteger([|0uy; 128uy|]), -32768I)
        equal(Numerics.BigInteger([|255uy; 127uy; 255uy|]), -32769I)
        // large numbers
        equal(Numerics.BigInteger([|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]), 111222333444555666777888999I)
        equal(Numerics.BigInteger([|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]), -111222333444555666777888999I)

    testCase "Member values of decimal type can be compared" <| fun () -> // See #747
        equal(true, decimalOne < decimalTwo)
        equal(false, decimalOne > decimalTwo)

    testCase "Sign operator works" <| fun () -> // See #1311
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
]