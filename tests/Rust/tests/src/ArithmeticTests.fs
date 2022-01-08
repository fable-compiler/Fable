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
    float 5 / 2. |> equal 2.5

[<Fact>]
let ``Infix modulo can be generated`` () =
    4 % 3 |> equal 1

// [<Fact>]
// let ``Math.DivRem works with ints`` () =
//     Math.DivRem(5, 2) |> equal (2, 1)
//     Math.DivRem(4, 2) |> equal (2, 0)

// [<Fact>]
// let ``Math.DivRem works with ints and ref`` () =
//     let rem = ref -1
//     Math.DivRem(5, 2, rem) |> equal 2
//     rem.Value |> equal 1
//     Math.DivRem(4, 2, rem) |> equal 2
//     rem.Value |> equal 0

// [<Fact>]
// let ``Math.DivRem works with longs`` () =
//     Math.DivRem(5L, 2L) |> equal (2L, 1L)
//     Math.DivRem(4L, 2L) |> equal (2L, 0L)

// [<Fact>]
// let ``Math.DivRem works with longs and ref`` () =
//     let rem = ref -1L
//     Math.DivRem(5L, 2L, rem) |> equal 2L
//     rem.Value |> equal 1L
//     Math.DivRem(4L, 2L, rem) |> equal 2L
//     rem.Value |> equal 0L

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

// [<Fact>]
// let ``Decimal literals can be generated`` () =
//     0M |> equal System.Decimal.Zero
//     1M |> equal System.Decimal.One
//     -1M |> equal System.Decimal.MinusOne
//     79228162514264337593543950335M |> equal System.Decimal.MaxValue
//     -79228162514264337593543950335M |> equal System.Decimal.MinValue

// [<Fact>]
// let ``Decimal.ToString works`` () =
//     string 001.23456M |> equal "1.23456"
//     string 1.23456M |> equal "1.23456"
//     string 0.12345M |> equal "0.12345"
//     string 0.01234M |> equal "0.01234"
//     string 0.00123M |> equal "0.00123"
//     string 0.00012M |> equal "0.00012"
//     string 0.00001M |> equal "0.00001"
//     string 0.00000M |> equal "0.00000"
//     string 0.12300M |> equal "0.12300"
//     string 0.0M |> equal "0.0"
//     string 0M |> equal "0"
//     string 1M |> equal "1"
//     string -1M |> equal "-1"
//     string 00000000000000000000000000000.M |> equal "0"
//     string 0.0000000000000000000000000000M |> equal "0.0000000000000000000000000000"
//     string 79228162514264337593543950335M |> equal "79228162514264337593543950335"
//     string -79228162514264337593543950335M |> equal "-79228162514264337593543950335"

// [<Fact>]
// let ``Decimal precision is kept`` () =
//     let items = [ 290.8M
//                     290.8M
//                     337.12M
//                     6.08M
//                     -924.8M ]
//     List.sum items |> equal 0M

// [<Fact>]
// let ``Decimal max precision is kept`` () =
//     let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286M
//     string pi |> equal "3.1415926535897932384626433833"

// [<Fact>]
// let ``Decimal average precision is kept`` () =
//     let items = [1M; 2M; 5M]
//     List.average items |> equal 2.6666666666666666666666666667M

// [<Fact>]
// let ``Decimal division precision is kept`` () =
//     string (8M / 3M) |> equal "2.6666666666666666666666666667"

// [<Fact>]
// let ``Decimal division works`` () =
//     let a = decimal 0.00001
//     let b = 1000.M
//     let c = decimal 0.00000001
//     a / b |> equal c

// [<Fact>]
// let ``Decimal Infix add works`` () =
//     4.0868M + 2.289348M |> equal 6.376148M

// [<Fact>]
// let ``Decimal Infix subtract works`` () =
//     4.0868M - 2.289348M |> equal 1.797452M

// [<Fact>]
// let ``Decimal Infix multiply works`` () =
//     4.0868M * 2.289348M |> equal 9.3561074064M

// [<Fact>]
// let ``Decimal Infix divide works`` () =
//     4M / 2M |> equal 2M

// [<Fact>]
// let ``Decimal Infix modulo works`` () =
//     4.0868M % 2.289348M |> equal 1.797452M

// [<Fact>]
// let ``Decimal.Add works`` () =
//     Decimal.Add(4.0868M, 2.289348M) |> equal 6.376148M

// [<Fact>]
// let ``Decimal.Subtract works`` () =
//     Decimal.Subtract(4.0868M, 2.289348M) |> equal 1.797452M

// [<Fact>]
// let ``Decimal.Multiply works`` () =
//     Decimal.Multiply(4.0868M, 2.289348M) |> equal 9.3561074064M

// [<Fact>]
// let ``Decimal.Divide works`` () =
//     Decimal.Divide(4M, 2M) |> equal 2M

// [<Fact>]
// let ``Decimal.Remainder works`` () =
//     Decimal.Remainder(4.0868M, 2.289348M) |> equal 1.797452M

// [<Fact>]
// let ``Decimal.Negate works`` () =
//     Decimal.Negate(4M) |> equal -4M

// [<Fact>]
// let ``Decimal Evaluation order is preserved by generated code`` () =
//     (4.4567M - 2.2234M) * 2.6492M + 1.2493M |> equal 7.16575836M

// [<Fact>]
// let ``Decimal constructors work`` () =
//     let d = 1.2493M
//     let bits = Decimal.GetBits(d)
//     let d2 = Decimal(bits)
//     let d3 = Decimal(bits.[0], bits.[1], bits.[2], false, 4uy)
//     d2 |> equal d
//     d3 |> equal d

// [<Fact>]
// let ``Decimal GetBits works`` () =
//     let d = Decimal([| -1; -1; -2; 0 |])
//     let bits = Decimal.GetBits(d)
//     let d2 = Decimal(bits)
//     let d3 = Decimal(bits.[0], bits.[1], bits.[2], true, 0uy)
//     d2 |> equal d
//     d3 |> equal -d

// [<Fact>]
// let ``Decimal abs works`` () =
//     abs -4M |> equal 4M

// [<Fact>]
// let ``Decimal round works`` () =
//     round 11.0M |> equal 11.M
//     round 11.01M |> equal 11.M
//     round 11.25M |> equal 11.M
//     round 11.50M |> equal 12.M
//     round 11.75M |> equal 12.M
//     round -11.0M |> equal -11.M
//     round -11.01M |> equal -11.M
//     round -11.25M |> equal -11.M
//     round -11.50M |> equal -12.M
//     round -11.75M |> equal -12.M
//     Math.Round 1.425M |> equal 1.M
//     Math.Round -1.425M |> equal -1.M
//     Math.Round 1.546M |> equal 2.M
//     Math.Round -1.546M |> equal -2.M

// [<Fact>]
// let ``Decimal round half to even works`` () =
//     round 1.5M |> equal 2.M
//     round 2.5M |> equal 2.M
//     round 3.5M |> equal 4.M
//     round -1.5M |> equal -2.M
//     round -2.5M |> equal -2.M
//     round -3.5M |> equal -4.M

// [<Fact>]
// let ``Decimal round with digits works`` () =
//     Math.Round(1.426M, 3) |> equal 1.426M
//     Math.Round(1.426M, 2) |> equal 1.43M
//     Math.Round(1.426M, 1) |> equal 1.4M
//     Math.Round(-1.426M, 3) |> equal -1.426M
//     Math.Round(-1.426M, 2) |> equal -1.43M
//     Math.Round(-1.426M, 1) |> equal -1.4M

// [<Fact>]
// let ``Decimal truncate works`` () =
//     truncate 11.0M |> equal 11.M
//     truncate 11.01M |> equal 11.M
//     truncate 11.25M |> equal 11.M
//     truncate 11.50M |> equal 11.M
//     truncate 11.75M |> equal 11.M
//     truncate -11.0M |> equal -11.M
//     truncate -11.01M |> equal -11.M
//     truncate -11.25M |> equal -11.M
//     truncate -11.50M |> equal -11.M
//     truncate -11.75M |> equal -11.M
//     Math.Truncate -12.5M |> equal -12.M
//     Math.Truncate 1.425M |> equal 1.M
//     Math.Truncate -1.425M |> equal -1.M
//     Math.Truncate 1.546M |> equal 1.M
//     Math.Truncate -1.546M |> equal -1.M

// [<Fact>]
// let ``Decimal ceil works`` () =
//     ceil 11.0M |> equal 11.M
//     ceil 11.01M |> equal 12.M
//     ceil 11.25M |> equal 12.M
//     ceil 11.50M |> equal 12.M
//     ceil 11.75M |> equal 12.M
//     ceil -11.0M |> equal -11.M
//     ceil -11.01M |> equal -11.M
//     ceil -11.25M |> equal -11.M
//     ceil -11.50M |> equal -11.M
//     ceil -11.75M |> equal -11.M
//     Math.Ceiling 11.25M |> equal 12.M
//     Math.Ceiling -11.25M |> equal -11.M

// [<Fact>]
// let ``Decimal floor works`` () =
//     floor 11.0M |> equal 11.M
//     floor 11.01M |> equal 11.M
//     floor 11.25M |> equal 11.M
//     floor 11.50M |> equal 11.M
//     floor 11.75M |> equal 11.M
//     floor -11.0M |> equal -11.M
//     floor -11.01M |> equal -12.M
//     floor -11.25M |> equal -12.M
//     floor -11.50M |> equal -12.M
//     floor -11.75M |> equal -12.M
//     Math.Floor 11.25M |> equal 11.M
//     Math.Floor -11.25M |> equal -12.M

// [<Fact>]
// let ``Decimal pown works`` () =
//     pown 2.2M 3 |> equal 10.648M

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
    float 5L / 2. |> equal 2.5

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

// [<Fact>]
// let ``Big integers addition works`` () =
//     let x = 59823749821707124891298739821798327321028091380980I
//     let y = bigint 1L
//     let z = 1I
//     (x + y + z) |> equal 59823749821707124891298739821798327321028091380982I

// [<Fact>]
// let ``BigInt Infix add can be generated`` () =
//     4I + 2I |> equal 6I

// [<Fact>]
// let ``BigInt Infix subtract can be generated`` () =
//     4I - 2I |> equal 2I

// [<Fact>]
// let ``BigInt Infix multiply can be generated`` () =
//     4I * 2I |> equal 8I

// [<Fact>]
// let ``BigInt Infix divide can be generated`` () =
//     4I / 2I |> equal 2I

// [<Fact>]
// let ``BigInt Integer division doesn't produce floats`` () =
//     5. / 2. |> equal 2.5
//     5I / 2I |> equal 2I
//     5I / 3I |> equal 1I
//     // float 5I / 2. |> equal 2.5

// [<Fact>]
// let ``BigInt Infix modulo can be generated`` () =
//     4I % 3I |> equal 1I

// [<Fact>]
// let ``BigInt.DivRem works`` () = // See #1744
//     let quotient,remainder = bigint.DivRem(5I,2I)
//     2I |> equal quotient
//     1I |> equal remainder

// [<Fact>]
// let ``BigInt Evaluation order is preserved by generated code`` () =
//     (4I - 2I) * 2I + 1I |> equal 5I

// [<Fact>]
// let ``BigInt Bitwise and can be generated`` () =
//     6I &&& 2I |> equal 2I

// [<Fact>]
// let ``BigInt Bitwise or can be generated`` () =
//     4I ||| 2I |> equal 6I

// [<Fact>]
// let ``BigInt Bitwise xor can be generated`` () =
//     6I ^^^ 2I |> equal 4I

// [<Fact>]
// let ``BigInt Bitwise shift left can be generated`` () =
//     4I <<< 2 |> equal 16I

// [<Fact>]
// let ``BigInt Bitwise shift right can be generated`` () =
//     4I >>> 2 |> equal 1I

// [<Fact>]
// let ``BigInt abs works`` () =
//     abs -4I |> equal 4I

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

[<Fact>]
let ``pown works`` () =
    pown 2.2 3 |> checkTo3dp 10648.

[<Fact>]
let ``pow works`` () =
    2.2 ** 3.0 |> checkTo3dp 10648.

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
//     let result = Double.Parse("10.5", culture)
//     result |> equal 10.5

// [<Fact>]
// let ``Single.Parse works with IFormatProvider`` () =
//     // culture compiles to { } for now and it is ignored on the call-site
//     let culture = Globalization.CultureInfo.InvariantCulture
//     let result = Single.Parse("10.5", culture)
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

[<Fact>]
let ``atan2 works`` () =
    atan2 90. 15. |> checkTo3dp 1405.

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

[<Fact>]
let ``Math.pow works`` () =
    Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

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

[<Fact>]
let ``Math.atan2 works`` () =
    Math.Atan2(90., 15.) |> checkTo3dp 1405.

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
let ``Math.log with base works`` () =
    Math.Log(8.0, 2.0) |> checkTo3dp 3000.

[<Fact>]
let ``Math.log2 works`` () =
    Math.Log2 8.0 |> checkTo3dp 3000.

[<Fact>]
let ``Math.log10 works`` () =
    Math.Log10 232.12 |> checkTo3dp 2365.

[<Fact>]
let ``incr works`` () =
    let i = ref 5
    incr i
    i := !i + 1
    !i |> equal 7
    i.Value <- i.Value + 1
    i.Value |> equal 8

[<Fact>]
let ``decr works`` () =
    let i = ref 5
    decr i
    i := !i - 1
    !i |> equal 3
    i.Value <- i.Value - 1
    i.Value |> equal 2

// [<Fact>]
// let ``System.Random works`` () =
//     let rnd = Random()
//     let x = rnd.Next(5)
//     (x >= 0 && x < 5) |> equal true
//     let y = rnd.NextDouble()
//     (y >= 0.0 && y < 1.0) |> equal true

// // Note: Test could fail sometime during life of universe, if it picks all zeroes.
// [<Fact>]
// let ``System.Random.NextBytes works`` () =
//     let buffer = Array.create 16 0uy // guid-sized buffer
//     Random().NextBytes(buffer)
//     buffer = Array.create 16 0uy |> equal false

[<Fact>]
let ``Long integers equality works`` () =
    let x = 5L
    let y = 5L
    let z = 6L
    (x = y) |> equal true
    (y = z) |> equal false
    (y = x) |> equal true
    (z = x) |> equal false

// [<Fact>]
// let ``Long integers comparison works`` () =
//     let x = 5L
//     let y = 5L
//     let z = 6L
//     compare x y |> equal 0
//     compare y z |> equal -1
//     compare y x |> equal 0
//     compare z x |> equal 1

// [<Fact>]
// let ``bigint equality works`` () =
//     let a = 9007199254740992I
//     let b = 9007199254740993I
//     (a = b) |> equal false

// [<Fact>]
// let ``Big integers equality works`` () =
//     let x = 59823749821707124891298739821798327321028091380980I
//     let y = 59823749821707124891298739821798327321028091380980I
//     let z = 59823749821707124891298739821798327321028091380981I
//     (x = y) |> equal true
//     (y = z) |> equal false
//     equals y x |> equal true
//     equals z x |> equal false

// [<Fact>]
// let ``Big integers comparison works`` () =
//     let x = 5I
//     let y = 5I
//     let z = 6I
//     compare x y |> equal 0
//     compare y z |> equal -1
//     compare y x |> equal 0
//     compare z x |> equal 1

// [<Fact>]
// let ``Big integer to byte array works`` () =
//     // values with high bit both 0 and 1 for different array lengths
//     32767I.ToByteArray() |> equal [|255uy; 127uy|]
//     32768I.ToByteArray() |> equal [|0uy; 128uy; 0uy|]
//     -32768I.ToByteArray() |> equal [|0uy; 128uy|]
//     -32769I.ToByteArray() |> equal [|255uy; 127uy; 255uy|]
//     // large numbers
//     111222333444555666777888999I.ToByteArray() |> equal [|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]
//     -111222333444555666777888999I.ToByteArray() |> equal [|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]

// [<Fact>]
// let ``Big integer from byte array works`` () =
//     // values with high bit both 0 and 1 for different array lengths
//     Numerics.BigInteger([|255uy; 127uy|]) |> equal 32767I
//     Numerics.BigInteger([|0uy; 128uy; 0uy|]) |> equal 32768I
//     Numerics.BigInteger([|0uy; 128uy|]) |> equal -32768I
//     Numerics.BigInteger([|255uy; 127uy; 255uy|]) |> equal -32769I
//     // large numbers
//     Numerics.BigInteger([|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]) |> equal 111222333444555666777888999I
//     Numerics.BigInteger([|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]) |> equal -111222333444555666777888999I

// [<Fact>]
// let ``Member values of decimal type can be compared`` () = // See #747
//     1M < 2M |> equal true
//     1M > 2M |> equal false

// [<Fact>]
// let ``Sign operator works`` () = // See #1311
//     sign 1 |> equal 1
//     sign 34 |> equal 1
//     sign 1L |> equal 1
//     sign 36L |> equal 1
//     sign 1. |> equal 1
//     sign 89 |> equal 1
//     sign 1 |> equal 1
//     sign 0 |> equal 0
//     sign 0L |> equal 0
//     sign 0. |> equal 0
//     sign -1 |> equal -1
//     sign -56 |> equal -1
//     sign -1L |> equal -1
//     sign -72L |> equal -1
//     sign -1. |> equal -1
//     sign -89. |> equal -1

// [<Fact>]
// let ``Formatting of decimal works`` () =

//     let formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")

//     formatNumber 0.0M |> equal "0,00"
//     formatNumber 0.020M |> equal "0,02"
//     formatNumber 0.20M |> equal "0,20"
//     formatNumber 2.0M |> equal "2,00"


// [<Fact>]
// let ``Formatting of decimal works with inline`` () =

//     let inline formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")

//     formatNumber 0.0M |> equal "0,00"
//     formatNumber 0.020M |> equal "0,02"
//     formatNumber 0.20M |> equal "0,20"
//     formatNumber 2.0M |> equal "2,00"

// [<Fact>]
// let ``Formatting of € works with inline`` () =

//     let inline formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")

//     let inline formatEuro (d:decimal) = (formatNumber d) + " €"

//     formatEuro 0.0M |> equal "0,00 €"
//     formatEuro 0.020M |> equal "0,02 €"
//     formatEuro 0.20M |> equal "0,20 €"
//     formatEuro 2.0M |> equal "2,00 €"
