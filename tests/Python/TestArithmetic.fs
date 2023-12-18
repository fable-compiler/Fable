module Fable.Tests.Arithmetic

open System
open Fable.Tests.Util
open Util.Testing

let [<Literal>] posLiteral = 5
let [<Literal>] negLiteral = -345
let notALiteral = 5

let checkTo3dp (expected: float) actual =
    floor (actual * 1000.) |> equal expected

[<Fact>]
let ``test Adding floats works`` () =
    3.141 + 2.85 |> equal 5.991

[<Fact>]
let ``test Infix add can be generated`` () =
    4 + 2 |> equal 6

[<Fact>]
let ``test Int32 literal addition is optimized`` () =
    posLiteral + 7 |> equal 12
    notALiteral + 7 |> equal 12

[<Fact>]
let ``test Unary negation with negative literal values works`` () =
    -negLiteral |> equal 345

[<Fact>]
let ``test Unary negation with integer MinValue works`` () =
    -(-128y) |> equal SByte.MinValue
    -(-32768s) |> equal Int16.MinValue
    -(-2147483648) |> equal Int32.MinValue
    -(-9223372036854775808L) |> equal Int64.MinValue

[<Fact>]
let ``test Infix subtract can be generated`` () =
    4 - 2 |> equal 2

[<Fact>]
let ``test Infix multiply can be generated`` () =
    4 * 2 |> equal 8

[<Fact>]
let ``test Infix divide can be generated`` () =
    4 / 2 |> equal 2

[<Fact>]
let ``test Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5 / 2 |> equal 2
    5 / 3 |> equal 1
    float 5 / 2. |> equal 2.5

[<Fact>]
let ``test Infix modulo can be generated`` () =
    4 % 3 |> equal 1

// [<Fact>]
// let ``test Math.DivRem works with bytes`` () =
//     Math.DivRem(5y, 2y) |> equal struct (2y, 1y)
//     Math.DivRem(4y, 2y) |> equal struct (2y, 0y)

// [<Fact>]
// let ``test Math.DivRem works with ints`` () =
//     Math.DivRem(5, 2) |> equal struct (2, 1)
//     Math.DivRem(4, 2) |> equal struct (2, 0)

// [<Fact>]
// let ``test Math.DivRem works with longs`` () =
//     Math.DivRem(5L, 2L) |> equal struct (2L, 1L)
//     Math.DivRem(4L, 2L) |> equal struct (2L, 0L)

// [<Fact>]
// let ``test Math.DivRem works with ints and outref`` () =
//     let mutable rem = -1
//     Math.DivRem(5, 2, &rem) |> equal 2
//     rem |> equal 1
//     Math.DivRem(4, 2, &rem) |> equal 2
//     rem |> equal 0

// [<Fact>]
// let ``test Math.DivRem works with longs and outref`` () =
//     let mutable rem = -1L
//     Math.DivRem(5L, 2L, &rem) |> equal 2L
//     rem |> equal 1L
//     Math.DivRem(4L, 2L, &rem) |> equal 2L
//     rem |> equal 0L

[<Fact>]
let ``test Evaluation order is preserved by generated code`` () =
    (4 - 2) * 2 + 1 |> equal 5

[<Fact>]
let ``test Bitwise and can be generated`` () =
    6 &&& 2 |> equal 2

[<Fact>]
let ``test Bitwise or can be generated`` () =
    4 ||| 2 |> equal 6

[<Fact>]
let ``test Bitwise shift left can be generated`` () =
    4 <<< 2 |> equal 16

[<Fact>]
let ``test Bitwise shift left with unsigned integer works`` () =
    1u <<< 31 |> equal 2147483648u

[<Fact>]
let ``test Bitwise OR on large unsigned integer works`` () =
    0x80000000u ||| 0u |> equal (0x80000000u ||| 0u >>> 0)
    0x80000000UL||| 0UL|> equal (0x80000000UL||| 0UL>>> 0)

[<Fact>]
let ``test Bitwise AND on large unsigned integer works`` () =
    0x80000000u &&& 0xffffffffu |> equal (0x80000000u &&& 0xffffffffu >>> 0)
    0x80000000UL&&& 0xffffffffUL|> equal (0x80000000UL&&& 0xffffffffUL>>> 0)

[<Fact>]
let ``test Bitwise XOR on large unsigned integer works`` () =
    0x80000000u ^^^ 0u |> equal (0x80000000u ^^^ 0u >>> 0)
    0x80000000UL^^^ 0UL|> equal (0x80000000UL^^^ 0UL>>> 0)

[<Fact>]
let ``test Bitwise NOT on large unsigned integer works`` () =
    (~~~0x80000000u >>> 0) |> equal ~~~0x80000000u
    (~~~0x80000000UL>>> 0) |> equal ~~~0x80000000UL

[<Fact>]
let ``test Bitwise shift right can be generated`` () = // See #1530
    4 >>> 2 |> equal 1

[<Fact>]
let ``test Zero fill shift right for unsigned`` () = // See #646
    0x80000000 >>> 1 |> equal -1073741824
    0x80000000u >>> 1 |> equal 1073741824u
    0x80000000UL>>> 1 |> equal 1073741824UL

[<Fact>]
let ``test UInt64 multiplication with 0 returns uint`` () = // See #1480
    0x0UL * 0x1UL |> equal 0x0UL

[<Fact>]
let ``test Decimal literals can be generated`` () =
    0M |> equal Decimal.Zero
    1M |> equal Decimal.One
    -1M |> equal Decimal.MinusOne
    // FIXME: 79228162514264337593543950335M |> equal Decimal.MaxValue
    // FIXME: -79228162514264337593543950335M |> equal Decimal.MinValue

[<Fact>]
let ``test Decimal.ToString works`` () =
    string 001.23456M |> equal "1.23456"
    string 1.23456M |> equal "1.23456"
    string 0.12345M |> equal "0.12345"
    string 0.01234M |> equal "0.01234"
    string 0.00123M |> equal "0.00123"
    string 0.00012M |> equal "0.00012"
    string 0.00001M |> equal "0.00001"
    // FIXME:
    // string 0.00000M |> equal "0.00000"
    // string 0.12300M |> equal "0.12300"
    // string 0.0M |> equal "0.0"
    string 0M |> equal "0"
    string 1M |> equal "1"
    string -1M |> equal "-1"
    string 00000000000000000000000000000.M |> equal "0"
    // FIXME: string 0.0000000000000000000000000000M |> equal "0.0000000000000000000000000000"
    string 79228162514264337593543950335M |> equal "79228162514264337593543950335"
    string -79228162514264337593543950335M |> equal "-79228162514264337593543950335"

[<Fact>]
let ``test Decimal precision is kept`` () =
    let items = [290.8M; 290.8M; 337.12M; 6.08M; -924.8M]
    List.sum items |> equal 0M

[<Fact>]
let ``test Decimal max precision is kept`` () =
    let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286M
    string pi |> equal "3.1415926535897932384626433833"

// [<Fact>]
// let ``test Decimal average precision is kept`` () =
//     let items = [1M; 2M; 5M]
//     List.average items |> equal 2.6666666666666666666666666667M

[<Fact>]
let ``test Decimal division precision is kept`` () =
    string (8M / 3M) |> equal "2.6666666666666666666666666667"

[<Fact>]
let ``test Decimal division works`` () =
    let a = decimal 0.00001
    let b = 1000.M
    let c = decimal 0.00000001
    a / b |> equal c

[<Fact>]
let ``test Decimal Infix add works`` () =
    4.0868M + 2.289348M |> equal 6.376148M

[<Fact>]
let ``test Decimal Infix subtract works`` () =
    4.0868M - 2.289348M |> equal 1.797452M

[<Fact>]
let ``test Decimal Infix multiply works`` () =
    4.0868M * 2.289348M |> equal 9.3561074064M

[<Fact>]
let ``test Decimal Infix divide works`` () =
    4M / 2M |> equal 2M

[<Fact>]
let ``test Decimal Infix modulo works`` () =
    4.0868M % 2.289348M |> equal 1.797452M

[<Fact>]
let ``test Decimal.Add works`` () =
    Decimal.Add(4.0868M, 2.289348M) |> equal 6.376148M

[<Fact>]
let ``test Decimal.Subtract works`` () =
    Decimal.Subtract(4.0868M, 2.289348M) |> equal 1.797452M

[<Fact>]
let ``test Decimal.Multiply works`` () =
    Decimal.Multiply(4.0868M, 2.289348M) |> equal 9.3561074064M

[<Fact>]
let ``test Decimal.Divide works`` () =
    Decimal.Divide(4M, 2M) |> equal 2M

[<Fact>]
let ``test Decimal.Remainder works`` () =
    Decimal.Remainder(4.0868M, 2.289348M) |> equal 1.797452M

[<Fact>]
let ``test Decimal.Negate works`` () =
    Decimal.Negate(4M) |> equal -4M

[<Fact>]
let ``test Decimal Evaluation order is preserved by generated code`` () =
    (4.4567M - 2.2234M) * 2.6492M + 1.2493M |> equal 7.16575836M

// [<Fact>]
// let ``test Decimal constructors work`` () =
//     let d = 1.2493M
//     let bits = Decimal.GetBits(d)
//     let d2 = Decimal(bits)
//     let d3 = Decimal(bits[0], bits[1], bits[2], false, 4uy)
//     d2 |> equal d
//     d3 |> equal d

// [<Fact>]
// let ``test Decimal GetBits works`` () =
//     let d = Decimal([| -1; -1; -2; 0 |])
//     let bits = Decimal.GetBits(d)
//     let d2 = Decimal(bits)
//     let d3 = Decimal(bits[0], bits[1], bits[2], true, 0uy)
//     d2 |> equal d
//     d3 |> equal -d

[<Fact>]
let ``test Decimal abs works`` () =
    abs -4M |> equal 4M

// [<Fact>]
// let ``test Decimal round works`` () =
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
// let ``test Decimal round half to even works`` () =
//     round 1.5M |> equal 2.M
//     round 2.5M |> equal 2.M
//     round 3.5M |> equal 4.M
//     round -1.5M |> equal -2.M
//     round -2.5M |> equal -2.M
//     round -3.5M |> equal -4.M

// [<Fact>]
// let ``test Decimal round with digits works`` () =
//     Math.Round(1.426M, 3) |> equal 1.426M
//     Math.Round(1.426M, 2) |> equal 1.43M
//     Math.Round(1.426M, 1) |> equal 1.4M
//     Math.Round(-1.426M, 3) |> equal -1.426M
//     Math.Round(-1.426M, 2) |> equal -1.43M
//     Math.Round(-1.426M, 1) |> equal -1.4M

// [<Fact>]
// let ``test Decimal truncate works`` () =
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
// let ``test Decimal ceil works`` () =
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
// let ``test Decimal floor works`` () =
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
// let ``test Decimal pown works`` () =
//     pown 2.2M 3 |> equal 10.648M

[<Fact>]
let ``test Int64 Infix add can be generated`` () =
    4L + 2L |> equal 6L

[<Fact>]
let ``test Int64 Infix subtract can be generated`` () =
    4L - 2L |> equal 2L

[<Fact>]
let ``test Int64 Infix multiply can be generated`` () =
    4L * 2L |> equal 8L

[<Fact>]
let ``test Int64 Infix divide can be generated`` () =
    4L / 2L |> equal 2L

[<Fact>]
let ``test Int64 Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5L / 2L |> equal 2L
    5L / 3L |> equal 1L
    float 5L / 2. |> equal 2.5

[<Fact>]
let ``test Int64 Infix modulo can be generated`` () =
    4L % 3L |> equal 1L

[<Fact>]
let ``test Int64 Evaluation order is preserved by generated code`` () =
    (4L - 2L) * 2L + 1L |> equal 5L

[<Fact>]
let ``test Int64 Bitwise and can be generated`` () =
    6L &&& 2L |> equal 2L

[<Fact>]
let ``test Int64 Bitwise or can be generated`` () =
    4L ||| 2L |> equal 6L

[<Fact>]
let ``test Int64 Bitwise shift left can be generated`` () =
    4L <<< 2 |> equal 16L

[<Fact>]
let ``test Int64 Bitwise shift right can be generated`` () =
    4L >>> 2 |> equal 1L

[<Fact>]
let ``test UInt64 Bitwise shift right can be generated`` () = // See #1482
    15210016002388773605UL >>> 33 |> equal 1770678907UL

[<Fact>]
let ``test Int64 abs works`` () =
    abs -4L |> equal 4L

[<Fact>]
let ``test Big integers addition works`` () =
    let x = 59823749821707124891298739821798327321028091380980I
    let y = bigint 1L
    let z = 1I
    (x + y + z) |> equal 59823749821707124891298739821798327321028091380982I

[<Fact>]
let ``test BigInt Infix add can be generated`` () =
    4I + 2I |> equal 6I

[<Fact>]
let ``test BigInt Infix subtract can be generated`` () =
    4I - 2I |> equal 2I

[<Fact>]
let ``test BigInt Infix multiply can be generated`` () =
    4I * 2I |> equal 8I

[<Fact>]
let ``test BigInt Infix divide can be generated`` () =
    4I / 2I |> equal 2I

[<Fact>]
let ``test BigInt Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5I / 2I |> equal 2I
    5I / 3I |> equal 1I
    // float 5I / 2. |> equal 2.5

[<Fact>]
let ``test BigInt Infix modulo can be generated`` () =
    4I % 3I |> equal 1I

// [<Fact>]
// let ``test BigInt.DivRem works`` () = // See #1744
//     let quotient,remainder = bigint.DivRem(5I,2I)
//     2I |> equal quotient
//     1I |> equal remainder

[<Fact>]
let ``test BigInt Evaluation order is preserved by generated code`` () =
    (4I - 2I) * 2I + 1I |> equal 5I

[<Fact>]
let ``test BigInt Bitwise and can be generated`` () =
    6I &&& 2I |> equal 2I

[<Fact>]
let ``test BigInt Bitwise or can be generated`` () =
    4I ||| 2I |> equal 6I

[<Fact>]
let ``test BigInt Bitwise xor can be generated`` () =
    6I ^^^ 2I |> equal 4I

[<Fact>]
let ``test BigInt Bitwise shift left can be generated`` () =
    4I <<< 2 |> equal 16I

[<Fact>]
let ``test BigInt Bitwise shift right can be generated`` () =
    4I >>> 2 |> equal 1I

[<Fact>]
let ``test BigInt abs works`` () =
    abs -4I |> equal 4I

[<Fact>]
let ``test abs works`` () =
    abs -4 |> equal 4

[<Fact>]
let ``test round works`` () =
    round -12.5 |> equal -12.
    round 1.5 |> equal 2.
    round 1.535 |> equal 2.
    round 1.525 |> equal 2.

[<Fact>]
let ``test ceil works`` () =
    ceil 11.25 |> equal 12.
    ceil -11.25 |> equal -11.
    Math.Ceiling 11.25 |> equal 12.

[<Fact>]
let ``test floor works`` () =
    floor 11.75 |> equal 11.
    floor -11.75 |> equal -12.
    Math.Floor 11.25 |> equal 11.

[<Fact>]
let ``test pown works`` () =
    pown 2.2 3 |> checkTo3dp 10648.

[<Fact>]
let ``test pow works`` () =
    2.2 ** 3.0 |> checkTo3dp 10648.

[<Fact>]
let ``test sqrt works`` () =
    sqrt 4.5 |> checkTo3dp 2121.

// As per https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
[<Fact>]
let ``test sqrt matches .NET core implementation`` () =
    checkTo3dp 1732. (sqrt 3.0)
    sqrt 0.0  |> equal 0.0
    Double.IsNaN (sqrt -3.0) |> equal true
    Double.IsNaN (sqrt Double.NaN) |> equal true
    Double.IsNaN (sqrt Double.NegativeInfinity) |> equal true
    sqrt Double.PositiveInfinity |> equal Double.PositiveInfinity

// [<Fact>]
// let ``test Single comparison works`` () =
//     let x = 5.0f
//     let y = 5.0f
//     let z = 6.0f
//     let n = Single.NaN
//     compare x y |> equal 0
//     compare y z |> equal -1
//     compare y x |> equal 0
//     compare z x |> equal 1
//     compare x n |> equal 1
//     compare n y |> equal -1
//     compare n n |> equal 0
//     n = n |> equal false

// [<Fact>]
// let ``test Double comparison works`` () =
//     let x = 5.0
//     let y = 5.0
//     let z = 6.0
//     let n = Double.NaN
//     compare x y |> equal 0
//     compare y z |> equal -1
//     compare y x |> equal 0
//     compare z x |> equal 1
//     compare x n |> equal 1
//     compare n y |> equal -1
//     compare n n |> equal 0
//     n = n |> equal false

// [<Fact>]
// let ``test Double.Parse works with IFormatProvider`` () =
//     // culture compiles to { } for now and it is ignored on the call-site
//     let culture = Globalization.CultureInfo.InvariantCulture
//     let result = Double.Parse("10.5", culture)
//     result |> equal 10.5

// [<Fact>]
// let ``test Single.Parse works with IFormatProvider`` () =
//     // culture compiles to { } for now and it is ignored on the call-site
//     let culture = Globalization.CultureInfo.InvariantCulture
//     let result = Single.Parse("10.5", culture)
//     float result |> equal 10.5

[<Fact>]
let ``test acos works`` () =
    acos 0.25 |> checkTo3dp 1318.

[<Fact>]
let ``test asin works`` () =
    asin 0.25 |> checkTo3dp 252.

[<Fact>]
let ``test atan works`` () =
    atan 0.25 |> checkTo3dp 244.

[<Fact>]
let ``test atan2 works`` () =
    atan2 90. 15. |> checkTo3dp 1405.

[<Fact>]
let ``test cos works`` () =
    cos 0.25 |> checkTo3dp 968.

[<Fact>]
let ``test sin works`` () =
    sin 0.25 |> checkTo3dp 247.

[<Fact>]
let ``test tan works`` () =
    tan 0.25 |> checkTo3dp 255.

[<Fact>]
let ``test cosh works`` () =
    cosh 0.25 |> checkTo3dp 1031.

[<Fact>]
let ``test sinh works`` () =
    sinh 0.25 |> checkTo3dp 252.

[<Fact>]
let ``test tanh works`` () =
    tanh 0.25 |> checkTo3dp 244.

[<Fact>]
let ``test exp works`` () =
    exp 8.0 |> checkTo3dp 2980957.

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L228
[<Fact>]
let ``test log works`` () =
    log 232.12 |> checkTo3dp 5447.
    checkTo3dp 1098. (log 3.0)
    log 0.0 |> equal Double.NegativeInfinity
    Double.IsNaN (log -2.0) |> equal true
    Double.IsNaN (log Double.NaN) |> equal true
    Double.IsNaN (log Double.NegativeInfinity) |> equal true
    log Double.PositiveInfinity |> equal Double.PositiveInfinity

// https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L239
[<Fact>]
let ``test Math.Log with base works`` () =
    Math.Log(8.0, 2.0) |> equal 3.0
    Math.Log(3.0, 3.0) |> equal 1.0
    Math.Log(14., 3.0) |> checkTo3dp 2402.
    // FIXME: Math.Log(0.0, 3.0) |> equal Double.NegativeInfinity
    Math.Log(Double.PositiveInfinity, 3.0) |> equal Double.PositiveInfinity
    // FIXME: Double.IsNaN (Math.Log(-3.0, 3.0)) |> equal true
    Double.IsNaN (Math.Log(Double.NaN, 3.0)) |> equal true
    // FIXME: Double.IsNaN (Math.Log(Double.NegativeInfinity, 3.0)) |> equal true

[<Fact>]
let ``test log10 works`` () =
    log10 232.12 |> checkTo3dp 2365.

[<Fact>]
let ``test Math.PI works`` () =
    checkTo3dp 3141. Math.PI

[<Fact>]
let ``test Math.E works`` () =
    checkTo3dp 2718. Math.E

[<Fact>]
let ``test Math.Abs works`` () =
    Math.Abs -4 |> equal 4

[<Fact>]
let ``test Math.Pow works`` () =
    Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

[<Fact>]
let ``test Math.Sqrt works`` () =
    Math.Sqrt 4.5 |> checkTo3dp 2121.

[<Fact>]
let ``test Math.Round works`` () =
    Math.Round -12.5 |> equal -12.
    Math.Round 1.425 |> equal 1.
    Math.Round -1.425 |> equal -1.
    Math.Round 1.546 |> equal 2.
    Math.Round -1.546 |> equal -2.

[<Fact>]
let ``test Math.Round with digits works`` () =
    Math.Round(1.426, 2) |> equal 1.43
    Math.Round(1.426, 1) |> equal 1.4
    Math.Round(-1.426, 2) |> equal -1.43
    Math.Round(-1.426, 1) |> equal -1.4

[<Fact>]
let ``test Math.Truncate works`` () =
    Math.Truncate -12.5 |> equal -12.
    Math.Truncate 1.425 |> equal 1.
    Math.Truncate -1.425 |> equal -1.
    Math.Truncate 1.546 |> equal 1.
    Math.Truncate -1.546 |> equal -1.

[<Fact>]
let ``test Math.Ceil works`` () =
    Math.Ceiling 11.25 |> equal 12.

[<Fact>]
let ``test Math.Floor works`` () =
    Math.Floor 11.75 |> equal 11.

[<Fact>]
let ``test Math.Acos works`` () =
    Math.Acos 0.25 |> checkTo3dp 1318.

[<Fact>]
let ``test Math.Asin works`` () =
    Math.Asin 0.25 |> checkTo3dp 252.

[<Fact>]
let ``test Math.Atan works`` () =
    Math.Atan 0.25 |> checkTo3dp 244.

[<Fact>]
let ``test Math.Atan2 works`` () =
    Math.Atan2(90., 15.) |> checkTo3dp 1405.

[<Fact>]
let ``test Math.Cos works`` () =
    Math.Cos(0.1 * Math.PI) |> checkTo3dp 951.

[<Fact>]
let ``test Math.Sin works`` () =
    Math.Sin(0.25 * Math.PI) |> checkTo3dp 707.

[<Fact>]
let ``test Math.Tan works`` () =
    Math.Tan(0.5) |> checkTo3dp 546.

[<Fact>]
let ``test Math.Exp works`` () =
    Math.Exp 8.0 |> checkTo3dp 2980957.

[<Fact>]
let ``test Math.Log works`` () =
    Math.Log 232.12 |> checkTo3dp 5447.

// [<Fact>]
// let ``test Math.Log2 works`` () =
//     Math.Log2 8.0 |> checkTo3dp 3000.

[<Fact>]
let ``test Math.Log10 works`` () =
    Math.Log10 232.12 |> checkTo3dp 2365.

// [<Fact>]
// let ``test BigInt.Log works`` () =
//     bigint.Log 123I |> checkTo3dp 4812.

// [<Fact>]
// let ``test BigInt.Log with base works`` () =
//     bigint.Log(123I, 10.0) |> checkTo3dp 2089.

// [<Fact>]
// let ``test BigInt.Log10 works`` () =
//     bigint.Log10 123I |> checkTo3dp 2089.

// [<Fact>]
// let ``test Numeric Log2 works`` () =
//     SByte.Log2 8y |> equal 3y
//     Int16.Log2 8s |> equal 3s
//     Int32.Log2 8  |> equal 3
//     Int64.Log2 8L |> equal 3L
//     Byte.Log2 8uy |> equal 3uy
//     UInt16.Log2 8us |> equal 3us
//     UInt32.Log2 8u  |> equal 3u
//     UInt64.Log2 8UL |> equal 3UL
//     Double.Log2 8.0 |> equal 3.0
//     Single.Log2 8.0f |> equal 3.0f
//     bigint.Log2 8I |> equal 3I

// [<Fact>]
// let ``test Numeric Min works`` () =
//     SByte.Min(-4y, 3y) |> equal -4y
//     Int16.Min(-4s, 3s) |> equal -4s
//     Int32.Min(-4, 3)  |> equal -4
//     Int64.Min(-4L, 3L) |> equal -4L
//     Byte.Min(4uy, 3uy) |> equal 3uy
//     UInt16.Min(4us, 3us) |> equal 3us
//     UInt32.Min(4u, 3u)  |> equal 3u
//     UInt64.Min(4UL, 3UL) |> equal 3UL
//     Double.Min(-4.0, 3.0) |> equal -4.0
//     Single.Min(-4.0f, 3.0f) |> equal -4.0f
//     Decimal.Min(-4.0M, 3.0M) |> equal -4.0M
//     bigint.Min(-4I, 3I) |> equal -4I

// [<Fact>]
// let ``test Numeric Max works`` () =
//     SByte.Max(-4y, 3y) |> equal 3y
//     Int16.Max(-4s, 3s) |> equal 3s
//     Int32.Max(-4, 3)  |> equal 3
//     Int64.Max(-4L, 3L) |> equal 3L
//     Byte.Max(4uy, 3uy) |> equal 4uy
//     UInt16.Max(4us, 3us) |> equal 4us
//     UInt32.Max(4u, 3u)  |> equal 4u
//     UInt64.Max(4UL, 3UL) |> equal 4UL
//     Double.Max(-4.0, 3.0) |> equal 3.0
//     Single.Max(-4.0f, 3.0f) |> equal 3.0f
//     Decimal.Max(-4.0M, 3.0M) |> equal 3.0M
//     bigint.Max(-4I, 3I) |> equal 3I

// [<Fact>]
// let ``test Numeric MinMagnitude works`` () =
//     SByte.MinMagnitude(-4y, 3y) |> equal 3y
//     Int16.MinMagnitude(-4s, 3s) |> equal 3s
//     Int32.MinMagnitude(-4, 3)  |> equal 3
//     Int64.MinMagnitude(-4L, 3L) |> equal 3L
//     Double.MinMagnitude(-4.0, 3.0) |> equal 3.0
//     Single.MinMagnitude(-4.0f, 3.0f) |> equal 3.0f
//     Decimal.MinMagnitude(-4.0M, 3.0M) |> equal 3.0M
//     bigint.MinMagnitude(-4I, 3I) |> equal 3I

// [<Fact>]
// let ``test Numeric MaxMagnitude works`` () =
//     SByte.MaxMagnitude(-4y, 3y) |> equal -4y
//     Int16.MaxMagnitude(-4s, 3s) |> equal -4s
//     Int32.MaxMagnitude(-4, 3)  |> equal -4
//     Int64.MaxMagnitude(-4L, 3L) |> equal -4L
//     Double.MaxMagnitude(-4.0, 3.0) |> equal -4.0
//     Single.MaxMagnitude(-4.0f, 3.0f) |> equal -4.0f
//     Decimal.MaxMagnitude(-4.0M, 3.0M) |> equal -4.0M
//     bigint.MaxMagnitude(-4I, 3I) |> equal -4I

// [<Fact>]
// let ``test Numeric Clamp works`` () =
//     SByte.Clamp(5y, -4y, 3y) |> equal 3y
//     Int16.Clamp(5s, -4s, 3s) |> equal 3s
//     Int32.Clamp(5, -4, 3)  |> equal 3
//     Int64.Clamp(5L, -4L, 3L) |> equal 3L
//     Byte.Clamp(5uy, 3uy, 4uy) |> equal 4uy
//     UInt16.Clamp(5us, 3us, 4us) |> equal 4us
//     UInt32.Clamp(5u, 3u, 4u)  |> equal 4u
//     UInt64.Clamp(5UL, 3UL, 4UL) |> equal 4UL
//     Double.Clamp(5.0, -4.0, 3.0) |> equal 3.0
//     Single.Clamp(5.0f, -4.0f, 3.0f) |> equal 3.0f
//     Decimal.Clamp(5.0M, -4.0M, 3.0M) |> equal 3.0M
//     bigint.Clamp(5I, -4I, 3I) |> equal 3I

[<Fact>]
let ``test Math.Clamp works`` () =
    Math.Clamp(5y, -4y, 3y) |> equal 3y
    Math.Clamp(5s, -4s, 3s) |> equal 3s
    Math.Clamp(5, -4, 3)  |> equal 3
    Math.Clamp(5L, -4L, 3L) |> equal 3L
    Math.Clamp(5uy, 3uy, 4uy) |> equal 4uy
    Math.Clamp(5us, 3us, 4us) |> equal 4us
    Math.Clamp(5u, 3u, 4u)  |> equal 4u
    Math.Clamp(5UL, 3UL, 4UL) |> equal 4UL
    Math.Clamp(5.0, -4.0, 3.0) |> equal 3.0
    Math.Clamp(5.0f, -4.0f, 3.0f) |> equal 3.0f
    Math.Clamp(5.0M, -4.0M, 3.0M) |> equal 3.0M

[<Fact>]
let ``test Math.Min works`` () =
    Math.Min(-4.0, 3.0) |> equal -4.0
    MathF.Min(-4.0f, 3.0f) |> equal -4.0f

[<Fact>]
let ``test Math.Max works`` () =
    Math.Max(-4.0, 3.0) |> equal 3.0
    MathF.Max(-4.0f, 3.0f) |> equal 3.0f

// [<Fact>]
// let ``test Math.MinMagnitude works`` () =
//     Math.MinMagnitude(-4.0, 3.0) |> equal 3.0
//     MathF.MinMagnitude(-4.0f, 3.0f) |> equal 3.0f

// [<Fact>]
// let ``test Math.MaxMagnitude works`` () =
//     Math.MaxMagnitude(-4.0, 3.0) |> equal -4.0
//     MathF.MaxMagnitude(-4.0f, 3.0f) |> equal -4.0f

[<Fact>]
let ``test incr works`` () =
    let i = ref 5
    incr i
    i := !i + 1
    !i |> equal 7
    i.Value <- i.Value + 1
    i.Value |> equal 8

[<Fact>]
let ``test decr works`` () =
    let i = ref 5
    decr i
    i := !i - 1
    !i |> equal 3
    i.Value <- i.Value - 1
    i.Value |> equal 2

[<Fact>]
let ``test System.Random works`` () =
    let rnd = Random()
    let x = rnd.Next()
    x >= 0 |> equal true
    let x = rnd.Next(5)
    (x >= 0 && x < 5) |> equal true
    let x = rnd.Next(14, 20)
    (x >= 14 && x < 20) |> equal true
    let x = rnd.Next(-14, -10)
    (x >= -14 && x < -10) |> equal true
    let x = rnd.NextDouble()
    (x >= 0.0 && x < 1.0) |> equal true
    throwsAnyError <| fun () -> rnd.Next(-10)
    throwsAnyError <| fun () -> rnd.Next(14, 10)

// // Note: Test could fail sometime during life of universe, if it picks all zeroes.
// [<Fact>]
// let ``test System.Random.NextBytes works`` () =
//     let buffer = Array.create 16 0uy // guid-sized buffer
//     Random().NextBytes(buffer)
//     buffer.Length |> equal 16
//     buffer = Array.create 16 0uy |> equal false
//     throwsAnyError <| fun () -> Random().NextBytes(null)

// [<Fact>]
// let ``test System.Random seeded works`` () =
//     let rnd = Random(1234)
//     rnd.Next() |> equal 857019877
//     rnd.Next(100) |> equal 89
//     rnd.Next(1000, 10000) |> equal 3872
//     rnd.NextDouble() |> equal 0.9467375338760845
//     throwsAnyError <| fun () -> rnd.Next(-10)
//     throwsAnyError <| fun () -> rnd.Next(14, 10)

// [<Fact>]
// let ``test System.Random.NextBytes seeded works`` () =
//     let buffer = Array.create 4 0uy // guid-sized buffer
//     Random(5432).NextBytes(buffer)
//     buffer |> equal [|152uy; 238uy; 227uy; 30uy|]
//     throwsAnyError <| fun () -> Random().NextBytes(null)

[<Fact>]
let ``test Long integer equality works`` () =
    let x = 5L
    let y = 5L
    let z = 6L
    (x = y) |> equal true
    (y = z) |> equal false
    (y = x) |> equal true
    (z = x) |> equal false

[<Fact>]
let ``test Long integer comparison works`` () =
    let x = 5L
    let y = 5L
    let z = 6L
    compare x y |> equal 0
    compare y z |> equal -1
    compare y x |> equal 0
    compare z x |> equal 1

[<Fact>]
let ``test Decimal equality works`` () =
    let x = 5m
    let y = 5m
    let z = 6m
    (x = y) |> equal true
    (y = z) |> equal false
    (y = x) |> equal true
    (z = x) |> equal false

[<Fact>]
let ``test Decimal comparison works`` () =
    let x = 5m
    let y = 5m
    let z = 6m
    compare x y |> equal 0
    compare y z |> equal -1
    compare y x |> equal 0
    compare z x |> equal 1

[<Fact>]
let ``test Big integer equality works`` () =
    let x = 59823749821707124891298739821798327321028091380980I
    let y = 59823749821707124891298739821798327321028091380980I
    let z = 59823749821707124891298739821798327321028091380981I
    (x = y) |> equal true
    (y = z) |> equal false
    (y = x) |> equal true
    (z = x) |> equal false

[<Fact>]
let ``test Big integers comparison works`` () =
    let x = 5I
    let y = 5I
    let z = 6I
    compare x y |> equal 0
    compare y z |> equal -1
    compare y x |> equal 0
    compare z x |> equal 1

// [<Fact>]
// let ``test Big integer to byte array works`` () =
//     // values with high bit both 0 and 1 for different array lengths
//     32767I.ToByteArray() |> equal [|255uy; 127uy|]
//     32768I.ToByteArray() |> equal [|0uy; 128uy; 0uy|]
//     -32768I.ToByteArray() |> equal [|0uy; 128uy|]
//     -32769I.ToByteArray() |> equal [|255uy; 127uy; 255uy|]
//     // large numbers
//     111222333444555666777888999I.ToByteArray() |> equal [|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]
//     -111222333444555666777888999I.ToByteArray() |> equal [|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]

// [<Fact>]
// let ``test Big integer from byte array works`` () =
//     // values with high bit both 0 and 1 for different array lengths
//     Numerics.BigInteger([|255uy; 127uy|]) |> equal 32767I
//     Numerics.BigInteger([|0uy; 128uy; 0uy|]) |> equal 32768I
//     Numerics.BigInteger([|0uy; 128uy|]) |> equal -32768I
//     Numerics.BigInteger([|255uy; 127uy; 255uy|]) |> equal -32769I
//     // large numbers
//     Numerics.BigInteger([|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]) |> equal 111222333444555666777888999I
//     Numerics.BigInteger([|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]) |> equal -111222333444555666777888999I

[<Fact>]
let ``test Member values of decimal type can be compared`` () = // See #747
    1M < 2M |> equal true
    1M > 2M |> equal false

[<Fact>]
let ``test Sign operator works with ints`` () = // See #1311
    sign 1 |> equal 1
    sign 2 |> equal 1
    sign 0 |> equal 0
    sign -1 |> equal -1
    sign -2 |> equal -1

[<Fact>]
let ``test Sign operator works with longs`` () =
    sign 1L |> equal 1
    sign 2L |> equal 1
    sign 0L |> equal 0
    sign -1L |> equal -1
    sign -2L |> equal -1

[<Fact>]
let ``test Sign operator works with floats`` () =
    sign 1. |> equal 1
    sign 2. |> equal 1
    sign 0. |> equal 0
    sign -1. |> equal -1
    sign -2. |> equal -1

[<Fact>]
let ``test Sign operator works with decimals`` () =
    sign 1m |> equal 1
    sign 2m |> equal 1
    sign 0m |> equal 0
    sign -1m |> equal -1
    sign -2m |> equal -1

[<Fact>]
let ``test Sign operator works with bigints`` () =
    sign 1I |> equal 1
    sign 2I |> equal 1
    sign 0I |> equal 0
    sign -1I |> equal -1
    sign -2I |> equal -1

// [<Fact>]
// let ``test Formatting of decimal works`` () =
//     let formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")
//     formatNumber 0.0M |> equal "0,00"
//     formatNumber 0.020M |> equal "0,02"
//     formatNumber 0.20M |> equal "0,20"
//     formatNumber 2.0M |> equal "2,00"

// [<Fact>]
// let ``test Formatting of decimal works with inline`` () =
//     let inline formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")
//     formatNumber 0.0M |> equal "0,00"
//     formatNumber 0.020M |> equal "0,02"
//     formatNumber 0.20M |> equal "0,20"
//     formatNumber 2.0M |> equal "2,00"

// [<Fact>]
// let ``test Formatting of € works with inline`` () =
//     let inline formatNumber (d:decimal) =
//         (sprintf "%.2f" d).Replace(",","").Replace(".",",")
//     let inline formatEuro (d:decimal) = (formatNumber d) + " €"
//     formatEuro 0.0M |> equal "0,00 €"
//     formatEuro 0.020M |> equal "0,02 €"
//     formatEuro 0.20M |> equal "0,20 €"
//     formatEuro 2.0M |> equal "2,00 €"

// module UnitTests =
//     open Common.Imports.Vectors

//     type [<Measure>] m

//     let square (x: float<'a>) = x * x

//     [<Fact>]
//     let ``test can square a unit`` () =
//         let d = 3.<m>
//         let dSquared = square d
//         dSquared |> equal 9.<m^2>

//     [<Fact>]
//     let ``test Can add two union vectors with same units but remove generics in output`` () =
//         let a = Vector2(2.<m>, 1.<m>)
//         let b = Vector2(3.<m>, 2.<m>)
//         let res = a + b
//         equal (Vector2(5.<m> ,3.<m>)) res

//     [<Fact>]
//     let ``test Can add two record vectors with same units but remove generics in output`` () =
//         let a = { x = 2f<m>; y = 1f<m> }
//         let b = { x = 3f<m>; y = 1f<m> }
//         let res = a + b
//         equal { x = 5f<m>; y = 2f<m> } res

[<Fact>]
let ``test extreme values work`` () =
    0.0 / 0.0 |> Double.IsNaN |> equal true
    0.0 / (-0.0) |> Double.IsNaN |> equal true
    1.0 / infinity |> equal 0
    1.0 / (-infinity) |> equal 0
    1.0 / 0.0 |> Double.IsInfinity |> equal true

    1.0 / (-0.0)
    |> Double.IsNegativeInfinity
    |> equal true

    -1.0 / (-0.0)
    |> Double.IsNegativeInfinity
    |> equal false

    -infinity < infinity |> equal true
    (-0.0) < 0.0 |> equal false
