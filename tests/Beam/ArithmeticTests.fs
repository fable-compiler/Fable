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
let ``test Infix add can be generated`` () =
    4 + 2 |> equal 6

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

[<Fact>]
let ``test Evaluation order is preserved by generated code`` () =
    (4 - 2) * 2 + 1 |> equal 5

[<Fact>]
let ``test Adding floats works`` () =
    3.141 + 2.85 |> equal 5.991

[<Fact>]
let ``test Unary negation works`` () =
    let x = 5
    -x |> equal -5

[<Fact>]
let ``test Unary plus works`` () =
    let x = 5
    +x |> equal 5

[<Fact>]
let ``test Bitwise and can be generated`` () =
    6 &&& 2 |> equal 2

[<Fact>]
let ``test Bitwise or can be generated`` () =
    4 ||| 2 |> equal 6

[<Fact>]
let ``test Bitwise xor can be generated`` () =
    6 ^^^ 2 |> equal 4

[<Fact>]
let ``test Bitwise shift left can be generated`` () =
    4 <<< 2 |> equal 16

[<Fact>]
let ``test Bitwise shift right can be generated`` () =
    4 >>> 2 |> equal 1

[<Fact>]
let ``test Logical and works`` () =
    (true && true) |> equal true
    (true && false) |> equal false
    (false && true) |> equal false
    (false && false) |> equal false

[<Fact>]
let ``test Logical or works`` () =
    (true || true) |> equal true
    (true || false) |> equal true
    (false || true) |> equal true
    (false || false) |> equal false

[<Fact>]
let ``test Logical not works`` () =
    not true |> equal false
    not false |> equal true

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
let ``test Equality works`` () =
    (1 = 1) |> equal true
    (1 = 2) |> equal false

[<Fact>]
let ``test Inequality works`` () =
    (1 <> 2) |> equal true
    (1 <> 1) |> equal false

[<Fact>]
let ``test Less than works`` () =
    (1 < 2) |> equal true
    (2 < 1) |> equal false

[<Fact>]
let ``test Greater than works`` () =
    (2 > 1) |> equal true
    (1 > 2) |> equal false

[<Fact>]
let ``test Less than or equal works`` () =
    (1 <= 2) |> equal true
    (1 <= 1) |> equal true
    (2 <= 1) |> equal false

[<Fact>]
let ``test Greater than or equal works`` () =
    (2 >= 1) |> equal true
    (1 >= 1) |> equal true
    (1 >= 2) |> equal false

// --- Int64 expanded ---

[<Fact>]
let ``test Int64 Integer division doesn't produce floats`` () =
    5L / 2L |> equal 2L
    5L / 3L |> equal 1L

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
let ``test Int64 abs works`` () =
    abs -4L |> equal 4L

// --- BigInt ---

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
    5I / 2I |> equal 2I
    5I / 3I |> equal 1I

[<Fact>]
let ``test BigInt Infix modulo can be generated`` () =
    4I % 3I |> equal 1I

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

// --- Math functions ---

[<Fact>]
let ``test abs works`` () =
    abs -4 |> equal 4

// Note: Erlang round/1 uses "round half away from zero", not .NET's "round half to even" (banker's rounding).
// round(-12.5) = -13 in Erlang, -12 in .NET. We test the values that are consistent.
[<Fact>]
let ``test round works`` () =
    round 1.5 |> equal 2.
    round 1.535 |> equal 2.
    round 1.525 |> equal 2.

[<Fact>]
let ``test ceil works`` () =
    ceil 11.25 |> equal 12.
    ceil -11.25 |> equal -11.

[<Fact>]
let ``test floor works`` () =
    floor 11.75 |> equal 11.
    floor -11.75 |> equal -12.

[<Fact>]
let ``test pow works`` () =
    2.2 ** 3.0 |> checkTo3dp 10648.

[<Fact>]
let ``test sqrt works`` () =
    sqrt 4.5 |> checkTo3dp 2121.

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

[<Fact>]
let ``test log works`` () =
    log 232.12 |> checkTo3dp 5447.

[<Fact>]
let ``test log10 works`` () =
    log10 232.12 |> checkTo3dp 2365.

// --- Math class methods ---

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
    Math.Round 1.425 |> equal 1.
    Math.Round -1.425 |> equal -1.
    Math.Round 1.546 |> equal 2.
    Math.Round -1.546 |> equal -2.

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

[<Fact>]
let ``test Math.Log with base works`` () =
    Math.Log(8.0, 2.0) |> equal 3.0
    Math.Log(3.0, 3.0) |> equal 1.0

[<Fact>]
let ``test Math.Log10 works`` () =
    Math.Log10 232.12 |> checkTo3dp 2365.

[<Fact>]
let ``test Math.Min works`` () =
    Math.Min(-4.0, 3.0) |> equal -4.0

[<Fact>]
let ``test Math.Max works`` () =
    Math.Max(-4.0, 3.0) |> equal 3.0

[<Fact>]
let ``test Math.Clamp works`` () =
    Math.Clamp(5, -4, 3) |> equal 3
    Math.Clamp(-10, -4, 3) |> equal -4
    Math.Clamp(0, -4, 3) |> equal 0

// --- Sign ---

[<Fact>]
let ``test Sign operator works with ints`` () =
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

// --- Equality and comparison ---

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

[<Fact>]
let ``test Sign operator works with bigints`` () =
    sign 1I |> equal 1
    sign 2I |> equal 1
    sign 0I |> equal 0
    sign -1I |> equal -1
    sign -2I |> equal -1

[<Fact>]
let ``test Decimal literals can be generated`` () =
    0M |> equal Decimal.Zero
    1M |> equal Decimal.One
    -1M |> equal Decimal.MinusOne

[<Fact>]
let ``test Decimal.ToString works`` () =
    string 001.23456M |> equal "1.23456"
    string 1.23456M |> equal "1.23456"

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

[<Fact>]
let ``test Decimal abs works`` () =
    abs -4M |> equal 4M

[<Fact>]
let ``test Decimal equality works`` () =
    let x = 5m
    let y = 5m
    let z = 6m
    (x = y) |> equal true
    (x = z) |> equal false

[<Fact>]
let ``test Decimal comparison works`` () =
    let x = 5m
    let y = 5m
    let z = 6m
    compare x y |> equal 0
    compare x z |> equal -1
    compare z x |> equal 1

[<Fact>]
let ``test Member values of decimal type can be compared`` () =
    1M < 2M |> equal true
    1M > 2M |> equal false

[<Fact>]
let ``test Sign operator works with decimals`` () =
    sign 1m |> equal 1
    sign 2m |> equal 1
    sign 0m |> equal 0
    sign -1m |> equal -1
    sign -2m |> equal -1

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
let ``test ref cell basic operations work`` () =
    let x = ref 10
    equal 10 !x
    x.Value <- 20
    equal 20 x.Value
    x := 30
    equal 30 !x

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

// --- More tests ported from Python ---

[<Fact>]
let ``test Int32 literal addition is optimized`` () =
    posLiteral + 7 |> equal 12
    notALiteral + 7 |> equal 12

[<Fact>]
let ``test Unary negation with negative literal values works`` () =
    -negLiteral |> equal 345

// TODO: Erlang has arbitrary precision integers, so -(-128y) = 128, not SByte.MinValue (-128).
// .NET fixed-width types overflow but Erlang integers don't, making this test invalid for Beam.
// [<Fact>]
// let ``test Unary negation with integer MinValue works`` () =
//     -(-128y) |> equal SByte.MinValue
//     -(-32768s) |> equal Int16.MinValue
//     -(-2147483648) |> equal Int32.MinValue
//     -(-9223372036854775808L) |> equal Int64.MinValue

[<Fact>]
let ``test pown works`` () =
    pown 2.2 3 |> checkTo3dp 10648.

[<Fact>]
let ``test Math.Round with digits works`` () =
    Math.Round(1.426, 2) |> equal 1.43
    Math.Round(1.426, 1) |> equal 1.4
    Math.Round(-1.426, 2) |> equal -1.43
    Math.Round(-1.426, 1) |> equal -1.4

[<Fact>]
let ``test sqrt matches .NET core implementation`` () =
    checkTo3dp 1732. (sqrt 3.0)
    sqrt 0.0 |> equal 0.0

[<Fact>]
let ``test Bitwise shift left with unsigned integer works`` () =
    1u <<< 31 |> equal 2147483648u

[<Fact>]
let ``test UInt64 multiplication with 0 returns uint`` () =
    0x0UL * 0x1UL |> equal 0x0UL

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
let ``test Zero fill shift right for unsigned`` () =
    0x80000000 >>> 1 |> equal -1073741824
    0x80000000u >>> 1 |> equal 1073741824u
    0x80000000UL>>> 1 |> equal 1073741824UL

// TODO: ~~~ on unsigned integers uses LogicalNotDynamic which is not supported by Fable
// [<Fact>]
// let ``test Bitwise NOT on large unsigned integer works`` () =
//     (~~~0x80000000u >>> 0) |> equal ~~~0x80000000u
//     (~~~0x80000000UL>>> 0) |> equal ~~~0x80000000UL

// TODO: UInt64 shift right requires unsigned masking (Erlang >>> is arithmetic shift)
// [<Fact>]
// let ``test UInt64 Bitwise shift right can be generated`` () =
//     15210016002388773605UL >>> 33 |> equal 1770678907UL

// TODO: infinity/NaN not available in Erlang (1.0/0.0 raises badarith)
// [<Fact>]
// let ``test extreme values work`` () =
//     0.0 / 0.0 |> Double.IsNaN |> equal true

// --- More tests ported from Python (batch 2) ---

[<Fact>]
let ``test MathF.Min works`` () =
    MathF.Min(-4.0f, 3.0f) |> equal -4.0f

[<Fact>]
let ``test MathF.Max works`` () =
    MathF.Max(-4.0f, 3.0f) |> equal 3.0f

[<Fact>]
let ``test infinity works`` () =
    infinity > 1000000.0 |> equal true

[<Fact>]
let ``test negative infinity works`` () =
    -infinity < -1000000.0 |> equal true

[<Fact>]
let ``test infinity comparison works`` () =
    let dist = infinity
    dist = infinity |> equal true
