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

// --- Math.Log2 ---

[<Fact>]
let ``test Math.Log2 works`` () =
    Math.Log2 8.0 |> checkTo3dp 3000.

// --- Math.DivRem ---

[<Fact>]
let ``test Math.DivRem works with ints`` () =
    Math.DivRem(5, 2) |> equal (2, 1)
    Math.DivRem(4, 2) |> equal (2, 0)

// TODO: DivRem with ref requires byref/address-of support which uses different
// ref patterns in Beam (process dictionary refs vs map-based refs).
// [<Fact>]
// let ``test Math.DivRem works with ints and ref`` () =
//     let rem = ref -1
//     Math.DivRem(5, 2, rem) |> equal 2
//     rem.Value |> equal 1
//     Math.DivRem(4, 2, rem) |> equal 2
//     rem.Value |> equal 0

[<Fact>]
let ``test Math.DivRem works with longs`` () =
    Math.DivRem(5L, 2L) |> equal (2L, 1L)
    Math.DivRem(4L, 2L) |> equal (2L, 0L)

// TODO: DivRem with ref requires byref/address-of support which uses different
// ref patterns in Beam (process dictionary refs vs map-based refs).
// [<Fact>]
// let ``test Math.DivRem works with longs and ref`` () =
//     let rem = ref -1L
//     Math.DivRem(5L, 2L, rem) |> equal 2L
//     rem.Value |> equal 1L
//     Math.DivRem(4L, 2L, rem) |> equal 2L
//     rem.Value |> equal 0L

// --- Math.MinMagnitude / MaxMagnitude ---

[<Fact>]
let ``test Math.MinMagnitude works`` () =
    Math.MinMagnitude(-4.0, 3.0) |> equal 3.0
    MathF.MinMagnitude(-4.0f, 3.0f) |> equal 3.0f

[<Fact>]
let ``test Math.MaxMagnitude works`` () =
    Math.MaxMagnitude(-4.0, 3.0) |> equal -4.0
    MathF.MaxMagnitude(-4.0f, 3.0f) |> equal -4.0f

// --- BigInt.Log / Log10 / DivRem ---

[<Fact>]
let ``test BigInt.Log works`` () =
    bigint.Log 123I |> checkTo3dp 4812.

[<Fact>]
let ``test BigInt.Log with base works`` () =
    bigint.Log(123I, 10.0) |> checkTo3dp 2089.

[<Fact>]
let ``test BigInt.Log10 works`` () =
    bigint.Log10 123I |> checkTo3dp 2089.

[<Fact>]
let ``test BigInt.DivRem works`` () =
    let quotient, remainder = bigint.DivRem(5I, 2I)
    2I |> equal quotient
    1I |> equal remainder

// --- Numeric type methods ---

[<Fact>]
let ``test Numeric Min works`` () =
    Int32.Min(-4, 3) |> equal -4
    Int64.Min(-4L, 3L) |> equal -4L
    Double.Min(-4.0, 3.0) |> equal -4.0
    Single.Min(-4.0f, 3.0f) |> equal -4.0f
    Decimal.Min(-4.0M, 3.0M) |> equal -4.0M
    bigint.Min(-4I, 3I) |> equal -4I

[<Fact>]
let ``test Numeric Max works`` () =
    Int32.Max(-4, 3) |> equal 3
    Int64.Max(-4L, 3L) |> equal 3L
    Double.Max(-4.0, 3.0) |> equal 3.0
    Single.Max(-4.0f, 3.0f) |> equal 3.0f
    Decimal.Max(-4.0M, 3.0M) |> equal 3.0M
    bigint.Max(-4I, 3I) |> equal 3I

[<Fact>]
let ``test Numeric MinMagnitude works`` () =
    Int32.MinMagnitude(-4, 3) |> equal 3
    Int64.MinMagnitude(-4L, 3L) |> equal 3L
    Double.MinMagnitude(-4.0, 3.0) |> equal 3.0
    Single.MinMagnitude(-4.0f, 3.0f) |> equal 3.0f
    Decimal.MinMagnitude(-4.0M, 3.0M) |> equal 3.0M
    bigint.MinMagnitude(-4I, 3I) |> equal 3I

[<Fact>]
let ``test Numeric MaxMagnitude works`` () =
    Int32.MaxMagnitude(-4, 3) |> equal -4
    Int64.MaxMagnitude(-4L, 3L) |> equal -4L
    Double.MaxMagnitude(-4.0, 3.0) |> equal -4.0
    Single.MaxMagnitude(-4.0f, 3.0f) |> equal -4.0f
    Decimal.MaxMagnitude(-4.0M, 3.0M) |> equal -4.0M
    bigint.MaxMagnitude(-4I, 3I) |> equal -4I

[<Fact>]
let ``test Numeric Clamp works`` () =
    Int32.Clamp(5, -4, 3) |> equal 3
    Int64.Clamp(5L, -4L, 3L) |> equal 3L
    Double.Clamp(5.0, -4.0, 3.0) |> equal 3.0
    Single.Clamp(5.0f, -4.0f, 3.0f) |> equal 3.0f
    Decimal.Clamp(5.0M, -4.0M, 3.0M) |> equal 3.0M
    bigint.Clamp(5I, -4I, 3I) |> equal 3I

// TODO: Erlang math:log2 returns float, but Numeric Log2 for integer types expects integer result.
// SByte.Log2, Int16.Log2, Byte.Log2, UInt16.Log2, UInt32.Log2, UInt64.Log2 would need trunc().
// [<Fact>]
// let ``test Numeric Log2 works`` () =
//     Int32.Log2 8 |> equal 3
//     Int64.Log2 8L |> equal 3L
//     Double.Log2 8.0 |> equal 3.0
//     Single.Log2 8.0f |> equal 3.0f
//     bigint.Log2 8I |> equal 3I

[<Fact>]
let ``test Math.Clamp expanded works`` () =
    Math.Clamp(5, -4, 3) |> equal 3
    Math.Clamp(5L, -4L, 3L) |> equal 3L
    Math.Clamp(5.0, -4.0, 3.0) |> equal 3.0
    Math.Clamp(5.0f, -4.0f, 3.0f) |> equal 3.0f
    Math.Clamp(5.0M, -4.0M, 3.0M) |> equal 3.0M

// --- Decimal round/ceil/floor/truncate ---

[<Fact>]
let ``test Decimal round works`` () =
    round 11.0M |> equal 11.M
    round 11.01M |> equal 11.M
    round 11.25M |> equal 11.M
    round 11.50M |> equal 12.M
    round 11.75M |> equal 12.M
    round -11.0M |> equal -11.M
    round -11.01M |> equal -11.M
    round -11.25M |> equal -11.M
    round -11.50M |> equal -12.M
    round -11.75M |> equal -12.M
    Math.Round 1.425M |> equal 1.M
    Math.Round -1.425M |> equal -1.M
    Math.Round 1.546M |> equal 2.M
    Math.Round -1.546M |> equal -2.M

[<Fact>]
let ``test Decimal round half to even works`` () =
    round 1.5M |> equal 2.M
    round 2.5M |> equal 2.M
    round 3.5M |> equal 4.M
    round -1.5M |> equal -2.M
    round -2.5M |> equal -2.M
    round -3.5M |> equal -4.M

[<Fact>]
let ``test Decimal round with digits works`` () =
    Math.Round(1.426M, 3) |> equal 1.426M
    Math.Round(1.426M, 2) |> equal 1.43M
    Math.Round(1.426M, 1) |> equal 1.4M
    Math.Round(-1.426M, 3) |> equal -1.426M
    Math.Round(-1.426M, 2) |> equal -1.43M
    Math.Round(-1.426M, 1) |> equal -1.4M

[<Fact>]
let ``test Decimal truncate works`` () =
    truncate 11.0M |> equal 11.M
    truncate 11.01M |> equal 11.M
    truncate 11.25M |> equal 11.M
    truncate 11.50M |> equal 11.M
    truncate 11.75M |> equal 11.M
    truncate -11.0M |> equal -11.M
    truncate -11.01M |> equal -11.M
    truncate -11.25M |> equal -11.M
    truncate -11.50M |> equal -11.M
    truncate -11.75M |> equal -11.M
    Math.Truncate -12.5M |> equal -12.M
    Math.Truncate 1.425M |> equal 1.M
    Math.Truncate -1.425M |> equal -1.M
    Math.Truncate 1.546M |> equal 1.M
    Math.Truncate -1.546M |> equal -1.M

[<Fact>]
let ``test Decimal ceil works`` () =
    ceil 11.0M |> equal 11.M
    ceil 11.01M |> equal 12.M
    ceil 11.25M |> equal 12.M
    ceil 11.50M |> equal 12.M
    ceil 11.75M |> equal 12.M
    ceil -11.0M |> equal -11.M
    ceil -11.01M |> equal -11.M
    ceil -11.25M |> equal -11.M
    ceil -11.50M |> equal -11.M
    ceil -11.75M |> equal -11.M
    Math.Ceiling 11.25M |> equal 12.M
    Math.Ceiling -11.25M |> equal -11.M

[<Fact>]
let ``test Decimal floor works`` () =
    floor 11.0M |> equal 11.M
    floor 11.01M |> equal 11.M
    floor 11.25M |> equal 11.M
    floor 11.50M |> equal 11.M
    floor 11.75M |> equal 11.M
    floor -11.0M |> equal -11.M
    floor -11.01M |> equal -12.M
    floor -11.25M |> equal -12.M
    floor -11.50M |> equal -12.M
    floor -11.75M |> equal -12.M
    Math.Floor 11.25M |> equal 11.M
    Math.Floor -11.25M |> equal -12.M

[<Fact>]
let ``test Decimal pown works`` () =
    pown 2.2M 3 |> equal 10.648M

// --- Random seeded tests ---

[<Fact>]
let ``test System.Random seeded is deterministic`` () =
    let rnd1 = Random(1234)
    let a1 = rnd1.Next()
    let a2 = rnd1.Next(100)
    let a3 = rnd1.Next(1000, 10000)
    let a4 = rnd1.NextDouble()
    let rnd2 = Random(1234)
    let b1 = rnd2.Next()
    let b2 = rnd2.Next(100)
    let b3 = rnd2.Next(1000, 10000)
    let b4 = rnd2.NextDouble()
    a1 |> equal b1
    a2 |> equal b2
    a3 |> equal b3
    a4 |> equal b4

[<Fact>]
let ``test System.Random seeded validates arguments`` () =
    let rnd = Random(42)
    throwsAnyError <| fun () -> rnd.Next(-10)
    throwsAnyError <| fun () -> rnd.Next(14, 10)

[<Fact>]
let ``test System.Random.NextBytes works`` () =
    let buffer = Array.create 16 0uy
    Random().NextBytes(buffer)
    buffer.Length |> equal 16
    buffer = Array.create 16 0uy |> equal false

[<Fact>]
let ``test System.Random.NextBytes seeded is deterministic`` () =
    let buffer1 = Array.create 4 0uy
    let rnd1 = Random(5432)
    rnd1.NextBytes(buffer1)
    let buffer2 = Array.create 4 0uy
    let rnd2 = Random(5432)
    rnd2.NextBytes(buffer2)
    buffer1 |> equal buffer2

// TODO: Decimal constructors from GetBits require low/mid/high int32 representation
// which doesn't map to the fixed-scale integer approach used in Beam.
// [<Fact>]
// let ``test Decimal constructors work`` () =
//     let d = 1.2493M
//     let bits = Decimal.GetBits(d)
//     let d2 = Decimal(bits)
//     d2 |> equal d

// TODO: BigInt byte array conversion requires two's complement encoding
// which isn't natively supported in Erlang.
// [<Fact>]
// let ``test Big integer to byte array works`` () =
//     32767I.ToByteArray() |> equal [|255uy; 127uy|]

// [<Fact>]
// let ``test Big integer from byte array works`` () =
//     Numerics.BigInteger([|255uy; 127uy|]) |> equal 32767I
