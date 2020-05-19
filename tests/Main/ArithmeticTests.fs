module Fable.Tests.Arithmetic

open System
open Util.Testing

let [<Literal>] aLiteral = 5
let notALiteral = 5
let [<Literal>] literalNegativeValue = -345

let checkTo3dp (expected: float) actual =
    floor (actual * 1000.) |> equal expected

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
        4 + 2 |> equal 6

    testCase "Int32 literal addition is optimized" <| fun () ->
        aLiteral + 7 |> equal 12
        notALiteral + 7 |> equal 12

    testCase "Unary negation with negative literal values works" <| fun () ->
        -literalNegativeValue |> equal 345

    testCase "Unary negation with integer MinValue works" <| fun () ->
        -(-128y) |> equal System.SByte.MinValue
        -(-32768s) |> equal System.Int16.MinValue
        -(-2147483648) |> equal System.Int32.MinValue
        -(-9223372036854775808L) |> equal System.Int64.MinValue

    testCase "Infix subtract can be generated" <| fun () ->
        4 - 2 |> equal 2

    testCase "Infix multiply can be generated" <| fun () ->
        4 * 2 |> equal 8

    testCase "Infix divide can be generated" <| fun () ->
        4 / 2 |> equal 2

    testCase "Integer division doesn't produce floats" <| fun () ->
        5. / 2. |> equal 2.5
        5 / 2 |> equal 2
        5 / 3 |> equal 1
        // float 5 / 2. |> equal 2.5 // TODO: Number conversion

    testCase "Infix modulo can be generated" <| fun () ->
        4 % 3 |> equal 1

    testCase "Evaluation order is preserved by generated code" <| fun () ->
        (4 - 2) * 2 + 1 |> equal 5

    testCase "Bitwise and can be generated" <| fun () ->
        6 &&& 2 |> equal 2

    testCase "Bitwise or can be generated" <| fun () ->
        4 ||| 2 |> equal 6

    testCase "Bitwise shift left can be generated" <| fun () ->
        4 <<< 2 |> equal 16

    testCase "Bitwise shift left with unsigned integer works" <| fun () ->
        1u <<< 31 |> equal 2147483648u

    testCase "Bitwise OR on large unsigned integer works" <| fun () ->
        0x80000000u ||| 0u |> equal (0x80000000u ||| 0u >>> 0)

    testCase "Bitwise AND on large unsigned integer works" <| fun () ->
        0x80000000u &&& 0xffffffffu |> equal (0x80000000u &&& 0xffffffffu >>> 0)

    testCase "Bitwise XOR on large unsigned integer works" <| fun () ->
        0x80000000u ^^^ 0u |> equal (0x80000000u ^^^ 0u >>> 0)

    testCase "Bitwise Invert on large unsigned integer works" <| fun () ->
        (~~~0x80000000u >>> 0) |> equal ~~~0x80000000u

    testCase "Bitwise shift right can be generated" <| fun () -> // See #1530
        4 >>> 2 |> equal 1

    testCase "Zero fill shift right (>>>) for uint32" <| fun () -> // See #646
        0x80000000 >>> 1 |> equal -1073741824
        0x80000000u >>> 1 |> equal 1073741824u

    testCase "UInt64 multiplication with 0 returns uint" <| fun () -> // See #1480
        0x0UL * 0x1UL |> equal 0x0UL

    testCase "Decimal literals can be generated" <| fun () ->
        0M |> equal System.Decimal.Zero
        1M |> equal System.Decimal.One
        -1M |> equal System.Decimal.MinusOne
        79228162514264337593543950335M |> equal System.Decimal.MaxValue
        -79228162514264337593543950335M |> equal System.Decimal.MinValue

    testCase "Decimal.ToString works" <| fun () ->
        string 001.23456M |> equal "1.23456"
        string 1.23456M |> equal "1.23456"
        string 0.12345M |> equal "0.12345"
        string 0.01234M |> equal "0.01234"
        string 0.00123M |> equal "0.00123"
        string 0.00012M |> equal "0.00012"
        string 0.00001M |> equal "0.00001"
        string 0.00000M |> equal "0.00000"
        string 0.12300M |> equal "0.12300"
        string 0.0M |> equal "0.0"
        string 0M |> equal "0"
        string 1M |> equal "1"
        string -1M |> equal "-1"
        string 00000000000000000000000000000.M |> equal "0"
        string 0.0000000000000000000000000000M |> equal "0.0000000000000000000000000000"
        string 79228162514264337593543950335M |> equal "79228162514264337593543950335"
        string -79228162514264337593543950335M |> equal "-79228162514264337593543950335"

    testCase "Decimal precision is kept" <| fun () ->
        let items = [ 290.8M
                      290.8M
                      337.12M
                      6.08M
                      -924.8M ]
        List.sum items |> equal 0M

    testCase "Decimal max precision is kept" <| fun () ->
        let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286M
        string pi |> equal "3.1415926535897932384626433833"

    testCase "Decimal average precision is kept" <| fun () ->
        let items = [1M; 2M; 5M]
        List.average items |> equal 2.6666666666666666666666666667M

    testCase "Decimal division precision is kept" <| fun () ->
        string (8M / 3M) |> equal "2.6666666666666666666666666667"

    testCase "Decimal division works" <| fun () ->
        let a = decimal 0.00001
        let b = 1000.M
        let c = decimal 0.00000001
        a / b |> equal c

    testCase "Decimal Infix add works" <| fun () ->
        4.0868M + 2.289348M |> equal 6.376148M

    testCase "Decimal Infix subtract works" <| fun () ->
        4.0868M - 2.289348M |> equal 1.797452M

    testCase "Decimal Infix multiply works" <| fun () ->
        4.0868M * 2.289348M |> equal 9.3561074064M

    testCase "Decimal Infix divide works" <| fun () ->
        4M / 2M |> equal 2M

    testCase "Decimal Infix modulo works" <| fun () ->
        4.0868M % 2.289348M |> equal 1.797452M

    testCase "Decimal.Add works" <| fun () ->
        Decimal.Add(4.0868M, 2.289348M) |> equal 6.376148M

    testCase "Decimal.Subtract works" <| fun () ->
        Decimal.Subtract(4.0868M, 2.289348M) |> equal 1.797452M

    testCase "Decimal.Multiply works" <| fun () ->
        Decimal.Multiply(4.0868M, 2.289348M) |> equal 9.3561074064M

    testCase "Decimal.Divide works" <| fun () ->
        Decimal.Divide(4M, 2M) |> equal 2M

    testCase "Decimal.Remainder works" <| fun () ->
        Decimal.Remainder(4.0868M, 2.289348M) |> equal 1.797452M

    testCase "Decimal.Negate works" <| fun () ->
        Decimal.Negate(4M) |> equal -4M

    testCase "Decimal Evaluation order is preserved by generated code" <| fun () ->
        (4.4567M - 2.2234M) * 2.6492M + 1.2493M |> equal 7.16575836M

    testCase "Decimal constructors work" <| fun () ->
        let d = 1.2493M
        let bits = Decimal.GetBits(d)
        let d2 = Decimal(bits)
        let d3 = Decimal(bits.[0], bits.[1], bits.[2], false, 4uy)
        d2 |> equal d
        d3 |> equal d

    testCase "Decimal GetBits works" <| fun () ->
        let d = Decimal([| -1; -1; -2; 0 |])
        let bits = Decimal.GetBits(d)
        let d2 = Decimal(bits)
        let d3 = Decimal(bits.[0], bits.[1], bits.[2], true, 0uy)
        d2 |> equal d
        d3 |> equal -d

    testCase "Decimal abs works" <| fun () ->
        abs -4M |> equal 4M

    testCase "Decimal round works" <| fun () ->
        round -12.5M |> equal -12.M
        round 1.5M |> equal 2.M
        round 1.535M |> equal 2.M
        round 1.525M |> equal 2.M
        Math.Round 1.425M |> equal 1.M
        Math.Round -1.425M |> equal -1.M
        Math.Round 1.546M |> equal 2.M
        Math.Round -1.546M |> equal -2.M

    testCase "Decimal round half to even works" <| fun () ->
        round 1.5M |> equal 2.M
        round 2.5M |> equal 2.M
        round 3.5M |> equal 4.M
        round -1.5M |> equal -2.M
        round -2.5M |> equal -2.M
        round -3.5M |> equal -4.M

    testCase "Decimal round with digits works" <| fun () ->
        Math.Round(1.426M, 2) |> equal 1.43M
        Math.Round(1.426M, 1) |> equal 1.4M
        Math.Round(-1.426M, 2) |> equal -1.43M
        Math.Round(-1.426M, 1) |> equal -1.4M

    testCase "Decimal truncate works" <| fun () ->
        Math.Truncate -12.5M |> equal -12.M
        Math.Truncate 1.425M |> equal 1.M
        Math.Truncate -1.425M |> equal -1.M
        Math.Truncate 1.546M |> equal 1.M
        Math.Truncate -1.546M |> equal -1.M

    testCase "Decimal ceil works" <| fun () ->
        ceil 11.25M |> equal 12.M
        ceil -11.25M |> equal -11.M
        Math.Ceiling 11.25M |> equal 12.M

    testCase "Decimal floor works" <| fun () ->
        floor 11.75M |> equal 11.M
        floor -11.75M |> equal -12.M
        Math.Floor 11.25M |> equal 11.M

    testCase "Decimal pown works" <| fun () ->
        pown 2.2M 3 |> equal 10.648M

    testCase "Int64 Infix add can be generated" <| fun () ->
        4L + 2L |> equal 6L

    testCase "Int64 Infix subtract can be generated" <| fun () ->
        4L - 2L |> equal 2L

    testCase "Int64 Infix multiply can be generated" <| fun () ->
        4L * 2L |> equal 8L

    testCase "Int64 Infix divide can be generated" <| fun () ->
        4L / 2L |> equal 2L

    testCase "Int64 Integer division doesn't produce floats" <| fun () ->
        5. / 2. |> equal 2.5
        5L / 2L |> equal 2L
        5L / 3L |> equal 1L
        // float 5L / 2. |> equal 2.5 // TODO: Number conversion

    testCase "Int64 Infix modulo can be generated" <| fun () ->
        4L % 3L |> equal 1L

    testCase "Int64 Evaluation order is preserved by generated code" <| fun () ->
        (4L - 2L) * 2L + 1L |> equal 5L

    testCase "Int64 Bitwise and can be generated" <| fun () ->
        6L &&& 2L |> equal 2L

    testCase "Int64 Bitwise or can be generated" <| fun () ->
        4L ||| 2L |> equal 6L

    testCase "Int64 Bitwise shift left can be generated" <| fun () ->
        4L <<< 2 |> equal 16L

    testCase "Int64 Bitwise shift right can be generated" <| fun () ->
        4L >>> 2 |> equal 1L

    testCase "UInt64 Bitwise shift right can be generated" <| fun () -> // See #1482
      15210016002388773605UL >>> 33 |> equal 1770678907UL

    testCase "Int64 abs works" <| fun () ->
        abs -4L |> equal 4L

    testCase "Big integers addition works" <| fun () ->
        let x = 59823749821707124891298739821798327321028091380980I
        let y = bigint 1L
        let z = 1I
        (x + y + z) |> equal 59823749821707124891298739821798327321028091380982I

    testCase "BigInt Infix add can be generated" <| fun () ->
        4I + 2I |> equal 6I

    testCase "BigInt Infix subtract can be generated" <| fun () ->
        4I - 2I |> equal 2I

    testCase "BigInt Infix multiply can be generated" <| fun () ->
        4I * 2I |> equal 8I

    testCase "BigInt Infix divide can be generated" <| fun () ->
        4I / 2I |> equal 2I

    testCase "BigInt Integer division doesn't produce floats" <| fun () ->
        5. / 2. |> equal 2.5
        5I / 2I |> equal 2I
        5I / 3I |> equal 1I
        // float 5I / 2. |> equal 2.5

    testCase "BigInt Infix modulo can be generated" <| fun () ->
        4I % 3I |> equal 1I

    testCase "BigInt.DivRem works" <| fun () -> // See #1744
        let quotient,remainder = bigint.DivRem(5I,2I)
        2I |> equal quotient
        1I |> equal remainder

    testCase "BigInt Evaluation order is preserved by generated code" <| fun () ->
        (4I - 2I) * 2I + 1I |> equal 5I

    testCase "BigInt Bitwise and can be generated" <| fun () ->
        6I &&& 2I |> equal 2I

    testCase "BigInt Bitwise or can be generated" <| fun () ->
        4I ||| 2I |> equal 6I

    testCase "BigInt Bitwise xor can be generated" <| fun () ->
        6I ^^^ 2I |> equal 4I

    testCase "BigInt Bitwise shift left can be generated" <| fun () ->
        4I <<< 2 |> equal 16I

    testCase "BigInt Bitwise shift right can be generated" <| fun () ->
        4I >>> 2 |> equal 1I

    testCase "BigInt abs works" <| fun () ->
        abs -4I |> equal 4I

    testCase "abs works" <| fun () ->
        abs -4 |> equal 4

    testCase "round works" <| fun () ->
        round -12.5 |> equal -12.
        round 1.5 |> equal 2.
        round 1.535 |> equal 2.
        round 1.525 |> equal 2.

    testCase "ceil works" <| fun () ->
        ceil 11.25 |> equal 12.
        ceil -11.25 |> equal -11.
        Math.Ceiling 11.25 |> equal 12.

    testCase "floor works" <| fun () ->
        floor 11.75 |> equal 11.
        floor -11.75 |> equal -12.
        Math.Floor 11.25 |> equal 11.

    testCase "pown works" <| fun () ->
        pown 2.2 3 |> checkTo3dp 10648.

    testCase "sqrt works" <| fun () ->
        sqrt 4.5 |> checkTo3dp 2121.

    // As per https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
    testCase "sqrt matches .net core implementation" <| fun () ->
        checkTo3dp 1732. (sqrt 3.0)
        sqrt 0.0  |> equal 0.0
        isNaN (sqrt -3.0) |> equal true
        isNaN (sqrt System.Double.NaN) |> equal true
        isNaN (sqrt negativeInfinity) |> equal true
        sqrt positiveInfinity |> equal positiveInfinity

    testCase "Double.Parse works with IFormatProvider" <| fun () ->
        // culture compiles to { } for now and it is ignored on the call-site
        let culture = Globalization.CultureInfo.InvariantCulture
        let result = System.Double.Parse("10.5", culture)
        result |> equal 10.5

    testCase "Single.Parse works with IFormatProvider" <| fun () ->
        // culture compiles to { } for now and it is ignored on the call-site
        let culture = Globalization.CultureInfo.InvariantCulture
        let result = System.Single.Parse("10.5", culture)
        float result |> equal 10.5

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

    testCase "cosh works" <| fun () ->
        cosh 0.25 |> checkTo3dp 1031.

    testCase "sinh works" <| fun () ->
        sinh 0.25 |> checkTo3dp 252.

    testCase "tanh works" <| fun () ->
        tanh 0.25 |> checkTo3dp 244.

    testCase "exp works" <| fun () ->
        exp 8.0 |> checkTo3dp 2980957.

    // https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L228
    testCase "log works" <| fun () ->
        log 232.12 |> checkTo3dp 5447.
        checkTo3dp 1098. (log 3.0)
        log 0.0 |> equal negativeInfinity
        isNaN (log -2.0) |> equal true
        isNaN (log System.Double.NaN) |> equal true
        isNaN (log negativeInfinity) |> equal true
        log positiveInfinity |> equal positiveInfinity

    // https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L239
    testCase "Math.Log(double, double) works" <| fun () ->
        Math.Log(8.0, 2.0) |> equal 3.0
        Math.Log(3.0, 3.0) |> equal 1.0
        Math.Log(14., 3.0) |> checkTo3dp 2402.
        Math.Log(0.0, 3.0) |> equal negativeInfinity
        Math.Log(positiveInfinity, 3.0) |> equal positiveInfinity
        isNaN (Math.Log(-3.0, 3.0)) |> equal true
        isNaN (Math.Log(System.Double.NaN, 3.0)) |> equal true
        isNaN (Math.Log(negativeInfinity, 3.0)) |> equal true

    testCase "log10 works" <| fun () ->
        log10 232.12 |> checkTo3dp 2365.

    testCase "PI works" <| fun () ->
        checkTo3dp 3141. Math.PI

    testCase "E works" <| fun () ->
        checkTo3dp 2718. Math.E

    testCase "Math.abs works" <| fun () ->
        Math.Abs -4 |> equal 4

    testCase "Math.pown works" <| fun () ->
        Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

    testCase "Math.sqrt works" <| fun () ->
        Math.Sqrt 4.5 |> checkTo3dp 2121.

    testCase "Math.round works" <| fun () ->
        Math.Round -12.5 |> equal -12.
        Math.Round 1.425 |> equal 1.
        Math.Round -1.425 |> equal -1.
        Math.Round 1.546 |> equal 2.
        Math.Round -1.546 |> equal -2.

    testCase "Math.round with digits works" <| fun () ->
        Math.Round(1.426, 2) |> equal 1.43
        Math.Round(1.426, 1) |> equal 1.4
        Math.Round(-1.426, 2) |> equal -1.43
        Math.Round(-1.426, 1) |> equal -1.4

    testCase "Math.truncate works" <| fun () ->
        Math.Truncate -12.5 |> equal -12.
        Math.Truncate 1.425 |> equal 1.
        Math.Truncate -1.425 |> equal -1.
        Math.Truncate 1.546 |> equal 1.
        Math.Truncate -1.546 |> equal -1.

    testCase "Math.ceil works" <| fun () ->
        Math.Ceiling 11.25 |> equal 12.

    testCase "Math.floor works" <| fun () ->
        Math.Floor 11.75 |> equal 11.

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
        !i |> equal 6

    testCase "decr works" <| fun () ->
        let i = ref 5
        decr i
        !i |> equal 4

    testCase "System.Random works" <| fun () ->
        let rnd = System.Random()
        let x = rnd.Next(5)
        (x >= 0 && x < 5) |> equal true

        let y = rnd.NextDouble()
        (y >= 0.0 && y < 1.0) |> equal true

    // Note: Test could fail sometime during life of universe, if it picks all zeroes.
    testCase "System.Random.NextBytes works" <| fun () ->
        let buffer = Array.create 16 0uy // guid-sized buffer
        System.Random().NextBytes(buffer)
        buffer = Array.create 16 0uy |> equal false

    testCase "Long integers equality works" <| fun () ->
        let x = 5L
        let y = 5L
        let z = 6L
        (x = y) |> equal true
        (y = z) |> equal false
        equals y x |> equal true
        equals z x |> equal false

    testCase "Long integers comparison works" <| fun () ->
        let x = 5L
        let y = 5L
        let z = 6L
        compare x y |> equal 0
        compare y z |> equal -1
        compareTo y x |> equal 0
        compareTo z x |> equal 1

    testCase "bigint equality works" <| fun () ->
        let a = 9007199254740992I
        let b = 9007199254740993I
        (a = b) |> equal false

    testCase "Big integers equality works" <| fun () ->
        let x = 59823749821707124891298739821798327321028091380980I
        let y = 59823749821707124891298739821798327321028091380980I
        let z = 59823749821707124891298739821798327321028091380981I
        (x = y) |> equal true
        (y = z) |> equal false
        equals y x |> equal true
        equals z x |> equal false

    testCase "Big integers comparison works" <| fun () ->
        let x = 5I
        let y = 5I
        let z = 6I
        compare x y |> equal 0
        compare y z |> equal -1
        compareTo y x |> equal 0
        compareTo z x |> equal 1

    testCase "Big integer to byte array works" <| fun () ->
        // values with high bit both 0 and 1 for different array lengths
        32767I.ToByteArray() |> equal [|255uy; 127uy|]
        32768I.ToByteArray() |> equal [|0uy; 128uy; 0uy|]
        -32768I.ToByteArray() |> equal [|0uy; 128uy|]
        -32769I.ToByteArray() |> equal [|255uy; 127uy; 255uy|]
        // large numbers
        111222333444555666777888999I.ToByteArray() |> equal [|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]
        -111222333444555666777888999I.ToByteArray() |> equal [|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]

    testCase "Big integer from byte array works" <| fun () ->
        // values with high bit both 0 and 1 for different array lengths
        Numerics.BigInteger([|255uy; 127uy|]) |> equal 32767I
        Numerics.BigInteger([|0uy; 128uy; 0uy|]) |> equal 32768I
        Numerics.BigInteger([|0uy; 128uy|]) |> equal -32768I
        Numerics.BigInteger([|255uy; 127uy; 255uy|]) |> equal -32769I
        // large numbers
        Numerics.BigInteger([|231uy; 216uy; 2uy; 164uy; 86uy; 149uy; 8uy; 199uy; 62uy; 0uy; 92uy|]) |> equal 111222333444555666777888999I
        Numerics.BigInteger([|25uy; 39uy; 253uy; 91uy; 169uy; 106uy; 247uy; 56uy; 193uy; 255uy; 163uy|]) |> equal -111222333444555666777888999I

    testCase "Member values of decimal type can be compared" <| fun () -> // See #747
        decimalOne < decimalTwo |> equal true
        decimalOne > decimalTwo |> equal false

    testCase "Sign operator works" <| fun () -> // See #1311
        sign 1 |> equal 1
        sign 34 |> equal 1
        sign 1L |> equal 1
        sign 36L |> equal 1
        sign 1. |> equal 1
        sign 89 |> equal 1
        sign 1 |> equal 1
        sign 0 |> equal 0
        sign 0L |> equal 0
        sign 0. |> equal 0
        sign -1 |> equal -1
        sign -56 |> equal -1
        sign -1L |> equal -1
        sign -72L |> equal -1
        sign -1. |> equal -1
        sign -89. |> equal -1

    testCase "Formatting of decimal works" <| fun () ->

        let formatNumber (d:decimal) =
            (sprintf "%.2f" d).Replace(",","").Replace(".",",")

        formatNumber 0.0M |> equal "0,00"
        formatNumber 0.020M |> equal "0,02"
        formatNumber 0.20M |> equal "0,20"
        formatNumber 2.0M |> equal "2,00"


    testCase "Formatting of decimal works with inline" <| fun () ->

        let inline formatNumber (d:decimal) =
            (sprintf "%.2f" d).Replace(",","").Replace(".",",")

        formatNumber 0.0M |> equal "0,00"
        formatNumber 0.020M |> equal "0,02"
        formatNumber 0.20M |> equal "0,20"
        formatNumber 2.0M |> equal "2,00"

    testCase "Formatting of € works with inline" <| fun () ->

        let inline formatNumber (d:decimal) =
            (sprintf "%.2f" d).Replace(",","").Replace(".",",")

        let inline formatEuro (d:decimal) = (formatNumber d) + " €"

        formatEuro 0.0M |> equal "0,00 €"
        formatEuro 0.020M |> equal "0,02 €"
        formatEuro 0.20M |> equal "0,20 €"
        formatEuro 2.0M |> equal "2,00 €"
]
