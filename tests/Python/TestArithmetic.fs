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

[<Fact>]
let ``test Infix add can be generated`` () =
    4 + 2 |> equal 6

[<Fact>]
let ``test Int32 literal addition is optimized`` () =
    aLiteral + 7 |> equal 12
    notALiteral + 7 |> equal 12

[<Fact>]
let ``test Unary negation with negative literal values works`` () =
     -literalNegativeValue |> equal 345

[<Fact>]
let ``test Unary negation with integer MinValue works`` () =
    -(-128y) |> equal System.SByte.MinValue
    -(-32768s) |> equal System.Int16.MinValue
    -(-2147483648) |> equal System.Int32.MinValue
    -(-9223372036854775808L) |> equal System.Int64.MinValue

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

[<Fact>]
let ``test Infix modulo can be generated`` () =
    4 % 3 |> equal 1

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

[<Fact>]
let ``test Bitwise AND on large unsigned integer works`` () =
    0x80000000u &&& 0xffffffffu |> equal (0x80000000u &&& 0xffffffffu >>> 0)

[<Fact>]
let ``test Bitwise XOR on large unsigned integer works`` () =
    0x80000000u ^^^ 0u |> equal (0x80000000u ^^^ 0u >>> 0)

[<Fact>]
let ``test Bitwise Invert on large unsigned integer works`` () =
    (~~~0x80000000u >>> 0) |> equal ~~~0x80000000u

[<Fact>]
let ``test Bitwise shift right can be generated`` () =
    4 >>> 2 |> equal 1

[<Fact>]
let ``test Zero fill shift right (>>>) for uint32`` () =
    0x80000000 >>> 1 |> equal -1073741824
    0x80000000u >>> 1 |> equal 1073741824u

[<Fact>]
let ``test UInt64 multiplication with 0 returns uint``() =
    0x0UL * 0x1UL |> equal 0x0UL

[<Fact>]
let ``test Decimal literals can be generated`` () =
    0M |> equal System.Decimal.Zero
    1M |> equal System.Decimal.One
    -1M |> equal System.Decimal.MinusOne
    // FIXME: 79228162514264337593543950335M |> equal System.Decimal.MaxValue
    // FIXME: -79228162514264337593543950335M |> equal System.Decimal.MinValue

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
    // string 0.0000000000000000000000000000M |> equal "0.0000000000000000000000000000"
    string 79228162514264337593543950335M |> equal "79228162514264337593543950335"
    string -79228162514264337593543950335M |> equal "-79228162514264337593543950335"

[<Fact>]
let ``test Decimal precision is kept`` () =
    let items = [ 290.8M
                  290.8M
                  337.12M
                  6.08M
                  -924.8M ]
    List.sum items |> equal 0M

[<Fact>]
let ``test Decimal max precision is kept`` () =
    let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286M
    string pi |> equal "3.1415926535897932384626433833"

//[<Fact>]
//let ``test Decimal average precision is kept`` () =
//    let items = [1M; 2M; 5M]
//    List.average items |> equal 2.6666666666666666666666666667M

[<Fact>]
let ``test Decimal division precision is kept`` () =
    string (8M / 3M) |> equal "2.6666666666666666666666666667"
    

//[<Fact>]
//let ``test Decimal division works`` () =
//    let a = decimal 0.00001
//    let b = 1000.M
//    let c = decimal 0.00000001
//    a / b |> equal c

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
let ``test pown works``() =
    pown 2.2 3 |> checkTo3dp 10648.

[<Fact>]
let ``test sqrt works`` () =
    sqrt 4.5 |> checkTo3dp 2121.

// As per https://github.com/dotnet/corefx/blob/master/src/System.Runtime.Extensions/tests/System/Math.cs#L217
[<Fact>]
let ``test sqrt matches .net core implementation`` () =
    checkTo3dp 1732. (sqrt 3.0)
    sqrt 0.0  |> equal 0.0
    isNaN (sqrt -3.0) |> equal true
    isNaN (sqrt System.Double.NaN) |> equal true
    isNaN (sqrt negativeInfinity) |> equal true
    sqrt positiveInfinity |> equal positiveInfinity

[<Fact>]
let ``test power works`` () =
    let x = 10.0 ** 2.
    x |> equal 100.0

[<Fact>]
let ``test extreme values work`` () =
    0.0 / 0.0 |> isNaN |> equal true
    0.0 / (-0.0) |> isNaN |> equal true
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
