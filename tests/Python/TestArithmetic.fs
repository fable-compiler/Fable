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

// FIXME
// [<Fact>]
// let ``test Unary negation with negative literal values works`` () =
//     -literalNegativeValue |> equal 345

[<Fact>]
let ``test Unary negation with integer MinValue works`` () =
    -(-128y) |> equal System.SByte.MinValue
    -(-32768s) |> equal System.Int16.MinValue
    -(-2147483648) |> equal System.Int32.MinValue
    // FIXME -(-9223372036854775808L) |> equal System.Int64.MinValue

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
