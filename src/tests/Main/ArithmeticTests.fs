[<Util.Testing.TestFixture>]
module Fable.Tests.Arithmetic
open System
open Util.Testing

[<Test>]
let ``Infix add can be generated``() =
    Assert.AreEqual (4 + 2, 6)

[<Test>]
let ``Infix subtract can be generated``() =
    Assert.AreEqual (4 - 2, 2)

[<Test>]
let ``Infix multiply can be generated``() =
    Assert.AreEqual (4 * 2, 8)

[<Test>]
let ``Infix divide can be generated``() =
    Assert.AreEqual (4 / 2, 2)

[<Test>]
let ``Integer division doesn't produce floats``() =
    Assert.AreEqual (5. / 2., 2.5)
    Assert.AreEqual (5 / 2, 2)
    Assert.AreEqual (5 / 3, 1)
    Assert.AreEqual (float 5 / 2., 2.5)

[<Test>]
let ``Infix modulo can be generated``() =
    Assert.AreEqual (4 % 3, 1)

[<Test>]
let ``Evaluation order is preserved by generated code``() =
    Assert.AreEqual ((4 - 2) * 2 + 1, 5)

[<Test>]
let ``Bitwise and can be generated``() =
    Assert.AreEqual (6 &&& 2, 2)

[<Test>]
let ``Bitwise or can be generated``() =
    Assert.AreEqual (4 ||| 2, 6)

[<Test>]
let ``Bitwise shift left can be generated``() =
    Assert.AreEqual (4 <<< 2, 16)

[<Test>]
let ``Bitwise shift right can be generated``() =
    Assert.AreEqual (4 >>> 2, 1)

[<Test>]
let ``Int64 Infix add can be generated``() =
    Assert.AreEqual (4L + 2L, 6L)

[<Test>]
let ``Int64 Infix subtract can be generated``() =
    Assert.AreEqual (4L - 2L, 2L)

[<Test>]
let ``Int64 Infix multiply can be generated``() =
    Assert.AreEqual (4L * 2L, 8L)

[<Test>]
let ``Int64 Infix divide can be generated``() =
    Assert.AreEqual (4L / 2L, 2L)

[<Test>]
let ``Int64 Integer division doesn't produce floats``() =
    Assert.AreEqual (5. / 2., 2.5)
    Assert.AreEqual (5L / 2L, 2L)
    Assert.AreEqual (5L / 3L, 1L)
    Assert.AreEqual (float 5L / 2., 2.5)

[<Test>]
let ``Int64 Infix modulo can be generated``() =
    Assert.AreEqual (4L % 3L, 1L)

[<Test>]
let ``Int64 Evaluation order is preserved by generated code``() =
    Assert.AreEqual ((4L - 2L) * 2L + 1L, 5L)

[<Test>]
let ``Int64 Bitwise and can be generated``() =
    Assert.AreEqual (6L &&& 2L, 2L)

[<Test>]
let ``Int64 Bitwise or can be generated``() =
    Assert.AreEqual (4L ||| 2L, 6L)

[<Test>]
let ``Int64 Int64 Bitwise shift left can be generated``() =
    Assert.AreEqual (4L <<< 2, 16L)

[<Test>]
let ``Int64 Bitwise shift right can be generated``() =
    Assert.AreEqual (4L >>> 2, 1L)

[<Test>]
let ``Int64 abs works``() =
    Assert.AreEqual (abs -4L, 4L)

[<Test>]
let ``abs works``() =
    Assert.AreEqual (abs -4, 4)

[<Test>]
let ``round works``() =
    Assert.AreEqual (round -12.5, -12.)
    Assert.AreEqual (round 1.5, 2.)
    Assert.AreEqual (round 1.535, 2.)
    Assert.AreEqual (round 1.525, 2.)
    Assert.AreEqual (System.Math.Round(1.55, 1), 1.6)

[<Test>]
let ``ceil works``() =
    Assert.AreEqual (ceil 11.25, 12.)

[<Test>]
let ``floor works``() =
    Assert.AreEqual (floor 11.75, 11.)

let checkTo3dp (expected: float) actual =
    Assert.AreEqual (floor(actual * 1000.), expected)

[<Test>]
let ``pown works``() =
    pown 2.2 3 |> checkTo3dp 10648.

[<Test>]
let ``sqrt works``() =
    sqrt 4.5 |> checkTo3dp 2121.

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

[<Test>]
let ``log works``() =
    log 232.12 |> checkTo3dp 5447.

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
    Assert.AreEqual(Math.Abs -4, 4)

[<Test>]
let ``Math.pown works``() =
    Math.Pow(2.2, 3.0) |> checkTo3dp 10648.

[<Test>]
let ``Math.sqrt works``() =
    Math.Sqrt 4.5 |> checkTo3dp 2121.

[<Test>]
let ``Math.round works``() =
    Assert.AreEqual(Math.Round -12.5, -12.)

[<Test>]
let ``Math.ceil works``() =
    Assert.AreEqual(Math.Ceiling 11.25, 12.)

[<Test>]
let ``Math.floor works``() =
    Assert.AreEqual(Math.Floor 11.75, 11.)

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
    Assert.AreEqual(!i, 6)

[<Test>]
let ``decr works``() =
    let i = ref 5
    decr i
    Assert.AreEqual(!i, 4)

[<Test>]
let ``System.Random works``() =
    let rnd = System.Random()
    let x = rnd.Next(5)
    Assert.AreEqual(true, x >= 0 && x < 5)
