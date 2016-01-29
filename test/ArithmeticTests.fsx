#r "../packages/NUnit/lib/net45/nunit.framework.dll"

[<NUnit.Framework.TestFixture>] 
module ArithmeticTests =
    open System
    open NUnit.Framework

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

(*
    let checkTo3dp x = Assert.AreEqual (round(%x * 1000.), 0)

    [<Test>]
    let ``PI works``() = checkTo3dp <@ Math.PI @>

    [<Test>]
    let ``E works``() = checkTo3dp <@ Math.E @>

    [<Test>]
    let ``Math.abs works``() = checkTo3dp <@ float(Math.Abs -4) @>

    [<Test>]
    let ``Math.acos works``() = checkTo3dp <@ Math.Acos 0.25 @>

    [<Test>]
    let ``Math.asin works``() = checkTo3dp <@ Math.Asin 0.25 @>

    [<Test>]
    let ``Math.atan works``() = checkTo3dp <@ Math.Atan 0.25 @>

    [<Test>]
    let ``Math.atan2 works``() = checkTo3dp <@ Math.Atan2(-0.25, -0.25) @>

    [<Test>]
    let ``Math.ceil works``() = checkTo3dp <@ Math.Ceiling 11.25 @>

    [<Test>]
    let ``Math.cos works``() = checkTo3dp <@ Math.Cos(0.25 * Math.PI) @>

    [<Test>]
    let ``Math.exp works``() = checkTo3dp <@ Math.Exp 8.0 @>

    [<Test>]
    let ``Math.floor works``() = checkTo3dp <@ Math.Floor 11.75 @>

    [<Test>]
    let ``Math.log works``() = checkTo3dp <@ Math.Log 232.12 @>

    [<Test>]
    let ``Math.log10 works``() = checkTo3dp <@ Math.Log10 232.12 @>

    [<Test>]
    let ``Math.pown works``() = checkTo3dp <@ Math.Pow(2.2, 3.0) @>

    [<Test>]
    let ``Math.round works``() = checkTo3dp <@ Math.Round -12.5 @>

    [<Test>]
    let ``Math.sin works``() = checkTo3dp <@ Math.Sin(0.25 * Math.PI) @>

    [<Test>]
    let ``Math.sqrt works``() = checkTo3dp <@ Math.Sqrt 4.0 @>

    [<Test>]
    let ``Math.tan works``() = checkTo3dp <@ Math.Tan(0.25 * Math.PI) @>

    [<Test>]
    let ``abs works``() = checkTo3dp <@ float(abs -4) @>

    [<Test>]
    let ``acos works``() = checkTo3dp <@ acos 0.25 @>

    [<Test>]
    let ``asin works``() = checkTo3dp <@ asin 0.25 @>

    [<Test>]
    let ``atan works``() = checkTo3dp <@ atan 0.25 @>

    [<Test>]
    let ``atan2 works``() = checkTo3dp <@ atan2 -0.25 -0.25 @>

    [<Test>]
    let ``ceil works``() = checkTo3dp <@ ceil 11.25 @>

    [<Test>]
    let ``cos works``() = checkTo3dp <@ cos(0.25 * Math.PI) @>

    [<Test>]
    let ``exp works``() = checkTo3dp <@ exp 8.0 @>

    [<Test>]
    let ``floor works``() = checkTo3dp <@ floor 11.75 @>

    [<Test>]
    let ``log works``() = checkTo3dp <@ Microsoft.FSharp.Core.Operators.log (232.12) @>

    [<Test>]
    let ``log10 works``() = checkTo3dp <@ log10 232.12 @>

    [<Test>]
    let ``pown works``() = checkTo3dp <@ pown 2.2 3 @>

    [<Test>]
    let ``round works``() = checkTo3dp <@ round -12.5 @>

    [<Test>]
    let ``sin works``() = checkTo3dp <@ sin(0.25 * Math.PI) @>

    [<Test>]
    let ``sqrt works``() = checkTo3dp <@ sqrt 4.0 @>

    [<Test>]
    let ``tan works``() = checkTo3dp <@ tan(0.25 * Math.PI) @>
*)