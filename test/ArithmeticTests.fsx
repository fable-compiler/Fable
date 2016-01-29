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

    [<Test>]
    let ``abs works``() =
        Assert.AreEqual (abs -4, 4)

    [<Test>]
    let ``round works``() =
        Assert.AreEqual (round -12.5, -12)

    [<Test>]
    let ``ceil works``() =
        Assert.AreEqual (ceil 11.25, 12)

    [<Test>]
    let ``floor works``() =
        Assert.AreEqual (floor 11.75, 11)

    let checkTo3dp expected actual =
        Assert.AreEqual (floor(actual * 1000.), expected)

    [<Test>]
    let ``pown works``() =
        pown 2.2 3 |> checkTo3dp 10648

    [<Test>]
    let ``sqrt works``() =
        sqrt 4.5 |> checkTo3dp 2121
        
    [<Test>]
    let ``exp works``() =
        exp 8.0 |> checkTo3dp 2980957

    [<Test>]
    let ``acos works``() =
        acos 0.25 |> checkTo3dp 1318

    [<Test>]
    let ``asin works``() =
        asin 0.25 |> checkTo3dp 252

    [<Test>]
    let ``atan works``() =
        atan 0.25 |> checkTo3dp 244

    [<Test>]
    let ``atan2 works``() =
        atan2 90. 15. |> checkTo3dp 1405

    [<Test>]
    let ``cos works``() =
        cos 0.25 |> checkTo3dp 968

    [<Test>]
    let ``sin works``() =
        sin 0.25 |> checkTo3dp 247

    [<Test>]
    let ``tan works``() =
        tan 0.25 |> checkTo3dp 255

    [<Test>]
    let ``log works``() =
        log 232.12 |> checkTo3dp 5447

    [<Test>]
    let ``log10 works``() =
        log10 232.12 |> checkTo3dp 2365

(*
    [<Test>]
    let ``PI works``() = checkTo3dp 3141. Math.PI

    [<Test>]
    let ``E works``() = checkTo3dp 2718. Math.E

    [<Test>]
    let ``sin works``() = checkTo3dp <@ sin(0.25 * Math.PI) @>

    [<Test>]
    let ``sqrt works``() = checkTo3dp <@ sqrt 4.0 @>

    [<Test>]
    let ``tan works``() = checkTo3dp <@ tan(0.25 * Math.PI) @>

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
*)