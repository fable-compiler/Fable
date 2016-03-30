[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Enum

open System
open FSharp.Core.LanguagePrimitives
open NUnit.Framework
open Fable.Tests.Util

type Fruits = 
| Apple = 1
| Banana = 2
| Coconut = 3

[<Test>]
let ``Enum operator = works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple = Fruits.Apple |> equal true
    a = Fruits.Apple |> equal true
    Fruits.Apple = a |> equal true
    a = a |> equal true

    Fruits.Apple = Fruits.Banana |> equal false
    a = Fruits.Banana |> equal false
    Fruits.Banana = a |> equal false
    a = b |> equal false

[<Test>]
let ``Enum operator <> works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple <> Fruits.Apple |> equal false
    a <> Fruits.Apple |> equal false
    Fruits.Apple <> a |> equal false
    a <> a |> equal false

    Fruits.Apple <> Fruits.Banana |> equal true
    a <> Fruits.Banana |> equal true
    Fruits.Banana <> a |> equal true
    a <> b |> equal true

[<Test>]
let ``Enum operator < works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple < Fruits.Apple |> equal false
    a < Fruits.Apple |> equal false
    Fruits.Apple < a |> equal false
    a < a |> equal false

    Fruits.Apple < Fruits.Banana |> equal true
    a < Fruits.Banana |> equal true
    Fruits.Banana < a |> equal false
    a < b |> equal true

[<Test>]
let ``Enum operator <= works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple <= Fruits.Apple |> equal true
    a <= Fruits.Apple |> equal true
    Fruits.Apple <= a |> equal true
    a <= a |> equal true

    Fruits.Apple <= Fruits.Banana |> equal true
    a <= Fruits.Banana |> equal true
    Fruits.Banana <= a |> equal false
    a <= b |> equal true

[<Test>]
let ``Enum operator > works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple > Fruits.Apple |> equal false
    a > Fruits.Apple |> equal false
    Fruits.Apple > a |> equal false
    a > a |> equal false

    Fruits.Apple > Fruits.Banana |> equal false
    a > Fruits.Banana |> equal false
    Fruits.Banana > a |> equal true
    a > b |> equal false

[<Test>]
let ``Enum operator >= works``() =
    let a = Fruits.Apple
    let b = Fruits.Banana

    Fruits.Apple >= Fruits.Apple |> equal true
    a >= Fruits.Apple |> equal true
    Fruits.Apple >= a |> equal true
    a >= a |> equal true

    Fruits.Apple >= Fruits.Banana |> equal false
    a >= Fruits.Banana |> equal false
    Fruits.Banana >= a |> equal true
    a >= b |> equal false

[<Test>]
let ``EnumOfValue works``() =
    EnumOfValue 1 |> equal Fruits.Apple
    EnumOfValue 2 |> equal Fruits.Banana
    EnumOfValue 3 |> equal Fruits.Coconut
