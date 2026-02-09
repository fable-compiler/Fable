module Fable.Tests.Enum

open System
open FSharp.Core.LanguagePrimitives
open Fable.Tests.Util
open Util.Testing

type Fruits =
| Apple = 1
| Banana = 2
| Coconut = 4

type Vegetables =
| Tomato of string
| Lettuce of string

[<Fact>]
let ``test Enum HasFlag works`` () =
    let value = Fruits.Apple ||| Fruits.Banana
    equal true (value.HasFlag Fruits.Apple)
    equal true (value.HasFlag Fruits.Banana)
    equal false (value.HasFlag Fruits.Coconut)

[<Fact>]
let ``test Enum operator equal works`` () =
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

[<Fact>]
let ``test Enum operator not-equal works`` () =
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

[<Fact>]
let ``test Enum operator less-than works`` () =
    let a = Fruits.Apple
    let b = Fruits.Banana
    a < a |> equal false
    a < b |> equal true
    b < a |> equal false

[<Fact>]
let ``test Enum operator less-equal works`` () =
    let a = Fruits.Apple
    let b = Fruits.Banana
    a <= a |> equal true
    a <= b |> equal true
    b <= a |> equal false

[<Fact>]
let ``test Enum operator greater-than works`` () =
    let a = Fruits.Apple
    let b = Fruits.Banana
    a > a |> equal false
    a > b |> equal false
    b > a |> equal true

[<Fact>]
let ``test Enum operator greater-equal works`` () =
    let a = Fruits.Apple
    let b = Fruits.Banana
    a >= a |> equal true
    a >= b |> equal false
    b >= a |> equal true

[<Fact>]
let ``test EnumOfValue works`` () =
    EnumOfValue 1 |> equal Fruits.Apple
    EnumOfValue 2 |> equal Fruits.Banana
    EnumOfValue 4 |> equal Fruits.Coconut

[<Fact>]
let ``test EnumToValue works`` () =
    EnumToValue Fruits.Apple |> equal 1
    EnumToValue Fruits.Banana |> equal 2
    EnumToValue Fruits.Coconut |> equal 4

[<Fact>]
let ``test Enum operator enum works`` () =
    enum 1 |> equal Fruits.Apple
    enum 2 |> equal Fruits.Banana
    enum 4 |> equal Fruits.Coconut

[<Fact>]
let ``test Pattern matching on enum works`` () =
    let fruit = Fruits.Apple
    let veggie = Tomato("kumato")
    let result =
        match fruit with
        | Fruits.Apple ->
            match veggie with
            | Tomato kind -> kind
            | _ -> "foo"
        | Fruits.Banana
        | Fruits.Coconut ->
            match veggie with
            | Lettuce kind -> kind
            | _ -> "bar"
        | _ -> "invalid choice"
    result |> equal "kumato"

[<Fact>]
let ``test Enum bitwise OR works`` () =
    let combined = Fruits.Apple ||| Fruits.Coconut
    int combined |> equal 5

[<Fact>]
let ``test Enum bitwise AND works`` () =
    let combined = Fruits.Apple ||| Fruits.Banana ||| Fruits.Coconut
    let masked = combined &&& Fruits.Banana
    int masked |> equal 2

[<Fact>]
let ``test Enum int conversion works`` () =
    int Fruits.Apple |> equal 1
    int Fruits.Banana |> equal 2
    int Fruits.Coconut |> equal 4
