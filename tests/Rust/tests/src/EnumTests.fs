module Fable.Tests.EnumTests

open System
open Util.Testing

type Fruits =
| Apple = 1
| Banana = 2
| Coconut = 4

type Vegetables =
| Tomato of string
| Lettuce of string

let myRootValue =
    match 5 with
    // mutable is used to prevent immutable binding optimization
    | 0 -> let mutable r = 4 in r + 4
    | 5 -> let mutable r = 3 in r + 7
    | _ -> -1

module EnumOperations =
    let inline internal enumOfValue1<'T,'U when 'U: enum<'T>> (value: 'T) : 'U = LanguagePrimitives.EnumOfValue<'T,'U>(value)
    let inline internal enumOfValue2 value = LanguagePrimitives.EnumOfValue value
    // let enumOfValue3 value = LanguagePrimitives.EnumOfValue value
    // let enumOfValue4 = LanguagePrimitives.EnumOfValue

type BinaryBoolean =
    | A = 0uy
    | B = 1uy
    | C = 2uy
    | D = 3uy
    | E = 4uy

type MyEnum =
    | Foo = 1y
    | Bar = 5y
    | Baz = 8y

[<Fact>]
let ``Enum.HasFlag works`` () =
    let value = Fruits.Apple ||| Fruits.Banana
    equal true (value.HasFlag Fruits.Apple)
    equal true (value.HasFlag Fruits.Banana)
    equal false (value.HasFlag Fruits.Coconut)

[<Fact>]
let ``Enum operator eq works`` () =
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
let ``Enum operator neq works`` () =
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
let ``Enum operator lt works `` () =
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

[<Fact>]
let ``Enum operator lte works`` () =
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

[<Fact>]
let ``Enum operator gt works`` () =
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

[<Fact>]
let ``Enum operator gte works`` () =
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

[<Fact>]
let ``EnumOfValue works`` () =
    LanguagePrimitives.EnumOfValue 1 |> equal Fruits.Apple
    LanguagePrimitives.EnumOfValue 2 |> equal Fruits.Banana
    LanguagePrimitives.EnumOfValue 4 |> equal Fruits.Coconut

[<Fact>]
let ``EnumOfValue works in inlined functions`` () = // #1536
    let e0 : BinaryBoolean = LanguagePrimitives.EnumOfValue 0uy
    let e1 : BinaryBoolean = EnumOperations.enumOfValue1 1uy
    let e2 : BinaryBoolean = EnumOperations.enumOfValue2 2uy
    // let e3 : BinaryBoolean = EnumOperations.enumOfValue3 3uy
    // let e4 : BinaryBoolean = EnumOperations.enumOfValue4 4uy
    e0 |> equal BinaryBoolean.A
    e1 |> equal BinaryBoolean.B
    e2 |> equal BinaryBoolean.C
    // e3 |> equal BinaryBoolean.D
    // e4 |> equal BinaryBoolean.E
    match e2 with
    | BinaryBoolean.C -> ()
    | _ -> failwith "unexpected"

[<Fact>]
let ``EnumToValue works`` () =
    LanguagePrimitives.EnumToValue Fruits.Apple |> equal 1
    LanguagePrimitives.EnumToValue Fruits.Banana |> equal 2
    LanguagePrimitives.EnumToValue Fruits.Coconut |> equal 4

[<Fact>]
let ``Enum operator enum works`` () =
    enum 1 |> equal Fruits.Apple
    enum 2 |> equal Fruits.Banana
    enum 4 |> equal Fruits.Coconut

[<Fact>]
let ``Pattern matching can be nested within a switch statement`` () = // See #483
    let fruit = Fruits.Apple
    let veggie = Tomato("kumato")
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
    |> equal "kumato"

[<Fact>]
let ``Non-scoped (in JS) variables with same name can be used`` () = // See #700
    equal 10 myRootValue

// // See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-395138456
// [<Fact>]
// let ``Multiple cases with same target don't create variable duplication`` () =
//     let test arg =
//         match arg with
//         | 0 | 1 | 2 | 3 ->
//             let test2 x = x + x
//             test2
//         | _ -> id
//     (test 1) 5 |> equal 10

// See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-396456500
[<Fact>]
let ``Decision targets can be found`` () =
    let test x =
        match x with
        | (4 | 5) as r -> r
        | _ -> 1
    test 2 |> equal 1

// See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-396457352
[<Fact>]
let ``Decision targets in nested pattern matching can be found`` () =
    let test x =
        match x with
        | 1 -> 1
        | 2 ->
            match x with
            | (4 | 5) as r -> r
            | _ -> 1
        | _ -> 0
    test 2 |> equal 1

// [<Fact>]
// let ``Enum.GetValues works`` () =
//     let check (ar: Array) =
//         ar
//         |> Seq.cast<sbyte>
//         |> Seq.toList
//         |> equal [1y; 5y; 8y]
//     let t = typeof<MyEnum>
//     Enum.GetValues(t) |> check
//     t.GetEnumValues() |> check

// [<Fact>]
// let ``Enum.GetNames works`` () =
//     let t = typeof<MyEnum>
//     Enum.GetNames(t) |> equal [|"Foo"; "Bar"; "Baz"|]
//     t.GetEnumNames() |> equal [|"Foo"; "Bar"; "Baz"|]

// [<Fact>]
// let ``Enum.GetName works`` () =
//     let t = typeof<MyEnum>
//     Enum.GetName(t, MyEnum.Foo) |> equal "Foo"
//     Enum.GetName(t, box 5y) |> equal "Bar"
//     Enum.GetName(t, MyEnum.Baz) |> equal "Baz"

// [<Fact>]
// let ``Enum.IsDefined works`` () =
//     let t = typeof<MyEnum>
//     Enum.IsDefined(t, "Foo") |> equal true
//     Enum.IsDefined(t, "Baz") |> equal true
//     Enum.IsDefined(t, "Ozu") |> equal false
//     Enum.IsDefined(t, 5y) |> equal true
//     Enum.IsDefined(t, 10y) |> equal false

// [<Fact>]
// let ``Enum.Parse works`` () =
//     let t = typeof<MyEnum>
//     Enum.Parse(t, "Foo") |> equal (box MyEnum.Foo)
//     Enum.Parse(t, "Bar") |> equal (box MyEnum.Bar)
//     Enum.Parse(t, "8") |> equal (box MyEnum.Baz)
//     Enum.Parse<MyEnum>("Foo") |> equal (MyEnum.Foo)
//     Enum.Parse<MyEnum>("Bar") |> equal (MyEnum.Bar)
//     Enum.Parse<MyEnum>("8") |> equal (MyEnum.Baz)

// [<Fact>]
// let ``Enum.TryParse works`` () =
//     Enum.TryParse<MyEnum>("Foo") |> equal (true, MyEnum.Foo)
//     Enum.TryParse<MyEnum>("Bar") |> equal (true, MyEnum.Bar)
//     Enum.TryParse<MyEnum>("Ozu") |> fst |> equal false
