module Fable.Tests.Enum

open System
open FSharp.Core.LanguagePrimitives
open Util.Testing

type Fruits =
| Apple = 1
| Banana = 2
| Coconut = 4

type Vegetables =
| Tomato of string
| Lettuce of string

let myRootValue =
    // mutable is used to prevent immutable binding optimization
    let mutable f = fun x ->
        match x with
        | 0 -> let mutable r = 4 in r + 4
        | 5 -> let mutable r = 3 in r + 7
        | _ -> -1
    f 5

module EnumOperations =
    let inline enumOfValue1<'T,'U when 'U: enum<'T>> (value: 'T) : 'U = LanguagePrimitives.EnumOfValue<'T,'U>(value)
    let inline enumOfValue2 value = LanguagePrimitives.EnumOfValue value
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

let tests =
  testList "Enum" [
    testCase "Enum.HasFlag works" <| fun () ->
        let value = Fruits.Apple ||| Fruits.Banana
        equal true (value.HasFlag Fruits.Apple)
        equal true (value.HasFlag Fruits.Banana)
        equal false (value.HasFlag Fruits.Coconut)

    testCase "Enum operator = works" <| fun () ->
        let a = Fruits.Apple
        let b = Fruits.Banana

        Fruits.Apple = Fruits.Apple |> equal true
        a = Fruits.Apple |> equal true
        Fruits.Apple = a |> equal true
        a = a |> equal true

// TypeScript complains if we compare two different constants
#if !FABLE_COMPILER_TYPESCRIPT
        Fruits.Apple = Fruits.Banana |> equal false
#endif
        let mutable f = fun a b ->
            a = Fruits.Banana |> equal false
            Fruits.Banana = a |> equal false
            a = b |> equal false
        f a b

    testCase "Enum operator <> works" <| fun () ->
        let a = Fruits.Apple
        let b = Fruits.Banana

        Fruits.Apple <> Fruits.Apple |> equal false
        a <> Fruits.Apple |> equal false
        Fruits.Apple <> a |> equal false
        a <> a |> equal false

// TypeScript complains if we compare two different constants
#if !FABLE_COMPILER_TYPESCRIPT
        Fruits.Apple <> Fruits.Banana |> equal true
#endif
        let mutable f = fun a b ->
            a <> Fruits.Banana |> equal true
            Fruits.Banana <> a |> equal true
            a <> b |> equal true
        f a b

    testCase "Enum operator < works" <| fun () ->
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

    testCase "Enum operator <= works" <| fun () ->
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

    testCase "Enum operator > works" <| fun () ->
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

    testCase "Enum operator >= works" <| fun () ->
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

    testCase "EnumOfValue works" <| fun () ->
        EnumOfValue 1 |> equal Fruits.Apple
        EnumOfValue 2 |> equal Fruits.Banana
        EnumOfValue 4 |> equal Fruits.Coconut

    testCase "EnumOfValue works in inlined functions" <| fun () -> // #1536
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

    testCase "EnumToValue works" <| fun () ->
        EnumToValue Fruits.Apple |> equal 1
        EnumToValue Fruits.Banana |> equal 2
        EnumToValue Fruits.Coconut |> equal 4

    testCase "Enum operator enum works" <| fun () ->
        enum 1 |> equal Fruits.Apple
        enum 2 |> equal Fruits.Banana
        enum 4 |> equal Fruits.Coconut

    testCase "Pattern matching can be nested within a switch statement" <| fun () -> // See #483
        let mutable f = fun fruit veggie ->
            match fruit with
            | Fruits.Apple ->
                match veggie with
                | Tomato kind -> kind.Replace("to","")
                | _ -> "foo"
            | Fruits.Banana
            | Fruits.Coconut ->
                match veggie with
                | Lettuce kind -> kind
                | _ -> "bar"
            | _ -> "invalid choice"
            |> equal "kuma"

        let fruit = Fruits.Apple
        let veggie = Tomato("kumato")
        f fruit veggie

    testCase "Non-scoped (in JS) variables with same name can be used" <| fun () -> // See #700
        equal 10 myRootValue

    // See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-395138456
    testCase "Multiple cases with same target don't create variable duplication" <| fun () ->
        let mutable test = fun arg ->
            match arg with
            | 0 | 1 | 2 | 3 ->
                let test2 x = x + x
                test2
            | _ -> id
        (test 1) 5 |> equal 10

    // See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-396456500
    testCase "Decision targets can be found" <| fun () ->
        let mutable test = fun x ->
            match x with
            | (4 | 5) as r -> r
            | _ -> 1
        test 2 |> equal 1

    // See https://github.com/fable-compiler/Fable/issues/1415#issuecomment-396457352
    testCase "Decision targets in nested pattern matching can be found" <| fun () ->
        let mutable test = fun x ->
            match x with
            | 1 -> 1
            | 2 | 3 | 4 | 5 ->
                match x with
                | (4 | 5) as r -> r
                | _ -> 1
            | _ -> 0
        test 2 |> equal 1

    testCase "Enum.GetValues works" <| fun () ->
        let check (ar: Array) =
            ar
            |> Seq.cast<sbyte>
            |> Seq.toList
            |> equal [1y; 5y; 8y]
        let t = typeof<MyEnum>
        Enum.GetValues(t) |> check
        t.GetEnumValues() |> check

    testCase "Enum.GetNames works" <| fun () ->
        let t = typeof<MyEnum>
        Enum.GetNames(t) |> equal [|"Foo"; "Bar"; "Baz"|]
        t.GetEnumNames() |> equal [|"Foo"; "Bar"; "Baz"|]

    testCase "Enum.GetName works" <| fun () ->
        let t = typeof<MyEnum>
        Enum.GetName(t, MyEnum.Foo) |> equal "Foo"
        Enum.GetName(t, box 5y) |> equal "Bar"
        Enum.GetName(t, MyEnum.Baz) |> equal "Baz"

    testCase "Enum.IsDefined works" <| fun () ->
        let t = typeof<MyEnum>
        Enum.IsDefined(t, "Foo") |> equal true
        Enum.IsDefined(t, "Baz") |> equal true
        Enum.IsDefined(t, "Ozu") |> equal false
        Enum.IsDefined(t, 5y) |> equal true
        Enum.IsDefined(t, 10y) |> equal false

    testCase "Enum.Parse works" <| fun () ->
        let t = typeof<MyEnum>
        Enum.Parse(t, "Foo") |> equal (box MyEnum.Foo)
        Enum.Parse(t, "Bar") |> equal (box MyEnum.Bar)
        Enum.Parse(t, "8") |> equal (box MyEnum.Baz)
        Enum.Parse<MyEnum>("Foo") |> equal (MyEnum.Foo)
        Enum.Parse<MyEnum>("Bar") |> equal (MyEnum.Bar)
        Enum.Parse<MyEnum>("8") |> equal (MyEnum.Baz)

    testCase "Enum.TryParse works" <| fun () ->
        Enum.TryParse<MyEnum>("Foo") |> equal (true, MyEnum.Foo)
        Enum.TryParse<MyEnum>("Bar") |> equal (true, MyEnum.Bar)
        Enum.TryParse<MyEnum>("Ozu") |> fst |> equal false
  ]