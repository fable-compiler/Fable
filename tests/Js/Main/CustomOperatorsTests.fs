module Fable.Tests.CustomOperators

open System
open Util.Testing

type Point =
    { x: float; y: float }
    static member (+) (p1: Point, p2: Point) = { x=p1.x + p2.x; y=p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x=p1.x - p2.x; y=p1.y - p2.y }
    static member inline (*) (p1: Point, p2: Point) = { x=p1.x * p2.x; y=p1.y * p2.y }

let inline genericAdd (x: ^a) (y: ^b): ^c = x + y

type MyRecord =
    { value: int }
    static member (+) (x: MyRecord, y: int) = { value = x.value + y }
    static member (+) (x: int, y: MyRecord) = x + y.value + 2

type CustomPow =
    { Ok: bool }
    static member Pow(x: CustomPow, n: int) = { Ok = true }

let typeOperators = [
    testCase "Custom operators with types work" <| fun () ->
        let p1 = { x=5.; y=10. }
        let p2 = { x=2.; y=1. }
        equal 7. (p1 + p2).x
        equal 9. (p1 - p2).y

    testCase "Inline custom operators with types work" <| fun () -> // See #230
        let p1 = { x=5.; y=10. }
        let p2 = { x=2.; y=1. }
        equal 10. (p1 * p2).x

    testCase "Overloads of a custom operators work" <| fun () ->
        let x = { value = 5 }
        x + 2 |> equal { value = 7 }
        3 + x |> equal 10

    testCase "Custom Pow works" <| fun () -> // See #2496
        let x = { Ok = false }
        x ** 2 |> equal { Ok = true }
]

let (+) (x: int) (y: int) = x * y

let (-) (x: int) (y: int) = x / y

let (||||) x y = x + y

let inline (>>) x y = x * y * 2

let moduleOperators = [
    testCase "Overloads of a custom operators can be inlined" <| fun () ->
        let x = { value = 5 }
        genericAdd 4 5 |> equal 9
        genericAdd x 2 |> equal { value = 7 }
        genericAdd 3 x |> equal 10

    testCase "Custom operators work" <| fun () ->
        5 + 5 |> equal 25
        10 - 2 |> equal 5
        2 |||| 2 |> equal 4

    testCase "Inline custom operators work" <| fun () ->
        5 >> 5 |> equal 50
]

let operatorsAsFunctions = [
    testCase "logical or" <| fun () ->
        let x = [true] |> List.fold (||) false
        x |> equal true
    testCase "logical and" <| fun () ->
        let x = [true] |> List.fold (&&) false
        x |> equal false
]

type [<Measure>] px
type [<Measure>] em

type D1 = D1
type D2 = D2
type D3 = D3

type ToLength = ToLength with
    static member (&.) (ToLength, x: int<px>) = fun D1 _ _ -> String.Join("", string x, "px")
    static member (&.) (ToLength, x: int<em>) = fun D1 D2 _ -> String.Join("", string x, "em")

let inline toLen x : string = (Unchecked.defaultof<ToLength> &. x) D1 D2 D3

let operatorsForOverloads = [
    testCase "first overload" <| fun () ->
        toLen 1<px>
        |> equal "1px"
    testCase "second overload" <| fun () ->
        toLen 1<em>
        |> equal "1em"
]

let tests =
  testList "Miscellaneous"
    (typeOperators @ moduleOperators @ operatorsAsFunctions @ operatorsForOverloads)
