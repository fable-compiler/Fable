module Common.Imports

type IHasAdd =
    abstract Add: x: int -> y: int -> int

type MyRecord = {
    a: int
}

module MyRecord =
    let create a = { a = a }

type MyUnion =
    | A of int
    | B of string

module MyUnion =
    let createA i =
        A i

type MyClass() =
    static member Mul x y = x * y
    member _.Sub x y = x - y
    interface IHasAdd with
        member _.Add x y = x + y

module Vectors =

    type Vector2<[<Measure>] 'u> =
        Vector2 of x: float<'u> * y: float<'u>
    with
        static member inline ( + ) (Vector2(ax, ay), Vector2(bx, by)) = Vector2(ax + bx, ay + by)
        static member inline ( * ) (scalar, Vector2(x, y)) = Vector2(scalar * x, scalar * y)

    [<Struct>]
    type Vector2R<[<Measure>] 'u> = {
        x: float32<'u>
        y: float32<'u>
    }
    with
        static member inline ( + ) (a, b) = { x = a.x + b.x; y = a.y + b.y }
