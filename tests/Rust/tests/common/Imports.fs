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
