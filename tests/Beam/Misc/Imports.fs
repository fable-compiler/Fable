module Fable.Tests.Imports

type IHasAdd =
    abstract Add: x: int -> y: int -> int

type MyClass() =
    static member Mul x y = x * y
    member _.Sub x y = x - y
    interface IHasAdd with
        member _.Add x y = x + y
