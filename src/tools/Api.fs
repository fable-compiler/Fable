
namespace Api

type IAdder<'T> =
    abstract Add: 'T * 'T -> 'T

type ISquarer<'T> =
    abstract Square: 'T -> 'T

open Fable.Core
open Fable.Core.Experimental

type Helper =
    static member AddSquareAndPrint(x, y, [<Inject; Implicit>] ?adder: IAdder<'T>,
                                          [<Inject; Implicit>] ?squarer: ISquarer<'T>) =
        adder.Value.Add(x, y)
        |> squarer.Value.Square
        |> printfn "%A"
