
namespace Api

type IAdder<'T> =
    abstract Add: 'T * 'T -> 'T

open Fable.Core
open Fable.Core.Experimental

type Helper =
    static member AddAndPrint(x, y, [<Inject; Implicit>] ?adder: IAdder<'T>) =
        adder.Value.Add(x, y) |> printfn "%A"
