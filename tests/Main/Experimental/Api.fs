
namespace Api

type IAdder<'T> = 'T -> 'T -> 'T
type ISquarer<'T> = 'T -> 'T

open Fable.Core
open Fable.Core.Experimental

type Helper =
    static member AddAndSquare(x, y, [<Inject; Implicit>] ?adder: ('T -> 'T -> 'T),
                                     [<Inject; Implicit>] ?squarer: ('T -> 'T)) =
        adder.Value x y
        |> squarer.Value
