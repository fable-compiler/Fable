namespace Fable.Core

type IArrayCons<'T> =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract FromSequence: 'T seq -> 'T array

type IGenericAdder<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T

type IGenericAverager<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T
    abstract DivideByInt: 'T * int -> 'T
