namespace Fable.Core

type IGenericAdder<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T

type IGenericAverager<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T
    abstract DivideByInt: 'T * int -> 'T
