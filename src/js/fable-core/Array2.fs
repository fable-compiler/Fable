module Array

open Fable.Core

type ArrayCons =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract From: 'T seq -> 'T array

let map (f: 'T->'U) (source: 'T[]) (cons: ArrayCons) =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]
    target
