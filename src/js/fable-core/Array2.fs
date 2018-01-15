module Array

open Fable.Core

type ArrayCons =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract From: 'T seq -> 'T array

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f i source.[i]
    target

let map (f: 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    // TODO: Should we involve currying and re-use mapIndexed here?
    // Don't quite know yet how is it optimized so can't decide on performance vs code reuse.
    // mapIndexed (fun _ x -> f x) source

    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]
    target

let indexed (source: 'T[]) (cons: ArrayCons) =
    // TODO: How do we get concrete instance of ArrayCons for dynamic JS Arrays creation?
    // Passing as a parameter for now.
    source |> mapIndexed (fun i x -> i, x) <| cons;
