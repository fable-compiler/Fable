module Array

open Fable.Core

type ArrayCons =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract From: 'T seq -> 'T array
