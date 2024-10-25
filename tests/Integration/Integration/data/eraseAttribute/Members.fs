module EraseAttributeMembers

open Fable.Core

type input() =

    [<Erase>]
    member this.onChange
        with set (_: obj -> unit) = ()


type input with

    [<Erase>]
    member this.onChange2 set (_: obj -> unit) = ()
