module Library

type SecondaryCons(x: int) =
    new () = SecondaryCons(5)
    member __.Value = x