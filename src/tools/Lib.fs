module Lib

type Foo =
    { x: int }
    member __.Z() = 5
    member this.Add(x, y) = x + y + this.Z()
    static member Add(x, y) = x - y