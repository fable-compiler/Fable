module App

let greeting = Library.greeting "world"
let doubled = Library.double 21
let plain = Library.plain 1
let circles = Library.circles [ Library.Circle 1.0; Library.Square 2.0 ]

type PlainValChild =
    inherit Library.PlainVal
    val M: int
    new () = { inherit Library.PlainVal(1); M = 2 }
