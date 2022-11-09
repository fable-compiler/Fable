module Fable.QuickTest

open System
open Fable.Core.Cy

type Account = {
    Name: string
    Balance: int
}

let b = { Name = "Jane"; Balance = 200 }

let add a b = a + b

let adder = Func<_,_,_>(add)

let test ([<Python>] a: int) b = a + b

let a = "test"

[<Python>]
let y = adder.Invoke(1, 2)