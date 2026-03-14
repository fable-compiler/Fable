module NamedFunction

let inline iterateTwice ([<InlineIfLambda>] action) (array: 'T[]) =
    for i = 0 to array.Length-1 do
        action array[i]
    for i = 0 to array.Length-1 do
        action array[i]

let arr = [| 1.. 100 |]
let mutable sum = 0

let addToSum x = sum <- sum + x
arr |> iterateTwice addToSum
