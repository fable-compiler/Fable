module QuickTest

// Operators
let add a b = a + b
let result = add 3 4

// IfThenElse
let max a b =
    if a > b then
        a
    else
        b

// Lambda as value
let applyFn f x = f x

// Pattern matching
let describe x =
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "other"

// Lists
let myList = [ 1; 2; 3 ]

// DU pattern matching
type Shape =
    | Circle of float
    | Square of float

let area shape =
    match shape with
    | Circle r -> 3.14159 * r * r
    | Square s -> s * s

printfn "Hello from BEAM!"
