
let main () =
    let x = 1
    let y = 2
    let a = "hello world"
    x

// let hello () = "hello world"

[<Struct>]
type Simple1 = {
    X: int
    Y: int
}

let create y =
    { X = 1; Y = y}

let m () =
    let x = 1 + 1
    { X = x; Y = 2 }

let another x =
    let b = 2
    x + 1 + b

type Simple2 = {
    X: int
    Y: int
}

let addBoth a b =
    { X = a.X + b.X ; Y = a.Y + b.Y}
let forwardToAddBoth x =
    addBoth {X = 1; Y = 2} x
let addMore i a b =
    let first = a.X + b.X + i
    let second = a.Y + b.Y + i + first
    { X = first ; Y = second }
let condition1 x =
    if x.X = 1 then
        if x.Y > 3 then
            2
        else 4
    else 3

type DU =
    | A
    | B of int

let stuff () =
    let m = A
    let n = B 4
    n