open Fable.Core

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
    | C of a: int * b: int

let stuff () =
    let m = A
    let n = B 4
    n

let matchstuff = function
    | A -> 0
    | B i -> i
    | C _ -> 1

[<Emit("assert($0)")>]
let assertTrue (x: bool) =
    nativeOnly

let add2Eq4 () =
    let res = 2 + 2
    assertTrue(res = 4)
    1

let main () =
    //add2Eq4()
    let a = "hello world"
    1
