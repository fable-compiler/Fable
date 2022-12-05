
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