module Util

let reverse (s: string) =
    s
    |> Seq.rev
    |> Seq.map string
    |> String.concat ""

// References to core lib from subfolder work
let greet s =
    printfn "Hello %s!" s 